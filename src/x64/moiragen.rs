use crate::{codegen::MoiraGenerator, moira::MoiraProgram, tacky::TackyProgram};
use anyhow::Result;

use super::moira_inst::{BinaryOperator, CondCode, Instruction, Operand, Register, UnaryOperator};
pub struct X64MoiraGenerator {
    moira: MoiraProgram<Instruction>,
}

impl Default for X64MoiraGenerator {
    fn default() -> Self {
        Self::new()
    }
}
use crate::tacky;
impl X64MoiraGenerator {
    pub fn new() -> Self {
        Self {
            moira: MoiraProgram::new(),
        }
    }
    fn gen_function(&mut self, function: &crate::tacky::Function) -> Result<()> {
        for instruction in &function.instructions {
            self.gen_instruction(instruction)?;
        }
        Ok(())
    }
    fn gen_instruction(&mut self, instruction: &crate::tacky::Instruction) -> Result<()> {
        match instruction {
            tacky::Instruction::Return(value) => {
                let value = self.get_value(value);

                let minst = Instruction::Mov(value, Operand::Register(Register::AX));
                self.moira.add_instruction(minst);

                let instruction = Instruction::Ret;
                self.moira.add_instruction(instruction);
            }
            tacky::Instruction::Unary(unary_operator, value, value1) => {
                let value = self.get_value(value);
                let value1 = self.get_value(value1);

                let op = match unary_operator {
                    tacky::UnaryOperator::Negate => UnaryOperator::Neg,
                    tacky::UnaryOperator::Complement => UnaryOperator::Not,
                    tacky::UnaryOperator::LogicalNot => {
                        self.moira.add_instruction(Instruction::Cmp(
                            Operand::Immediate(0),
                            value.clone(),
                        ));
                        self.moira.add_instruction(Instruction::Mov(
                            Operand::Immediate(0),
                            value1.clone(),
                        ));
                        self.moira
                            .add_instruction(Instruction::SetCC(CondCode::E, value1.clone()));
                        return Ok(());
                    }
                };
                self.moira
                    .add_instruction(Instruction::Mov(value, value1.clone()));
                self.moira.add_instruction(Instruction::Unary(op, value1));
            }
            tacky::Instruction::Binary(binary_operator, src1, src2, dest) => {
                self.gen_binary(binary_operator, src1, src2, dest)?;
            }
            tacky::Instruction::Copy(src, dest) => {
                let src = self.get_value(src);
                let dest = self.get_value(dest);
                self.moira.add_instruction(Instruction::Mov(src, dest));
            }
            tacky::Instruction::Jump(label) => {
                self.moira.add_instruction(Instruction::Jmp(label.clone()));
            }
            tacky::Instruction::JumpIfZero(value, label) => {
                let value = self.get_value(value);
                self.moira
                    .add_instruction(Instruction::Cmp(Operand::Immediate(0), value.clone()));
                self.moira
                    .add_instruction(Instruction::JmpCC(CondCode::E, label.clone()));
            }
            tacky::Instruction::JumpIfNotZero(value, label) => {
                let value = self.get_value(value);
                self.moira
                    .add_instruction(Instruction::Cmp(Operand::Immediate(0), value.clone()));
                self.moira
                    .add_instruction(Instruction::JmpCC(CondCode::NE, label.clone()));
            }
            tacky::Instruction::Label(label) => {
                self.moira
                    .add_instruction(Instruction::Label(label.clone()));
            }
            tacky::Instruction::FunCall(name, args, dest) => {
                let dest = self.get_value(dest);

                //    self.moira
                //   .add_instruction(Instruction::Call(name.clone(), args.clone(), dest));
            }
        }
        Ok(())
    }
    fn gen_binary(
        &mut self,
        binary_operator: &tacky::BinaryOperator,
        src1: &tacky::Value,
        src2: &tacky::Value,
        dest: &tacky::Value,
    ) -> Result<()> {
        let src1 = self.get_value(src1);
        let src2 = self.get_value(src2);
        let dest = self.get_value(dest);

        let op = match binary_operator {
            tacky::BinaryOperator::Add => Some(BinaryOperator::Add),
            tacky::BinaryOperator::Subtract => Some(BinaryOperator::Sub),
            tacky::BinaryOperator::Multiply => Some(BinaryOperator::Mult),
            tacky::BinaryOperator::BitAnd => Some(BinaryOperator::BitAnd),
            tacky::BinaryOperator::BitOr => Some(BinaryOperator::BitOr),
            tacky::BinaryOperator::BitXor => Some(BinaryOperator::BitXor),
            tacky::BinaryOperator::ShiftLeft => Some(BinaryOperator::ShiftLeft),
            tacky::BinaryOperator::ShiftRight => Some(BinaryOperator::ShiftRight),
            _ => None,
        };

        if let Some(op) = op {
            self.moira
                .add_instruction(Instruction::Mov(src1, dest.clone()));
            self.moira
                .add_instruction(Instruction::Binary(op, src2, dest));
        } else {
            match binary_operator {
                tacky::BinaryOperator::Divide | tacky::BinaryOperator::Remainder => {
                    self.moira
                        .add_instruction(Instruction::Mov(src1, Operand::Register(Register::AX)));
                    self.moira.add_instruction(Instruction::Cdq);
                    self.moira.add_instruction(Instruction::Idiv(src2));

                    match binary_operator {
                        tacky::BinaryOperator::Divide => {
                            let instruction =
                                Instruction::Mov(Operand::Register(Register::AX), dest);
                            self.moira.add_instruction(instruction);
                        }
                        tacky::BinaryOperator::Remainder => {
                            let instruction =
                                Instruction::Mov(Operand::Register(Register::DX), dest);
                            self.moira.add_instruction(instruction);
                        }

                        _ => unreachable!(),
                    }
                }
                tacky::BinaryOperator::Equal
                | tacky::BinaryOperator::NotEqual
                | tacky::BinaryOperator::LessThan
                | tacky::BinaryOperator::LessThanOrEqual
                | tacky::BinaryOperator::GreaterThan
                | tacky::BinaryOperator::GreaterThanOrEqual => {
                    self.moira.add_instruction(Instruction::Cmp(src2, src1));
                    self.moira
                        .add_instruction(Instruction::Mov(Operand::Immediate(0), dest.clone()));
                    let cc = match binary_operator {
                        tacky::BinaryOperator::Equal => CondCode::E,
                        tacky::BinaryOperator::NotEqual => CondCode::NE,
                        tacky::BinaryOperator::LessThan => CondCode::L,
                        tacky::BinaryOperator::LessThanOrEqual => CondCode::LE,
                        tacky::BinaryOperator::GreaterThan => CondCode::G,
                        tacky::BinaryOperator::GreaterThanOrEqual => CondCode::GE,
                        _ => unreachable!(),
                    };
                    self.moira.add_instruction(Instruction::SetCC(cc, dest));
                }
                _ => {
                    unreachable!();
                }
            }
        }
        Ok(())
    }
    fn get_value(&self, value: &crate::tacky::Value) -> Operand {
        match value {
            tacky::Value::Int(value) => Operand::Immediate(*value),
            tacky::Value::Variable(register) => Operand::Pseudo(register.clone()),
        }
    }
}

impl MoiraGenerator for X64MoiraGenerator {
    type InstructionType = Instruction;
    fn generate_moira(&mut self, program: &TackyProgram) -> Result<&MoiraProgram<Instruction>> {
        for function in &program.functions {
            self.moira.gen_function(function)?;
            self.gen_function(function)?;
        }
        Ok(&self.moira)
    }
}
