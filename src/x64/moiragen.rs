use std::cmp::max;

use crate::{
    codegen::MoiraGenerator,
    moira::{MoiraProgram, StaticVariable},
    tacky::TackyProgram,
};
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
    fn moira(&mut self, inst: Instruction) {
        self.moira.add_instruction(inst);
    }
    fn gen_function(&mut self, function: &crate::tacky::Function) -> Result<()> {
        let reglist: [Register; 4] = [Register::RCX, Register::RDX, Register::R8, Register::R9];

        // let mut argoff = 0;
        for idx in 0..function.parameters.len() {
            if idx < reglist.len() {
                let param = &function.parameters[idx];
                self.moira(Instruction::Mov(
                    Operand::Register(reglist[idx].clone()),
                    Operand::Pseudo(param.clone()),
                ));
            } else {
                let param = &function.parameters[(function.parameters.len() - idx) + 3];
                self.moira(Instruction::Mov(
                    Operand::Stack(((function.parameters.len() - idx + 5) * 8) as i32),
                    Operand::Pseudo(param.clone()),
                ));
            }
        }
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
                let reglist: [Register; 4] =
                    [Register::RCX, Register::RDX, Register::R8, Register::R9];
                let stack_delta = max(args.len(), 4) as i32 * 8;

                for idx in 0..args.len() {
                    if idx < reglist.len() {
                        let arg = &args[idx];
                        let argval = self.get_value(arg);
                        self.moira(Instruction::Mov(
                            argval,
                            Operand::Register(reglist[idx].clone()),
                        ));
                    } else {
                        let arg = &args[(args.len() - idx) + 3];
                        let argval = self.get_value(arg);
                        match argval {
                            Operand::Immediate(v) => {
                                self.moira(Instruction::Push(Operand::Immediate(v)))
                            }

                            Operand::Pseudo(v) => {
                                self.moira(Instruction::Mov(
                                    Operand::Pseudo(v.clone()),
                                    Operand::Register(Register::AX),
                                ));
                                self.moira(Instruction::Push(Operand::Register(Register::RAX)));
                            }
                            Operand::Data(v) => {
                                self.moira(Instruction::Mov(
                                    Operand::Data(v.clone()),
                                    Operand::Register(Register::AX),
                                ));
                                self.moira(Instruction::Push(Operand::Register(Register::RAX)));
                            }
                            _ => panic!("Invalid Operand"),
                        }
                    }
                }
                self.moira(Instruction::AllocateStack(32));
                self.moira(Instruction::Call(name.clone()));
                self.moira(Instruction::DeallocateStack(stack_delta));
                self.moira(Instruction::Mov(Operand::Register(Register::AX), dest));
            }
            _ => todo!(),
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
            tacky::Value::Variable(register) => {
                if self.moira.top_vars.iter().any(|v| v.name == *register) {
                    Operand::Data(register.clone())
                } else {
                    Operand::Pseudo(register.clone())
                }
            }
            tacky::Value::Long(value) => todo!(), //Operand::Immediate(*value as i32),
        }
    }
}

impl MoiraGenerator for X64MoiraGenerator {
    type InstructionType = Instruction;
    fn generate_moira(&mut self, program: &TackyProgram) -> Result<&MoiraProgram<Instruction>> {
        for data in program.static_variables.values() {
            let v = match data.value {
                Some(tacky::Value::Int(value)) => value,

                None => 0,
                _ => panic!("Invalid static variable value"),
            };
            self.moira.top_vars.push(StaticVariable {
                name: data.name.clone(),
                global: data.global,
                value: v,
                external: data.external,
            });
        }
        for function in &program.functions {
            self.moira.gen_function(function)?;
            self.gen_function(function)?;
        }
        Ok(&self.moira)
    }
}
