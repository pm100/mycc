use crate::{codegen::MoiraGenerator, moira::MoiraProgram, tacky::TackyProgram};
use anyhow::Result;

use super::moira_inst::{BinaryOperator, Instruction, Operand, Register, UnaryOperator};
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

                self.moira
                    .add_instruction(Instruction::Mov(value, value1.clone()));

                let op = match unary_operator {
                    crate::tacky::UnaryOperator::Negate => UnaryOperator::Neg,
                    crate::tacky::UnaryOperator::Complement => UnaryOperator::Not,
                };
                self.moira.add_instruction(Instruction::Unary(op, value1));
            }
            tacky::Instruction::Binary(binary_operator, src1, src2, dest) => {
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
                        // tacky::BinaryOperator::ShiftLeft => {
                        //     self.moira.add_instruction(Instruction::Mov(
                        //         src1,
                        //         Operand::Register(Register::AX),
                        //     ));
                        //     self.moira.add_instruction(Instruction::Mov(
                        //         src2,
                        //         Operand::Register(Register::CL),
                        //     ));
                        //     self.moira.add_instruction(Instruction::Binary(
                        //         BinaryOperator::ShiftLeft,
                        //         Operand::Register(Register::CL),
                        //         Operand::Register(Register::AX),
                        //     ));
                        //     self.moira.add_instruction(Instruction::Mov(
                        //         Operand::Register(Register::AX),
                        //         dest,
                        //     ));
                        // }
                        // tacky::BinaryOperator::ShiftRight => {
                        //     self.moira.add_instruction(Instruction::Mov(
                        //         src1,
                        //         Operand::Register(Register::AX),
                        //     ));
                        //     self.moira.add_instruction(Instruction::Mov(
                        //         src2,
                        //         Operand::Register(Register::CL),
                        //     ));
                        //     self.moira.add_instruction(Instruction::Binary(
                        //         BinaryOperator::ShiftRight,
                        //         Operand::Register(Register::CL),
                        //         Operand::Register(Register::AX),
                        //     ));
                        //     self.moira.add_instruction(Instruction::Mov(
                        //         Operand::Register(Register::AX),
                        //         dest,
                        //     ));
                        // }
                        //  _ => {}
                        tacky::BinaryOperator::Divide | tacky::BinaryOperator::Remainder => {
                            self.moira.add_instruction(Instruction::Mov(
                                src1,
                                Operand::Register(Register::AX),
                            ));
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

                                _ => panic!("Unsupported binary operator"),
                            }
                        }
                        _ => {
                            panic!("Unsupported binary operator");
                        }
                    }
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
    // fn build(&mut self, source: &Path, output: &Path) -> Result<()> {
    //     crate::cpp::assemble_link(source, output)
    // }
}
