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
            crate::tacky::Instruction::Return(value) => {
                let value = self.get_value(value);

                let minst = Instruction::Mov(value, Operand::Register(Register::AX));
                self.moira.add_instruction(minst);

                let instruction = Instruction::Ret;
                self.moira.add_instruction(instruction);
            }
            crate::tacky::Instruction::Unary(unary_operator, value, value1) => {
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
            crate::tacky::Instruction::Binary(binary_operator, src1, src2, dest) => {
                let src1 = self.get_value(src1);
                let src2 = self.get_value(src2);
                let dest = self.get_value(dest);

                let op = match binary_operator {
                    crate::tacky::BinaryOperator::Add => Some(BinaryOperator::Add),
                    crate::tacky::BinaryOperator::Subtract => Some(BinaryOperator::Sub),
                    crate::tacky::BinaryOperator::Multiply => Some(BinaryOperator::Mult),
                    _ => None,
                };
                //     crate::tacky::BinaryOperator::Divide => crate::moira::BinaryOperator::Div,
                //     crate::tacky::BinaryOperator::Remainder => crate::moira::BinaryOperator::Mod,
                // };
                if let Some(op) = op {
                    self.moira
                        .add_instruction(Instruction::Mov(src1, dest.clone()));
                    self.moira
                        .add_instruction(Instruction::Binary(op, src2, dest));
                } else {
                    self.moira
                        .add_instruction(Instruction::Mov(src1, Operand::Register(Register::AX)));
                    self.moira.add_instruction(Instruction::Cdq);
                    self.moira.add_instruction(Instruction::Idiv(src2));

                    match binary_operator {
                        crate::tacky::BinaryOperator::Divide => {
                            let instruction =
                                Instruction::Mov(Operand::Register(Register::AX), dest);
                            self.moira.add_instruction(instruction);
                        }
                        crate::tacky::BinaryOperator::Remainder => {
                            let instruction =
                                Instruction::Mov(Operand::Register(Register::DX), dest);
                            self.moira.add_instruction(instruction);
                        }
                        _ => panic!("Unsupported binary operator"),
                    }
                }
            }
        }
        Ok(())
    }

    fn get_value(&self, value: &crate::tacky::Value) -> Operand {
        match value {
            crate::tacky::Value::Int(value) => Operand::Immediate(*value),
            crate::tacky::Value::Variable(register) => Operand::Pseudo(register.clone()),
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
