
use crate::{codegen::MoiraGenerator, moira::MoiraProgram, tacky::TackyProgram};
use anyhow::Result;

use super::moira_inst::{Instruction, Operand, Register, UnaryOperator};
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
