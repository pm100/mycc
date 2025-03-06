use crate::moira::{Function, MoiraProgram};

use crate::codegen::MoiraCompiler;
use anyhow::{bail, Result};
use std::collections::HashMap;
use std::io::Write;
use std::{io::BufWriter, path::Path};

use super::moira_inst::{Instruction, Operand, Register, UnaryOperator};
pub struct X64CodeGenerator {
    pseudo_registers: HashMap<String, i32>,
    next_offset: i32,
}

const SCRATCH_REGISTER: Register = Register::R10;

impl MoiraCompiler for X64CodeGenerator {
    type InstructionType = Instruction;
    fn generate_asm(&mut self, moira: &MoiraProgram<Instruction>, file: &Path) -> Result<()> {
        let mut writer = BufWriter::new(std::fs::File::create(file)?);
        self.gen_program(moira, &mut writer)?;
        Ok(())
    }
}

impl Default for X64CodeGenerator {
    fn default() -> Self {
        Self::new()
    }
}

impl X64CodeGenerator {
    pub fn new() -> Self {
        Self {
            pseudo_registers: HashMap::new(),
            next_offset: 0,
        }
    }

    fn gen_program(
        &mut self,
        moira: &MoiraProgram<Instruction>,
        writer: &mut BufWriter<std::fs::File>,
    ) -> Result<()> {
        writeln!(writer, "INCLUDELIB LIBCMT")?;
        writeln!(writer, "_TEXT   SEGMENT")?;
        for idx in 0..moira.functions.len() {
            let function = moira.functions[idx].clone();
            self.gen_function(&function, writer)?;
        }
        writeln!(writer, "_TEXT   ENDS")?;
        writeln!(writer, "END")?;
        Ok(())
    }
    fn gen_function(
        &mut self,
        function: &Function<Instruction>,
        writer: &mut BufWriter<std::fs::File>,
    ) -> Result<()> {
        let fixed_iinstructions = self.fixup_pass(function)?;

        self.gen_prologue(function, writer)?;
        for instruction in fixed_iinstructions.iter() {
            self.gen_instruction(instruction, writer)?;
        }

        writeln!(writer, "{} ENDP", function.name)?;

        Ok(())
    }
    fn gen_prologue(
        &mut self,
        //    moira: &mut MoiraProgram<Instruction>,
        function: &Function<Instruction>,
        writer: &mut BufWriter<std::fs::File>,
    ) -> Result<()> {
        writeln!(writer, "PUBLIC {}", function.name)?;
        writeln!(writer, "{} PROC", function.name)?;
        writeln!(writer, "        push rbp")?;
        writeln!(writer, "        mov rbp, rsp")?;
        writeln!(writer, "        sub rsp, {}", self.next_offset)?;
        Ok(())
    }

    fn gen_instruction(
        &mut self,
        instruction: &Instruction,
        writer: &mut BufWriter<std::fs::File>,
    ) -> Result<()> {
        match instruction {
            Instruction::Ret => {
                writeln!(writer, "        mov rsp, rbp")?;
                writeln!(writer, "        pop rbp")?;
                writeln!(writer, "        ret")?;
            }
            Instruction::Mov(src, dest) => {
                let opsrc = self.get_operand(src)?;
                let opdest = self.get_operand(dest)?;
                writeln!(writer, "        mov {}, {}", opdest, opsrc)?;
            }
            Instruction::Unary(operator, operand) => {
                let oper = match operator {
                    UnaryOperator::Neg => "neg",
                    UnaryOperator::Not => "not",
                };
                let op = self.get_operand(operand)?;
                writeln!(writer, "        {} {}", oper, op)?;
            }
            _ => bail!("Unsupported instruction"),
        }
        Ok(())
    }
    fn get_operand(&mut self, value: &Operand) -> Result<String> {
        match value {
            Operand::Immediate(value) => Ok(value.to_string()),
            Operand::Pseudo(_) => panic!("Pseudo register not supported"),
            Operand::Register(register) => Ok(match register {
                Register::AX => "eax".to_string(),
                Register::R10 => "r10d".to_string(),
            }),
            Operand::Stack(offset) => Ok(format!("DWORD PTR[rbp-{}]", offset)),
        }
    }
    fn fixup_pass(&mut self, function: &Function<Instruction>) -> Result<Vec<Instruction>> {
        self.next_offset = 0;
        self.pseudo_registers.clear();
        let mut new_instructions = Vec::new();
        for instruction in function.instructions.iter() {
            let new_ins = match instruction {
                Instruction::AllocateStack(_) => {
                    todo!();
                }
                Instruction::Mov(src, dest) => {
                    let new_src = self.fix_pseudo(src)?;
                    let new_dest = self.fix_pseudo(dest)?;
                    if Self::is_stack(&new_src) && Self::is_stack(&new_dest) {
                        let scratch = Operand::Register(SCRATCH_REGISTER);
                        let load = Instruction::Mov(new_src, scratch.clone());
                        let store = Instruction::Mov(scratch, new_dest);
                        (load, Some(store))
                    } else {
                        (Instruction::Mov(new_src, new_dest), None)
                    }
                }
                Instruction::Unary(op, dest) => {
                    (Instruction::Unary(op.clone(), self.fix_pseudo(dest)?), None)
                }

                _ => (instruction.clone(), None),
            };
            println!("new_ins: {:?}", new_ins.0);
            new_instructions.push(new_ins.0);
            if let Some(ins) = new_ins.1 {
                println!("new_ins: {:?}", ins);
                new_instructions.push(ins);
            }
        }

        Ok(new_instructions)
    }
    fn is_stack(operand: &Operand) -> bool {
        matches!(operand, Operand::Stack(_))
    }
    fn fix_pseudo(&mut self, operand: &Operand) -> Result<Operand> {
        if let Operand::Pseudo(pseudo_name) = operand {
            let offset = if let Some(offset) = self.pseudo_registers.get(pseudo_name) {
                *offset
            } else {
                self.next_offset += 4;
                self.pseudo_registers
                    .insert(pseudo_name.to_string(), self.next_offset);
                self.next_offset
            };
            return Ok(Operand::Stack(offset));
        }
        Ok(operand.clone())
    }
}
