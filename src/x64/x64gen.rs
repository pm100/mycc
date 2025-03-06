use crate::moira::{Function, MoiraProgram};

use crate::codegen::MoiraCompiler;
use anyhow::{bail, Result};
use std::collections::HashMap;
use std::io::Write;
use std::{io::BufWriter, path::Path};

use super::moira_inst::{BinaryOperator, Instruction, Operand, Register, UnaryOperator};
pub struct X64CodeGenerator {
    pseudo_registers: HashMap<String, i32>,
    next_offset: i32,
}

const SCRATCH_REGISTER1: Register = Register::R10;
const SCRATCH_REGISTER2: Register = Register::R11;

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
            Instruction::Binary(operator, left, right) => {
                let op = match operator {
                    BinaryOperator::Add => "add",
                    BinaryOperator::Sub => "sub",
                    BinaryOperator::Mult => "imul",
                };
                let left = self.get_operand(left)?;
                let right = self.get_operand(right)?;
                writeln!(writer, "        {} {}, {}", op, right, left)?;
            }
            Instruction::Idiv(divisor) => {
                let op = self.get_operand(divisor)?;
                writeln!(writer, "        idiv {}", op)?;
            }
            Instruction::Cdq => {
                writeln!(writer, "        cdq")?;
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
                Register::DX => "edx".to_string(),
                Register::R11 => "r11d".to_string(),
            }),
            Operand::Stack(offset) => Ok(format!("DWORD PTR[rbp-{}]", offset)),
        }
    }
    fn fixup_pass(&mut self, function: &Function<Instruction>) -> Result<Vec<Instruction>> {
        self.next_offset = 0;
        self.pseudo_registers.clear();
        let mut new_instructions = Vec::new();
        for instruction in function.instructions.iter() {
            match instruction {
                Instruction::AllocateStack(_) => {
                    todo!();
                }
                Instruction::Mov(src, dest) => {
                    let new_src = self.fix_pseudo(src)?;
                    let new_dest = self.fix_pseudo(dest)?;
                    if Self::is_stack(&new_src) && Self::is_stack(&new_dest) {
                        let scratch = Operand::Register(SCRATCH_REGISTER1);
                        new_instructions.push(Instruction::Mov(new_src, scratch.clone()));
                        new_instructions.push(Instruction::Mov(scratch, new_dest));
                    } else {
                        new_instructions.push(Instruction::Mov(new_src, new_dest));
                    }
                }
                Instruction::Binary(op, src, dest) => {
                    let new_src = self.fix_pseudo(src)?;
                    let new_dest = self.fix_pseudo(dest)?;

                    match op {
                        BinaryOperator::Add | BinaryOperator::Sub => {
                            if Self::is_stack(&new_src) && Self::is_stack(&new_dest) {
                                let scratch = Operand::Register(SCRATCH_REGISTER1);
                                new_instructions.push(Instruction::Mov(new_src, scratch.clone()));
                                new_instructions.push(Instruction::Binary(
                                    op.clone(),
                                    scratch,
                                    new_dest,
                                ));
                            } else {
                                new_instructions.push(Instruction::Binary(
                                    op.clone(),
                                    new_src,
                                    new_dest,
                                ));
                            }
                        }
                        BinaryOperator::Mult => {
                            if Self::is_stack(&new_dest) {
                                let scratch = Operand::Register(SCRATCH_REGISTER2);
                                new_instructions.push(Instruction::Mov(new_src, scratch.clone()));
                                new_instructions.push(Instruction::Binary(
                                    op.clone(),
                                    new_dest.clone(),
                                    scratch.clone(),
                                ));
                                new_instructions.push(Instruction::Mov(scratch, new_dest));
                            } else {
                                new_instructions.push(Instruction::Mov(new_src, new_dest));
                            }
                        }
                    }
                }

                Instruction::Unary(op, dest) => {
                    new_instructions.push(Instruction::Unary(op.clone(), self.fix_pseudo(dest)?));
                }
                Instruction::Idiv(operand) => {
                    if let Operand::Immediate(imm) = operand {
                        new_instructions.push(Instruction::Mov(
                            Operand::Immediate(*imm),
                            Operand::Register(SCRATCH_REGISTER1),
                        ));
                        new_instructions
                            .push(Instruction::Idiv(Operand::Register(SCRATCH_REGISTER1)));
                    } else {
                        new_instructions.push(Instruction::Idiv(self.fix_pseudo(operand)?));
                    }
                }
                _ => {
                    new_instructions.push(instruction.clone());
                }
            };
        }
        for instr in new_instructions.iter() {
            println!("{:?}", instr);
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
