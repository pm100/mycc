use crate::moira::{Function, MoiraProgram};

use crate::codegen::MoiraCompiler;
use anyhow::Result;
use std::collections::HashMap;
use std::io::Write;
use std::{io::BufWriter, path::Path};

use super::moira_inst::{BinaryOperator, CondCode, Instruction, Operand, Register, UnaryOperator};
pub struct X64CodeGenerator {
    pseudo_registers: HashMap<String, i32>,
    next_offset: i32,
    func_table: HashMap<String, FunctionDesc>,
}
#[derive(Debug, Clone)]
struct FunctionDesc {
    name: String,
    external: bool,
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
            func_table: HashMap::new(),
        }
    }

    fn gen_program(
        &mut self,
        moira: &MoiraProgram<Instruction>,
        writer: &mut BufWriter<std::fs::File>,
    ) -> Result<()> {
        // all the functions we define in this file
        self.func_table = moira
            .functions
            .iter()
            .map(|f| {
                (
                    f.name.clone(),
                    FunctionDesc {
                        name: f.name.clone(),

                        external: false,
                    },
                )
            })
            .collect::<HashMap<_, _>>();
        println!(
            "Generating x64 code for {:?} functions",
            self.func_table.len()
        );
        writeln!(writer, "bits 64")?;
        writeln!(writer, "default rel")?;
        self.gen_vars(moira, writer)?;
        writeln!(writer, "segment .text")?;
        for idx in 0..moira.functions.len() {
            let function = moira.functions[idx].clone();
            self.gen_function(&function, writer)?;
        }

        for name in self
            .func_table
            .iter()
            .filter(|(_, v)| v.external)
            .map(|(_, v)| v.name.clone())
        {
            writeln!(writer, "extern {}", name)?;
        }

        Ok(())
    }

    fn gen_vars(
        &mut self,
        moira: &MoiraProgram<Instruction>,
        writer: &mut BufWriter<std::fs::File>,
    ) -> Result<()> {
        for var in moira.top_vars.iter() {
            writeln!(writer, "segment .data")?;
            if var.external {
                writeln!(writer, "extern {}", var.name)?;
            } else {
                if var.global {
                    writeln!(writer, "global {}", var.name)?;
                }
                writeln!(writer, "{} dd {}", var.name, var.value)?;
            }
        }
        Ok(())
    }
    fn gen_function(
        &mut self,
        function: &Function<Instruction>,
        writer: &mut BufWriter<std::fs::File>,
    ) -> Result<()> {
        let fixed_instructions = self.fixup_pass(function)?;

        self.gen_prologue(function, writer)?;
        for instruction in fixed_instructions.iter() {
            self.gen_instruction(instruction, writer)?;
        }

        Ok(())
    }
    fn gen_prologue(
        &mut self,
        //    moira: &mut MoiraProgram<Instruction>,
        function: &Function<Instruction>,
        writer: &mut BufWriter<std::fs::File>,
    ) -> Result<()> {
        println!("Generating prologue for function: {}", function.name);

        if function.global {
            writeln!(writer, "global {}", function.name)?;
        }

        writeln!(writer, "{}: ", function.name)?;
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
                let dest_str = self.get_operand(dest, 4)?;
                let src_str = self.get_operand(
                    src,
                    if dest_str == "cl" && Self::is_memory(src) {
                        1
                    } else {
                        4
                    },
                )?;
                writeln!(writer, "        mov {}, {}", dest_str, src_str)?;
            }
            Instruction::Unary(operator, operand) => {
                let oper = match operator {
                    UnaryOperator::Neg => "neg",
                    UnaryOperator::Not => "not",
                };
                let op = self.get_operand(operand, 4)?;
                writeln!(writer, "        {} {}", oper, op)?;
            }
            Instruction::Binary(operator, left, right) => {
                let op = match operator {
                    BinaryOperator::Add => "add",
                    BinaryOperator::Sub => "sub",
                    BinaryOperator::Mult => "imul",
                    BinaryOperator::BitAnd => "and",
                    BinaryOperator::BitOr => "or",
                    BinaryOperator::BitXor => "xor",
                    BinaryOperator::ShiftLeft => "sal",
                    BinaryOperator::ShiftRight => "sar",
                };
                let left_str = self.get_operand(left, 4)?;
                let right_str = self.get_operand(right, 4)?;
                writeln!(writer, "        {} {}, {}", op, right_str, left_str)?;
            }
            Instruction::Idiv(divisor) => {
                let op = self.get_operand(divisor, 4)?;
                writeln!(writer, "        idiv {}", op)?;
            }
            Instruction::Cdq => {
                writeln!(writer, "        cdq")?;
            }
            Instruction::Cmp(left, right) => {
                let left_str = self.get_operand(left, 4)?;
                let right_str = self.get_operand(right, 4)?;
                writeln!(writer, "        cmp {}, {}", right_str, left_str)?;
            }
            Instruction::Jmp(label) => {
                writeln!(writer, "        jmp {}", label)?;
            }
            Instruction::JmpCC(cond, label) => {
                writeln!(writer, "        j{} {}", Self::translate_cc(cond), label)?;
            }
            Instruction::SetCC(cond, dest) => {
                let dest_str = self.get_operand(dest, 1)?;

                writeln!(
                    writer,
                    "        set{} {}",
                    Self::translate_cc(cond),
                    dest_str
                )?;
            }
            Instruction::Label(label) => {
                writeln!(writer, "{}:", label)?;
            }
            Instruction::Call(name) => {
                writeln!(writer, "        call {}", name)?;
            }
            Instruction::Push(operand) => {
                let operand_str = self.get_operand(operand, 8)?;
                writeln!(writer, "        push {}", operand_str)?;
            }
            Instruction::DeallocateStack(size) => {
                writeln!(writer, "        add rsp, {}", size)?;
            }
            Instruction::AllocateStack(size) => {
                writeln!(writer, "        sub rsp, {}", size)?;
            }
        }
        Ok(())
    }
    fn get_operand(&mut self, value: &Operand, opsize: usize) -> Result<String> {
        let val_str = match value {
            Operand::Immediate(value) => value.to_string(),
            Operand::Pseudo(_) => panic!("Pseudo register not supported"),
            Operand::Register(register) => match register {
                Register::AX => "eax".to_string(),
                Register::R10 => "r10d".to_string(),
                Register::DX => "edx".to_string(),
                Register::R11 => "r11d".to_string(),
                Register::CL => "cl".to_string(),
                Register::R8 => "r8d".to_string(),
                Register::R9 => "r9d".to_string(),
                Register::RDX => "edx".to_string(),
                Register::RCX => "ecx".to_string(),
                Register::RAX => "rax".to_string(),
            },
            Operand::Stack(offset) => {
                let qual = match opsize {
                    1 => "BYTE",
                    2 => "WORD",
                    4 => "DWORD",
                    _ => "QWORD",
                };
                format!("{} [rbp{:+}]", qual, offset)
            }
            Operand::Data(data) => {
                let qual = match opsize {
                    1 => "BYTE",
                    2 => "WORD",
                    4 => "DWORD",
                    _ => "QWORD",
                };
                format!("{} [{}]", qual, data)
            }
        };
        Ok(val_str)
    }

    fn fixup_pass(&mut self, function: &Function<Instruction>) -> Result<Vec<Instruction>> {
        self.next_offset = 0;
        self.pseudo_registers.clear();
        let mut new_instructions = Vec::new();
        for instruction in function.instructions.iter() {
            match instruction {
                Instruction::Mov(src, dest) => {
                    let new_src = self.fix_pseudo(src)?;
                    let new_dest = self.fix_pseudo(dest)?;
                    if Self::is_memory(&new_src) && Self::is_memory(&new_dest) {
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
                        BinaryOperator::Add
                        | BinaryOperator::Sub
                        | BinaryOperator::BitAnd
                        | BinaryOperator::BitOr
                        | BinaryOperator::BitXor => {
                            if Self::is_memory(&new_src) && Self::is_memory(&new_dest) {
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
                            if Self::is_memory(&new_dest) {
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
                        BinaryOperator::ShiftLeft | BinaryOperator::ShiftRight => {
                            if Self::is_memory(&new_dest) {
                                let scratch = Operand::Register(Register::CL);
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
                        } // _ => todo!(),
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
                Instruction::Cmp(src, dest) => {
                    let new_dest = self.fix_pseudo(dest)?;
                    let new_src = self.fix_pseudo(src)?;
                    if Self::is_constant(&new_dest) {
                        let scratch = Operand::Register(SCRATCH_REGISTER2);
                        new_instructions.push(Instruction::Mov(new_dest, scratch.clone()));
                        new_instructions.push(Instruction::Cmp(new_src.clone(), scratch.clone()));
                    } else if Self::is_memory(&new_dest) && Self::is_memory(&new_src) {
                        let scratch = Operand::Register(SCRATCH_REGISTER1);
                        new_instructions.push(Instruction::Mov(new_dest, scratch.clone()));
                        new_instructions.push(Instruction::Cmp(new_src.clone(), scratch.clone()));
                    } else {
                        new_instructions.push(Instruction::Cmp(new_src, new_dest));
                    }
                }
                Instruction::SetCC(cond, dest) => {
                    new_instructions.push(Instruction::SetCC(cond.clone(), self.fix_pseudo(dest)?));
                }
                Instruction::Push(operand) => {
                    new_instructions.push(Instruction::Push(self.fix_pseudo(operand)?));
                }
                Instruction::Call(name) => {
                    self.func_table
                        .entry(name.clone())
                        .or_insert_with(|| FunctionDesc {
                            name: name.clone(),

                            external: true,
                        });
                    new_instructions.push(Instruction::Call(name.clone()));
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
    fn is_memory(operand: &Operand) -> bool {
        matches!(operand, Operand::Stack(_)) || matches!(operand, Operand::Data(_))
    }
    fn is_constant(operand: &Operand) -> bool {
        matches!(operand, Operand::Immediate(_))
    }
    fn translate_cc(cc: &CondCode) -> String {
        match cc {
            CondCode::E => "e",
            CondCode::NE => "ne",
            CondCode::G => "g",
            CondCode::GE => "ge",
            CondCode::L => "l",
            CondCode::LE => "le",
        }
        .to_string()
    }
    fn _get_operand_size(&self, operand: &Operand) -> i32 {
        match operand {
            Operand::Immediate(_) => 4,
            Operand::Register(Register::CL) => 1,
            Operand::Register(_) => 4,
            Operand::Pseudo(_) => 4,
            Operand::Stack(_) => 4,
            Operand::Data(_) => 4,
        }
    }
    fn fix_pseudo(&mut self, operand: &Operand) -> Result<Operand> {
        if let Operand::Pseudo(pseudo_name) = operand {
            assert!(pseudo_name.contains('$'));
            let offset = if let Some(offset) = self.pseudo_registers.get(pseudo_name) {
                *offset
            } else {
                self.next_offset += 4;
                self.pseudo_registers
                    .insert(pseudo_name.to_string(), self.next_offset);
                self.next_offset
            };
            return Ok(Operand::Stack(-offset));
        }
        Ok(operand.clone())
    }
}
