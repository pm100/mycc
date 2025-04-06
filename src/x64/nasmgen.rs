use crate::moira::{Function, MoiraProgram};

use crate::codegen::MoiraCompiler;
use crate::tacky::StaticInit;
use anyhow::Result;
use std::collections::HashMap;
use std::io::Write;
use std::{io::BufWriter, path::Path};

use super::moira_inst::{
    AssemblyType, BinaryOperator, CondCode, Instruction, Operand, Register, UnaryOperator,
};
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
                match var.value {
                    StaticInit::InitI32(value) => writeln!(writer, "{} dd {}", var.name, value)?,
                    StaticInit::InitI64(value) => writeln!(writer, "{} dq {}", var.name, value)?,
                    StaticInit::InitNone => {
                        let size = match var.stype {
                            crate::tacky::SymbolType::Int32 => "dd",
                            crate::tacky::SymbolType::Int64 => "dq",
                            _ => unreachable!("Unsupported type for static variable"),
                        };
                        writeln!(writer, "{} {} 0", var.name, size)?
                    }
                }
                //                writeln!(writer, "{} dd {}", var.name, var.value)?;
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
            Instruction::Mov(assembly_type, src, dest) => {
                let dest_str = self.get_operand(dest, &assembly_type)?;
                let src_str = self.get_operand(
                    src,
                    if dest_str == "cl" && Self::is_memory(src) {
                        &AssemblyType::Byte
                    } else {
                        &assembly_type
                    },
                )?;
                writeln!(writer, "        mov {}, {}", dest_str, src_str)?;
            }
            Instruction::Unary(operator, assembly_type, operand) => {
                let oper = match operator {
                    UnaryOperator::Neg => "neg",
                    UnaryOperator::Not => "not",
                };
                let op = self.get_operand(operand, &assembly_type)?;
                writeln!(writer, "        {} {}", oper, op)?;
            }
            Instruction::Binary(operator, assembly_type, left, right) => {
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
                let left_str = self.get_operand(left, &assembly_type)?;
                let right_str = self.get_operand(right, &assembly_type)?;
                writeln!(writer, "        {} {}, {}", op, right_str, left_str)?;
            }
            Instruction::Idiv(assembly_type, divisor) => {
                let op = self.get_operand(divisor, &assembly_type)?;
                writeln!(writer, "        idiv {}", op)?;
            }
            Instruction::Cdq(assembly_type) => {
                match assembly_type {
                    AssemblyType::LongWord => writeln!(writer, "        cdq")?,
                    AssemblyType::QuadWord => writeln!(writer, "        cqo")?,
                    _ => panic!("Invalid assembly type for cdq"),
                };
            }
            Instruction::Cmp(assembly_type, left, right) => {
                let left_str = self.get_operand(left, &assembly_type)?;
                let right_str = self.get_operand(right, &assembly_type)?;
                writeln!(writer, "        cmp {}, {}", right_str, left_str)?;
            }
            Instruction::Jmp(label) => {
                writeln!(writer, "        jmp {}", label)?;
            }
            Instruction::JmpCC(cond, label) => {
                writeln!(writer, "        j{} {}", Self::translate_cc(cond), label)?;
            }
            Instruction::SetCC(cond, dest) => {
                let dest_str = self.get_operand(dest, &AssemblyType::Byte)?;

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
                let operand_str = self.get_operand(operand, &AssemblyType::QuadWord)?;
                writeln!(writer, "        push {}", operand_str)?;
            }
            Instruction::DeallocateStack(size) => {
                writeln!(writer, "        add rsp, {}", size)?;
            }
            Instruction::AllocateStack(size) => {
                writeln!(writer, "        sub rsp, {}", size)?;
            }
            Instruction::SignExtend(src, dest) => {
                let src_str = self.get_operand(src, &AssemblyType::LongWord)?;
                let dest_str = self.get_operand(dest, &AssemblyType::QuadWord)?;
                writeln!(writer, "        movsxd {}, {}", dest_str, src_str)?;
            } // Instruction::Truncate(src, dest) => {
              //     let src_str = self.get_operand(src, &AssemblyType::QuadWord)?;
              //     let dest_str = self.get_operand(dest, &AssemblyType::LongWord)?;
              //     writeln!(writer, "        movzx {}, {}", dest_str, src_str)?;
              // }
        }
        Ok(())
    }

    fn get_reg_name(register: &Register, assembly_type: &AssemblyType) -> String {
        match register {
            Register::RAX => if *assembly_type == AssemblyType::LongWord {
                "eax"
            } else {
                "rax"
            }
            .to_string(),
            Register::RDX => if *assembly_type == AssemblyType::LongWord {
                "edx"
            } else {
                "rdx"
            }
            .to_string(),
            Register::RCX => if *assembly_type == AssemblyType::LongWord {
                "ecx"
            } else {
                "rcx"
            }
            .to_string(),
            Register::R8 => if *assembly_type == AssemblyType::LongWord {
                "r8d"
            } else {
                "r8"
            }
            .to_string(),
            Register::R9 => if *assembly_type == AssemblyType::LongWord {
                "r9d"
            } else {
                "r9"
            }
            .to_string(),
            Register::R10 => if *assembly_type == AssemblyType::LongWord {
                "r10d"
            } else {
                "r10"
            }
            .to_string(),
            Register::R11 => if *assembly_type == AssemblyType::LongWord {
                "r11d"
            } else {
                "r11"
            }
            .to_string(),
            Register::CL => if *assembly_type == AssemblyType::LongWord {
                "cl"
            } else {
                "cl"
            }
            .to_string(),
        }
    }
    fn get_operand(&mut self, value: &Operand, assembly_type: &AssemblyType) -> Result<String> {
        let val_str = match value {
            Operand::ImmediateI32(value) => value.to_string(),
            Operand::ImmediateI64(value) => value.to_string(),
            Operand::Pseudo(_) => panic!("Pseudo register not supported"),
            Operand::Register(register) => Self::get_reg_name(register, assembly_type),

            Operand::Stack(offset) => {
                let qual = match assembly_type {
                    AssemblyType::LongWord => "DWORD",
                    AssemblyType::QuadWord => "QWORD",
                    AssemblyType::Byte => "BYTE",
                    AssemblyType::Word => "WORD",
                };

                format!("{} [rbp{:+}]", qual, offset)
            }
            Operand::Data(data) => {
                let qual = match assembly_type {
                    AssemblyType::LongWord => "DWORD",
                    AssemblyType::QuadWord => "QWORD",
                    AssemblyType::Byte => "BYTE",
                    AssemblyType::Word => "WORD",
                };
                format!("{} [{}]", qual, data)
            }
        };
        Ok(val_str)
    }
    fn fix_long_immediate(
        &mut self,
        operand: &Operand,
        instructions: &mut Vec<Instruction>,
    ) -> Operand {
        match operand {
            Operand::ImmediateI64(val) => {
                if *val > i32::MAX as i64 || *val < i32::MIN as i64 {
                    instructions.push(Instruction::Mov(
                        AssemblyType::QuadWord,
                        operand.clone(),
                        Operand::Register(SCRATCH_REGISTER1),
                    ));
                    Operand::Register(SCRATCH_REGISTER1)
                } else {
                    operand.clone()
                }
            }
            _ => operand.clone(),
        }
    }
    fn fixup_pass(&mut self, function: &Function<Instruction>) -> Result<Vec<Instruction>> {
        self.next_offset = 0;
        self.pseudo_registers.clear();
        let mut new_instructions = Vec::new();
        for instruction in function.instructions.iter() {
            match instruction {
                Instruction::Mov(assembly_type, src, dest) => {
                    let new_src = self.fix_pseudo(src, &assembly_type)?;
                    let new_dest = self.fix_pseudo(dest, &assembly_type)?;
                    let new_src = self.fix_long_immediate(&new_src, &mut new_instructions);
                    if Self::is_memory(&new_src) && Self::is_memory(&new_dest) {
                        let scratch = Operand::Register(SCRATCH_REGISTER1);
                        new_instructions.push(Instruction::Mov(
                            assembly_type.clone(),
                            new_src,
                            scratch.clone(),
                        ));
                        new_instructions.push(Instruction::Mov(
                            assembly_type.clone(),
                            scratch,
                            new_dest,
                        ));
                    } else {
                        new_instructions.push(Instruction::Mov(
                            assembly_type.clone(),
                            new_src,
                            new_dest,
                        ));
                    }
                }
                Instruction::Binary(op, assembly_type, src, dest) => {
                    let new_src = self.fix_pseudo(src, &assembly_type)?;
                    let new_dest = self.fix_pseudo(dest, &assembly_type)?;
                    let new_src = self.fix_long_immediate(&new_src, &mut new_instructions);
                    match op {
                        BinaryOperator::Add
                        | BinaryOperator::Sub
                        | BinaryOperator::BitAnd
                        | BinaryOperator::BitOr
                        | BinaryOperator::BitXor => {
                            if Self::is_memory(&new_src) && Self::is_memory(&new_dest) {
                                let scratch = Operand::Register(SCRATCH_REGISTER1);
                                new_instructions.push(Instruction::Mov(
                                    assembly_type.clone(),
                                    new_src,
                                    scratch.clone(),
                                ));
                                new_instructions.push(Instruction::Binary(
                                    op.clone(),
                                    assembly_type.clone(),
                                    scratch,
                                    new_dest,
                                ));
                            } else {
                                new_instructions.push(Instruction::Binary(
                                    op.clone(),
                                    assembly_type.clone(),
                                    new_src,
                                    new_dest,
                                ));
                            }
                        }
                        BinaryOperator::Mult => {
                            if Self::is_memory(&new_dest) {
                                let scratch = Operand::Register(SCRATCH_REGISTER2);
                                new_instructions.push(Instruction::Mov(
                                    assembly_type.clone(),
                                    new_src,
                                    scratch.clone(),
                                ));
                                new_instructions.push(Instruction::Binary(
                                    op.clone(),
                                    assembly_type.clone(),
                                    new_dest.clone(),
                                    scratch.clone(),
                                ));
                                new_instructions.push(Instruction::Mov(
                                    assembly_type.clone(),
                                    scratch,
                                    new_dest,
                                ));
                            } else {
                                new_instructions.push(Instruction::Mov(
                                    assembly_type.clone(),
                                    new_src,
                                    new_dest,
                                ));
                            }
                        }
                        BinaryOperator::ShiftLeft | BinaryOperator::ShiftRight => {
                            if Self::is_memory(&new_dest) {
                                let scratch = Operand::Register(Register::CL);
                                new_instructions.push(Instruction::Mov(
                                    assembly_type.clone(),
                                    new_src,
                                    scratch.clone(),
                                ));
                                new_instructions.push(Instruction::Binary(
                                    op.clone(),
                                    assembly_type.clone(),
                                    scratch,
                                    new_dest,
                                ));
                            } else {
                                new_instructions.push(Instruction::Binary(
                                    op.clone(),
                                    assembly_type.clone(),
                                    new_src,
                                    new_dest,
                                ));
                            }
                        } // _ => todo!(),
                    }
                }

                Instruction::Unary(op, assembly_type, dest) => {
                    new_instructions.push(Instruction::Unary(
                        op.clone(),
                        assembly_type.clone(),
                        self.fix_pseudo(dest, &assembly_type)?,
                    ));
                }
                Instruction::Idiv(assembly_type, operand) => match operand {
                    Operand::ImmediateI32(imm) => {
                        new_instructions.push(Instruction::Mov(
                            assembly_type.clone(),
                            Operand::ImmediateI32(*imm),
                            Operand::Register(SCRATCH_REGISTER1),
                        ));
                        new_instructions.push(Instruction::Idiv(
                            assembly_type.clone(),
                            Operand::Register(SCRATCH_REGISTER1),
                        ));
                    }
                    Operand::ImmediateI64(imm) => {
                        new_instructions.push(Instruction::Mov(
                            assembly_type.clone(),
                            Operand::ImmediateI64(*imm),
                            Operand::Register(SCRATCH_REGISTER1),
                        ));
                        new_instructions.push(Instruction::Idiv(
                            assembly_type.clone(),
                            Operand::Register(SCRATCH_REGISTER1),
                        ));
                    }
                    _ => {
                        new_instructions.push(Instruction::Idiv(
                            assembly_type.clone(),
                            self.fix_pseudo(operand, &assembly_type)?,
                        ));
                    }
                },
                Instruction::Cmp(assembly_type, src, dest) => {
                    let new_dest = self.fix_pseudo(dest, &assembly_type)?;
                    let new_src = self.fix_pseudo(src, &assembly_type)?;
                    let new_src = self.fix_long_immediate(&new_src, &mut new_instructions);
                    if Self::is_constant(&new_dest) {
                        let scratch = Operand::Register(SCRATCH_REGISTER2);
                        new_instructions.push(Instruction::Mov(
                            assembly_type.clone(),
                            new_dest,
                            scratch.clone(),
                        ));
                        new_instructions.push(Instruction::Cmp(
                            assembly_type.clone(),
                            new_src.clone(),
                            scratch.clone(),
                        ));
                    } else if Self::is_memory(&new_dest) && Self::is_memory(&new_src) {
                        let scratch = Operand::Register(SCRATCH_REGISTER1);
                        new_instructions.push(Instruction::Mov(
                            assembly_type.clone(),
                            new_dest,
                            scratch.clone(),
                        ));
                        new_instructions.push(Instruction::Cmp(
                            assembly_type.clone(),
                            new_src.clone(),
                            scratch.clone(),
                        ));
                    } else {
                        new_instructions.push(Instruction::Cmp(
                            assembly_type.clone(),
                            new_src,
                            new_dest,
                        ));
                    }
                }
                Instruction::SetCC(cond, dest) => {
                    new_instructions.push(Instruction::SetCC(
                        cond.clone(),
                        self.fix_pseudo(dest, &AssemblyType::LongWord)?,
                    ));
                }
                Instruction::Push(operand) => {
                    let operand = self.fix_long_immediate(&operand, &mut new_instructions);
                    new_instructions.push(Instruction::Push(
                        self.fix_pseudo(&operand, &AssemblyType::QuadWord)?,
                    ));
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
                Instruction::SignExtend(src, dest) => {
                    let new_src = self.fix_pseudo(src, &AssemblyType::LongWord)?;
                    let new_dest = self.fix_pseudo(dest, &AssemblyType::QuadWord)?;
                    match (Self::is_constant(&new_src), Self::is_memory(&new_dest)) {
                        (false, false) => {
                            new_instructions.push(Instruction::SignExtend(new_src, new_dest));
                        }
                        (false, true) => {
                            let scratch = Operand::Register(SCRATCH_REGISTER1);

                            new_instructions
                                .push(Instruction::SignExtend(new_src, scratch.clone()));
                            new_instructions.push(Instruction::Mov(
                                AssemblyType::QuadWord,
                                scratch.clone(),
                                new_dest.clone(),
                            ));
                        }
                        (true, true) => {
                            let scratch1 = Operand::Register(SCRATCH_REGISTER1);
                            let scratch2 = Operand::Register(SCRATCH_REGISTER2);
                            new_instructions.push(Instruction::Mov(
                                AssemblyType::LongWord,
                                new_src,
                                scratch1.clone(),
                            ));
                            new_instructions
                                .push(Instruction::SignExtend(scratch1.clone(), scratch2.clone()));
                            new_instructions.push(Instruction::Mov(
                                AssemblyType::QuadWord,
                                scratch2.clone(),
                                new_dest.clone(),
                            ));
                        }
                        (true, false) => {
                            let scratch = Operand::Register(SCRATCH_REGISTER1);
                            new_instructions.push(Instruction::Mov(
                                AssemblyType::LongWord,
                                new_src,
                                scratch.clone(),
                            ));
                            new_instructions.push(Instruction::SignExtend(scratch, new_dest));
                        }
                        _ => {}
                    }
                }
                // Instruction::Truncate(src, dest) => {
                //     let new_src = self.fix_pseudo(src)?;
                //     let new_dest = self.fix_pseudo(dest)?;
                //     if Self::is_memory(&new_src) && Self::is_memory(&new_dest) {
                //         let scratch = Operand::Register(SCRATCH_REGISTER1);
                //         new_instructions.push(Instruction::Mov(
                //             AssemblyType::QuadWord,
                //             new_src,
                //             scratch.clone(),
                //         ));
                //         new_instructions
                //             .push(Instruction::Truncate(scratch.clone(), new_dest.clone()));
                //     } else {
                //         new_instructions.push(Instruction::Truncate(new_src, new_dest));
                //     }
                // }
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
        matches!(operand, Operand::ImmediateI32(_)) || matches!(operand, Operand::ImmediateI64(_))
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
            Operand::ImmediateI32(_) => 4,
            Operand::ImmediateI64(_) => 8,
            Operand::Register(Register::CL) => 1,
            Operand::Register(_) => 4,
            Operand::Pseudo(_) => 4,
            Operand::Stack(_) => 4,
            Operand::Data(_) => 4,
        }
    }
    fn get_operand_size(assembly_type: &AssemblyType) -> i32 {
        match assembly_type {
            AssemblyType::LongWord => 4,
            AssemblyType::QuadWord => 8,
            AssemblyType::Byte => 1,
            AssemblyType::Word => 2,
        }
    }
    fn fix_pseudo(&mut self, operand: &Operand, assembly_type: &AssemblyType) -> Result<Operand> {
        if let Operand::Pseudo(pseudo_name) = operand {
            assert!(pseudo_name.contains('$'));
            let offset = if let Some(offset) = self.pseudo_registers.get(pseudo_name) {
                *offset
            } else {
                self.next_offset += Self::get_operand_size(assembly_type);
                self.pseudo_registers
                    .insert(pseudo_name.to_string(), self.next_offset);
                self.next_offset
            };
            return Ok(Operand::Stack(-offset));
        }
        Ok(operand.clone())
    }
}
