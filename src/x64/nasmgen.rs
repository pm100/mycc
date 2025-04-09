use crate::moira::{Function, MoiraProgram, StaticConstant};

use crate::codegen::MoiraCompiler;
use crate::tacky::{StaticInit, SymbolType};
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
const FLOAT_SCRATCH1: Register = Register::XMM14;
const FLOAT_SCRATCH2: Register = Register::XMM15;

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
    fn double_to_hex(value: f64) -> String {
        let hex = value.to_bits();
        format!("{:x}", hex)
    }
    fn gen_vars(
        &mut self,
        moira: &MoiraProgram<Instruction>,
        writer: &mut BufWriter<std::fs::File>,
    ) -> Result<()> {
        let mut extra_static_vars = Vec::new();
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
                    StaticInit::InitU32(value) => writeln!(writer, "{} dd {}", var.name, value)?,
                    StaticInit::InitU64(value) => writeln!(writer, "{} dq {}", var.name, value)?,
                    StaticInit::InitDouble(value) => {
                        writeln!(writer, "{}: dq 0x{}", var.name, Self::double_to_hex(value))?
                    }
                    StaticInit::InitNone => {
                        match var.stype {
                            SymbolType::Int32 | crate::tacky::SymbolType::UInt32 => {
                                writeln!(writer, "{} dd 0", var.name)?
                            }
                            SymbolType::Int64 | crate::tacky::SymbolType::UInt64 => {
                                writeln!(writer, "{} dq 0", var.name)?
                            }
                            SymbolType::Double => {
                                extra_static_vars.push(StaticConstant {
                                    name: var.name.clone(),
                                    align: 16,
                                    value: StaticInit::InitDouble(0.0),
                                });
                            }
                            _ => unreachable!("Unsupported type for static variable"),
                        };
                    }
                }
                //                writeln!(writer, "{} dd {}", var.name, var.value)?;
            }
        }
        //  extra_static_vars.eappend(moira.static_constants.values().collect::<Vec<_>>());
        for const_value in moira
            .static_constants
            .values()
            .chain(extra_static_vars.iter())
        {
            writeln!(writer, "segment .rodata")?;
            match const_value.value {
                StaticInit::InitDouble(value) => {
                    writeln!(writer, "align 16")?;
                    writeln!(
                        writer,
                        "{}: dq 0x{}",
                        const_value.name,
                        Self::double_to_hex(value)
                    )?;
                }
                _ => unreachable!("Unsupported type for static constant"),
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
        //println!("Generating prologue for function: {}", function.name);

        if function.global {
            writeln!(writer, "global {}", function.name)?;
        }
        let adjust = self.next_offset % 16;
        writeln!(writer, "{}: ", function.name)?;
        writeln!(writer, "        push rbp")?;
        writeln!(writer, "        mov rbp, rsp")?;
        writeln!(writer, "        sub rsp, {}", self.next_offset + adjust)?;
        Ok(())
    }

    fn gen_instruction(
        &mut self,
        instruction: &Instruction,
        writer: &mut BufWriter<std::fs::File>,
    ) -> Result<()> {
        println!("Generating instruction: {:?}", instruction);
        match instruction {
            Instruction::Ret => {
                writeln!(writer, "        mov rsp, rbp")?;
                writeln!(writer, "        pop rbp")?;
                writeln!(writer, "        ret")?;
            }
            Instruction::Mov(assembly_type, src, dest) => {
                let dest_str = self.get_operand(dest, assembly_type)?;
                let src_str = self.get_operand(
                    src,
                    if dest_str == "cl" && Self::is_memory(src) {
                        &AssemblyType::Byte
                    } else {
                        assembly_type
                    },
                )?;
                writeln!(
                    writer,
                    "        {} {}, {}",
                    if *assembly_type == AssemblyType::Double {
                        "movsd"
                    } else {
                        "mov"
                    },
                    dest_str,
                    src_str
                )?;
            }
            Instruction::Unary(operator, assembly_type, operand) => {
                let oper = match operator {
                    UnaryOperator::Neg => "neg",
                    UnaryOperator::Not => "not",
                    UnaryOperator::FNeg => todo!(),
                };
                let op = self.get_operand(operand, assembly_type)?;
                writeln!(writer, "        {} {}", oper, op)?;
            }
            Instruction::Binary(operator, assembly_type, left, right) => {
                let op = match (operator, assembly_type) {
                    (BinaryOperator::Add, _) => "add",
                    (BinaryOperator::Sub, _) => "sub",
                    (BinaryOperator::Mult, _) => "imul",
                    (BinaryOperator::BitAnd, _) => "and",
                    (BinaryOperator::BitOr, _) => "or",
                    (BinaryOperator::BitXor, AssemblyType::Double) => "xorpd",
                    (BinaryOperator::BitXor, _) => "xor",
                    (BinaryOperator::ShiftLeft, _) => "sal",
                    (BinaryOperator::ShiftRightArith, _) => "sar",
                    (BinaryOperator::ShiftRight, _) => "shr",
                    (BinaryOperator::FAdd, _) => "addsd",
                    (BinaryOperator::FSub, _) => "subsd",
                    (BinaryOperator::FMul, _) => "mulsd",
                    (BinaryOperator::FDiv, _) => "divsd",
                };
                let left_str = self.get_operand(left, assembly_type)?;
                let right_str = self.get_operand(right, assembly_type)?;
                writeln!(writer, "        {} {}, {}", op, right_str, left_str)?;
            }
            Instruction::Idiv(assembly_type, divisor) => {
                let op = self.get_operand(divisor, assembly_type)?;
                writeln!(writer, "        idiv {}", op)?;
            }
            Instruction::Div(assembly_type, divisor) => {
                let op = self.get_operand(divisor, assembly_type)?;
                writeln!(writer, "        div {}", op)?;
            }
            Instruction::Cdq(assembly_type) => {
                match assembly_type {
                    AssemblyType::LongWord => writeln!(writer, "        cdq")?,
                    AssemblyType::QuadWord => writeln!(writer, "        cqo")?,
                    _ => panic!("Invalid assembly type for cdq"),
                };
            }
            Instruction::Cmp(assembly_type, left, right) => {
                let left_str = self.get_operand(left, assembly_type)?;
                let right_str = self.get_operand(right, assembly_type)?;
                writeln!(writer, "        cmp {}, {}", right_str, left_str)?;
            }
            Instruction::FCmp(assembly_type, left, right) => {
                let left_str = self.get_operand(left, assembly_type)?;
                let right_str = self.get_operand(right, assembly_type)?;
                writeln!(writer, "        comisd {}, {}", right_str, left_str)?;
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
            }
            Instruction::Cvttsdsi(assembly_type, src, dest) => {
                let src_str = self.get_operand(src, &AssemblyType::Double)?;
                let dest_str = self.get_operand(dest, assembly_type)?;
                writeln!(writer, "        cvttsd2si {}, {}", dest_str, src_str)?;
            }
            Instruction::Cvtsi2sd(assembly_type, src, dest) => {
                let src_str = self.get_operand(src, assembly_type)?;
                let dest_str = self.get_operand(dest, assembly_type)?;
                writeln!(writer, "        cvtsi2sd {}, {}", dest_str, src_str)?;
            }
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
            Register::CL => "cl".to_string(),
            Register::XMM0 => "xmm0".to_string(),
            Register::XMM1 => "xmm1".to_string(),
            Register::XMM2 => "xmm2".to_string(),
            Register::XMM3 => "xmm3".to_string(),
            Register::XMM4 => "xmm4".to_string(),
            Register::XMM5 => "xmm5".to_string(),
            Register::XMM6 => "xmm6".to_string(),
            Register::XMM7 => "xmm7".to_string(),
            Register::XMM14 => "xmm14".to_string(),
            Register::XMM15 => "xmm15".to_string(),
        }
    }
    fn get_operand(&mut self, value: &Operand, assembly_type: &AssemblyType) -> Result<String> {
        let val_str = match value {
            Operand::ImmediateI32(value) => value.to_string(),
            Operand::ImmediateI64(value) => value.to_string(),
            Operand::ImmediateU32(value) => value.to_string(),
            Operand::ImmediateU64(value) => value.to_string(),
            Operand::ImmediateF64(value) => todo!(),
            Operand::Pseudo(_) => panic!("Pseudo register not supported"),
            Operand::Register(register) => Self::get_reg_name(register, assembly_type),

            Operand::Stack(offset) => {
                let qual = match assembly_type {
                    AssemblyType::LongWord => "DWORD",
                    AssemblyType::QuadWord => "QWORD",
                    AssemblyType::Byte => "BYTE",
                    AssemblyType::Word => "WORD",
                    AssemblyType::Double => "",
                };

                format!("{} [rbp{:+}]", qual, offset)
            }
            Operand::Data(data) => {
                let qual = match assembly_type {
                    AssemblyType::LongWord => "DWORD",
                    AssemblyType::QuadWord => "QWORD",
                    AssemblyType::Byte => "BYTE",
                    AssemblyType::Word => "WORD",
                    AssemblyType::Double => "",
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
            Operand::ImmediateU64(val) => {
                if *val > i32::MAX as u64 {
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
    pub fn get_scratch_register1(assembly_type: &AssemblyType) -> Operand {
        match assembly_type {
            AssemblyType::LongWord | AssemblyType::QuadWord => Operand::Register(SCRATCH_REGISTER1),
            AssemblyType::Double => Operand::Register(FLOAT_SCRATCH1),
            _ => unreachable!(),
        }
    }
    pub fn get_scratch_register2(assembly_type: &AssemblyType) -> Operand {
        match assembly_type {
            AssemblyType::LongWord | AssemblyType::QuadWord => Operand::Register(SCRATCH_REGISTER2),
            AssemblyType::Double => Operand::Register(FLOAT_SCRATCH2),
            _ => unreachable!(),
        }
    }
    fn fixup_pass(&mut self, function: &Function<Instruction>) -> Result<Vec<Instruction>> {
        self.next_offset = 0;
        self.pseudo_registers.clear();
        let mut new_instructions = Vec::new();
        println!("Fixing up function: {}", function.name);
        for instruction in function.instructions.iter() {
            println!("Fixing up instruction: {:?}", instruction);
            match instruction {
                Instruction::Mov(assembly_type, src, dest) => {
                    let new_src = self.fix_pseudo(src, assembly_type)?;
                    let new_dest = self.fix_pseudo(dest, assembly_type)?;
                    let new_src = self.fix_long_immediate(&new_src, &mut new_instructions);
                    if Self::is_memory(&new_src) && Self::is_memory(&new_dest) {
                        let scratch = Self::get_scratch_register1(assembly_type);
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
                    let new_src = self.fix_pseudo(src, assembly_type)?;
                    let new_dest = self.fix_pseudo(dest, assembly_type)?;
                    let new_src = self.fix_long_immediate(&new_src, &mut new_instructions);
                    match op {
                        BinaryOperator::Add
                        | BinaryOperator::Sub
                        | BinaryOperator::BitAnd
                        | BinaryOperator::BitOr
                        | BinaryOperator::BitXor => {
                            if Self::is_memory(&new_src) && Self::is_memory(&new_dest) {
                                let scratch = Self::get_scratch_register1(assembly_type);
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
                                let scratch = Self::get_scratch_register2(assembly_type);
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
                        BinaryOperator::ShiftLeft
                        | BinaryOperator::ShiftRightArith
                        | BinaryOperator::ShiftRight => {
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
                        }
                        BinaryOperator::FSub
                        | BinaryOperator::FAdd
                        | BinaryOperator::FDiv
                        | BinaryOperator::FMul => {
                            if Self::is_memory(&new_dest) {
                                let scratch = Self::get_scratch_register2(&AssemblyType::Double);
                                new_instructions.push(Instruction::Mov(
                                    assembly_type.clone(),
                                    new_dest.clone(),
                                    scratch.clone(),
                                ));
                                new_instructions.push(Instruction::Binary(
                                    op.clone(),
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
                        self.fix_pseudo(dest, assembly_type)?,
                    ));
                }
                Instruction::Idiv(assembly_type, operand) => match operand {
                    Operand::ImmediateI32(imm) => {
                        new_instructions.push(Instruction::Mov(
                            assembly_type.clone(),
                            Operand::ImmediateI32(*imm),
                            Self::get_scratch_register1(assembly_type),
                        ));
                        new_instructions.push(Instruction::Idiv(
                            assembly_type.clone(),
                            Self::get_scratch_register1(assembly_type),
                        ));
                    }
                    Operand::ImmediateI64(imm) => {
                        new_instructions.push(Instruction::Mov(
                            assembly_type.clone(),
                            Operand::ImmediateI64(*imm),
                            Self::get_scratch_register1(assembly_type),
                        ));
                        new_instructions.push(Instruction::Idiv(
                            assembly_type.clone(),
                            Self::get_scratch_register1(assembly_type),
                        ));
                    }
                    _ => {
                        new_instructions.push(Instruction::Idiv(
                            assembly_type.clone(),
                            self.fix_pseudo(operand, assembly_type)?,
                        ));
                    }
                },
                Instruction::Div(assembly_type, operand) => match operand {
                    Operand::ImmediateU32(imm) => {
                        new_instructions.push(Instruction::Mov(
                            assembly_type.clone(),
                            Operand::ImmediateU32(*imm),
                            Self::get_scratch_register1(assembly_type),
                        ));
                        new_instructions.push(Instruction::Div(
                            assembly_type.clone(),
                            Self::get_scratch_register1(assembly_type),
                        ));
                    }
                    Operand::ImmediateU64(imm) => {
                        new_instructions.push(Instruction::Mov(
                            assembly_type.clone(),
                            Operand::ImmediateU64(*imm),
                            Self::get_scratch_register1(assembly_type),
                        ));
                        new_instructions.push(Instruction::Div(
                            assembly_type.clone(),
                            Self::get_scratch_register1(assembly_type),
                        ));
                    }
                    _ => {
                        new_instructions.push(Instruction::Div(
                            assembly_type.clone(),
                            self.fix_pseudo(operand, assembly_type)?,
                        ));
                    }
                },
                Instruction::Cmp(assembly_type, src, dest) => {
                    let new_dest = self.fix_pseudo(dest, assembly_type)?;
                    let new_src = self.fix_pseudo(src, assembly_type)?;
                    let new_src = self.fix_long_immediate(&new_src, &mut new_instructions);
                    if Self::is_constant(&new_dest) {
                        let scratch = Self::get_scratch_register2(assembly_type);
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
                        let scratch = Self::get_scratch_register1(assembly_type);
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
                    let operand = self.fix_long_immediate(operand, &mut new_instructions);
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
                            let scratch = Self::get_scratch_register1(&AssemblyType::QuadWord);

                            new_instructions
                                .push(Instruction::SignExtend(new_src, scratch.clone()));
                            new_instructions.push(Instruction::Mov(
                                AssemblyType::QuadWord,
                                scratch.clone(),
                                new_dest.clone(),
                            ));
                        }
                        (true, true) => {
                            let scratch1 = Self::get_scratch_register1(&AssemblyType::QuadWord);
                            let scratch2 = Self::get_scratch_register2(&AssemblyType::QuadWord);
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
                            let scratch = Self::get_scratch_register1(&AssemblyType::QuadWord);
                            new_instructions.push(Instruction::Mov(
                                AssemblyType::LongWord,
                                new_src,
                                scratch.clone(),
                            ));
                            new_instructions.push(Instruction::SignExtend(scratch, new_dest));
                        } // _ => {}
                    }
                }
                Instruction::Cvtsi2sd(assembly_type, src, dest) => {
                    let new_src = self.fix_pseudo(src, assembly_type)?;
                    let new_dest = self.fix_pseudo(dest, &AssemblyType::Double)?;
                    if Self::is_memory(&new_dest) {
                        let scratch = Self::get_scratch_register2(&AssemblyType::Double);

                        new_instructions.push(Instruction::Cvtsi2sd(
                            assembly_type.clone(),
                            new_src,
                            scratch.clone(),
                        ));
                        new_instructions.push(Instruction::Mov(
                            AssemblyType::Double,
                            scratch,
                            new_dest,
                        ));
                    } else {
                        new_instructions.push(Instruction::Cvtsi2sd(
                            assembly_type.clone(),
                            new_src,
                            new_dest,
                        ));
                    }
                }
                Instruction::Cvttsdsi(assembly_type, src, dest) => {
                    let new_src = self.fix_pseudo(src, &AssemblyType::QuadWord)?;
                    let new_dest = self.fix_pseudo(dest, &AssemblyType::QuadWord)?;
                    if Self::is_memory(&new_dest) {
                        let scratch = Self::get_scratch_register2(assembly_type);

                        new_instructions.push(Instruction::Cvttsdsi(
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
                        new_instructions.push(Instruction::Cvttsdsi(
                            assembly_type.clone(),
                            new_src,
                            new_dest,
                        ));
                    }
                }
                Instruction::FCmp(assembly_type, src, dest) => {
                    let new_src = self.fix_pseudo(src, &AssemblyType::Double)?;
                    let new_dest = self.fix_pseudo(dest, &AssemblyType::Double)?;
                    if Self::is_memory(&new_dest) {
                        let scratch = Self::get_scratch_register2(assembly_type);

                        new_instructions.push(Instruction::Mov(
                            assembly_type.clone(),
                            new_dest,
                            scratch.clone(),
                        ));
                        new_instructions.push(Instruction::FCmp(
                            assembly_type.clone(),
                            new_src.clone(),
                            scratch.clone(),
                        ));
                    } else {
                        new_instructions.push(Instruction::Mov(
                            assembly_type.clone(),
                            new_src,
                            new_dest.clone(),
                        ));
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
    fn is_memory(operand: &Operand) -> bool {
        matches!(operand, Operand::Stack(_)) || matches!(operand, Operand::Data(_))
    }
    fn is_constant(operand: &Operand) -> bool {
        matches!(operand, Operand::ImmediateI32(_))
            || matches!(operand, Operand::ImmediateI64(_))
            || matches!(operand, Operand::ImmediateU32(_))
            || matches!(operand, Operand::ImmediateU64(_))
    }
    fn translate_cc(cc: &CondCode) -> String {
        match cc {
            CondCode::E => "e",
            CondCode::NE => "ne",
            CondCode::G => "g",
            CondCode::GE => "ge",
            CondCode::L => "l",
            CondCode::LE => "le",
            CondCode::A => "a",
            CondCode::AE => "ae",
            CondCode::B => "b",
            CondCode::BE => "be",
            CondCode::P => "p",
        }
        .to_string()
    }

    fn get_operand_size(assembly_type: &AssemblyType) -> i32 {
        match assembly_type {
            AssemblyType::LongWord => 4,
            AssemblyType::QuadWord | AssemblyType::Double => 8,
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
