use crate::parser::Parser;
use crate::symbols::SymbolType;
use crate::tacky::StaticInit;
use crate::x64::moira::{Function, MoiraProgram};

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
}

const SCRATCH_REGISTER1: Register = Register::R10;
const SCRATCH_REGISTER2: Register = Register::R11;
const FLOAT_SCRATCH1: Register = Register::XMM14;
const FLOAT_SCRATCH2: Register = Register::XMM15;

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
            //      func_table: HashMap::new(),
        }
    }
    pub fn generate_asm(&mut self, moira: &MoiraProgram, file: &Path) -> Result<()> {
        let mut writer = BufWriter::new(std::fs::File::create(file)?);
        self.gen_program(moira, &mut writer)?;
        Ok(())
    }
    fn gen_program(
        &mut self,
        moira: &MoiraProgram,
        writer: &mut BufWriter<std::fs::File>,
    ) -> Result<()> {
        self.gen_vars(moira, writer)?;
        writeln!(writer, ".text")?;
        for idx in 0..moira.functions.len() {
            let function = moira.functions[idx].clone();
            self.gen_function(&function, writer)?;
        }

        Ok(())
    }
    fn double_to_hex(value: f64) -> String {
        let hex = value.to_bits();
        format!("{:x}", hex)
    }

    pub fn calculate_alignment(stype: &SymbolType) -> usize {
        println!("Calculating alignment for {:?}", stype);
        match stype {
            SymbolType::Int32 | SymbolType::UInt32 => 4,
            SymbolType::Int64 | SymbolType::UInt64 | SymbolType::Pointer(_) => 8,
            SymbolType::Double => 16,
            SymbolType::Char | SymbolType::UChar | SymbolType::SChar => 1, // is it tho?
            SymbolType::Array(_, _) => {
                let size = Parser::get_total_object_size(stype).unwrap();
                if size >= 16 {
                    return 16;
                }
                match Parser::get_inner_array_type(stype).unwrap() {
                    SymbolType::Int32 | SymbolType::UInt32 => 4,
                    SymbolType::Int64 | SymbolType::UInt64 | SymbolType::Pointer(_) => 8,
                    SymbolType::Double => 16,
                    SymbolType::Char | SymbolType::UChar | SymbolType::SChar => 1, // is it tho?
                    SymbolType::Struct(sdef) => sdef.borrow().alignment,
                    _ => todo!(),
                }
            }
            SymbolType::Struct(sdef) | SymbolType::Union(sdef) => {
                return sdef.borrow().alignment;
            }
            _ => unreachable!(),
        }
    }
    fn gen_vars(
        &mut self,
        moira: &MoiraProgram,
        writer: &mut BufWriter<std::fs::File>,
    ) -> Result<()> {
        for var in moira.top_vars.iter() {
            if var.external {
                writeln!(writer, ".extern {}", var.name)?;
                continue;
            }
            let empty = var.init.iter().all(|v| match v {
                StaticInit::InitDouble(0.0) => true,
                StaticInit::InitI32(0) => true,
                StaticInit::InitI64(0) => true,
                StaticInit::InitU32(0) => true,
                StaticInit::InitU64(0) => true, // null pointer too
                StaticInit::InitChar(0) => true,
                StaticInit::InitUChar(0) => true,
                StaticInit::InitString(s, _) => s.is_empty(),
                StaticInit::InitNone(_) => true,
                _ => false,
            });
            if empty {
                writeln!(writer, ".bss")?;
            } else {
                writeln!(writer, ".data")?;
            }
            if var.global {
                writeln!(writer, ".globl {}", var.name)?;
            }

            let align = Self::calculate_alignment(&var.stype);
            writeln!(writer, ".align {}", align)?;

            writeln!(writer, "{}: ", var.name)?;
            if empty {
                let size = Parser::get_total_object_size(&var.stype)?;

                writeln!(writer, " .zero {}", size)?;
            } else {
                for init in var.init.iter() {
                    match init {
                        StaticInit::InitNone(count) => {
                            writeln!(writer, " .zero {}", count)?;
                        }
                        StaticInit::InitI32(value) => writeln!(writer, " .long {}", value)?,
                        StaticInit::InitI64(value) => writeln!(writer, " .quad {}", value)?,
                        StaticInit::InitU32(value) => writeln!(writer, " .long {}", value)?,
                        StaticInit::InitU64(value) => writeln!(writer, " .quad {}", value)?,
                        StaticInit::InitChar(value) => writeln!(writer, " .byte {}", value)?,
                        StaticInit::InitUChar(value) => writeln!(writer, " .byte {}", value)?,
                        StaticInit::InitDouble(value) => {
                            writeln!(writer, " .quad 0x{}", Self::double_to_hex(*value))?
                        }
                        StaticInit::InitString(value, zero) => {
                            let value = value.replace('\\', "\\\\").replace('"', "\\\"");
                            if *zero {
                                //        writeln!(writer, " .byte 0")?;
                                writeln!(writer, " .asciz \"{}\"", value)?;
                            } else {
                                writeln!(writer, " .ascii \"{}\"", value)?;
                            }
                        }
                        StaticInit::PointerInit(name) => {
                            writeln!(writer, " .quad {}", name)?;
                        }
                    }
                }
            }
        }

        for const_value in moira.static_constants.values() {
            writeln!(writer, ".section .rodata, \"r\"")?;
            if const_value.align != 0 {
                writeln!(writer, ".align {}", const_value.align)?;
            }
            writeln!(writer, "{}: ", const_value.name)?;
            for init in const_value.init.iter() {
                match init {
                    StaticInit::InitI32(value) => writeln!(writer, " .long {}", value)?,
                    StaticInit::InitI64(value) => writeln!(writer, " .quad {}", value)?,
                    StaticInit::InitU32(value) => writeln!(writer, " .long {}", value)?,
                    StaticInit::InitU64(value) => writeln!(writer, " .quad {}", value)?,
                    StaticInit::InitChar(value) => writeln!(writer, " .byte {}", value)?,
                    StaticInit::InitUChar(value) => writeln!(writer, " .byte {}", value)?,
                    StaticInit::InitDouble(value) => {
                        writeln!(writer, " .quad 0x{}", Self::double_to_hex(*value))?
                    }

                    StaticInit::InitString(value, zero) => {
                        let value = value.replace('\\', "\\\\").replace('"', "\\\"");
                        if *zero {
                            writeln!(writer, " .asciz \"{}\"", value)?;
                        } else {
                            writeln!(writer, " .ascii \"{}\"", value)?;
                        }
                    }
                    _ => unreachable!(),
                }
            }
        }
        Ok(())
    }
    fn gen_function(
        &mut self,
        function: &Function,
        writer: &mut BufWriter<std::fs::File>,
    ) -> Result<()> {
        let fixed_instructions = self.fixup_pass(function)?;
        for ps in self.pseudo_registers.iter() {
            println!("{:?} -> {} 0x{:x}", ps.0, ps.1, ps.1);
        }

        self.gen_prologue(function, writer)?;
        for instruction in fixed_instructions.iter() {
            self.gen_instruction(instruction, writer)?;
        }

        Ok(())
    }
    fn gen_prologue(
        &mut self,
        function: &Function,
        writer: &mut BufWriter<std::fs::File>,
    ) -> Result<()> {
        if function.global {
            writeln!(writer, ".globl {}", function.name)?;
        }
        // ensure stack is 16 byte aligned
        let mut adjust = self.next_offset % 16;
        if adjust != 0 {
            adjust = 16 - adjust;
        }
        writeln!(writer, "{}: ", function.name)?;
        writeln!(writer, "        pushq %rbp")?;
        writeln!(writer, "        movq  %rsp,%rbp")?;
        writeln!(writer, "        subq ${}, %rsp", self.next_offset + adjust)?;
        Ok(())
    }
    fn get_instruction_size_qual(assembly_type: &AssemblyType) -> &'static str {
        match assembly_type {
            AssemblyType::LongWord => "l",
            AssemblyType::QuadWord => "q",
            AssemblyType::Byte => "b",
            AssemblyType::Word => "w",
            AssemblyType::Double => "",
            AssemblyType::ByteArray(_, _) => "",
        }
    }
    fn gen_instruction(
        &mut self,
        instruction: &Instruction,
        writer: &mut BufWriter<std::fs::File>,
    ) -> Result<()> {
        match instruction {
            Instruction::Ret => {
                writeln!(writer, "        movq %rbp, %rsp")?;
                writeln!(writer, "        popq %rbp")?;
                writeln!(writer, "        ret")?;
            }
            Instruction::Mov(assembly_type, src, dest) => {
                let dest_str = self.get_operand(dest, assembly_type)?;
                let src_str = self.get_operand(
                    src,
                    if dest_str == "%cl" && Self::is_memory(src) {
                        &AssemblyType::Byte
                    } else {
                        assembly_type
                    },
                )?;
                writeln!(
                    writer,
                    "        {} {}, {}",
                    if *assembly_type == AssemblyType::Double {
                        "movsd".to_string()
                    } else {
                        format!("mov{}", Self::get_instruction_size_qual(assembly_type)).to_string()
                    },
                    src_str,
                    dest_str
                )?;
            }
            Instruction::Unary(operator, assembly_type, operand) => {
                let qual = Self::get_instruction_size_qual(assembly_type);
                let oper = match operator {
                    UnaryOperator::Neg => "neg",
                    UnaryOperator::Not => "not",
                    UnaryOperator::FNeg => todo!(),
                };
                let op = self.get_operand(operand, assembly_type)?;
                writeln!(writer, "        {}{} {}", oper, qual, op)?;
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
                let qual = match assembly_type {
                    AssemblyType::LongWord => "l",
                    AssemblyType::QuadWord => "q",
                    AssemblyType::Byte => "b",
                    AssemblyType::Word => "w",
                    AssemblyType::Double => "",
                    AssemblyType::ByteArray(_, _) => todo!(),
                };
                let op = format!("{}{}", op, qual);
                let left_at = if matches!(
                    operator,
                    BinaryOperator::ShiftRight
                        | BinaryOperator::ShiftLeft
                        | BinaryOperator::ShiftRightArith
                ) {
                    AssemblyType::Byte
                } else {
                    assembly_type.clone()
                };
                let left_str = self.get_operand(left, &left_at)?;
                let right_str = self.get_operand(right, assembly_type)?;
                writeln!(writer, "        {} {}, {}", op, left_str, right_str)?;
            }
            Instruction::Idiv(assembly_type, divisor) => {
                let op = self.get_operand(divisor, assembly_type)?;
                let qual = Self::get_instruction_size_qual(assembly_type);
                writeln!(writer, "        idiv{} {}", qual, op)?;
            }
            Instruction::Div(assembly_type, divisor) => {
                let op = self.get_operand(divisor, assembly_type)?;
                let qual = Self::get_instruction_size_qual(assembly_type);
                writeln!(writer, "        div{} {}", qual, op)?;
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
                let qual = Self::get_instruction_size_qual(assembly_type);
                writeln!(writer, "        cmp{} {}, {}", qual, left_str, right_str)?;
            }
            Instruction::FCmp(assembly_type, left, right) => {
                let left_str = self.get_operand(left, assembly_type)?;
                let right_str = self.get_operand(right, assembly_type)?;
                writeln!(writer, "        comisd {}, {}", left_str, right_str)?;
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
            Instruction::Pop(operand) => {
                let at = Self::get_assembly_type_from_operand(operand);
                let qual = Self::get_instruction_size_qual(&at);
                let operand_str = self.get_operand(operand, &AssemblyType::QuadWord)?;
                writeln!(writer, "        pop{} {}", qual, operand_str)?;
            }
            Instruction::DeallocateStack(size) => {
                writeln!(writer, "        addq ${}, %rsp", size)?;
            }
            Instruction::AllocateStack(size) => {
                writeln!(writer, "        subq ${}, %rsp", size)?;
            }
            Instruction::MovSignExtend(src_at, dest_at, src, dest) => {
                let src_str = self.get_operand(src, src_at)?;
                let dest_str = self.get_operand(dest, dest_at)?;
                let dest_qual = Self::get_instruction_size_qual(dest_at);
                let src_qual = Self::get_instruction_size_qual(src_at);
                writeln!(
                    writer,
                    "        movs{}{} {}, {}",
                    src_qual, dest_qual, src_str, dest_str
                )?;
            }
            Instruction::MovZeroExtend(src_at, dest_at, src, dest) => {
                let src_str = self.get_operand(src, src_at)?;
                let dest_str = self.get_operand(dest, dest_at)?;
                let dest_qual = Self::get_instruction_size_qual(dest_at);
                let src_qual = Self::get_instruction_size_qual(src_at);
                writeln!(
                    writer,
                    "        movz{}{} {}, {}",
                    src_qual, dest_qual, src_str, dest_str
                )?;
            }
            Instruction::Cvttsdsi(assembly_type, src, dest) => {
                let src_str = self.get_operand(src, &AssemblyType::Double)?;
                let dest_str = self.get_operand(dest, assembly_type)?;
                let src_qual = Self::get_instruction_size_qual(assembly_type);
                writeln!(
                    writer,
                    "        cvttsd2si{} {}, {}",
                    src_qual, src_str, dest_str
                )?;
            }
            Instruction::Cvtsi2sd(assembly_type, src, dest) => {
                let src_str = self.get_operand(src, assembly_type)?;
                let dest_str = self.get_operand(dest, assembly_type)?;
                let src_qual = Self::get_instruction_size_qual(assembly_type);
                writeln!(
                    writer,
                    "        cvtsi2sd{} {}, {}",
                    src_qual, src_str, dest_str
                )?;
            }
            Instruction::Lea(src, dest) => {
                let src_str = self.get_operand(src, &AssemblyType::QuadWord)?;
                let dest_str = self.get_operand(dest, &AssemblyType::QuadWord)?;
                writeln!(writer, "        lea {}, {}", src_str, dest_str)?;
            }
            Instruction::CopyBlock(src, dest, size) => {
                let src_str = self.get_operand(src, &AssemblyType::ByteArray(0, 0))?;
                let dest_str = self.get_operand(dest, &AssemblyType::ByteArray(0, 0))?;
                // source and dest are either register with addr or variable
                if src.is_register() {
                    writeln!(writer, "	movq	{}, %rcx", src_str)?;
                } else {
                    writeln!(writer, "	lea	{} ,%rcx", src_str)?;
                }
                if dest.is_register() {
                    writeln!(writer, "	movq	{} ,%rax", dest_str)?;
                } else {
                    writeln!(writer, "	lea	{}, %rax", dest_str)?;
                }
                writeln!(writer, "	movq %rax,%rdi")?;
                writeln!(writer, "	movq %rcx,%rsi")?;
                writeln!(writer, "	movq %rax,%rdi ")?;
                writeln!(writer, "	movl  ${}, %ecx", size)?;
                writeln!(writer, "  rep movsb")?;
            }
        }
        Ok(())
    }

    fn get_reg_name(register: &Register, assembly_type: &AssemblyType) -> String {
        let assembly_type = if assembly_type == &AssemblyType::ByteArray(0, 0) {
            AssemblyType::QuadWord
        } else {
            assembly_type.clone()
        };
        let str = match (register, assembly_type) {
            (Register::RAX, AssemblyType::QuadWord) => "rax",
            (Register::RAX, AssemblyType::LongWord) => "eax",
            (Register::RAX, AssemblyType::Byte) => "al",

            (Register::RDX, AssemblyType::QuadWord) => "rdx",
            (Register::RDX, AssemblyType::LongWord) => "edx",
            (Register::RDX, AssemblyType::Word) => "dx",
            (Register::RDX, AssemblyType::Byte) => "dl",

            (Register::RCX, AssemblyType::QuadWord) => "rcx",
            (Register::RCX, AssemblyType::LongWord) => "ecx",
            (Register::RCX, AssemblyType::Word) => "cx",
            (Register::RCX, AssemblyType::Byte) => "cl",

            (Register::XMM0, _) => "xmm0",
            (Register::XMM1, _) => "xmm1",
            (Register::XMM2, _) => "xmm2",
            (Register::XMM3, _) => "xmm3",
            (Register::XMM4, _) => "xmm4",
            (Register::XMM5, _) => "xmm5",
            (Register::XMM6, _) => "xmm6",
            (Register::XMM7, _) => "xmm7",
            (Register::XMM14, _) => "xmm14",
            (Register::XMM15, _) => "xmm15",

            (Register::RBP, _) => "rbp",
            (Register::RSP, _) => "rsp",

            (Register::R8, AssemblyType::QuadWord) => "r8",
            (Register::R8, AssemblyType::LongWord) => "r8d",
            (Register::R8, AssemblyType::Word) => "r8w",
            (Register::R8, AssemblyType::Byte) => "r8b",

            (Register::R9, AssemblyType::QuadWord) => "r9",
            (Register::R9, AssemblyType::LongWord) => "r9d",
            (Register::R9, AssemblyType::Word) => "r9w",

            (Register::R9, AssemblyType::Byte) => "r9b",

            (Register::R10, AssemblyType::QuadWord) => "r10",
            (Register::R10, AssemblyType::LongWord) => "r10d",
            (Register::R10, AssemblyType::Word) => "r10w",
            (Register::R10, AssemblyType::Byte) => "r10b",

            (Register::R11, AssemblyType::QuadWord) => "r11",
            (Register::R11, AssemblyType::LongWord) => "r11d",
            (Register::R11, AssemblyType::Byte) => "r11b",

            _ => panic!("Invalid register or assembly type"),
        };
        format!("%{}", str)
    }
    fn get_operand(&mut self, value: &Operand, assembly_type: &AssemblyType) -> Result<String> {
        let val_str = match value {
            Operand::ImmediateI32(value) => format!("${}", value),
            Operand::ImmediateI64(value) => format!("${}", value),
            Operand::ImmediateU32(value) => format!("${}", value),
            Operand::ImmediateU64(value) => format!("${}", value),
            Operand::ImmediateI8(value) => format!("${}", value),
            Operand::ImmediateU8(value) => format!("${}", value),
            // format!("${}", value),
            // Operand::ImmediateF64(value) => todo!(),
            Operand::Pseudo(_) => panic!("Pseudo register not supported"),
            Operand::Register(register) => Self::get_reg_name(register, assembly_type),

            Operand::Memory(register, offset) => {
                format!(
                    "{:+}({})",
                    offset,
                    Self::get_reg_name(register, &AssemblyType::QuadWord),
                )
            }
            Operand::Data(data, offset) => {
                if *offset == 0 {
                    format!("{}(%rip)", data)
                } else {
                    format!("{}+{}(%rip)", offset, data)
                }
            }
            Operand::PseudoMem(data, _size, _offset, _align) => {
                format!(" {}", data)
            }
            Operand::Indexed(base, index, scale) => {
                format!(
                    " ({} , {} , {})",
                    Self::get_reg_name(base, &AssemblyType::QuadWord),
                    Self::get_reg_name(index, &AssemblyType::QuadWord),
                    scale
                )
            }
        };
        Ok(val_str)
    }
    fn fix_big_immediate(
        &mut self,
        operand: &Operand,
        instructions: &mut Vec<Instruction>,
        fix_32: bool,
    ) -> Operand {
        let ret = match operand {
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
        };
        if fix_32 {
            match ret {
                Operand::ImmediateU32(val) => {
                    if val & 0x80000000 != 0 {
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
                _ => ret.clone(),
            }
        } else {
            ret
        }
    }
    pub fn get_scratch_register1(assembly_type: &AssemblyType) -> Operand {
        match assembly_type {
            AssemblyType::Double => Operand::Register(FLOAT_SCRATCH1),
            _ => Operand::Register(SCRATCH_REGISTER1),
        }
    }
    pub fn get_scratch_register2(assembly_type: &AssemblyType) -> Operand {
        match assembly_type {
            AssemblyType::LongWord | AssemblyType::QuadWord => Operand::Register(SCRATCH_REGISTER2),
            AssemblyType::Double => Operand::Register(FLOAT_SCRATCH2),
            AssemblyType::Byte => Operand::Register(SCRATCH_REGISTER2),
            _ => unreachable!(),
        }
    }

    fn get_assembly_type_from_operand(operand: &Operand) -> AssemblyType {
        match operand {
            Operand::Register(_) => AssemblyType::QuadWord,
            Operand::ImmediateI32(_) => AssemblyType::LongWord,
            Operand::ImmediateI64(_) => AssemblyType::QuadWord,
            Operand::ImmediateU32(_) => AssemblyType::LongWord,
            Operand::ImmediateU64(_) => AssemblyType::QuadWord,
            Operand::ImmediateI8(_) => AssemblyType::Byte,
            Operand::ImmediateU8(_) => AssemblyType::Byte,
            Operand::Memory(_, _) => AssemblyType::QuadWord,
            Operand::Data(_, _) => AssemblyType::QuadWord,
            Operand::PseudoMem(_, size, _, align) => AssemblyType::ByteArray(*size, *align),
            Operand::Indexed(_, _, _) => AssemblyType::QuadWord,
            Operand::Pseudo(_) => AssemblyType::QuadWord,
        }
    }
    fn fixup_pass(&mut self, function: &Function) -> Result<Vec<Instruction>> {
        self.next_offset = 0;
        self.pseudo_registers.clear();
        let mut new_instructions = Vec::new();
        println!("Fixing up function: {}", function.name);
        for instruction in function.instructions.iter() {
            match instruction {
                Instruction::Mov(assembly_type, src, dest) => {
                    let dest_assembly_type = Self::get_assembly_type_from_operand(dest);
                    let new_src = self.fix_pseudo(src, assembly_type)?;
                    let new_dest = self.fix_pseudo(dest, &dest_assembly_type)?;
                    let new_src = self.fix_big_immediate(&new_src, &mut new_instructions, false);
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
                    let new_src = self.fix_big_immediate(&new_src, &mut new_instructions, true);
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
                                new_instructions.push(Instruction::Binary(
                                    op.clone(),
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
                                let scratch = Operand::Register(Register::RCX);
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
                    let new_src = self.fix_big_immediate(&new_src, &mut new_instructions, true);
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
                    let operand = self.fix_big_immediate(operand, &mut new_instructions, true);
                    new_instructions.push(Instruction::Push(
                        self.fix_pseudo(&operand, &AssemblyType::QuadWord)?,
                    ));
                }
                Instruction::Pop(operand) => {
                    new_instructions.push(Instruction::Pop(
                        self.fix_pseudo(operand, &AssemblyType::QuadWord)?,
                    ));
                }
                Instruction::Call(name) => {
                    new_instructions.push(Instruction::Call(name.clone()));
                }
                Instruction::MovSignExtend(src_at, dest_at, src, dest) => {
                    let new_src = self.fix_pseudo(src, src_at)?;
                    let new_dest = self.fix_pseudo(dest, dest_at)?;
                    match (Self::is_constant(&new_src), Self::is_memory(&new_dest)) {
                        (false, false) => {
                            new_instructions.push(Instruction::MovSignExtend(
                                src_at.clone(),
                                dest_at.clone(),
                                new_src,
                                new_dest,
                            ));
                        }
                        (false, true) => {
                            let scratch = Self::get_scratch_register1(dest_at);

                            new_instructions.push(Instruction::MovSignExtend(
                                src_at.clone(),
                                dest_at.clone(),
                                new_src,
                                scratch.clone(),
                            ));
                            new_instructions.push(Instruction::Mov(
                                dest_at.clone(),
                                scratch.clone(),
                                new_dest.clone(),
                            ));
                        }
                        (true, true) => {
                            let scratch1 = Self::get_scratch_register1(src_at);
                            let scratch2 = Self::get_scratch_register2(dest_at);
                            new_instructions.push(Instruction::Mov(
                                dest_at.clone(),
                                new_src,
                                scratch1.clone(),
                            ));
                            new_instructions.push(Instruction::MovSignExtend(
                                src_at.clone(),
                                dest_at.clone(),
                                scratch1.clone(),
                                scratch2.clone(),
                            ));
                            new_instructions.push(Instruction::Mov(
                                dest_at.clone(),
                                scratch2.clone(),
                                new_dest.clone(),
                            ));
                        }
                        (true, false) => {
                            let scratch = Self::get_scratch_register1(dest_at);
                            new_instructions.push(Instruction::Mov(
                                src_at.clone(),
                                new_src,
                                scratch.clone(),
                            ));
                            new_instructions.push(Instruction::MovSignExtend(
                                src_at.clone(),
                                dest_at.clone(),
                                scratch,
                                new_dest,
                            ));
                        } // _ => {}
                    }
                }
                Instruction::MovZeroExtend(src_at, dest_at, src, dest) => {
                    let new_src = self.fix_pseudo(src, src_at)?;
                    let new_dest = self.fix_pseudo(dest, dest_at)?;
                    match (Self::is_constant(&new_src), Self::is_memory(&new_dest)) {
                        (false, false) => {
                            if *src_at == AssemblyType::LongWord {
                                new_instructions.push(Instruction::Mov(
                                    dest_at.clone(),
                                    new_src,
                                    new_dest,
                                ));
                            } else {
                                new_instructions.push(Instruction::MovZeroExtend(
                                    src_at.clone(),
                                    dest_at.clone(),
                                    new_src,
                                    new_dest,
                                ));
                            }
                        }
                        (false, true) => {
                            let scratch = Self::get_scratch_register1(dest_at);

                            if *src_at == AssemblyType::LongWord {
                                new_instructions.push(Instruction::Mov(
                                    AssemblyType::LongWord,
                                    new_src,
                                    scratch.clone(),
                                ));
                            } else {
                                new_instructions.push(Instruction::MovZeroExtend(
                                    src_at.clone(),
                                    dest_at.clone(),
                                    new_src.clone(),
                                    scratch.clone(),
                                ));
                            }
                            new_instructions.push(Instruction::Mov(
                                dest_at.clone(),
                                scratch.clone(),
                                new_dest.clone(),
                            ));
                        }
                        (true, true) => {
                            let scratch1 = Self::get_scratch_register1(src_at);
                            let scratch2 = Self::get_scratch_register2(dest_at);
                            new_instructions.push(Instruction::Mov(
                                dest_at.clone(),
                                new_src,
                                scratch1.clone(),
                            ));
                            new_instructions.push(Instruction::MovZeroExtend(
                                src_at.clone(),
                                dest_at.clone(),
                                scratch1.clone(),
                                scratch2.clone(),
                            ));
                            new_instructions.push(Instruction::Mov(
                                dest_at.clone(),
                                scratch2.clone(),
                                new_dest.clone(),
                            ));
                        }
                        (true, false) => {
                            let scratch = Self::get_scratch_register1(dest_at);
                            new_instructions.push(Instruction::Mov(
                                src_at.clone(),
                                new_src,
                                scratch.clone(),
                            ));
                            new_instructions.push(Instruction::MovZeroExtend(
                                src_at.clone(),
                                dest_at.clone(),
                                scratch,
                                new_dest,
                            ));
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

                Instruction::Lea(src, dst) => {
                    let new_src = self.fix_pseudo(src, &AssemblyType::QuadWord)?;
                    let new_dest = self.fix_pseudo(dst, &AssemblyType::QuadWord)?;
                    if Self::is_memory(&new_dest) {
                        let scratch = Self::get_scratch_register1(&AssemblyType::QuadWord);
                        new_instructions.push(Instruction::Lea(new_src, scratch.clone()));
                        new_instructions.push(Instruction::Mov(
                            AssemblyType::QuadWord,
                            scratch,
                            new_dest,
                        ));
                    } else {
                        new_instructions.push(Instruction::Lea(new_src, new_dest));
                    }
                }
                Instruction::CopyBlock(src, dest, size) => {
                    let new_src = self.fix_pseudo(src, &AssemblyType::QuadWord)?;
                    let new_dest = self.fix_pseudo(dest, &AssemblyType::QuadWord)?;
                    new_instructions.push(Instruction::CopyBlock(new_src, new_dest, *size));
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
        matches!(operand, Operand::Memory(_, _)) || matches!(operand, Operand::Data(_, _))
    }
    fn is_constant(operand: &Operand) -> bool {
        matches!(
            operand,
            Operand::ImmediateI32(_)
                | Operand::ImmediateI64(_)
                | Operand::ImmediateU32(_)
                | Operand::ImmediateU64(_)
                | Operand::ImmediateI8(_)
                | Operand::ImmediateU8(_)
        )
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
            _ => unreachable!(),
        }
    }

    fn lookup_pseudo(&mut self, pseudo_name: &str, size: i32, align: usize) -> i32 {
        if let Some(offset) = self.pseudo_registers.get(pseudo_name) {
            *offset
        } else {
            let mut pad = (self.next_offset + size) % align as i32;
            if pad > 0 {
                pad = align as i32 - pad;
            }
            let old_off = self.next_offset;
            self.next_offset += size + pad;
            println!(
                "Allocating pseudo register {} at offset {} (align={}, pad={} old-off={} size ={}",
                pseudo_name, self.next_offset, align, pad, old_off, size
            );
            self.pseudo_registers
                .insert(pseudo_name.to_string(), self.next_offset);
            self.next_offset
        }
    }

    fn assembly_type_alignment(assembly_type: &AssemblyType) -> usize {
        match assembly_type {
            AssemblyType::LongWord => 4,
            AssemblyType::QuadWord => 8,
            AssemblyType::Byte => 1,
            AssemblyType::Word => 2,
            AssemblyType::Double => 8,
            AssemblyType::ByteArray(_, align) => *align,
        }
    }
    fn fix_pseudo(&mut self, operand: &Operand, assembly_type: &AssemblyType) -> Result<Operand> {
        Ok(match operand {
            Operand::Pseudo(pseudo_name) => {
                assert!(pseudo_name.contains('$'));
                let align = Self::assembly_type_alignment(assembly_type);
                let offset =
                    self.lookup_pseudo(pseudo_name, Self::get_operand_size(assembly_type), align);
                Operand::Memory(Register::RBP, -offset)
            }
            Operand::PseudoMem(pseudo_name, total_size, offset, align) => {
                println!(
                    "Fixing pseudo operand: {:?} assembly_type: {:?} align: {}",
                    operand, assembly_type, align
                );
                assert!(pseudo_name.contains('$'));
                if pseudo_name.contains('$') {
                    let stack_offset = self.lookup_pseudo(pseudo_name, *total_size as i32, *align);
                    Operand::Memory(Register::RBP, -stack_offset + *offset as i32)
                } else {
                    Operand::Data(pseudo_name.clone(), *offset)
                }
            }
            _ => operand.clone(),
        })
    }
}
