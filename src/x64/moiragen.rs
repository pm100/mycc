/*

Reads the TACKY code and generates the MOIRA code for it

*/

use crate::tacky;
use core::panic;
use std::path::Path;

use crate::{
    codegen::BackEnd,
    parser::Parser,
    symbols::SymbolType,
    tacky::{StaticConstant, TackyProgram},
    x64::gnugen::X64CodeGenerator,
};
use anyhow::Result;

use super::{
    moira::MoiraProgram,
    moira_inst::{
        AssemblyType, BinaryOperator, CondCode, Instruction, Operand, Register, UnaryOperator,
    },
};
pub struct X64BackEnd {
    moira: MoiraProgram,
    instruction_counter: usize,
    temp_count: usize,
    const_0: String,
}

impl X64BackEnd {
    pub fn new() -> Self {
        let mut s = Self {
            moira: MoiraProgram::new(),
            instruction_counter: 0,
            const_0: String::new(),
            temp_count: 0,
        };
        s.const_0 = s.make_static_constant(0.0);
        s
    }

    fn generate_moira(&mut self, program: &TackyProgram) -> Result<&MoiraProgram> {
        for data in program.static_variables.values() {
            self.moira.top_vars.push(data.clone());
        }
        for sc in program.static_constants.values() {
            self.moira
                .static_constants
                .insert(sc.name.clone(), sc.clone());
        }

        for function in &program.functions {
            self.moira.gen_function(function)?;
            self.gen_function(function)?;
        }
        Ok(&self.moira)
    }

    fn moira(&mut self, inst: Instruction) {
        self.moira.add_instruction(inst);
    }
    fn get_assembly_type(stype: &SymbolType) -> AssemblyType {
        match stype {
            SymbolType::Int32 => AssemblyType::LongWord,
            SymbolType::Int64 => AssemblyType::QuadWord,
            SymbolType::UInt32 => AssemblyType::LongWord,
            SymbolType::UInt64 => AssemblyType::QuadWord,
            SymbolType::Double => AssemblyType::Double,
            SymbolType::Pointer(_) => AssemblyType::QuadWord,
            SymbolType::Char => AssemblyType::Byte,
            SymbolType::SChar => AssemblyType::Byte,
            SymbolType::UChar => AssemblyType::Byte,
            SymbolType::Function(_, _) => AssemblyType::QuadWord, // TODO
            SymbolType::Array(_, _) => {
                let align = X64CodeGenerator::calculate_alignment(stype);
                AssemblyType::ByteArray(0, align)
            }
            SymbolType::Void => unreachable!(),
            SymbolType::Struct(_) | SymbolType::Union(_) => AssemblyType::ByteArray(0, 0),
        }
    }

    // chicken out route - copy entire struct into the stack of callee
    fn import_structure(&mut self, stype: &SymbolType, name: &str, op: &Operand) -> Result<()> {
        assert!(matches!(
            stype,
            SymbolType::Struct(_) | SymbolType::Union(_)
        ));
        let size = Parser::get_total_object_size(stype)?;
        //  Structs and unions of size 8, 16, 32, or 64 bits,
        if !(size == 1 || size == 2 || size == 4 || size == 8) {
            let align = X64CodeGenerator::calculate_alignment(stype);
            let copy = Operand::PseudoMem(name.to_string(), size, 0, align);

            self.moira(Instruction::Push(Operand::Register(Register::RCX)));
            self.moira(Instruction::CopyBlock(op.clone(), copy.clone(), size));
            self.moira(Instruction::Pop(Operand::Register(Register::RCX)));
        } else {
            let assembly_type = match size {
                1 => AssemblyType::Byte,
                2 => AssemblyType::Word,
                4 => AssemblyType::LongWord,
                8 => AssemblyType::QuadWord,
                _ => unreachable!(),
            };

            self.moira(Instruction::Mov(
                assembly_type.clone(),
                op.clone(),
                Operand::Pseudo(name.to_string()),
            ));
        };
        Ok(())
    }
    /*

    windows x64 calling convention

    - first 4 parameters in rcx, rdx, r8, r9
    - rest on stack, 8 bytes per parameter
    - return value in rax
    - stack aligned to 16 bytes
    - stack grows down, so first parameter is at 8 bytes from rbp

    - caller reseres space for the  register values just in case callee wants to store them

    - doubles go into xmm0, xmm1, xmm2, xmm3

    - structs are either in one register (if small enough) or passed by reference
     */
    fn gen_function(&mut self, function: &crate::tacky::Function) -> Result<()> {
        let int_reglist: [Register; 4] = [Register::RCX, Register::RDX, Register::R8, Register::R9];
        let float_reglist: [Register; 4] = [
            Register::XMM0,
            Register::XMM1,
            Register::XMM2,
            Register::XMM3,
        ];
        let ret_type = function.return_type.clone();

        // it the return type is a large struct then
        // - we allocate space for it
        // - we pass a pointer to it in rcx
        // - shunt the other params down by one register

        let shunt = if ret_type.is_struct_or_union() {
            let size = Parser::get_total_object_size(&ret_type)?;
            let register = Operand::Register(Register::RCX);

            if !(size == 1 || size == 2 || size == 4 || size == 8) {
                // save rcx so that return can use it

                self.moira(Instruction::Push(register));
                1
            } else {
                0
            }
        } else {
            0
        };

        for idx in 0..function.parameters.len() {
            let reg_idx = idx + shunt;
            if reg_idx < int_reglist.len() {
                let (param, stype) = &function.parameters[idx];

                if stype.is_struct_or_union() {
                    let reg = Operand::Register(int_reglist[reg_idx].clone());
                    self.import_structure(stype, param, &reg)?;
                } else {
                    let assembly_type = Self::get_assembly_type(stype);
                    let register = if assembly_type == AssemblyType::Double {
                        Operand::Register(float_reglist[reg_idx].clone())
                    } else {
                        Operand::Register(int_reglist[reg_idx].clone())
                    };
                    self.moira(Instruction::Mov(
                        assembly_type.clone(),
                        register,
                        Operand::Pseudo(param.clone()),
                    ));
                }
            } else {
                let (param, stype) =
                    &function.parameters[(function.parameters.len() - reg_idx) + 3];
                let assembly_type = Self::get_assembly_type(stype);
                if stype.is_struct_or_union() {
                    let reg = Operand::Register(Register::R10);
                    self.moira(Instruction::Mov(
                        assembly_type.clone(),
                        Operand::Memory(
                            Register::RBP,
                            ((function.parameters.len() - idx + 5) * 8) as i32,
                        ),
                        reg.clone(),
                    ));

                    self.import_structure(stype, param, &reg)?;
                } else {
                    self.moira(Instruction::Mov(
                        assembly_type.clone(),
                        Operand::Memory(
                            Register::RBP,
                            ((function.parameters.len() - idx + 5) * 8) as i32,
                        ),
                        Operand::Pseudo(param.clone()),
                    ));
                }
            }
        }
        for instruction in &function.instructions {
            self.instruction_counter += 1;
            self.gen_instruction(instruction)?;
        }
        Ok(())
    }
    fn generate_signed_immediate(assembly_type: AssemblyType, value: i32) -> Operand {
        match assembly_type {
            AssemblyType::LongWord => Operand::ImmediateI32(value),
            AssemblyType::QuadWord => Operand::ImmediateI64(value as i64),
            AssemblyType::Byte => Operand::ImmediateI8(value as i8),
            _ => todo!(),
        }
    }
    fn gen_label_name(&self, label: &str) -> String {
        format!("{}_{:04}", label, self.instruction_counter)
    }
    fn make_temporary(&mut self, stype: &SymbolType) -> tacky::Value {
        let temp_name = format!("ARG${}", self.temp_count);
        self.temp_count += 1;
        tacky::Value::Variable(temp_name.clone(), stype.clone())
    }

    // makes a local copy of a struct and return a pointer to it
    fn clone_struct(&mut self, arg: &tacky::Value, reg: &Operand) -> Result<()> {
        assert!(matches!(
            arg.stype(),
            SymbolType::Struct(_) | SymbolType::Union(_)
        ));
        let size = Parser::get_total_object_size(&arg.stype())?;
        if !(size == 1 || size == 2 || size == 4 || size == 8) {
            let (src_op, stype) = self.get_block_mem(arg, 0);
            let copy = self.make_temporary(&stype);
            let (dest_op, _dest_stype) = self.get_block_mem(&copy, 0);
            // copy block overwrites RCX
            self.moira(Instruction::Push(Operand::Register(Register::RCX)));
            self.moira(Instruction::CopyBlock(src_op, dest_op.clone(), size));
            self.moira(Instruction::Pop(Operand::Register(Register::RCX)));
            self.moira(Instruction::Lea(dest_op, reg.clone()));
            return Ok(());
        } else {
            let (src_op, _stype, _src_assembly_type) = self.get_value(arg);
            self.moira(Instruction::Mov(
                AssemblyType::QuadWord,
                src_op,
                reg.clone(),
            ));
        };
        Ok(())
    }
    fn gen_instruction(&mut self, instruction: &crate::tacky::Instruction) -> Result<()> {
        match instruction {
            tacky::Instruction::Return(value) => {
                if let Some(value) = value {
                    let (value, ret_type, assembly_type) = self.get_value(value);
                    let size = Parser::get_total_object_size(&ret_type)?;
                    if ret_type.is_struct_or_union()
                        && !(size == 1 || size == 2 || size == 4 || size == 8)
                    {
                        // picking up the RCX value that the prolog saved
                        let register = Operand::Register(Register::RAX);
                        self.moira(Instruction::Pop(register.clone()));
                        self.moira(Instruction::CopyBlock(value.clone(), register, size));
                    } else {
                        let ret_reg = if assembly_type == AssemblyType::Double {
                            Operand::Register(Register::XMM0)
                        } else {
                            Operand::Register(Register::RAX)
                        };
                        self.moira(Instruction::Mov(assembly_type, value, ret_reg));
                    }
                }
                let instruction = Instruction::Ret;
                self.moira(instruction);
            }
            tacky::Instruction::Unary(unary_operator, value1, value2) => {
                let (value1, stype1, assembly_type1) = self.get_value(value1);
                let (value2, stype2, assembly_type2) = self.get_value(value2);
                if *unary_operator != tacky::UnaryOperator::LogicalNot {
                    assert!(stype1 == stype2);
                };
                if Self::get_assembly_type(&stype1) == AssemblyType::Double {
                    match unary_operator {
                        tacky::UnaryOperator::Negate => {
                            let (const_minus_0, _, _) = self.get_value(&tacky::Value::Double(-0.0));
                            let scratch =
                                X64CodeGenerator::get_scratch_register1(&AssemblyType::Double);

                            self.moira(Instruction::Mov(
                                assembly_type1.clone(),
                                value1,
                                scratch.clone(),
                            ));
                            self.moira(Instruction::Binary(
                                BinaryOperator::BitXor,
                                assembly_type1.clone(),
                                const_minus_0,
                                scratch.clone(),
                            ));
                            self.moira(Instruction::Mov(
                                assembly_type1.clone(),
                                scratch.clone(),
                                value2.clone(),
                            ));
                        }
                        tacky::UnaryOperator::LogicalNot => {
                            let const_0 = self.get_value(&tacky::Value::Double(0.0));
                            self.moira(Instruction::FCmp(
                                assembly_type1.clone(),
                                value1.clone(),
                                const_0.0,
                            ));
                            self.moira(Instruction::Mov(
                                assembly_type2.clone(),
                                Self::generate_signed_immediate(assembly_type2, 0),
                                value2.clone(),
                            ));
                            let exit_label = self.gen_label_name("__nan");
                            self.moira(Instruction::JmpCC(CondCode::P, exit_label.clone()));
                            self.moira(Instruction::SetCC(CondCode::E, value2.clone()));
                            self.moira(Instruction::Label(exit_label.clone()));
                        }
                        _ => todo!(),
                    }
                } else {
                    let op = match unary_operator {
                        tacky::UnaryOperator::Negate => UnaryOperator::Neg,
                        tacky::UnaryOperator::Complement => UnaryOperator::Not,
                        tacky::UnaryOperator::LogicalNot => {
                            self.moira(Instruction::Cmp(
                                assembly_type1.clone(),
                                Self::generate_signed_immediate(assembly_type1.clone(), 0),
                                value1.clone(),
                            ));
                            self.moira(Instruction::Mov(
                                assembly_type1.clone(),
                                Self::generate_signed_immediate(assembly_type2.clone(), 0),
                                value2.clone(),
                            ));
                            self.moira(Instruction::SetCC(CondCode::E, value2.clone()));
                            return Ok(());
                        }
                    };
                    self.moira(Instruction::Mov(
                        assembly_type1.clone(),
                        value1,
                        value2.clone(),
                    ));
                    self.moira(Instruction::Unary(op, assembly_type1, value2));
                }
            }
            tacky::Instruction::Binary(binary_operator, src1, src2, dest) => {
                self.gen_binary(binary_operator, src1, src2, dest)?;
            }
            tacky::Instruction::Copy(src, dest) => {
                // bit blit to different types allowed

                if src.stype().is_struct_or_union() && dest.stype().is_struct_or_union() {
                    let (src_op, src_stype) = self.get_block_mem(src, 0);
                    let (dest_op, _dest_stype) = self.get_block_mem(dest, 0);
                    self.moira(Instruction::CopyBlock(
                        src_op,
                        dest_op,
                        Parser::get_total_object_size(&src_stype).unwrap(),
                    ));
                    return Ok(());
                } else {
                    let (src_op, _src_stype, src_assembly_type) = self.get_value(src);
                    let (dest_op, _dest_stype, dest_assembly_type) = self.get_value(dest);
                    assert!(src_assembly_type == dest_assembly_type);
                    self.moira(Instruction::Mov(src_assembly_type, src_op, dest_op));
                }
            }

            tacky::Instruction::Jump(label) => {
                self.moira(Instruction::Jmp(label.clone()));
            }
            tacky::Instruction::JumpIfZero(value, label) => {
                let (value, _stype, assembly_type) = self.get_value(value);
                if assembly_type == AssemblyType::Double {
                    let (const_0, _, _) = self.get_value(&tacky::Value::Double(0.0));
                    self.moira(Instruction::FCmp(
                        assembly_type.clone(),
                        value.clone(),
                        const_0,
                    ));
                    let exit_label = self.gen_label_name("__nan");
                    self.moira(Instruction::JmpCC(CondCode::P, exit_label.clone()));
                    self.moira(Instruction::JmpCC(CondCode::E, label.clone()));
                    self.moira(Instruction::Label(exit_label.clone()));
                } else {
                    self.moira(Instruction::Cmp(
                        assembly_type.clone(),
                        Self::generate_signed_immediate(assembly_type, 0),
                        value.clone(),
                    ));
                    self.moira(Instruction::JmpCC(CondCode::E, label.clone()));
                }
            }
            tacky::Instruction::JumpIfNotZero(value, label) => {
                let (value, _stype, assembly_type) = self.get_value(value);
                if assembly_type == AssemblyType::Double {
                    let (const_0, _, _) = self.get_value(&tacky::Value::Double(0.0));
                    self.moira(Instruction::FCmp(
                        assembly_type.clone(),
                        value.clone(),
                        const_0,
                    ));
                } else {
                    self.moira(Instruction::Cmp(
                        assembly_type.clone(),
                        Self::generate_signed_immediate(assembly_type, 0),
                        value.clone(),
                    ));
                }
                self.moira(Instruction::JmpCC(CondCode::NE, label.clone()));
            }
            tacky::Instruction::Label(label) => {
                self.moira(Instruction::Label(label.clone()));
            }
            tacky::Instruction::FunCall(name, args, dest) => {
                let int_reglist: [Register; 4] =
                    [Register::RCX, Register::RDX, Register::R8, Register::R9];
                let float_reglist: [Register; 4] = [
                    Register::XMM0,
                    Register::XMM1,
                    Register::XMM2,
                    Register::XMM3,
                ];

                let shunt = if dest.is_some() {
                    let retval = dest.clone().unwrap();
                    if retval.stype().is_struct_or_union() {
                        let (_, arg_stype, _arg_assembly_type) = self.get_value(&retval);
                        let size = Parser::get_total_object_size(&arg_stype)?;
                        let register = Operand::Register(Register::RCX);

                        if !(size == 1 || size == 2 || size == 4 || size == 8) {
                            // self.clone_struct(&retval, &register)?;
                            let align = X64CodeGenerator::calculate_alignment(&arg_stype);
                            self.moira(Instruction::Lea(
                                Operand::PseudoMem(
                                    retval.as_variable().unwrap().0.clone(),
                                    size,
                                    0,
                                    align,
                                ),
                                register.clone(),
                            ));
                            1
                        } else {
                            0
                        }
                    } else {
                        0
                    }
                } else {
                    0
                };
                let mut stack_delta = 32;

                // ensure stack stays 16 byte aligned
                if args.len() > 4 && (args.len() - 4) % 2 == 1 {
                    self.moira(Instruction::Push(Operand::Register(Register::RAX)));
                    stack_delta += 8;
                }
                for idx in 0..args.len() {
                    let reg_idx = idx + shunt;
                    if reg_idx < int_reglist.len() {
                        let arg = &args[idx];

                        if arg.stype().is_struct_or_union() {
                            self.clone_struct(
                                arg,
                                &Operand::Register(int_reglist[reg_idx].clone()),
                            )?;
                            // stack_delta += delta;
                        } else {
                            let (argval, _arg_stype, arg_assembly_type) = self.get_value(arg);
                            let register = if arg_assembly_type == AssemblyType::Double {
                                Operand::Register(float_reglist[reg_idx].clone())
                            } else {
                                Operand::Register(int_reglist[reg_idx].clone())
                            };
                            self.moira(Instruction::Mov(arg_assembly_type, argval, register));
                        }
                    } else {
                        let arg = &args[(args.len() - reg_idx) + 3];
                        let (argval, _arg_stype, arg_assembly_type) = self.get_value(arg);
                        if arg.stype().is_struct_or_union() {
                            self.clone_struct(arg, &Operand::Register(Register::R10))?;
                            //stack_delta += delta;
                            self.moira(Instruction::Push(Operand::Register(Register::R10)));
                            stack_delta += 8;
                        } else {
                            stack_delta += 8;
                            match argval {
                                Operand::ImmediateI32(_)
                                | Operand::ImmediateI64(_)
                                | Operand::ImmediateU32(_)
                                | Operand::ImmediateU64(_) => {
                                    self.moira(Instruction::Push(argval.clone()))
                                }

                                Operand::Pseudo(_) | Operand::Data(_, _) => {
                                    if arg_assembly_type == AssemblyType::Double
                                        || arg_assembly_type == AssemblyType::QuadWord
                                    {
                                        self.moira(Instruction::Push(argval.clone()))
                                    } else {
                                        self.moira(Instruction::Mov(
                                            arg_assembly_type,
                                            argval,
                                            Operand::Register(Register::RAX),
                                        ));
                                        self.moira(Instruction::Push(Operand::Register(
                                            Register::RAX,
                                        )));
                                    }
                                }

                                _ => panic!("Invalid Operand"),
                            }
                        }
                    }
                }
                self.moira(Instruction::AllocateStack(32));
                self.moira(Instruction::Call(name.clone()));
                self.moira(Instruction::DeallocateStack(stack_delta));
                if let Some(dest) = dest {
                    let (dest_op, stype, ret_assembly_type) = self.get_value(dest);
                    if dest.stype().is_struct_or_union() {
                        let size = Parser::get_total_object_size(&stype)?;

                        if !(size == 1 || size == 2 || size == 4 || size == 8) {
                        } else {
                            self.moira(Instruction::Mov(
                                ret_assembly_type.clone(),
                                Operand::Register(Register::RAX),
                                dest_op.clone(),
                            ));
                        }
                    } else {
                        let ret_reg = if ret_assembly_type == AssemblyType::Double {
                            Operand::Register(Register::XMM0)
                        } else {
                            Operand::Register(Register::RAX)
                        };
                        self.moira(Instruction::Mov(ret_assembly_type, ret_reg, dest_op));
                    }
                }
            }

            tacky::Instruction::SignExtend(src, dest) => {
                let (src, _src_stype, src_at) = self.get_value(src);
                let (dest, _dest_stype, dest_at) = self.get_value(dest);
                self.moira(Instruction::MovSignExtend(src_at, dest_at, src, dest));
            }
            tacky::Instruction::ZeroExtend(src, dest) => {
                let (src, _src_stype, src_at) = self.get_value(src);
                let (dest, _dest_stype, dest_at) = self.get_value(dest);

                self.moira(Instruction::MovZeroExtend(
                    src_at,
                    dest_at,
                    src,
                    dest.clone(),
                ));
            }
            tacky::Instruction::Truncate(src, dest) => {
                let (src, _src_stype, _) = self.get_value(src);
                let (dest, _dest_stype, _) = self.get_value(dest);
                self.moira(Instruction::Mov(AssemblyType::LongWord, src, dest));
            } //_ => todo!(),
            tacky::Instruction::DoubleToInt(src, dest) => {
                let (src, _src_stype, _) = self.get_value(src);
                let (dest, _dest_stype, dest_assembly_type) = self.get_value(dest);
                if dest_assembly_type == AssemblyType::Byte {
                    let scratch = X64CodeGenerator::get_scratch_register1(&AssemblyType::QuadWord);
                    self.moira(Instruction::Cvttsdsi(
                        AssemblyType::QuadWord,
                        src,
                        scratch.clone(),
                    ));
                    self.moira(Instruction::Mov(
                        AssemblyType::QuadWord,
                        scratch.clone(),
                        dest.clone(),
                    ));
                } else {
                    self.moira(Instruction::Cvttsdsi(dest_assembly_type, src, dest.clone()));
                }
            }
            tacky::Instruction::IntToDouble(src, dest) => {
                let (src, src_stype, src_assembly_type) = self.get_value(src);
                let (dest, _dest_stype, _dest_at) = self.get_value(dest);
                let (src, src_assembly_type) =
                    if matches!(src_stype, SymbolType::Char | SymbolType::SChar) {
                        let scratch =
                            X64CodeGenerator::get_scratch_register1(&AssemblyType::QuadWord);
                        self.moira(Instruction::MovSignExtend(
                            AssemblyType::Byte,
                            AssemblyType::QuadWord,
                            src,
                            scratch.clone(),
                        ));
                        (scratch, AssemblyType::QuadWord)
                    } else {
                        (src, src_assembly_type)
                    };
                self.moira(Instruction::Cvtsi2sd(src_assembly_type, src, dest));
            }
            tacky::Instruction::DoubleToUInt(src, dest) => {
                let (src, src_stype, _) = self.get_value(src);
                let (dest, dest_stype, _) = self.get_value(dest);
                let scratch_double = X64CodeGenerator::get_scratch_register1(&AssemblyType::Double);
                let scratch_int = X64CodeGenerator::get_scratch_register1(&AssemblyType::QuadWord);
                match dest_stype {
                    SymbolType::UInt32 => {
                        self.moira(Instruction::Cvttsdsi(
                            AssemblyType::QuadWord,
                            src,
                            scratch_int.clone(),
                        ));
                        self.moira(Instruction::Mov(
                            AssemblyType::LongWord,
                            scratch_int,
                            dest.clone(),
                        ));
                    }
                    SymbolType::UInt64 => {
                        let label1 = self.gen_label_name("__fp1");
                        let label2 = self.gen_label_name("__fp2");

                        let (const_max, _, _) =
                            self.get_value(&tacky::Value::Double(9223372036854775808.0));
                        self.moira(Instruction::FCmp(
                            AssemblyType::Double,
                            const_max.clone(),
                            src.clone(),
                        ));
                        self.moira(Instruction::JmpCC(CondCode::AE, label1.clone()));
                        self.moira(Instruction::Cvttsdsi(
                            AssemblyType::QuadWord,
                            src.clone(),
                            dest.clone(),
                        ));
                        self.moira(Instruction::Jmp(label2.clone()));
                        self.moira(Instruction::Label(label1.clone()));
                        self.moira(Instruction::Mov(
                            AssemblyType::Double,
                            src,
                            scratch_double.clone(),
                        ));
                        self.moira(Instruction::Binary(
                            BinaryOperator::FSub,
                            AssemblyType::Double,
                            const_max.clone(),
                            scratch_double.clone(),
                        ));
                        self.moira(Instruction::Cvttsdsi(
                            AssemblyType::QuadWord,
                            scratch_double,
                            dest.clone(),
                        ));
                        self.moira(Instruction::Mov(
                            AssemblyType::QuadWord,
                            Operand::ImmediateU64(9223372036854775808),
                            scratch_int.clone(),
                        ));
                        self.moira(Instruction::Binary(
                            BinaryOperator::Add,
                            AssemblyType::QuadWord,
                            scratch_int.clone(),
                            dest.clone(),
                        ));
                        self.moira(Instruction::Label(label2.clone()));
                    }
                    SymbolType::UChar => {
                        let scratch =
                            X64CodeGenerator::get_scratch_register1(&AssemblyType::QuadWord);
                        self.moira(Instruction::Cvttsdsi(
                            AssemblyType::LongWord,
                            src,
                            scratch.clone(),
                        ));
                        self.moira(Instruction::Mov(AssemblyType::Byte, scratch, dest));
                    }
                    _ => panic!("Invalid type for DoubleToUInt: {:?}", src_stype),
                }
            }
            tacky::Instruction::UIntToDouble(src, dest) => {
                let (src, src_stype, _) = self.get_value(src);
                let (dest, _dest_stype, _) = self.get_value(dest);
                match src_stype {
                    SymbolType::UInt32 => {
                        let scratch =
                            X64CodeGenerator::get_scratch_register1(&AssemblyType::QuadWord);
                        self.moira(Instruction::Mov(
                            AssemblyType::LongWord,
                            src,
                            scratch.clone(),
                        ));
                        self.moira(Instruction::Cvtsi2sd(AssemblyType::QuadWord, scratch, dest));
                    }
                    SymbolType::UInt64 => {
                        let label1 = self.gen_label_name("__fp1");
                        let label2 = self.gen_label_name("__fp2");
                        let scratch1 =
                            X64CodeGenerator::get_scratch_register1(&AssemblyType::QuadWord);
                        let scratch2 =
                            X64CodeGenerator::get_scratch_register2(&AssemblyType::QuadWord);

                        self.moira(Instruction::Cmp(
                            AssemblyType::QuadWord,
                            Self::generate_signed_immediate(AssemblyType::QuadWord, 0),
                            src.clone(),
                        ));
                        self.moira(Instruction::JmpCC(CondCode::L, label1.clone()));
                        self.moira(Instruction::Cvtsi2sd(
                            AssemblyType::QuadWord,
                            src.clone(),
                            dest.clone(),
                        ));
                        self.moira(Instruction::Jmp(label2.clone()));
                        self.moira(Instruction::Label(label1.clone()));
                        self.moira(Instruction::Mov(
                            AssemblyType::QuadWord,
                            src,
                            scratch1.clone(),
                        ));
                        self.moira(Instruction::Mov(
                            AssemblyType::QuadWord,
                            scratch1.clone(),
                            scratch2.clone(),
                        ));
                        self.moira(Instruction::Binary(
                            BinaryOperator::ShiftRight,
                            AssemblyType::QuadWord,
                            Self::generate_signed_immediate(AssemblyType::QuadWord, 1),
                            scratch2.clone(),
                        ));
                        self.moira(Instruction::Binary(
                            BinaryOperator::BitAnd,
                            AssemblyType::QuadWord,
                            Self::generate_signed_immediate(AssemblyType::QuadWord, 1),
                            scratch1.clone(),
                        ));
                        self.moira(Instruction::Binary(
                            BinaryOperator::BitOr,
                            AssemblyType::QuadWord,
                            scratch1.clone(),
                            scratch2.clone(),
                        ));
                        self.moira(Instruction::Cvtsi2sd(
                            AssemblyType::QuadWord,
                            scratch2,
                            dest.clone(),
                        ));
                        self.moira(Instruction::Binary(
                            BinaryOperator::FAdd,
                            AssemblyType::Double,
                            dest.clone(),
                            dest.clone(),
                        ));
                        self.moira(Instruction::Label(label2.clone()));
                    }
                    SymbolType::UChar => {
                        let scratch1 =
                            X64CodeGenerator::get_scratch_register1(&AssemblyType::QuadWord);
                        self.moira(Instruction::MovZeroExtend(
                            AssemblyType::Byte,
                            AssemblyType::LongWord,
                            src,
                            scratch1.clone(),
                        ));
                        self.moira(Instruction::Cvtsi2sd(
                            AssemblyType::LongWord,
                            scratch1.clone(),
                            dest.clone(),
                        ));
                    }
                    _ => panic!("Invalid type for UIntToDouble: {:?}", src_stype),
                }
            }
            tacky::Instruction::Load(ptr, dest) => {
                if ptr.stype().get_inner_type()?.is_struct_or_union() {
                    assert!(ptr.stype().get_inner_type()? == dest.stype());
                    let (ptr_op, _ptr_stype) = self.get_block_mem(ptr, 0);
                    let (dest_op, _dest_stype) = self.get_block_mem(dest, 0);
                    self.moira(Instruction::Mov(
                        AssemblyType::QuadWord,
                        ptr_op,
                        Operand::Register(Register::RAX),
                    ));

                    self.moira(Instruction::CopyBlock(
                        Operand::Register(Register::RAX),
                        dest_op,
                        Parser::get_total_object_size(&dest.stype()).unwrap(),
                    ));
                } else {
                    let (src, _src_stype, _) = self.get_value(ptr);
                    let (dest, _dest_stype, dest_assembly_type) = self.get_value(dest);
                    self.moira(Instruction::Mov(
                        AssemblyType::QuadWord,
                        src,
                        Operand::Register(Register::RAX),
                    ));
                    self.moira(Instruction::Mov(
                        dest_assembly_type,
                        Operand::Memory(Register::RAX, 0),
                        dest.clone(),
                    ));
                }
            }
            tacky::Instruction::Store(src, ptr) => {
                if ptr.stype().get_inner_type()?.is_struct_or_union() {
                    assert!(ptr.stype().get_inner_type()? == src.stype());
                    let (ptr_op, _ptr_stype) = self.get_block_mem(ptr, 0);
                    let (dest_op, _dest_stype) = self.get_block_mem(src, 0);
                    self.moira(Instruction::Mov(
                        AssemblyType::QuadWord,
                        ptr_op,
                        Operand::Register(Register::RAX),
                    ));

                    self.moira(Instruction::CopyBlock(
                        dest_op,
                        Operand::Register(Register::RAX),
                        Parser::get_total_object_size(&src.stype()).unwrap(),
                    ));
                } else {
                    let (src, _src_stype, src_assembly_type) = self.get_value(src);
                    let (ptr, _ptr_stype, _) = self.get_value(ptr);
                    self.moira(Instruction::Mov(
                        AssemblyType::QuadWord,
                        ptr,
                        Operand::Register(Register::RAX),
                    ));
                    self.moira(Instruction::Mov(
                        src_assembly_type,
                        src,
                        Operand::Memory(Register::RAX, 0),
                    ));
                }
            }
            tacky::Instruction::GetAddress(src, dest) => {
                let (src, _src_stype, _) = self.get_value(src);
                let (dest, _dest_stype, _) = self.get_value(dest);

                self.moira(Instruction::Lea(src, dest));
            }
            tacky::Instruction::AddPtr(ptr, idx, scal, dest) => {
                let (ptr, _ptr_stype, _) = self.get_value(ptr);
                let (idx, _idx_stype, _) = self.get_value(idx);
                //  let (scal, _scal_stype, _) = self.get_value(scal);
                let (dest, _dest_stype, _) = self.get_value(dest);
                match &idx {
                    Operand::ImmediateI64(idx) => {
                        self.moira(Instruction::Mov(
                            AssemblyType::QuadWord,
                            ptr.clone(),
                            Operand::Register(Register::RAX),
                        ));
                        self.moira(Instruction::Lea(
                            Operand::Memory(Register::RAX, (*idx as isize * *scal) as i32),
                            dest.clone(),
                        ));
                    }
                    Operand::Data(_, _) | Operand::Pseudo(_) => {
                        if *scal == 1 || *scal == 2 || *scal == 4 || *scal == 8 {
                            self.moira(Instruction::Mov(
                                AssemblyType::QuadWord,
                                ptr.clone(),
                                Operand::Register(Register::RAX),
                            ));
                            self.moira(Instruction::Mov(
                                AssemblyType::QuadWord,
                                idx.clone(),
                                Operand::Register(Register::RDX),
                            ));
                            self.moira(Instruction::Lea(
                                Operand::Indexed(Register::RAX, Register::RDX, *scal as usize),
                                dest.clone(),
                            ));
                        } else {
                            self.moira(Instruction::Mov(
                                AssemblyType::QuadWord,
                                ptr.clone(),
                                Operand::Register(Register::RAX),
                            ));
                            self.moira(Instruction::Mov(
                                AssemblyType::QuadWord,
                                idx.clone(),
                                Operand::Register(Register::RDX),
                            ));
                            self.moira(Instruction::Binary(
                                BinaryOperator::Mult,
                                AssemblyType::QuadWord,
                                Operand::ImmediateU64(*scal as u64),
                                Operand::Register(Register::RDX),
                            ));
                            self.moira(Instruction::Lea(
                                Operand::Indexed(Register::RAX, Register::RDX, 1),
                                dest.clone(),
                            ));
                        }
                    }
                    _ => unreachable!(),
                }
            }
            tacky::Instruction::CopyToOffset(src, dest, offset) => {
                let (src_op, src_stype, src_assembly_type) = self.get_value(src);
                let (dest_op, _dest_stype) = self.get_block_mem(dest, *offset);
                if src_stype.is_scalar() {
                    //  assert!(src_assembly_type == dest_assembly_type);
                    self.moira(Instruction::Mov(src_assembly_type, src_op, dest_op));

                    return Ok(());
                } else {
                    self.moira(Instruction::CopyBlock(
                        src_op,
                        dest_op,
                        Parser::get_total_object_size(&src_stype).unwrap(),
                    ));
                }
            }
            tacky::Instruction::CopyFromOffset(src, offset, dest) => {
                let (src_op, _src_stype) = self.get_block_mem(src, *offset);
                let (dest_op, dest_stype, dest_at) = self.get_value(dest);
                if dest_stype.is_scalar() {
                    //  assert!(src_assembly_type == dest_at);
                    self.moira(Instruction::Mov(dest_at, src_op, dest_op));
                } else {
                    self.moira(Instruction::CopyBlock(
                        src_op,
                        dest_op,
                        Parser::get_total_object_size(&dest_stype).unwrap(),
                    ));
                }
            }
        }
        Ok(())
    }

    fn are_same_type(stype1: &SymbolType, stype2: &SymbolType) -> bool {
        if stype1 == stype2 {
            return true;
        }
        if stype1.is_char() && stype2.is_s_char() {
            return true;
        }
        if stype1.is_s_char() && stype2.is_char() {
            return true;
        }
        false
    }
    fn gen_binary(
        &mut self,
        binary_operator: &tacky::BinaryOperator,
        src1: &tacky::Value,
        src2: &tacky::Value,
        dest: &tacky::Value,
    ) -> Result<()> {
        let (src1, src1_stype, assembly_type) = self.get_value(src1);
        let (src2, src2_stype, _) = self.get_value(src2);
        let (dest, dest_stype, dest_at) = self.get_value(dest);
        if src1_stype == SymbolType::Double {
            assert!(src2_stype == SymbolType::Double);
            if binary_operator == &tacky::BinaryOperator::Add
                || binary_operator == &tacky::BinaryOperator::Subtract
                || binary_operator == &tacky::BinaryOperator::Multiply
                || binary_operator == &tacky::BinaryOperator::Divide
            {
                assert!(dest_stype == SymbolType::Double);
            }

            let op = match binary_operator {
                tacky::BinaryOperator::Add => Some(BinaryOperator::FAdd),
                tacky::BinaryOperator::Subtract => Some(BinaryOperator::FSub),
                tacky::BinaryOperator::Multiply => Some(BinaryOperator::FMul),
                tacky::BinaryOperator::Divide => Some(BinaryOperator::FDiv),
                _ => None,
            };
            if let Some(op) = op {
                self.moira(Instruction::Mov(assembly_type.clone(), src1, dest.clone()));
                self.moira(Instruction::Binary(op, assembly_type.clone(), src2, dest));
                Ok(())
            } else {
                match binary_operator {
                    tacky::BinaryOperator::Equal
                    | tacky::BinaryOperator::NotEqual
                    | tacky::BinaryOperator::LessThan
                    | tacky::BinaryOperator::LessThanOrEqual
                    | tacky::BinaryOperator::GreaterThan
                    | tacky::BinaryOperator::GreaterThanOrEqual => {
                        assert!(src1_stype == src2_stype);
                        self.moira(Instruction::FCmp(assembly_type.clone(), src2, src1));

                        let cc = match binary_operator {
                            tacky::BinaryOperator::Equal => CondCode::E,
                            tacky::BinaryOperator::NotEqual => CondCode::NE,
                            tacky::BinaryOperator::LessThan => CondCode::B,
                            tacky::BinaryOperator::LessThanOrEqual => CondCode::BE,
                            tacky::BinaryOperator::GreaterThan => CondCode::A,
                            tacky::BinaryOperator::GreaterThanOrEqual => CondCode::AE,
                            _ => unreachable!(),
                        };
                        let nan_val = if *binary_operator == tacky::BinaryOperator::NotEqual {
                            Self::generate_signed_immediate(AssemblyType::LongWord, 1)
                        } else {
                            Self::generate_signed_immediate(AssemblyType::LongWord, 0)
                        };
                        self.moira(Instruction::Mov(
                            AssemblyType::LongWord,
                            nan_val,
                            dest.clone(),
                        ));
                        let exit_label = self.gen_label_name("__nan");
                        self.moira(Instruction::JmpCC(CondCode::P, exit_label.clone()));
                        self.moira(Instruction::SetCC(cc, dest));
                        self.moira(Instruction::Label(exit_label.clone()));
                    }
                    _ => {
                        unreachable!();
                    }
                }
                Ok(())
            }
        } else {
            let op = match binary_operator {
                tacky::BinaryOperator::Add => Some(BinaryOperator::Add),
                tacky::BinaryOperator::Subtract => Some(BinaryOperator::Sub),
                tacky::BinaryOperator::Multiply => Some(BinaryOperator::Mult),
                tacky::BinaryOperator::BitAnd => Some(BinaryOperator::BitAnd),
                tacky::BinaryOperator::BitOr => Some(BinaryOperator::BitOr),
                tacky::BinaryOperator::BitXor => Some(BinaryOperator::BitXor),
                tacky::BinaryOperator::ShiftLeft => Some(BinaryOperator::ShiftLeft),
                tacky::BinaryOperator::ShiftRight => {
                    if Parser::is_signed(&src1_stype) {
                        Some(BinaryOperator::ShiftRightArith)
                    } else {
                        Some(BinaryOperator::ShiftRight)
                    }
                }
                _ => None,
            };

            if let Some(op) = op {
                if op != BinaryOperator::ShiftLeft
                    && op != BinaryOperator::ShiftRightArith
                    && op != BinaryOperator::ShiftRight
                {
                    assert!(Self::are_same_type(&src1_stype, &src2_stype));
                    assert!(Self::are_same_type(&src1_stype, &dest_stype));
                }
                self.moira(Instruction::Mov(assembly_type.clone(), src1, dest.clone()));
                self.moira(Instruction::Binary(op, assembly_type.clone(), src2, dest));
            } else {
                match binary_operator {
                    tacky::BinaryOperator::Divide | tacky::BinaryOperator::Remainder => {
                        assert!(src1_stype == dest_stype);
                        self.moira(Instruction::Mov(
                            assembly_type.clone(),
                            src1,
                            Operand::Register(Register::RAX),
                        ));

                        if Parser::is_signed(&src1_stype) {
                            self.moira(Instruction::Cdq(assembly_type.clone()));
                            self.moira(Instruction::Idiv(assembly_type.clone(), src2));
                        } else {
                            self.moira(Instruction::Mov(
                                assembly_type.clone(),
                                Self::generate_signed_immediate(assembly_type.clone(), 0),
                                Operand::Register(Register::RDX),
                            ));
                            self.moira(Instruction::Div(assembly_type.clone(), src2));
                        }

                        match binary_operator {
                            tacky::BinaryOperator::Divide => {
                                let instruction = Instruction::Mov(
                                    assembly_type.clone(),
                                    Operand::Register(Register::RAX),
                                    dest,
                                );
                                self.moira(instruction);
                            }
                            tacky::BinaryOperator::Remainder => {
                                let instruction = Instruction::Mov(
                                    assembly_type,
                                    Operand::Register(Register::RDX),
                                    dest,
                                );
                                self.moira(instruction);
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
                        assert!(Self::are_same_type(&src1_stype, &src2_stype));
                        self.moira(Instruction::Cmp(assembly_type.clone(), src2, src1));
                        self.moira(Instruction::Mov(
                            dest_at.clone(),
                            Self::generate_signed_immediate(dest_at.clone(), 0),
                            dest.clone(),
                        ));
                        let cc = if Parser::is_signed(&src1_stype) {
                            match binary_operator {
                                tacky::BinaryOperator::Equal => CondCode::E,
                                tacky::BinaryOperator::NotEqual => CondCode::NE,
                                tacky::BinaryOperator::LessThan => CondCode::L,
                                tacky::BinaryOperator::LessThanOrEqual => CondCode::LE,
                                tacky::BinaryOperator::GreaterThan => CondCode::G,
                                tacky::BinaryOperator::GreaterThanOrEqual => CondCode::GE,
                                _ => unreachable!(),
                            }
                        } else {
                            match binary_operator {
                                tacky::BinaryOperator::Equal => CondCode::E,
                                tacky::BinaryOperator::NotEqual => CondCode::NE,
                                tacky::BinaryOperator::LessThan => CondCode::B,
                                tacky::BinaryOperator::LessThanOrEqual => CondCode::BE,
                                tacky::BinaryOperator::GreaterThan => CondCode::A,
                                tacky::BinaryOperator::GreaterThanOrEqual => CondCode::AE,
                                _ => unreachable!(),
                            }
                        };
                        self.moira(Instruction::SetCC(cc, dest));
                    }
                    _ => {
                        unreachable!();
                    }
                }
            }
            Ok(())
        }
    }

    pub fn make_static_constant(&mut self, value: f64) -> String {
        let strval = format!("{:?}", value).replace('-', "_");
        let const_label = if let Some(v) = self.moira.static_constants.get(&strval) {
            v.name.clone()
        } else {
            let const_label = format!("__const_{}", strval);
            self.moira.static_constants.insert(
                strval.clone(),
                StaticConstant {
                    name: const_label.clone(),
                    init: vec![tacky::StaticInit::InitDouble(value)],
                    align: 16,
                    global: false,
                    external: false,
                    stype: SymbolType::Double,
                },
            );
            const_label
        };
        const_label
    }
    fn get_value(&mut self, value: &crate::tacky::Value) -> (Operand, SymbolType, AssemblyType) {
        match value {
            tacky::Value::Int32(value) => (
                Operand::ImmediateI32(*value),
                SymbolType::Int32,
                AssemblyType::LongWord,
            ),
            tacky::Value::UInt32(value) => (
                Operand::ImmediateU32(*value),
                SymbolType::UInt32,
                AssemblyType::LongWord,
            ),
            tacky::Value::UInt64(value) => (
                Operand::ImmediateU64(*value),
                SymbolType::UInt64,
                AssemblyType::QuadWord,
            ),
            tacky::Value::Int64(value) => (
                Operand::ImmediateI64(*value),
                SymbolType::Int64,
                AssemblyType::QuadWord,
            ),
            tacky::Value::Char(value) => (
                Operand::ImmediateI8(*value),
                SymbolType::Char,
                AssemblyType::Byte,
            ),
            tacky::Value::String(_) => todo!(),
            tacky::Value::UChar(value) => (
                Operand::ImmediateU8(*value),
                SymbolType::UChar,
                AssemblyType::Byte,
            ),
            tacky::Value::Double(value) => {
                let const_label = self.make_static_constant(*value);
                let dest = Operand::Data(const_label, 0);

                (dest, SymbolType::Double, AssemblyType::Double)
            }
            tacky::Value::Void => unreachable!(),
            tacky::Value::Variable(register, symbol_type) => {
                let assembly_type = Self::get_assembly_type(symbol_type);
                if self.moira.top_vars.iter().any(|v| v.name == *register)
                    || self.moira.static_constants.contains_key(register)
                {
                    (
                        Operand::Data(register.clone(), 0),
                        symbol_type.clone(),
                        assembly_type,
                    )
                } else if symbol_type.is_array() || symbol_type.is_struct_or_union() {
                    let align = X64CodeGenerator::calculate_alignment(symbol_type);
                    let total_size = Parser::get_total_object_size(symbol_type).unwrap();
                    (
                        Operand::PseudoMem(register.clone(), total_size, 0, align),
                        symbol_type.clone(),
                        assembly_type,
                    )
                // } else if symbol_type.is_struct_or_union() {
                //     let align = X64CodeGenerator::calculate_alignment(symbol_type);
                //     let total_size = Parser::get_total_object_size(symbol_type).unwrap();

                //     (
                //         Operand::PseudoMem(register.clone(), total_size, 0, align),
                //         symbol_type.clone(),
                //         assembly_type,
                //     )
                } else if symbol_type.is_scalar() {
                    (
                        Operand::Pseudo(register.clone()),
                        symbol_type.clone(),
                        assembly_type,
                    )
                } else {
                    panic!("Invalid value type: {:?}", symbol_type);
                }
            }
        }
    }
    fn get_block_mem(
        &mut self,

        value: &crate::tacky::Value,
        offset: usize,
    ) -> (Operand, SymbolType) {
        let (name, symbol_type) = value.as_variable().unwrap();

        let align = X64CodeGenerator::calculate_alignment(symbol_type);
        let total_size = Parser::get_total_object_size(symbol_type).unwrap();
        if self.moira.top_vars.iter().any(|v| v.name == *name)
            || self.moira.static_constants.contains_key(name)
        {
            return (Operand::Data(name.clone(), offset), symbol_type.clone());
        }
        (
            Operand::PseudoMem(name.clone(), total_size, offset, align),
            symbol_type.clone(),
        )
    }
}

impl BackEnd for X64BackEnd {
    fn compile(&mut self, tacky: &TackyProgram, output: &Path) -> Result<()> {
        let moira = self.generate_moira(tacky)?;
        moira.dump();
        let mut x64gen = X64CodeGenerator::new();
        x64gen.generate_asm(moira, output)?;
        Ok(())
    }
}
