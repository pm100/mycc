use std::{cmp::max, collections::HashMap};

use crate::{
    codegen::MoiraGenerator,
    moira::{MoiraProgram, StaticVariable},
    parser::Symbol,
    tacky::{StaticInit, SymbolType, TackyProgram},
};
use anyhow::Result;

use super::moira_inst::{
    AssemblyType, BinaryOperator, CondCode, Instruction, Operand, Register, UnaryOperator,
};
pub struct X64MoiraGenerator {
    moira: MoiraProgram<Instruction>,
}

// impl Default for X64MoiraGenerator {
//     fn default() -> Self {
//         Self::new()
//     }
// }
use crate::tacky;
impl X64MoiraGenerator {
    pub fn new() -> Self {
        Self {
            moira: MoiraProgram::new(),
        }
    }
    fn moira(&mut self, inst: Instruction) {
        self.moira.add_instruction(inst);
    }
    fn get_assembly_type(stype: &SymbolType) -> AssemblyType {
        match stype {
            SymbolType::Int32 => AssemblyType::LongWord,
            SymbolType::Int64 => AssemblyType::QuadWord,
            _ => unreachable!(),
        }
    }
    fn gen_function(&mut self, function: &crate::tacky::Function) -> Result<()> {
        let reglist: [Register; 4] = [Register::RCX, Register::RDX, Register::R8, Register::R9];

        for idx in 0..function.parameters.len() {
            if idx < reglist.len() {
                let (param, stype) = &function.parameters[idx];
                let assembly_type = Self::get_assembly_type(stype);
                self.moira(Instruction::Mov(
                    assembly_type.clone(),
                    Operand::Register(reglist[idx].clone()),
                    Operand::Pseudo(param.clone()),
                ));
            } else {
                let (param, stype) = &function.parameters[(function.parameters.len() - idx) + 3];
                let assembly_type = Self::get_assembly_type(stype);
                self.moira(Instruction::Mov(
                    assembly_type.clone(),
                    Operand::Stack(((function.parameters.len() - idx + 5) * 8) as i32),
                    Operand::Pseudo(param.clone()),
                ));
            }
        }
        for instruction in &function.instructions {
            self.gen_instruction(instruction)?;
        }
        Ok(())
    }
    fn generate_immediate(assembly_type: AssemblyType, value: i32) -> Operand {
        match assembly_type {
            AssemblyType::LongWord => Operand::ImmediateI32(value),
            AssemblyType::QuadWord => Operand::ImmediateI64(value as i64),
            _ => todo!(),
        }
    }

    fn gen_instruction(&mut self, instruction: &crate::tacky::Instruction) -> Result<()> {
        //println!("gen_instruction: {:?}", instruction);
        match instruction {
            tacky::Instruction::Return(value) => {
                let (value, stype, assembly_type) = self.get_value(value);

                self.moira(Instruction::Mov(
                    assembly_type,
                    value,
                    Operand::Register(Register::RAX),
                ));

                let instruction = Instruction::Ret;
                self.moira(instruction);
            }
            tacky::Instruction::Unary(unary_operator, value1, value2) => {
                let (value1, stype1, assembly_type1) = self.get_value(value1);
                let (value2, stype2, _) = self.get_value(value2);
                assert!(stype1 == stype2);
                let op = match unary_operator {
                    tacky::UnaryOperator::Negate => UnaryOperator::Neg,
                    tacky::UnaryOperator::Complement => UnaryOperator::Not,
                    tacky::UnaryOperator::LogicalNot => {
                        self.moira(Instruction::Cmp(
                            assembly_type1.clone(),
                            Self::generate_immediate(assembly_type1.clone(), 0),
                            value1.clone(),
                        ));
                        self.moira(Instruction::Mov(
                            assembly_type1.clone(),
                            Self::generate_immediate(assembly_type1.clone(), 0),
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
            tacky::Instruction::Binary(binary_operator, src1, src2, dest) => {
                self.gen_binary(binary_operator, src1, src2, dest)?;
            }
            tacky::Instruction::Copy(src, dest) => {
                let (src, src_stype, src_assembly_type) = self.get_value(src);
                let (dest, dest_stype, _) = self.get_value(dest);
                assert!(src_stype == dest_stype);
                self.moira(Instruction::Mov(src_assembly_type, src, dest));
            }
            tacky::Instruction::Jump(label) => {
                self.moira(Instruction::Jmp(label.clone()));
            }
            tacky::Instruction::JumpIfZero(value, label) => {
                let (value, stype, assembly_type) = self.get_value(value);
                self.moira(Instruction::Cmp(
                    assembly_type.clone(),
                    Self::generate_immediate(assembly_type, 0),
                    value.clone(),
                ));
                self.moira(Instruction::JmpCC(CondCode::E, label.clone()));
            }
            tacky::Instruction::JumpIfNotZero(value, label) => {
                let (value, stype, assembly_type) = self.get_value(value);
                self.moira(Instruction::Cmp(
                    assembly_type.clone(),
                    Self::generate_immediate(assembly_type, 0),
                    value.clone(),
                ));
                self.moira(Instruction::JmpCC(CondCode::NE, label.clone()));
            }
            tacky::Instruction::Label(label) => {
                self.moira(Instruction::Label(label.clone()));
            }
            tacky::Instruction::FunCall(name, args, dest) => {
                let (dest, stype, ret_assembly_type) = self.get_value(dest);
                let reglist: [Register; 4] =
                    [Register::RCX, Register::RDX, Register::R8, Register::R9];
                let stack_delta = max(args.len(), 4) as i32 * 8;

                for idx in 0..args.len() {
                    if idx < reglist.len() {
                        let arg = &args[idx];
                        let (argval, arg_stype, arg_assembly_type) = self.get_value(arg);
                        self.moira(Instruction::Mov(
                            arg_assembly_type,
                            argval,
                            Operand::Register(reglist[idx].clone()),
                        ));
                    } else {
                        let arg = &args[(args.len() - idx) + 3];
                        let (argval, arg_stype, arg_assembly_type) = self.get_value(arg);
                        match argval {
                            Operand::ImmediateI32(v) => {
                                self.moira(Instruction::Push(Operand::ImmediateI32(v)))
                            }
                            Operand::ImmediateI64(v) => {
                                self.moira(Instruction::Push(Operand::ImmediateI64(v)))
                            }
                            Operand::Pseudo(v) => {
                                self.moira(Instruction::Mov(
                                    arg_assembly_type,
                                    Operand::Pseudo(v.clone()),
                                    Operand::Register(Register::RAX),
                                ));
                                self.moira(Instruction::Push(Operand::Register(Register::RAX)));
                            }
                            Operand::Data(v) => {
                                self.moira(Instruction::Mov(
                                    arg_assembly_type,
                                    Operand::Data(v.clone()),
                                    Operand::Register(Register::RAX),
                                ));
                                self.moira(Instruction::Push(Operand::Register(Register::RAX)));
                            }
                            _ => panic!("Invalid Operand"),
                        }
                    }
                }
                self.moira(Instruction::AllocateStack(32));
                self.moira(Instruction::Call(name.clone()));
                self.moira(Instruction::DeallocateStack(stack_delta));

                self.moira(Instruction::Mov(
                    ret_assembly_type,
                    Operand::Register(Register::RAX),
                    dest,
                ));
            }

            tacky::Instruction::SignExtend(src, dest) => {
                let (src, src_stype, _) = self.get_value(&src);
                let (dest, dest_stype, _) = self.get_value(&dest);
                //assert!(src_stype == dest_stype);
                self.moira(Instruction::SignExtend(src, dest));
            }
            tacky::Instruction::Truncate(src, dest) => {
                let (src, src_stype, _) = self.get_value(&src);
                let (dest, dest_stype, _) = self.get_value(&dest);
                //  assert!(src_stype == dest_stype);
                self.moira(Instruction::Mov(AssemblyType::LongWord, src, dest));
            }
            _ => todo!(),
        }
        Ok(())
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
        let (dest, dest_stype, _) = self.get_value(dest);

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
            if op != BinaryOperator::ShiftLeft && op != BinaryOperator::ShiftRight {
                assert!(src1_stype == src2_stype);
                assert!(src1_stype == dest_stype);
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
                    self.moira(Instruction::Cdq(assembly_type.clone()));
                    self.moira(Instruction::Idiv(assembly_type.clone(), src2));

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
                    assert!(src1_stype == src2_stype);
                    self.moira(Instruction::Cmp(assembly_type.clone(), src2, src1));
                    self.moira(Instruction::Mov(
                        assembly_type.clone(),
                        Self::generate_immediate(assembly_type.clone(), 0),
                        dest.clone(),
                    ));
                    let cc = match binary_operator {
                        tacky::BinaryOperator::Equal => CondCode::E,
                        tacky::BinaryOperator::NotEqual => CondCode::NE,
                        tacky::BinaryOperator::LessThan => CondCode::L,
                        tacky::BinaryOperator::LessThanOrEqual => CondCode::LE,
                        tacky::BinaryOperator::GreaterThan => CondCode::G,
                        tacky::BinaryOperator::GreaterThanOrEqual => CondCode::GE,
                        _ => unreachable!(),
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
    fn get_value(&self, value: &crate::tacky::Value) -> (Operand, SymbolType, AssemblyType) {
        match value {
            tacky::Value::Int32(value) => (
                Operand::ImmediateI32(*value),
                SymbolType::Int32,
                AssemblyType::LongWord,
            ),
            tacky::Value::Variable(register, symbol_type) => {
                let assembly_type = match symbol_type {
                    SymbolType::Int32 => AssemblyType::LongWord,
                    SymbolType::Int64 => AssemblyType::QuadWord,
                    _ => unreachable!(),
                };
                if self.moira.top_vars.iter().any(|v| v.name == *register) {
                    (
                        Operand::Data(register.clone()),
                        symbol_type.clone(),
                        assembly_type,
                    )
                } else {
                    (
                        Operand::Pseudo(register.clone()),
                        symbol_type.clone(),
                        assembly_type,
                    )
                }
            }
            tacky::Value::Int64(value) => (
                Operand::ImmediateI64(*value as i64),
                SymbolType::Int64,
                AssemblyType::QuadWord,
            ),
        }
    }
}

impl MoiraGenerator for X64MoiraGenerator {
    type InstructionType = Instruction;
    fn generate_moira(&mut self, program: &TackyProgram) -> Result<&MoiraProgram<Instruction>> {
        for data in program.static_variables.values() {
            // let v = match data.init {
            //     StaticInit => value,

            //     None => 0,
            //     _ => panic!("Invalid static variable value"),
            // };
            self.moira.top_vars.push(StaticVariable {
                name: data.name.clone(),
                global: data.global,
                value: data.init.clone(),
                external: data.external,
                stype: data.stype.clone(),
            });
        }
        for function in &program.functions {
            self.moira.gen_function(function)?;
            self.gen_function(function)?;
        }
        Ok(&self.moira)
    }
}
