use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Rem, Shl, Shr, Sub};

use anyhow::Result;
use backtrace::Symbol;
use bitflags::bitflags;
use num_traits::{WrappingAdd, WrappingMul, WrappingShl, WrappingShr, WrappingSub};

use crate::{
    parser::Parser,
    symbols::SymbolType,
    tacky::{BinaryOperator, Instruction, TackyProgram, UnaryOperator, Value},
};
bitflags! {
    pub struct OptimizeFlags: u8 {
        const CONSTANT_FOLD = 0b00000001;
        const DEAD_STORE = 0b00000010;
        const COPY_PROP = 0b00000100;
        const UNREACHABLE = 0b00001000;
    }
}
macro_rules! unary {
    ($src:ident,$op:path) => {
        match $src.stype() {
            SymbolType::Int32 => Value::Int32($op(*$src.as_int32().unwrap())),
            SymbolType::UInt32 => Value::UInt32($op(*$src.as_u_int32().unwrap())),
            SymbolType::Int64 => Value::Int64($op(*$src.as_int64().unwrap())),
            SymbolType::UInt64 => Value::UInt64($op(*$src.as_u_int64().unwrap())),
            //   SymbolType::Double => Value::Double($op(*$src.as_double().unwrap())),
            SymbolType::Char | SymbolType::SChar => Value::Char($op(*$src.as_char().unwrap())),
            SymbolType::UChar => Value::UChar($op(*$src.as_u_char().unwrap())),
            _ => panic!("Unsupported type for binop: {:?}", $src.stype()),
        }
    };
}
macro_rules! binop_num {
    ($left:ident,$right:ident, $op:path) => {
        match $left.stype() {
            SymbolType::Int32 => {
                Value::Int32($op($left.as_int32().unwrap(), $right.as_int32().unwrap()))
            }
            SymbolType::UInt32 => Value::UInt32($op(
                $left.as_u_int32().unwrap(),
                $right.as_u_int32().unwrap(),
            )),
            SymbolType::Int64 => {
                Value::Int64($op($left.as_int64().unwrap(), $right.as_int64().unwrap()))
            }
            SymbolType::UInt64 => Value::UInt64($op(
                $left.as_u_int64().unwrap(),
                $right.as_u_int64().unwrap(),
            )),
            // SymbolType::Double => {
            //     Value::Double($op($left.as_double().unwrap(), $right.as_double().unwrap()))
            // }
            SymbolType::Char | SymbolType::SChar => {
                Value::Char($op($left.as_char().unwrap(), $right.as_char().unwrap()))
            }
            SymbolType::UChar => {
                Value::UChar($op($left.as_u_char().unwrap(), $right.as_u_char().unwrap()))
            }
            _ => panic!("Unsupported type for binop: {:?}", $left.stype()),
        }
    };
}
macro_rules! binop_logic {
    ($left:ident,$right:ident, $op:path) => {
        match $left.stype() {
            SymbolType::Int32 => {
                Value::Int32($op(*$left.as_int32().unwrap(), *$right.as_int32().unwrap()))
            }
            SymbolType::UInt32 => Value::UInt32($op(
                *$left.as_u_int32().unwrap(),
                *$right.as_u_int32().unwrap(),
            )),
            SymbolType::Int64 => {
                Value::Int64($op(*$left.as_int64().unwrap(), *$right.as_int64().unwrap()))
            }
            SymbolType::UInt64 => Value::UInt64($op(
                *$left.as_u_int64().unwrap(),
                *$right.as_u_int64().unwrap(),
            )),

            SymbolType::Char | SymbolType::SChar => {
                Value::Char($op(*$left.as_char().unwrap(), *$right.as_char().unwrap()))
            }
            SymbolType::UChar => Value::UChar($op(
                *$left.as_u_char().unwrap(),
                *$right.as_u_char().unwrap(),
            )),
            _ => panic!("Unsupported type for binop: {:?}", $left.stype()),
        }
    };
}
macro_rules! binop_shift {
    ($left:ident,$right:ident, $op:path) => {{
        println!("binop_shift: {:?} {:?} ", $left.stype(), $right.stype(),);
        match $left.stype() {
            SymbolType::Int32 => Value::Int32($op(
                $left.as_int32().unwrap(),
                *$right.as_int32().unwrap() as u32,
            )),
            SymbolType::UInt32 => Value::UInt32($op(
                $left.as_u_int32().unwrap(),
                *$right.as_int32().unwrap() as u32,
            )),
            SymbolType::Int64 => Value::Int64($op(
                $left.as_int64().unwrap(),
                *$right.as_int32().unwrap() as u32,
            )),
            SymbolType::UInt64 => Value::UInt64($op(
                $left.as_u_int64().unwrap(),
                *$right.as_int32().unwrap() as u32,
            )),
            SymbolType::Char | SymbolType::SChar => Value::Char($op(
                $left.as_char().unwrap(),
                *$right.as_int32().unwrap() as u32,
            )),
            SymbolType::UChar => Value::UChar($op(
                $left.as_u_char().unwrap(),
                *$right.as_int32().unwrap() as u32,
            )),
            _ => panic!("Unsupported type for binop: {:?}", $left.stype()),
        }
    }};
}
macro_rules! binop_bool {
    ($left:ident,$right:ident, $op:path) => {
        match $left.stype() {
            SymbolType::Int32 => {
                let l = $left.as_int32().unwrap();
                let r = $right.as_int32().unwrap();
                let comp = $op(l, r);
                Value::Int32(if comp { 1 } else { 0 })
            }
            SymbolType::UInt32 => {
                let l = $left.as_u_int32().unwrap();
                let r = $right.as_u_int32().unwrap();
                let comp = $op(l, r);
                Value::Int32(if comp { 1 } else { 0 })
            }
            SymbolType::Int64 => {
                let l = $left.as_int64().unwrap();
                let r = $right.as_int64().unwrap();
                let comp = $op(l, r);
                Value::Int32(if comp { 1 } else { 0 })
            }
            SymbolType::UInt64 => {
                let l = $left.as_u_int64().unwrap();
                let r = $right.as_u_int64().unwrap();
                let comp = $op(l, r);
                Value::Int32(if comp { 1 } else { 0 })
            }
            SymbolType::Double => {
                let l = $left.as_double().unwrap();
                let r = $right.as_double().unwrap();
                let comp = $op(l, r);
                Value::Int32(if comp { 1 } else { 0 })
            }
            SymbolType::Char | SymbolType::SChar => {
                let l = $left.as_char().unwrap();
                let r = $right.as_char().unwrap();
                let comp = $op(l, r);
                Value::Int32(if comp { 1 } else { 0 })
            }
            SymbolType::UChar => {
                let l = $left.as_u_char().unwrap();
                let r = $right.as_u_char().unwrap();
                let comp = $op(l, r);
                Value::Int32(if comp { 1 } else { 0 })
            }
            _ => panic!("Unsupported type for binop: {:?}", $left.stype()),
        }
    };
}
impl Parser {
    pub fn optimize(&mut self, flags: OptimizeFlags) -> Result<()> {
        // let mut tacky = self.tacky.clone();
        loop {
            let mut changed_tacky = false;
            if flags.contains(OptimizeFlags::CONSTANT_FOLD) {
                let changed = &self.fold_constants()?;

                changed_tacky |= changed;
            }
            // if flags.contains(OptimizeFlags::DEAD_STORE) {
            //     change_count += self.eliminate_dead_stores()?
            // }
            // if flags.contains(OptimizeFlags::COPY_PROP) {
            //     change_count += self.propagate_copies()?
            // }
            // if flags.contains(OptimizeFlags::UNREACHABLE) {
            //     change_count += self.eliminate_unreachable_code()?
            // }
            if !changed_tacky {
                break;
            }
        }
        self.tacky.dump();
        //   let x = tacky.clone();
        Ok(())
    }
    fn is_zero(value: &Value) -> bool {
        match value {
            Value::Int32(v) => *v == 0,
            Value::UInt32(v) => *v == 0,
            Value::Int64(v) => *v == 0,
            Value::UInt64(v) => *v == 0,
            Value::Double(v) => *v == 0.0,
            Value::Char(v) => *v == 0,
            Value::UChar(v) => *v == 0,
            _ => todo!(), // Handle other types as needed
        }
    }
    fn fold_constants(&mut self) -> Result<bool> {
        // Implementation for constant folding
        for idx in 0..self.tacky.functions.len() {
            let old_instructions = std::mem::take(&mut self.tacky.functions[idx].instructions); //.clone();
            self.tacky.functions[idx].instructions = Vec::new();
            for instruction in old_instructions {
                let new_instruction = match instruction {
                    Instruction::Unary(ref op, ref src, ref dest) => {
                        if src.is_constant() {
                            let result = match op {
                                UnaryOperator::Complement => unary!(src, std::ops::Not::not),
                                UnaryOperator::LogicalNot => {
                                    Value::Int32(Self::is_zero(&src) as i32)
                                }
                                UnaryOperator::Negate => match src.stype() {
                                    SymbolType::Int32 => Value::Int32(-*src.as_int32().unwrap()),
                                    SymbolType::Int64 => Value::Int64(-*src.as_int64().unwrap()),
                                    SymbolType::Double => Value::Double(-*src.as_double().unwrap()),
                                    SymbolType::Char | SymbolType::SChar => {
                                        Value::Char(-*src.as_char().unwrap())
                                    }
                                    _ => src.clone(), // Handle other types as needed
                                },
                            };
                            Instruction::Copy(result, dest.clone())
                        } else {
                            instruction
                        }
                    }
                    Instruction::JumpIfZero(ref cond, ref label) => {
                        if cond.is_constant() {
                            if Self::is_zero(cond) {
                                Instruction::Jump(label.clone())
                            } else {
                                continue;
                            }
                        } else {
                            instruction
                        }
                    }
                    Instruction::JumpIfNotZero(ref cond, ref label) => {
                        if cond.is_constant() {
                            if !Self::is_zero(cond) {
                                Instruction::Jump(label.clone())
                            } else {
                                continue;
                            }
                        } else {
                            instruction
                        }
                    }
                    Instruction::Binary(ref op, ref left, ref right, ref dest) => {
                        if left.is_constant() && right.is_constant() {
                            if left.stype() != right.stype()
                                && !matches!(
                                    op,
                                    BinaryOperator::ShiftLeft | BinaryOperator::ShiftRight
                                )
                            {
                                panic!(
                                    "Type mismatch in binary operation: {:?} vs {:?}",
                                    left.stype(),
                                    right.stype()
                                );
                            };
                            let result = match op {
                                BinaryOperator::Add => {
                                    binop_num!(left, right, WrappingAdd::wrapping_add)
                                }
                                BinaryOperator::Subtract => {
                                    binop_num!(left, right, WrappingSub::wrapping_sub)
                                }
                                BinaryOperator::Multiply => {
                                    binop_num!(left, right, WrappingMul::wrapping_mul)
                                }
                                BinaryOperator::Divide => binop_num!(left, right, Div::div),
                                BinaryOperator::Remainder => binop_num!(left, right, Rem::rem),
                                BinaryOperator::BitAnd => binop_logic!(left, right, BitAnd::bitand),
                                BinaryOperator::BitXor => {
                                    binop_logic!(left, right, BitXor::bitxor)
                                }
                                BinaryOperator::BitOr => binop_logic!(left, right, BitOr::bitor),
                                BinaryOperator::ShiftLeft => {
                                    binop_shift!(left, right, WrappingShl::wrapping_shl)
                                }
                                BinaryOperator::ShiftRight => {
                                    binop_shift!(left, right, WrappingShr::wrapping_shr)
                                }

                                BinaryOperator::Equal => {
                                    binop_bool!(left, right, std::cmp::PartialEq::eq)
                                }
                                BinaryOperator::NotEqual => {
                                    binop_bool!(left, right, std::cmp::PartialEq::ne)
                                }
                                BinaryOperator::LessThan => {
                                    binop_bool!(left, right, std::cmp::PartialOrd::lt)
                                }
                                BinaryOperator::LessThanOrEqual => {
                                    binop_bool!(left, right, std::cmp::PartialOrd::le)
                                }
                                BinaryOperator::GreaterThan => {
                                    binop_bool!(left, right, std::cmp::PartialOrd::gt)
                                }
                                BinaryOperator::GreaterThanOrEqual => {
                                    binop_bool!(left, right, std::cmp::PartialOrd::ge)
                                }
                            };
                            Instruction::Copy(result, dest.clone())
                        } else {
                            instruction
                        }
                    }
                    _ => instruction,
                };
                self.tacky.functions[idx].instructions.push(new_instruction);
            }
        }
        Ok(false) // Placeholder
    }
}
