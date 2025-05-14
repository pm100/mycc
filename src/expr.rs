use crate::{
    lexer::Token,
    parser::Parser,
    symbols::SymbolType,
    tacky::{BinaryOperator, Instruction, PendingResult, StaticInit, UnaryOperator, Value},
};
use anyhow::{bail, Result};
//use backtrace::Symbol;
//use backtrace::Symbol;

impl Parser {
    pub(crate) fn do_rvalue_expression(&mut self) -> Result<Value> {
        self.static_init = false;
        let expr = self.do_expression(0)?;
        println!("do_rvalue_expression {:?}", expr);
        self.make_rvalue(&expr)
    }

    // entry point called when parsing initializers
    // strings get treated differently in init vs expr cases

    pub fn do_init_expression(&mut self, is_auto: bool, stype: &SymbolType) -> Result<Value> {
        self.static_init = !is_auto;
        let expr = self.do_expression(0)?;
        println!("make_init_rvalue {:?}", expr);
        let ret = match &expr {
            PendingResult::Dereference(var) => {
                assert!(var.is_pointer(), "Expected pointer, got {:?}", var);
                let ptype = Self::get_type(var);
                let stype = Self::get_pointee_type(&ptype)?;
                let deref_dest = match stype {
                    SymbolType::Array(t, _) => {
                        let name = var.as_variable().unwrap().0.clone();
                        let dest = Value::Variable(name, SymbolType::Pointer(t));
                        dest.clone()
                    }
                    _ => {
                        let dest = self.make_temporary(&stype);
                        self.instruction(Instruction::Load(var.clone(), dest.clone()));
                        dest
                    }
                };
                println!("dereferenced {:?}->{:?}", expr.clone(), deref_dest);
                deref_dest
            }
            PendingResult::PlainValue(var) => var.clone(),
        };

        if ret.is_string() {
            if stype.is_pointer() && !self.static_init {
                return self.array_to_pointer(&ret);
            }
            if stype.is_array() {
                return Ok(ret);
            }
        }
        Ok(ret)
    }
    fn do_expression(&mut self, min_prec: i32) -> Result<PendingResult> {
        let mut left = self.do_unary()?;

        loop {
            let token = self.peek()?;
            if Self::precedence(&token) < min_prec {
                break;
            }

            // these are all assignments of some kind
            // we need to check for lvalue here
            // plus make derefenced pointer into lvalue
            let dest = match token {
                Token::Assign
                | Token::AndEquals
                | Token::MinusEquals
                | Token::DivideEquals
                | Token::MultiplyEquals
                | Token::XorEquals
                | Token::OrEquals
                | Token::PlusEquals
                | Token::ShiftLeftEquals
                | Token::ShiftRightEquals
                | Token::RemainderEquals => {
                    self.next_token()?;

                    let right = self.do_expression(Self::precedence(&token))?;
                    let right_rv = self.make_rvalue(&right)?;
                    match token {
                        // simple assignment
                        Token::Assign => {
                            let result = self.store_into_lvalue(&right_rv, &left)?;

                            // ensure we do not return an lvalue

                            let result_type = result.stype();
                            let not_lvalue = self.make_temporary(&result_type);
                            self.instruction(Instruction::Copy(result, not_lvalue.clone()));

                            not_lvalue
                        }

                        _ => {
                            let op = Self::convert_compound(&token)?;
                            self.process_compound(&left, &right, &op)?
                        }
                    }
                }

                Token::LogicalAnd => {
                    self.next_token()?;
                    let left_rv = self.make_rvalue(&left)?;
                    let label_false = self.make_label("and_false");
                    let label_end = self.make_label("and_end");
                    self.instruction(Instruction::JumpIfZero(left_rv, label_false.clone()));
                    let right = self.do_expression(Self::precedence(&token) + 1)?;
                    let right = self.make_rvalue(&right)?;
                    self.instruction(Instruction::JumpIfZero(right, label_false.clone()));

                    let dest = self.make_temporary(&SymbolType::Int32);
                    self.instruction(Instruction::Copy(Value::Int32(1), dest.clone()));
                    self.instruction(Instruction::Jump(label_end.clone()));
                    self.instruction(Instruction::Label(label_false.clone()));
                    self.instruction(Instruction::Copy(Value::Int32(0), dest.clone()));
                    self.instruction(Instruction::Label(label_end.clone()));
                    dest
                }
                Token::LogicalOr => {
                    self.next_token()?;
                    let left_rv = self.make_rvalue(&left)?;
                    let label_true = self.make_label("or_true");
                    let label_end = self.make_label("or_end");
                    self.instruction(Instruction::JumpIfNotZero(left_rv, label_true.clone()));
                    let right = self.do_expression(Self::precedence(&token) + 1)?;
                    let right = self.make_rvalue(&right)?;
                    self.instruction(Instruction::JumpIfNotZero(right, label_true.clone()));

                    let dest = self.make_temporary(&SymbolType::Int32);
                    self.instruction(Instruction::Copy(Value::Int32(0), dest.clone()));
                    self.instruction(Instruction::Jump(label_end.clone()));
                    self.instruction(Instruction::Label(label_true.clone()));
                    self.instruction(Instruction::Copy(Value::Int32(1), dest.clone()));
                    self.instruction(Instruction::Label(label_end.clone()));
                    dest
                }
                Token::QuestionMark => {
                    self.next_token()?;
                    let left_rv = self.make_rvalue(&left)?;
                    let exit_label = self.make_label("ternary_exit");
                    let maybe_true_label = self.make_label("ternary_maybe_true");
                    let false_label = self.make_label("ternary_false");
                    let condition = left_rv;
                    self.instruction(Instruction::JumpIfZero(condition, false_label.clone()));

                    //true
                    let true_exp = self.do_expression(0)?;
                    let true_exp = self.make_rvalue(&true_exp)?;
                    let mut result = self.make_temporary(&Self::get_type(&true_exp));
                    self.instruction(Instruction::Copy(true_exp.clone(), result.clone()));
                    self.instruction(Instruction::Jump(maybe_true_label.clone()));

                    // false
                    self.instruction(Instruction::Label(false_label.clone()));
                    self.expect(Token::Colon)?;
                    let false_exp = self.do_expression(Self::precedence(&token))?;
                    let false_exp = self.make_rvalue(&false_exp)?;
                    if Self::get_type(&false_exp) != Self::get_type(&true_exp) {
                        let common_type = Self::get_common_type(
                            &Self::get_type(&true_exp),
                            &Self::get_type(&false_exp),
                        )?;
                        let false_converted = self.convert_to(&false_exp, &common_type, false)?;
                        result = self.make_temporary(&common_type);
                        self.instruction(Instruction::Copy(false_converted, result.clone()));
                        self.instruction(Instruction::Jump(exit_label.clone()));
                        self.instruction(Instruction::Label(maybe_true_label.clone()));
                        let true_converted = self.convert_to(&true_exp, &common_type, false)?;

                        self.instruction(Instruction::Copy(true_converted, result.clone()));
                    } else {
                        self.instruction(Instruction::Copy(false_exp, result.clone()));
                        self.instruction(Instruction::Label(maybe_true_label.clone()));
                    }
                    self.instruction(Instruction::Label(exit_label.clone()));
                    result
                }
                _ => {
                    let op = self.convert_binop()?;
                    let left_rv = self.make_rvalue(&left)?;

                    let left_type = Self::get_type(&left_rv);

                    let right = self.do_expression(Self::precedence(&token) + 1)?;
                    let right = self.make_rvalue(&right)?;
                    let right_type = Self::get_type(&right);
                    self.check_binary_allowed(&op, &left_type, &right_type)?;

                    // lets special case pointer arithmetic immediately
                    if (op == BinaryOperator::Add || op == BinaryOperator::Subtract)
                        && (left_type.is_pointer() || right_type.is_pointer())
                    {
                        if left_type.is_pointer() && right_type.is_pointer() {
                            let left_pointee = Self::get_pointee_type(&left_type)?;
                            if left_pointee != Self::get_pointee_type(&Self::get_type(&right))? {
                                bail!(
                                    "Cannot subtract two pointers of different types {:?} {:?}",
                                    left_type,
                                    Self::get_type(&right)
                                );
                            }
                            // must be subtract - the check will have thrown out add
                            assert!(op == BinaryOperator::Subtract);
                            let l_long = self.convert_to(&left_rv, &SymbolType::Int64, true)?;
                            let r_long = self.convert_to(&right, &SymbolType::Int64, true)?;
                            let diff = self.make_temporary(&SymbolType::Int64);
                            self.instruction(Instruction::Binary(
                                BinaryOperator::Subtract,
                                l_long,
                                r_long,
                                diff.clone(),
                            ));
                            let size = Self::get_total_object_size(&left_pointee)?;
                            let res = self.make_temporary(&SymbolType::Int64);

                            self.instruction(Instruction::Binary(
                                BinaryOperator::Divide,
                                diff,
                                Value::Int64(size as i64),
                                res.clone(),
                            ));
                            res
                        } else {
                            let (pointer, delta) = if left_type.is_pointer() {
                                (left_rv, right)
                            } else {
                                (right, left_rv)
                            };
                            self.do_pointer_arithmetic(&op, &pointer, &delta)?
                        }
                    } else if op == BinaryOperator::ShiftLeft || op == BinaryOperator::ShiftRight {
                        let promo = if left_type.is_character() {
                            self.convert_to(&left_rv, &SymbolType::Int32, true)?
                        } else {
                            left_rv.clone()
                        };
                        let dest = self.make_temporary(&promo.stype());
                        self.instruction(Instruction::Binary(op, promo, right, dest.clone()));
                        dest
                    } else {
                        let common_type =
                            if matches!(op, BinaryOperator::Equal | BinaryOperator::NotEqual)
                                && (left_rv.is_pointer() || right.is_pointer())
                            {
                                Self::get_common_pointer_type(&left_rv, &right)
                            } else {
                                Self::get_common_type(&left_type, &right_type)
                            }?;
                        let left = self.convert_to(&left_rv, &common_type, false)?;
                        let right = self.convert_to(&right, &common_type, false)?;
                        let dest_type = if op == BinaryOperator::Equal
                            || op == BinaryOperator::NotEqual
                            || op == BinaryOperator::LessThan
                            || op == BinaryOperator::LessThanOrEqual
                            || op == BinaryOperator::GreaterThan
                            || op == BinaryOperator::GreaterThanOrEqual
                        {
                            SymbolType::Int32
                        } else {
                            common_type.clone()
                        };
                        let dest = self.make_temporary(&dest_type);
                        let inst = Instruction::Binary(op, left, right, dest.clone());
                        self.instruction(inst);
                        dest
                    }
                }
            };
            left = PendingResult::PlainValue(dest);
        }
        Ok(left)
    }
    fn do_pointer_arithmetic(
        &mut self,
        op: &BinaryOperator,
        pointer: &Value,
        delta: &Value,
    ) -> Result<Value> {
        assert!(matches!(op, BinaryOperator::Add | BinaryOperator::Subtract));
        let pointer = if pointer.is_string() {
            if self.static_init {
                bail!("constant only here");
            }
            self.string_to_pointer(pointer)?
        } else {
            pointer.clone()
        };
        assert!(pointer.is_pointer());
        let delta = if *op == BinaryOperator::Subtract {
            match delta {
                Value::Int64(v) => Value::Int64(-v),
                Value::Int32(v) => Value::Int64(-v as i64),
                Value::UInt64(v) => Value::Int64(-(*v as i64)),
                Value::UInt32(v) => Value::Int64(-(*v as i64)),
                _ => {
                    let delta = self.convert_to(delta, &SymbolType::Int64, true)?;

                    let new_delta = self.make_temporary(&SymbolType::Int64);
                    self.instruction(Instruction::Unary(
                        UnaryOperator::Negate,
                        delta.clone(),
                        new_delta.clone(),
                    ));
                    new_delta
                }
            }
        } else {
            self.convert_to(delta, &SymbolType::Int64, true)?
        };
        assert!(Self::get_type(&delta) == SymbolType::Int64);
        let size =
            Self::get_total_object_size(&Self::get_pointee_type(&Self::get_type(&pointer))?)?;
        let update = self.make_temporary(&Self::get_type(&pointer));
        self.instruction(Instruction::AddPtr(
            pointer.clone(),
            delta.clone(),
            size as isize,
            update.clone(),
        ));
        Ok(update)
    }
    // x op= y
    fn process_compound(
        &mut self,
        left: &PendingResult,
        right: &PendingResult,
        op: &BinaryOperator,
    ) -> Result<Value> {
        // we have var op= value
        // a 2 step approach
        // evaluate (var op value) -> binval
        // store binval into var

        let left_rv = self.make_rvalue(left)?;
        let right_rv = self.make_rvalue(right)?;
        let left_type = Self::get_type(&left_rv);
        let right_type = Self::get_type(&right_rv);
        self.check_binary_allowed(op, &left_type, &right_type)?;

        let update = if left_rv.is_pointer() {
            self.do_pointer_arithmetic(op, &left_rv, &right_rv)?
        } else {
            let (left_conv, right_conv) =
                if *op == BinaryOperator::ShiftLeft || *op == BinaryOperator::ShiftRight {
                    (left_rv.clone(), right_rv.clone())
                } else {
                    let common_type = Self::get_common_type(&left_type.clone(), &right_type)?;
                    let left_conv = self.convert_to(&left_rv, &common_type, false)?;
                    let right_conv = self.convert_to(&right_rv, &common_type, false)?;
                    (left_conv, right_conv)
                };
            let left_conv_type = Self::get_type(&left_conv);
            let binval = self.make_temporary(&left_conv_type);

            self.instruction(Instruction::Binary(
                op.clone(),
                left_conv.clone(),
                right_conv.clone(),
                binval.clone(),
            ));
            binval
        };
        let result = self.store_into_lvalue(&update, left)?;

        // we need to make sure the the result is *not* an lvalue

        let result_type = Self::get_type(&result);
        let not_lvalue = self.make_temporary(&result_type);
        self.instruction(Instruction::Copy(result, not_lvalue.clone()));
        Ok(not_lvalue.clone())
    }

    pub fn convert_by_assignment(
        &mut self,
        value: &Value,
        target_type: &SymbolType,
    ) -> Result<Value> {
        println!("convert_by_assignment {:?} {:?}", value, target_type);
        let vtype = Self::get_type(value);
        if vtype == *target_type {
            // easy case
            return Ok(value.clone());
        }
        if Self::is_arithmetic(&vtype) && Self::is_arithmetic(target_type) {
            return self.convert_to(value, target_type, false);
        }
        if target_type.is_pointer()
            && vtype.is_array()
            && Self::get_inner_type(target_type)? == Self::get_inner_type(&vtype)?
        {
            // array to pointer decay
            return self.array_to_pointer(value);
        }
        if target_type.is_pointer() && Self::is_null_pointer_constant(value) {
            return Ok(Value::UInt64(0));
        }
        bail!("Cannot convert {:?} to {:?}", value, target_type);
    }

    pub fn get_type(value: &Value) -> SymbolType {
        match value {
            Value::Int32(_) => SymbolType::Int32,
            Value::Int64(_) => SymbolType::Int64,
            Value::UInt32(_) => SymbolType::UInt32,
            Value::UInt64(_) => SymbolType::UInt64,
            Value::Double(_) => SymbolType::Double,
            Value::Char(_) => SymbolType::Char,
            Value::UChar(_) => SymbolType::UChar,
            Value::String(_) => SymbolType::Pointer(Box::new(SymbolType::Char)),
            Value::Variable(_, t) => t.clone(),
            //   _ => todo!(),
        }
    }
    pub fn is_signed(symbol: &SymbolType) -> bool {
        match symbol {
            SymbolType::Int32 | SymbolType::Int64 => true,
            SymbolType::Char | SymbolType::SChar => true,
            SymbolType::UInt32 | SymbolType::UInt64 => false,
            _ => false,
        }
    }
    pub fn get_size_of_stype(symbol: &SymbolType) -> usize {
        match symbol {
            SymbolType::Int32 | SymbolType::UInt32 => 4,
            SymbolType::Int64 | SymbolType::UInt64 => 8,
            SymbolType::Double => 8,
            SymbolType::Function(_, _) => 8,
            SymbolType::Pointer(_) => 8,
            SymbolType::Char => 1,
            SymbolType::UChar => 1,
            SymbolType::SChar => 1,
            SymbolType::Array(t, _) => Self::get_total_object_size(t).unwrap(),
        }
    }
    fn get_common_pointer_type(a: &Value, b: &Value) -> Result<SymbolType> {
        let atype = Self::get_type(a);
        let btype = Self::get_type(b);
        if atype == btype {
            return Ok(atype.clone());
        }
        if Self::is_null_pointer_constant(a) {
            return Ok(btype.clone());
        }
        if Self::is_null_pointer_constant(b) {
            return Ok(atype.clone());
        }
        bail!(
            "Cannot convert between different pointer types {:?} {:?}",
            a,
            b
        );
    }

    fn get_common_type(a: &SymbolType, b: &SymbolType) -> Result<SymbolType> {
        let a = if a.is_character() {
            &SymbolType::Int32
        } else {
            a
        };
        let b = if b.is_character() {
            &SymbolType::Int32
        } else {
            b
        };
        if a == b {
            return Ok(a.clone());
        }
        if a == &SymbolType::Double || b == &SymbolType::Double {
            return Ok(SymbolType::Double);
        }

        let sizea = Self::get_size_of_stype(a);
        let sizeb = Self::get_size_of_stype(b);
        if sizea == sizeb {
            if Self::is_signed(a) {
                return Ok(b.clone());
            } else {
                return Ok(a.clone());
            }
        }
        Ok(if sizea > sizeb { a.clone() } else { b.clone() })
    }
    // note that this converts immediates as well as variables
    pub fn convert_to(
        &mut self,
        value: &Value,
        target_type: &SymbolType,
        explicit_cast: bool,
    ) -> Result<Value> {
        println!("convert {:?} to {:?}", value, target_type);
        let vtype = Self::get_type(value);

        let return_value = if vtype == *target_type {
            // easy case
            value.clone()
        } else {
            match (&value, target_type) {
                // arithmetic immediates
                // TODO - would be simpler if XIntXX was always i128
                (Value::Int32(v), SymbolType::Int64) => Value::Int64(*v as i64),
                (Value::Int32(v), SymbolType::UInt64) => Value::UInt64(*v as u64),
                (Value::Int32(v), SymbolType::UInt32) => Value::UInt32(*v as u32),
                (Value::Int64(v), SymbolType::Int32) => Value::Int32(*v as i32),
                (Value::Int64(v), SymbolType::UInt32) => Value::UInt32(*v as u32),
                (Value::Int64(v), SymbolType::UInt64) => Value::UInt64(*v as u64),
                (Value::UInt32(v), SymbolType::Int32) => Value::Int32(*v as i32),
                (Value::UInt32(v), SymbolType::Int64) => Value::Int64(*v as i64),
                (Value::UInt32(v), SymbolType::UInt64) => Value::UInt64(*v as u64),
                (Value::UInt64(v), SymbolType::Int32) => Value::Int32(*v as i32),
                (Value::UInt64(v), SymbolType::Int64) => Value::Int64(*v as i64),
                (Value::UInt64(v), SymbolType::UInt32) => Value::UInt32(*v as u32),
                (Value::UInt64(v), SymbolType::Double) => Value::Double(*v as f64),
                (Value::Int32(v), SymbolType::Double) => Value::Double(*v as f64),
                (Value::Int64(v), SymbolType::Double) => Value::Double(*v as f64),
                (Value::UInt32(v), SymbolType::Double) => Value::Double(*v as f64),
                (Value::Double(v), SymbolType::Int32) => Value::Int32(*v as i32),
                (Value::Double(v), SymbolType::Int64) => Value::Int64(*v as i64),
                (Value::Double(v), SymbolType::UInt32) => Value::UInt32(*v as u32),
                (Value::Double(v), SymbolType::UInt64) => Value::UInt64(*v as u64),
                (Value::Double(v), SymbolType::Char) => Value::Char(*v as i8),
                (Value::Double(v), SymbolType::UChar) => Value::UChar(*v as u8),
                (Value::Double(v), SymbolType::SChar) => Value::Char(*v as i8),
                (Value::Int32(v), SymbolType::SChar) => Value::Char(*v as i8),
                (Value::Int32(v), SymbolType::Char) => Value::Char(*v as i8),
                (Value::Int32(v), SymbolType::UChar) => Value::UChar(*v as u8),
                (Value::UInt32(v), SymbolType::Char) => Value::Char(*v as i8),
                (Value::UInt32(v), SymbolType::SChar) => Value::Char(*v as i8),
                (Value::UInt32(v), SymbolType::UChar) => Value::UChar(*v as u8),
                (Value::Int64(v), SymbolType::SChar) => Value::Char(*v as i8),
                (Value::Int64(v), SymbolType::Char) => Value::Char(*v as i8),
                (Value::Int64(v), SymbolType::UChar) => Value::UChar(*v as u8),
                (Value::UInt64(v), SymbolType::SChar) => Value::Char(*v as i8),
                (Value::UInt64(v), SymbolType::Char) => Value::Char(*v as i8),
                (Value::UInt64(v), SymbolType::UChar) => Value::UChar(*v as u8),
                (Value::Char(v), SymbolType::SChar) => Value::Char(*v),
                (Value::Char(v), SymbolType::UChar) => Value::UChar(*v as u8),
                (Value::Char(_), _) => bail!("Cannot convert char to integer type"),

                // null pointer conversion
                (
                    Value::UInt64(0) | Value::Int64(0) | Value::Int32(0) | Value::UInt32(0),
                    SymbolType::Pointer(_),
                ) => {
                    let dest = self.make_temporary(target_type);
                    self.instruction(Instruction::Copy(Value::Int64(0), dest.clone()));
                    return Ok(dest.clone());
                }

                // variables
                _ => {
                    let dest = self.make_temporary(target_type);
                    assert!(!value.is_string());
                    match (&vtype, target_type) {
                        (SymbolType::Pointer(_), SymbolType::Pointer(_)) => {
                            if !explicit_cast {
                                if Self::decay_arg(&vtype) == Self::decay_arg(target_type) {
                                    return Ok(value.clone());
                                }
                                bail!("Cannot implicitly convert pointer t1 to pointer t2");
                            }
                            return Ok(value.clone());
                        }
                        (SymbolType::Pointer(_), _) => {
                            if !explicit_cast {
                                bail!("Cannot convert pointer to non-pointer type");
                            }
                            if !Self::is_integer(target_type) {
                                bail!("Cannot convert pointer to non-integer type");
                            }
                            let tsize = Self::get_size_of_stype(target_type);
                            match tsize {
                                8 => {
                                    self.instruction(Instruction::Copy(value.clone(), dest.clone()))
                                }
                                4 | 1 => self.instruction(Instruction::Truncate(
                                    value.clone(),
                                    dest.clone(),
                                )),
                                _ => bail!("Cannot convert pointer to non-integer type"),
                            }
                            return Ok(dest.clone());
                        }
                        (_, SymbolType::Pointer(_)) => {
                            if !explicit_cast {
                                bail!("Cannot convert non-pointer to pointer type");
                            }
                            if !Self::is_integer(&vtype) {
                                bail!("Cannot convert non-integer to pointer type");
                            }
                            let vsize = Self::get_size_of_stype(&vtype);
                            match vsize {
                                8 => {
                                    self.instruction(Instruction::Copy(value.clone(), dest.clone()))
                                }
                                4 | 1 => self.instruction(Instruction::ZeroExtend(
                                    value.clone(),
                                    dest.clone(),
                                )),
                                _ => bail!("Cannot convert pointer to non-integer type"),
                            }

                            return Ok(dest.clone());
                        }
                        (SymbolType::Double, _) => {
                            if Self::is_signed(target_type) {
                                self.instruction(Instruction::DoubleToInt(
                                    value.clone(),
                                    dest.clone(),
                                ));
                            } else {
                                self.instruction(Instruction::DoubleToUInt(
                                    value.clone(),
                                    dest.clone(),
                                ));
                            }
                        }
                        (_, SymbolType::Double) => {
                            if Self::is_signed(&vtype) {
                                self.instruction(Instruction::IntToDouble(
                                    value.clone(),
                                    dest.clone(),
                                ));
                            } else {
                                self.instruction(Instruction::UIntToDouble(
                                    value.clone(),
                                    dest.clone(),
                                ));
                            }
                        }
                        _ => {
                            let tsize = Self::get_size_of_stype(target_type);
                            let vsize = Self::get_size_of_stype(&vtype);
                            if tsize == vsize {
                                // same size, just copy
                                self.instruction(Instruction::Copy(value.clone(), dest.clone()));
                            } else if tsize < vsize {
                                self.instruction(Instruction::Truncate(
                                    value.clone(),
                                    dest.clone(),
                                ));
                            } else if Self::is_signed(&vtype) {
                                self.instruction(Instruction::SignExtend(
                                    value.clone(),
                                    dest.clone(),
                                ));
                            } else {
                                self.instruction(Instruction::ZeroExtend(
                                    value.clone(),
                                    dest.clone(),
                                ));
                            }
                            //}
                        }
                    };
                    dest.clone()
                }
            }
        };
        Ok(return_value)
    }

    // this is used for assignments and ++/-- operators

    fn store_into_lvalue(&mut self, value: &Value, dest: &PendingResult) -> Result<Value> {
        println!("store_into_lvalue {:?} => {:?}", value, dest);
        match dest {
            PendingResult::Dereference(var) => {
                // dereference the pointer
                let ptype = Self::get_type(var);
                let stype = Self::get_pointee_type(&ptype)?;
                let converted = self.convert_by_assignment(value, &stype.clone())?;
                self.instruction(Instruction::Store(converted.clone(), var.clone()));
                Ok(converted.clone())
            }
            PendingResult::PlainValue(var) => {
                if let Value::Variable(name, stype) = var {
                    if !name.contains('$') {
                        let lookup = self.lookup_symbol(name);
                        if lookup.is_none() {
                            bail!("Variable {} not declared", name);
                        }
                    } else if !self.is_lvalue(var) {
                        bail!("Not lvalue VVVV{:?}", dest);
                    }
                    let converted = self.convert_by_assignment(value, &stype.clone())?;
                    self.instruction(Instruction::Copy(converted.clone(), var.clone()));
                    Ok(converted.clone())
                } else {
                    bail!("Cannot store into non-variable {:?}", dest);
                }
            }
        }
    }
    fn array_to_pointer(&mut self, value: &Value) -> Result<Value> {
        let (value, vtype) = if value.is_string() {
            let sname = self.make_static_string(value)?;
            let stype = SymbolType::Array(Box::new(SymbolType::Char), 0);
            (Value::Variable(sname, stype.clone()), stype)
        } else {
            (value.clone(), Self::get_type(value))
        };
        println!("array_to_pointer {:?} {:?}", value, vtype);
        if let SymbolType::Array(stype, _size) = vtype {
            let dest = self.make_temporary(&SymbolType::Pointer(Box::new(*stype)));
            self.instruction(Instruction::GetAddress(value.clone(), dest.clone()));
            Ok(dest)
        } else {
            Ok(value.clone())
        }
    }

    pub fn string_to_pointer(&mut self, value: &Value) -> Result<Value> {
        println!("string_to_pointer {:?}", value);
        let str = value.as_string().unwrap();
        let sname = self.make_static_string(value)?;
        let stype = SymbolType::Array(Box::new(SymbolType::Char), str.len() + 1);
        let dest = self.make_temporary(&SymbolType::Pointer(Box::new(SymbolType::Char)));
        self.instruction(Instruction::GetAddress(
            Value::Variable(sname, stype),
            dest.clone(),
        ));
        Ok(dest)
    }
    fn make_rvalue(&mut self, value: &PendingResult) -> Result<Value> {
        println!("make_rvalue {:?}", value);
        let ret = match &value {
            PendingResult::Dereference(var) => {
                assert!(var.is_pointer(), "Expected pointer, got {:?}", var);
                let ptype = Self::get_type(var);
                let stype = Self::get_pointee_type(&ptype)?;
                let deref_dest = match stype {
                    SymbolType::Array(t, _) => {
                        let name = var.as_variable().unwrap().0.clone();
                        let dest = Value::Variable(name, SymbolType::Pointer(t));
                        dest.clone()
                    }
                    _ => {
                        let dest = self.make_temporary(&stype);
                        self.instruction(Instruction::Load(var.clone(), dest.clone()));
                        dest
                    }
                };
                println!("dereferenced {:?}->{:?}", value.clone(), deref_dest);
                deref_dest
            }
            PendingResult::PlainValue(var) => var.clone(),
        };
        let ret = if !ret.is_string() || !self.static_init {
            self.array_to_pointer(&ret.clone())?
        } else {
            ret.clone()
        };
        Ok(ret)
    }

    fn do_unary(&mut self) -> Result<PendingResult> {
        /*
           <unary-exp> ::= <unop> <unary-exp>
                           | "(" { <type-specifier> }+ [ <abstract-declarator> ] ")" <unary-exp>
                           | <postfix-exp>
        */
        let token = self.peek()?;
        println!("do_unary {:?}", token);
        let ret_val = match token {
            //
            // Unary ops first
            //
            Token::PlusPlus | Token::MinusMinus => {
                self.next_token()?;
                let source = self.do_unary()?;

                let op = if token == Token::PlusPlus {
                    BinaryOperator::Add
                } else {
                    BinaryOperator::Subtract
                };

                let source_rv = self.make_rvalue(&source)?;
                let inc_val = match Self::get_type(&source_rv) {
                    SymbolType::Int32 => Value::Int32(1),
                    SymbolType::UInt32 => Value::UInt32(1),
                    SymbolType::Int64 => Value::Int64(1),
                    SymbolType::UInt64 => Value::UInt64(1),
                    SymbolType::Double => Value::Double(1.0),
                    SymbolType::Pointer(_) => Value::Int64(1),
                    SymbolType::Char => Value::Char(1),
                    SymbolType::UChar => Value::UChar(1),
                    SymbolType::SChar => Value::Char(1),
                    _ => bail!("Cannot use ++ or -- on {:?}", source),
                };
                let update = if source_rv.is_pointer() {
                    self.do_pointer_arithmetic(&op, &source_rv, &inc_val)?
                } else {
                    let update = self.make_temporary(&Self::get_type(&source_rv));
                    self.instruction(Instruction::Binary(
                        op,
                        source_rv.clone(),
                        inc_val,
                        update.clone(),
                    ));
                    update
                };
                self.store_into_lvalue(&update, &source)?;
                return Ok(PendingResult::PlainValue(update));
            }
            Token::Negate => {
                self.next_token()?;
                let source = self.do_unary()?;
                let source = self.make_rvalue(&source)?;
                if source.is_pointer() {
                    bail!("Cannot use ! on pointer type");
                }
                let source = if source.stype().is_character() {
                    self.convert_to(&source, &SymbolType::Int32, true)?
                } else {
                    source
                };
                let ret_dest = self.make_temporary(&Self::get_type(&source));
                let unop =
                    Instruction::Unary(UnaryOperator::Negate, source.clone(), ret_dest.clone());
                self.instruction(unop);
                PendingResult::PlainValue(ret_dest)
            }

            Token::Complement => {
                self.next_token()?;
                let source = self.do_unary()?;
                let source = self.make_rvalue(&source)?;
                if source.is_double() || source.is_pointer() {
                    bail!("Cannot use ~ on double or pointer type");
                }
                let source = if source.stype().is_character() {
                    self.convert_to(&source, &SymbolType::Int32, true)?
                } else {
                    source
                };
                let ret_dest = self.make_temporary(&Self::get_type(&source));
                let unop =
                    Instruction::Unary(UnaryOperator::Complement, source.clone(), ret_dest.clone());
                self.instruction(unop);
                PendingResult::PlainValue(ret_dest)
            }
            Token::Not => {
                self.next_token()?;
                let source = self.do_unary()?;
                let source = self.make_rvalue(&source)?;
                let ret_dest = self.make_temporary(&SymbolType::Int32);
                let unop = Instruction::Unary(UnaryOperator::LogicalNot, source, ret_dest.clone());
                self.instruction(unop);
                PendingResult::PlainValue(ret_dest)
            }
            // &  = address of
            Token::BitwiseAnd => {
                self.next_token()?;
                let source = self.do_unary()?;
                println!("address of {:?}", source);
                match &source {
                    PendingResult::Dereference(v) => PendingResult::PlainValue(v.clone()),
                    PendingResult::PlainValue(v) => {
                        if v.is_string() {
                            // string literal - return the pointer
                            let str = v.as_string().unwrap();
                            let name = format!("string${}", self.tacky.static_constants.len());
                            let arr_type =
                                SymbolType::Array(Box::new(SymbolType::Char), str.len() + 1);
                            self.tacky.add_static_constant(
                                &name,
                                vec![StaticInit::InitString(str.clone(), true)],
                                false,
                                false,
                                &arr_type,
                            );
                            //let stype = SymbolType::Pointer(Box::new(SymbolType::Char));
                            let dest = self
                                .make_temporary(&SymbolType::Pointer(Box::new(arr_type.clone())));
                            self.instruction(Instruction::GetAddress(
                                Value::Variable(name, arr_type),
                                dest.clone(),
                            ));
                            PendingResult::PlainValue(dest)
                        } else {
                            if !self.is_lvalue(v) {
                                bail!("not lvalue {:?}", v);
                            }
                            let ret_dest = self
                                .make_temporary(&SymbolType::Pointer(Box::new(Self::get_type(v))));
                            self.instruction(Instruction::GetAddress(v.clone(), ret_dest.clone()));
                            PendingResult::PlainValue(ret_dest)
                        }
                    }
                }
            }
            // * = dereference
            Token::Multiply => {
                self.next_token()?;
                let source = self.do_unary()?;
                let source = self.make_rvalue(&source)?;
                if !source.is_pointer() {
                    bail!("Expected pointer, got {:?}", source);
                }
                PendingResult::Dereference(source)
            }

            // not unary - is it a cast?
            Token::LeftParen
                if matches!(
                    self.peek_n(1)?,
                    Token::Int
                        | Token::Double
                        | Token::Signed
                        | Token::Unsigned
                        | Token::Long
                        | Token::Char
                ) =>
            {
                self.next_token()?;
                let specifiers = self.parse_specifiers(false)?;

                if specifiers.specified_type.is_none() {
                    bail!("Expected type specifier after (");
                }
                let token = self.peek()?;
                let base_type = specifiers.specified_type.unwrap();
                let target_type = if token == Token::RightParen {
                    base_type.clone()
                } else {
                    let decl = self.parse_abstract_declarator()?;
                    let abs = Self::process_abstract_declarator(&decl, &base_type)?;
                    println!("cast abstract decl {:?}", abs);

                    abs
                };
                self.expect(Token::RightParen)?;
                let source = self.do_unary()?;
                let source = self.make_rvalue(&source)?;

                let ret_dest = self.make_temporary(&target_type);
                let converted = self.convert_to(&source, &target_type, true)?;
                self.instruction(Instruction::Copy(converted, ret_dest.clone()));
                PendingResult::PlainValue(ret_dest)
            }
            _ => self.do_postfix()?,
        };
        Ok(ret_val)
    }
    fn process_index(&mut self, primary: &PendingResult) -> Result<PendingResult> {
        let token = self.peek()?;
        if token != Token::LeftBracket {
            return Ok(primary.clone());
        }
        self.next_token()?;

        // read the index value
        let index = self.do_expression(0)?;
        let index_rv = self.make_rvalue(&index)?;

        let ptr_rv = self.make_rvalue(primary)?;

        // we have a pointer and an index, lets get them the right way round
        let (ptr, index) = if ptr_rv.is_pointer() {
            (ptr_rv, index_rv)
        } else {
            let ptr = self.make_rvalue(&index)?;
            (ptr, ptr_rv)
        };
        println!("index {:?} {:?} {:?}", index, ptr, primary);
        if !Self::is_integer(&Self::get_type(&index)) {
            bail!("Expected integeryyy, got {:?}", index);
        }
        if !ptr.is_pointer() {
            bail!("Expected pointer, got {:?}", ptr);
        }

        let index = self.convert_to(&index, &SymbolType::Int64, true)?;
        self.expect(Token::RightBracket)?;

        let ptype = Self::get_type(&ptr);
        let stype = Self::get_pointee_type(&ptype)?;
        let dest = self.make_temporary(&ptype);
        let elem_size = Self::get_total_object_size(&stype)? as isize;

        self.instruction(Instruction::AddPtr(
            ptr.clone(),
            index,
            elem_size,
            dest.clone(),
        ));
        let idx_res = PendingResult::Dereference(dest.clone());
        let next_idx = self.process_index(&idx_res)?;
        Ok(next_idx)
    }

    fn do_postfix(&mut self) -> Result<PendingResult> {
        /*

        <postfix-exp> ::= <primary-exp> {postfixop}
        <postfixop> = "++" | "--" | "[" <exp> "]"
        */
        let mut primary = self.do_primary()?;
        loop {
            let token = self.peek()?;
            match token {
                Token::LeftBracket => primary = self.process_index(&primary)?,
                Token::PlusPlus | Token::MinusMinus => {
                    primary = self.do_post_dec_inc(&primary)?;
                }
                _ => {
                    break;
                }
            }
        }
        Ok(primary)
    }

    // handle postfix ++ and --
    fn do_post_dec_inc(&mut self, primary: &PendingResult) -> Result<PendingResult> {
        let token = self.next_token()?;

        let op = if token == Token::PlusPlus {
            BinaryOperator::Add
        } else {
            BinaryOperator::Subtract
        };

        let primary_rv = self.make_rvalue(primary)?;

        // the value we will return

        let ret_dest = self.make_temporary(&Self::get_type(&primary_rv));
        self.instruction(Instruction::Copy(primary_rv.clone(), ret_dest.clone()));

        let inc_val = match Self::get_type(&primary_rv) {
            SymbolType::Int32 => Value::Int32(1),
            SymbolType::UInt32 => Value::UInt32(1),
            SymbolType::Int64 => Value::Int64(1),
            SymbolType::UInt64 => Value::UInt64(1),
            SymbolType::Double => Value::Double(1.0),
            SymbolType::Pointer(_) => Value::UInt64(1),
            SymbolType::Char => Value::Char(1),
            SymbolType::UChar => Value::UChar(1),
            SymbolType::SChar => Value::Char(1),
            _ => bail!("Cannot use ++ or -- on {:?}", primary_rv),
        };
        //   self.check_binary_allowed(&op, &Self::get_type(&primary_rv), &Self::get_type(&inc_val))?;
        // the value we will store into the lvalue

        let update = if primary_rv.is_pointer() {
            self.do_pointer_arithmetic(&op, &primary_rv, &inc_val)?
        } else {
            let update = self.make_temporary(&Self::get_type(&primary_rv));
            self.instruction(Instruction::Binary(
                op,
                primary_rv.clone(),
                inc_val,
                update.clone(),
            ));
            update
        };
        self.store_into_lvalue(&update, primary)?;
        Ok(PendingResult::PlainValue(ret_dest))
    }
    fn do_primary(&mut self) -> Result<PendingResult> {
        /*
        <primary-exp> ::= <const>
                        | <identifier>
                        | "(" <exp> ")"
                        | <identifier> "(" [ <argument-list> ] ")"
        */
        let token = self.next_token()?;
        println!("do_primary {:?}", token);
        let primary = match token {
            Token::Constant(val) => {
                println!("val: {}", val);
                PendingResult::PlainValue(Value::Int32(val))
            }
            Token::LongConstant(val) => {
                println!("val: {}", val);
                PendingResult::PlainValue(Value::Int64(val))
            }
            Token::UConstant(val) => {
                println!("val: {}", val);
                PendingResult::PlainValue(Value::UInt32(val))
            }
            Token::ULongConstant(val) => {
                println!("val: {}", val);
                PendingResult::PlainValue(Value::UInt64(val))
            }
            Token::F64Constant(val) => {
                println!("val: {}", val);
                PendingResult::PlainValue(Value::Double(val))
            }
            Token::CharConstant(val) => {
                println!("val: {}", val);
                PendingResult::PlainValue(Value::Int32(val as i32))
            }
            Token::StringConstant(mut val) => {
                // merge consecutive string constants
                while let Token::StringConstant(append) = self.peek()? {
                    self.next_token()?;
                    val.push_str(&append);
                }

                let val = Value::String(val);
                println!("val: {:?}", val);

                PendingResult::PlainValue(val)
            }
            Token::LeftParen => {
                let ret_dest = self.do_expression(0)?;
                self.expect(Token::RightParen)?;
                ret_dest
            }
            Token::Identifier(name) => {
                // is this a function call?

                let token = self.peek()?;
                if token == Token::LeftParen {
                    // yes it is

                    let (return_type, symargs) =
                        if let Some((symbol, _)) = self.lookup_symbol(&name) {
                            if !symbol.stype.is_function() {
                                bail!("Expected function, got {:?}", name);
                            }
                            let st = symbol.stype.clone();
                            let ret = st.as_function().unwrap().1.clone();
                            let args = st.as_function().unwrap().0.clone();
                            (ret, args)
                        } else {
                            bail!("Function {} not declared", name);
                        };
                    self.next_token()?;
                    let mut args = Vec::new();
                    let mut argidx = 0;
                    loop {
                        let token = self.peek()?;
                        if token == Token::RightParen {
                            self.next_token()?;
                            break;
                        }

                        let arg = self.do_expression(0)?;
                        let arg = self.make_rvalue(&arg)?;

                        let converted_arg = self.convert_to(&arg, &symargs[argidx], false)?;

                        argidx += 1;
                        args.push(converted_arg);
                        let token = self.next_token()?;
                        match token {
                            Token::RightParen => {
                                break;
                            }
                            Token::Comma => {
                                if self.peek()? == Token::RightParen {
                                    bail!("redeundant comma in function call");
                                }
                            }
                            _ => {
                                bail!("Expected ) or , got {:?}", token);
                            }
                        }
                    }
                    if argidx != symargs.len() {
                        bail!(
                            "Function {} expected {} arguments, got {}",
                            name,
                            symargs.len(),
                            argidx
                        );
                    }

                    let ret_dest = self.make_temporary(&return_type.clone()); // TODO
                    self.instruction(Instruction::FunCall(name, args, ret_dest.clone()));
                    PendingResult::PlainValue(ret_dest)
                } else if let Some((symbol, _)) = self.lookup_symbol(&name) {
                    if symbol.stype.is_function() {
                        bail!("Expected variable, got function {:?}", name);
                    }
                    if symbol.scope_pull {
                        let real_symbol = self.get_global_symbol(&name).clone();

                        PendingResult::PlainValue(Value::Variable(
                            real_symbol.rename.clone(),
                            real_symbol.stype.clone(),
                        ))
                    } else {
                        PendingResult::PlainValue(Value::Variable(symbol.rename, symbol.stype))
                    }
                } else {
                    bail!("Variable {} not declared2", name);
                }
            }
            _ => {
                unreachable!("Expected constant, got {:?}", token);
            }
        };
        Ok(primary)
    }
    fn precedence(token: &Token) -> i32 {
        match token {
            Token::PlusPlus => 100,
            Token::MinusMinus => 100,

            Token::Multiply => 50,
            Token::Divide => 50,
            Token::Remainder => 50,

            Token::Negate => 45,
            Token::Add => 45,

            Token::ShiftLeft => 40,
            Token::ShiftRight => 40,

            Token::LessThan => 35,
            Token::LessThanOrEqual => 35,
            Token::GreaterThan => 35,
            Token::GreaterThanOrEqual => 35,

            Token::IsEqual => 30,
            Token::IsNotEqual => 30,

            Token::BitwiseAnd => 25,
            Token::BitwiseXor => 24,
            Token::BitwiseOr => 23,
            Token::LogicalAnd => 22,
            Token::LogicalOr => 21,

            Token::QuestionMark => 11,

            Token::Assign => 10,
            Token::AndEquals => 10,
            Token::MinusEquals => 10,
            Token::DivideEquals => 10,
            Token::MultiplyEquals => 10,
            Token::XorEquals => 10,
            Token::OrEquals => 10,
            Token::PlusEquals => 10,
            Token::ShiftLeftEquals => 10,
            Token::ShiftRightEquals => 10,
            Token::RemainderEquals => 10,

            _ => -1, // indicates this is not a binop
        }
    }
    fn convert_binop(&mut self) -> Result<BinaryOperator> {
        let token = self.next_token()?;
        match token {
            Token::Add => Ok(BinaryOperator::Add),
            Token::Negate => Ok(BinaryOperator::Subtract),
            Token::Multiply => Ok(BinaryOperator::Multiply),
            Token::Divide => Ok(BinaryOperator::Divide),
            Token::Remainder => Ok(BinaryOperator::Remainder),
            Token::BitwiseAnd => Ok(BinaryOperator::BitAnd),
            Token::BitwiseOr => Ok(BinaryOperator::BitOr),
            Token::BitwiseXor => Ok(BinaryOperator::BitXor),
            Token::ShiftLeft => Ok(BinaryOperator::ShiftLeft),
            Token::ShiftRight => Ok(BinaryOperator::ShiftRight),
            Token::IsEqual => Ok(BinaryOperator::Equal),
            Token::IsNotEqual => Ok(BinaryOperator::NotEqual),
            Token::LessThan => Ok(BinaryOperator::LessThan),
            Token::LessThanOrEqual => Ok(BinaryOperator::LessThanOrEqual),
            Token::GreaterThan => Ok(BinaryOperator::GreaterThan),
            Token::GreaterThanOrEqual => Ok(BinaryOperator::GreaterThanOrEqual),

            _ => bail!("Expected binop, got {:?}", token),
        }
    }
    fn convert_compound(token: &Token) -> Result<BinaryOperator> {
        match token {
            //  Token::Equals => Ok(BinaryOperator::Equal),
            Token::AndEquals => Ok(BinaryOperator::BitAnd),
            Token::MinusEquals => Ok(BinaryOperator::Subtract),
            Token::DivideEquals => Ok(BinaryOperator::Divide),
            Token::MultiplyEquals => Ok(BinaryOperator::Multiply),
            Token::XorEquals => Ok(BinaryOperator::BitXor),
            Token::OrEquals => Ok(BinaryOperator::BitOr),
            Token::PlusEquals => Ok(BinaryOperator::Add),
            Token::ShiftLeftEquals => Ok(BinaryOperator::ShiftLeft),
            Token::ShiftRightEquals => Ok(BinaryOperator::ShiftRight),
            Token::RemainderEquals => Ok(BinaryOperator::Remainder),
            _ => bail!("Expected compound, got {:?}", token),
        }
    }

    fn check_binary_allowed(
        &self,
        op: &BinaryOperator,
        left_type: &SymbolType,
        right_type: &SymbolType,
    ) -> Result<()> {
        match (op, left_type, right_type) {
            (BinaryOperator::Add, SymbolType::Pointer(_), SymbolType::Pointer(_)) => {
                bail!("Cannot add two pointers")
            }
            (
                BinaryOperator::Multiply | BinaryOperator::Divide | BinaryOperator::Remainder,
                SymbolType::Pointer(_),
                _,
            ) => {
                bail!("Cannot do math on pointers")
            }
            (
                BinaryOperator::Multiply | BinaryOperator::Divide | BinaryOperator::Remainder,
                _,
                SymbolType::Pointer(_),
            ) => {
                bail!("Cannot do math on pointers")
            }
            (
                BinaryOperator::GreaterThan
                | BinaryOperator::LessThan
                | BinaryOperator::GreaterThanOrEqual
                | BinaryOperator::LessThanOrEqual,
                SymbolType::Pointer(_),
                SymbolType::Double
                | SymbolType::Int32
                | SymbolType::Int64
                | SymbolType::UInt32
                | SymbolType::UInt64,
            ) => {
                bail!("Cannot compare pointer to number")
            }
            (
                BinaryOperator::Add | BinaryOperator::Subtract,
                SymbolType::Double,
                SymbolType::Pointer(_),
            ) => {
                bail!("Cannot add number to pointer")
            }
            (
                BinaryOperator::Add | BinaryOperator::Subtract,
                SymbolType::Pointer(_),
                SymbolType::Double,
            ) => {
                bail!("Cannot add number to pointer")
            }
            (BinaryOperator::ShiftLeft | BinaryOperator::ShiftRight, SymbolType::Pointer(_), _) => {
                bail!("Cannot shift pointer")
            }
            (BinaryOperator::ShiftLeft | BinaryOperator::ShiftRight, _, SymbolType::Pointer(_)) => {
                bail!("Cannot shift BY pointer")
            }
            (BinaryOperator::ShiftLeft | BinaryOperator::ShiftRight, SymbolType::Double, _) => {
                bail!("Cannot shift double")
            }
            (BinaryOperator::ShiftLeft | BinaryOperator::ShiftRight, _, SymbolType::Double) => {
                bail!("Cannot shift by double")
            }

            (BinaryOperator::BitOr, SymbolType::Pointer(_), _) => {
                bail!("Cannot bit or two pointers")
            }
            (BinaryOperator::BitAnd, SymbolType::Pointer(_), _) => {
                bail!("Cannot bit and two pointers")
            }
            (BinaryOperator::BitXor, SymbolType::Pointer(_), _) => {
                bail!("Cannot bit xor two pointers")
            }
            (BinaryOperator::BitOr, _, SymbolType::Pointer(_)) => {
                bail!("Cannot bit or two pointers")
            }
            (BinaryOperator::BitAnd, _, SymbolType::Pointer(_)) => {
                bail!("Cannot bit and two pointers")
            }
            (BinaryOperator::BitXor, _, SymbolType::Pointer(_)) => {
                bail!("Cannot bit xor two pointers")
            }
            (BinaryOperator::Remainder, _, SymbolType::Double) => bail!("Cannot mod double"),
            (BinaryOperator::Remainder, SymbolType::Double, _) => bail!("Cannot mod double"),
            (BinaryOperator::BitAnd, _, SymbolType::Double) => bail!("Cannot bitand double"),
            (BinaryOperator::BitAnd, SymbolType::Double, _) => bail!("Cannot bitand double"),
            (BinaryOperator::BitOr, _, SymbolType::Double) => bail!("Cannot bitor double"),
            (BinaryOperator::BitOr, SymbolType::Double, _) => bail!("Cannot bitor double"),
            (BinaryOperator::BitXor, _, SymbolType::Double) => bail!("Cannot bitxor double"),
            (BinaryOperator::BitXor, SymbolType::Double, _) => bail!("Cannot bitxor double"),
            (
                BinaryOperator::Subtract,
                SymbolType::Int32 | SymbolType::Int64 | SymbolType::UInt32 | SymbolType::UInt64,
                SymbolType::Pointer(_),
            ) => {
                bail!("Cannot subtract pointer from number")
            }
            _ => {} // all other combinations are allowed
        }
        Ok(())
    }
}
