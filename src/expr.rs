use crate::{
    lexer::Token,
    parser::Parser,
    symbols::SymbolType,
    tacky::{BinaryOperator, Instruction, PendingResult, UnaryOperator, Value}, //  x64::moira_inst::BinaryOperator,
};
use anyhow::{bail, Result};
//use backtrace::Symbol;

impl Parser {
    pub(crate) fn do_rvalue_expression(&mut self) -> Result<Value> {
        let expr = self.do_expression(0)?;
        println!("do_rvalue_expression {:?}", expr);
        self.make_rvalue(&expr)
    }
    fn do_expression(&mut self, min_prec: i32) -> Result<PendingResult> {
        let mut token = self.peek()?;

        let mut left = self.do_unary()?;

        // println!(
        //     "expleft: {:?} {:?} {}",
        //     //self.nest,
        //     left,
        //     token,
        //     Self::precedence(&token)
        // );

        loop {
            // println!("loop {:?} {:?} ", token, left);
            token = self.peek()?;
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

                    // let left_type = Self::get_type(&left);

                    let right = self.do_expression(Self::precedence(&token))?;
                    let right_rv = self.make_rvalue(&right)?;
                    let right_type = Self::get_type(&right_rv);
                    // println!(
                    //     "left: {:?} {:?} right: {:?}  {:?}",
                    //     left, left_type, right, right_type
                    // );

                    match token {
                        // simple assignment
                        Token::Assign => {
                            let result = self.store_into_lvalue(&right_rv, &left)?;

                            // ensure we do not return an lvalue

                            let result_type = Self::get_type(&result);
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
                        println!(
                            "false_exp: {:?} te:{:?} ct {:?}",
                            false_exp, true_exp, common_type
                        );
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
                        && (left_rv.is_pointer() || right.is_pointer())
                    {
                        let (pointer, delta) = if left_rv.is_pointer() {
                            (left_rv, right)
                        } else {
                            (right, left_rv)
                        };

                        let delta = self.convert_to(&delta, &SymbolType::Int64, true)?;
                        let delta = if op == BinaryOperator::Subtract {
                            if let Value::Int64(v) = delta {
                                Value::Int64(v * -1)
                            } else {
                                let new_delta = self.make_temporary(&SymbolType::Int64);
                                self.instruction(Instruction::Unary(
                                    UnaryOperator::Negate,
                                    delta.clone(),
                                    new_delta.clone(),
                                ));
                                new_delta
                            }
                        } else {
                            delta
                        };
                        let ptype = &Self::get_type(&pointer);
                        let dest = self.make_temporary(&ptype);
                        let pointed_type = Self::get_pointee_type(&ptype)?;
                        let inst = Instruction::AddPtr(
                            pointer,
                            delta,
                            Self::get_size_of_stype(&pointed_type) as isize,
                            dest.clone(),
                        );
                        self.instruction(inst);
                        dest
                    } else {
                        if op == BinaryOperator::ShiftLeft || op == BinaryOperator::ShiftRight {
                            let dest = self.make_temporary(&left_type.clone());
                            self.instruction(Instruction::Binary(op, left_rv, right, dest.clone()));
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
                }
            };
            left = PendingResult::PlainValue(dest);
        }
        Ok(left)
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

        let left_rv = self.make_rvalue(&left)?;
        let right_rv = self.make_rvalue(&right)?;
        let left_type = Self::get_type(&left_rv);
        let right_type = Self::get_type(&right_rv);
        println!(
            "===>>op: {:?} left: {:?} {:?}",
            op,
            left_rv.is_double(),
            left
        );
        self.check_binary_allowed(&op, &left_type, &right_type)?;

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

        let result = self.store_into_lvalue(&binval, &left)?;

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
            return Ok(self.convert_to(value, target_type, false)?);
        }
        if target_type.is_pointer() && Self::is_null_pointer_constant(value) {
            return Ok(self.convert_to(value, target_type, false)?);
        }
        // if target_type.is_pointer() && value.is_pointer() {
        //     return Ok(self.convert_to(value, target_type, true)?);
        // }
        bail!("Cannot convert {:?} to {:?}", value, target_type);
    }

    pub fn get_type(value: &Value) -> SymbolType {
        match value {
            Value::Int32(_) => SymbolType::Int32,
            Value::Int64(_) => SymbolType::Int64,
            Value::UInt32(_) => SymbolType::UInt32,
            Value::UInt64(_) => SymbolType::UInt64,
            Value::Double(_) => SymbolType::Double,
            Value::Variable(_, t) => t.clone(),
            //  Value::Dereference(v) => Self::get_type(v),
        }
    }
    pub fn is_signed(symbol: &SymbolType) -> bool {
        match symbol {
            SymbolType::Int32 | SymbolType::Int64 => true,
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
            SymbolType::Array(t, size) => {
                let elem_type = Self::get_inner_array_type(&symbol).unwrap();
                let elem_size = Self::get_size_of_stype(&elem_type);
                return elem_size * size;
            }
        }
    }
    fn get_common_pointer_type(a: &Value, b: &Value) -> Result<SymbolType> {
        let atype = Self::get_type(&a);
        let btype = Self::get_type(&b);
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
            match (value, target_type) {
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
                // null pointer conversion
                (
                    Value::UInt64(0) | Value::Int64(0) | Value::Int32(0) | Value::UInt32(0),
                    SymbolType::Pointer(_),
                ) => {
                    let dest = self.make_temporary(&target_type);
                    self.instruction(Instruction::Copy(Value::Int64(0), dest.clone()));
                    return Ok(dest.clone());
                }

                // variables
                _ => {
                    let dest = self.make_temporary(&target_type);
                    match (&vtype, target_type) {
                        (SymbolType::Pointer(_), SymbolType::Pointer(_)) => {
                            if !explicit_cast {
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
                                4 => self.instruction(Instruction::Truncate(
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
                                4 => self.instruction(Instruction::ZeroExtend(
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
        // let dest = &self.array_to_pointer(dest)?;
        match dest {
            PendingResult::Dereference(var) => {
                // dereference the pointer
                // let deref = Self::deref_pointer(dest)?;
                let ptype = Self::get_type(&var);
                let stype = Self::get_pointee_type(&ptype)?;
                let converted = self.convert_by_assignment(value, &stype.clone())?;
                self.instruction(Instruction::Store(converted.clone(), var.clone()));
                Ok(converted.clone())
            }
            PendingResult::PlainValue(var) => {
                if let Value::Variable(name, stype) = var {
                    if !name.contains('$') {
                        let lookup = self.lookup_symbol(&name);
                        if lookup.is_none() {
                            bail!("Variable {} not declared", name);
                        }
                    } else {
                        if !self.is_lvalue(&var) {
                            bail!("Not lvalue VVVV{:?}", dest);
                        }
                    }
                    //                    let stype = Self::get_type(var);
                    let converted = self.convert_by_assignment(value, &stype.clone())?;
                    self.instruction(Instruction::Copy(converted.clone(), var.clone()));
                    Ok(converted.clone())
                } else {
                    bail!("Cannot store into non-variable {:?}", dest);
                }
            }
        }
    }
    fn addr_of(&mut self, value: &PendingResult) -> Result<Value> {
        println!("addr_of {:?}", value);
        match value {
            PendingResult::Dereference(var) => {
                return Ok(var.clone());
            }
            PendingResult::PlainValue(var) => {
                let stype = Self::get_type(&var);
                let stype = if stype.is_array() {
                    Self::get_array_type(&stype)?
                } else {
                    stype.clone()
                };
                let dest = self.make_temporary(&SymbolType::Pointer(Box::new(stype)));
                self.instruction(Instruction::GetAddress(var.clone(), dest.clone()));
                return Ok(dest);
            }
        };
    }
    fn array_to_pointer(&mut self, value: &Value) -> Result<Value> {
        let vtype = Self::get_type(&value);
        //
        println!("array_to_pointer {:?} {:?}", value, vtype);
        if let SymbolType::Array(stype, _size) = vtype {
            // let stype = if stype.is_array() {
            //     Self::get_array_type(&stype)?
            // } else {
            //     *stype.clone()
            // };
            let dest = self.make_temporary(&SymbolType::Pointer(Box::new(*stype)));
            self.instruction(Instruction::GetAddress(value.clone(), dest.clone()));
            return Ok(dest);
        } else {
            return Ok(value.clone());
        }
    }
    fn make_rvalue(&mut self, value: &PendingResult) -> Result<Value> {
        println!("make_rvalue {:?}", value);
        // let value = self.array_to_pointer(&value.clone())?;
        let ret = match &value {
            PendingResult::Dereference(var) => {
                // let deref = Self::deref_pointer(&var)?;
                let ptype = Self::get_type(&var);
                let stype = Self::get_pointee_type(&ptype)?;
                let deref_dest = self.make_temporary(&stype);
                self.instruction(Instruction::Load(var.clone(), deref_dest.clone()));
                println!("dereferenced {:?}->{:?}", value.clone(), deref_dest);
                deref_dest
            }
            PendingResult::PlainValue(var) => var.clone(),
        };
        let ret = self.array_to_pointer(&ret.clone())?;
        Ok(ret.clone())
    }

    fn do_unary(&mut self) -> Result<PendingResult> {
        /*
           <unary-exp> ::= <unop> <unary-exp>
                           | "(" { <type-specifier> }+ [ <abstract-declarator> ] ")" <unary-exp>
                           | <postfix-exp>
        */
        let token = self.peek()?;
        // println!("do_unary {:?}", token);
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
                    _ => bail!("Cannot use ++ or -- on {:?}", source),
                };

                let update = self.make_temporary(&Self::get_type(&source_rv));
                self.instruction(Instruction::Binary(
                    op,
                    source_rv.clone(),
                    inc_val,
                    update.clone(),
                ));
                self.store_into_lvalue(&update, &source)?;
                return Ok(PendingResult::PlainValue(update));
            }
            Token::Negate => {
                self.next_token()?;
                let source = self.do_unary()?;
                let source = self.make_rvalue(&source)?;
                if source.is_pointer() {
                    bail!("Cannot use ~ on pointer type");
                }
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
                        if !self.is_lvalue(&v) {
                            bail!("not lvalue {:?}", v);
                        }
                        let ret_dest =
                            self.make_temporary(&SymbolType::Pointer(Box::new(Self::get_type(&v))));
                        self.instruction(Instruction::GetAddress(v.clone(), ret_dest.clone()));
                        PendingResult::PlainValue(ret_dest)
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
                // let (name, stype) = source.as_variable().unwrap();
                // let deref = Value::Variable(format!("*{}", name), stype.clone());

                println!("dereference {:?}", source);
                PendingResult::Dereference(source)
            }

            // not unary - is it a cast?
            Token::LeftParen
                if matches!(
                    self.peek_n(1)?,
                    Token::Int | Token::Double | Token::Signed | Token::Unsigned | Token::Long
                ) =>
            {
                self.next_token()?;
                let specifiers = self.parse_specifiers(false)?;

                if specifiers.specified_type.is_none() {
                    bail!("Expected type specifier after (");
                }
                let target_type = specifiers.specified_type.unwrap();
                let (abs, _) = self.parse_abstract_declarator(&target_type)?;
                let source = self.do_unary()?;
                let source = self.make_rvalue(&source)?;

                let ret_dest = self.make_temporary(&abs);
                let converted = self.convert_to(&source, &abs, true)?;
                self.instruction(Instruction::Copy(converted, ret_dest.clone()));
                PendingResult::PlainValue(ret_dest)
            }
            _ => self.do_postfix()?,
        };
        Ok(ret_val)
    }

    fn process_indexing(&mut self, primary: &PendingResult) -> Result<PendingResult> {
        // no braces - out
        let token = self.peek()?;

        if token != Token::LeftBracket {
            println!("++++no index:");
            return Ok(primary.clone());
        }
        let mut primary_rv = self.make_rvalue(&primary)?;
        let mut do_deref = true;
        let ret = loop {
            println!("++++input indexing primary {:?}", primary_rv);

            let token = self.peek()?;
            // no braces - out
            if token != Token::LeftBracket {
                println!("++++end of indexing primary returning:{:?}", primary_rv);
                break primary_rv.clone();
            }
            do_deref = true;

            // indexing needs a pointer
            // make_rvalue does that
            // let primary_rv = self.make_rvalue(&primary)?;
            // let ptr = if primary.is_pointer() {
            //     primary.clone()
            // } else {
            //     self.array_to_pointer(&primary_rv)?
            // };
            if !primary.is_pointer() {
                bail!("Expected pointer, got {:?}", primary);
            }
            let ptr = primary_rv.clone();
            // let ptr = if primary.is_array() {
            //     self.addr_of(primary)?
            // } else {
            //    self.make_rvalue(primary)?
            //};
            println!("++++indexing ptr =  {:?}", ptr);
            // read [idx]

            self.next_token()?;
            let index = self.do_expression(0)?;
            let index = self.make_rvalue(&index)?;
            if !Self::is_integer(&Self::get_type(&index)) {
                bail!("Expected integer, got {:?}", index);
            }
            let index = self.convert_to(&index, &SymbolType::Int64, true)?;
            self.expect(Token::RightBracket)?;

            // now create (ptr + idx)
            // note that idx is not scalled up
            // addptr takes the number of elements and the element size

            let ptype = Self::get_type(&ptr);
            let stype = Self::get_pointee_type(&ptype)?;
            let dest = self.make_temporary(&ptype);
            self.instruction(Instruction::AddPtr(
                ptr.clone(),
                index,
                Self::get_size_of_stype(&stype) as isize,
                dest.clone(),
            ));
            print!("++++indexing dest =  {:?}", dest);
            primary_rv = if dest.is_pointer() {
                let ptype = Self::get_pointee_type(&Self::get_type(&dest))?;
                if ptype.is_array() {
                    do_deref = false;
                    Value::Variable(
                        dest.as_variable().unwrap().0.clone(),
                        SymbolType::Pointer(Box::new(Self::get_array_type(&ptype)?)),
                    )
                } else {
                    dest
                }
            } else {
                dest
            };
            println!("=>++++indexing primary_rv =  {:?}", primary_rv);
            //primary_rv = dest;
            // let deref = Self::make_deref_pointer(&dest);
            //    let ret = self.process_indexing(&PendingResult::Dereference(dest.clone()))?;
        };
        println!("++++indexing primary returning:{:?}", ret);
        if ret.is_pointer() && do_deref {
            if Self::get_pointee_type(&Self::get_type(&ret))?.is_array() {
                return Ok(PendingResult::PlainValue(ret.clone()));
            } else {
                return Ok(PendingResult::Dereference(ret));
            }
        } else {
            return Ok(PendingResult::PlainValue(ret));
        }
    }
    fn do_postfix(&mut self) -> Result<PendingResult> {
        /*
        <postfix-exp> ::= <primary-exp> { "[" <exp> "] } ["++"|"--"]
        */
        println!("++++++++++++++do_postfix");
        let primary = self.do_primary()?;
        // let mut must_deref = None;
        // let primary_rv = self.make_rvalue(&primary)?;

        let primary = self.process_indexing(&primary)?;
        // let primary_rv = self.make_rvalue(&primary)?;
        println!("++++++++primary after indexes {:?}", primary);
        // postfix ++ and --
        let token = self.peek()?;
        if token == Token::PlusPlus || token == Token::MinusMinus {
            self.next_token()?;

            let op = if token == Token::PlusPlus {
                BinaryOperator::Add
            } else {
                BinaryOperator::Subtract
            };

            let primary_rv = self.make_rvalue(&primary)?;

            // the value we will return

            let ret_dest = self.make_temporary(&Self::get_type(&primary_rv));
            self.instruction(Instruction::Copy(primary_rv.clone(), ret_dest.clone()));

            // the value we will store into the lvalue

            let update = self.make_temporary(&Self::get_type(&primary_rv));

            let inc_val = match Self::get_type(&primary_rv) {
                SymbolType::Int32 => Value::Int32(1),
                SymbolType::UInt32 => Value::UInt32(1),
                SymbolType::Int64 => Value::Int64(1),
                SymbolType::UInt64 => Value::UInt64(1),
                SymbolType::Double => Value::Double(1.0),
                _ => bail!("Cannot use ++ or -- on {:?}", primary_rv),
            };
            self.instruction(Instruction::Binary(
                op,
                primary_rv.clone(),
                inc_val,
                update.clone(),
            ));
            self.store_into_lvalue(&update, &primary)?;
            return Ok(PendingResult::PlainValue(ret_dest));
        } else {
            return Ok(primary);
        }
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
                            //   let (args, ret) = st.as_function().unwrap().clone();
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
            (BinaryOperator::Subtract, SymbolType::Pointer(_), SymbolType::Pointer(_)) => {
                bail!("Cannot subtract two pointers")
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

            _ => {} // all other combinations are allowed
        }
        Ok(())
    }
}
