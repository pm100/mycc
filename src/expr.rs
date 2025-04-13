use crate::{
    lexer::Token,
    parser::Parser,
    symbols::SymbolType,
    tacky::{BinaryOperator, Instruction, UnaryOperator, Value}, //  x64::moira_inst::BinaryOperator,
};
use anyhow::{bail, Result};
use backtrace::Symbol;
use enum_as_inner::EnumAsInner;
#[derive(Debug, PartialEq, Clone, EnumAsInner)]
pub enum ExpResult {
    PlainValue(Value),
    DereferencedPointer(Value, SymbolType),
}
impl Parser {
    pub(crate) fn do_expression(&mut self, min_prec: i32) -> Result<ExpResult> {
        let mut token = self.peek()?;

        let save_exp_result = self.do_factor()?;
        let mut left = self.and_convert(save_exp_result.clone())?;
        let left_type = Self::get_type(&left);
        println!(
            "{}expleft: {:?} {:?} {}",
            self.nest,
            left,
            token,
            Self::precedence(&token)
        );
        self.nest.push(' ');
        self.nest.push(' ');

        loop {
            println!("loop {:?} {:?} {:?}", token, save_exp_result, left);
            token = self.peek()?;
            if Self::precedence(&token) < min_prec {
                break;
            }
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
                    if let Value::Variable(ref var, _) = left {
                        // if var.starts_with("$temp$") {
                        //     bail!("not lvalue");
                        // }

                        if !var.contains('$') {
                            let lookup = self.lookup_symbol(var);
                            if lookup.is_none() {
                                bail!("Variable {} not declared", var);
                            }
                        }

                        let right = self.do_expression_and_convert(Self::precedence(&token))?;
                        let right_type = Self::get_type(&right);

                        match token {
                            Token::Assign => {
                                //let converted = self.convert_to(&right, &left_type.clone());
                                let converted =
                                    self.convert_by_assignment(&right, &left_type.clone())?;
                                match save_exp_result {
                                    ExpResult::DereferencedPointer(ref ptr, ref stype) => {
                                        self.instruction(Instruction::Store(
                                            right.clone(),
                                            ptr.clone(),
                                        ));
                                    }
                                    ExpResult::PlainValue(_) => {
                                        if !self.is_lvalue(&left) {
                                            bail!("not lvalue {:?}", left);
                                        }
                                        self.instruction(Instruction::Copy(
                                            converted.clone(),
                                            left.clone(),
                                        ));
                                    }
                                }

                                // ensure we do not return an lvalue
                                let not_lvalue =
                                    Value::Variable(self.make_temporary(), left_type.clone());
                                self.instruction(Instruction::Copy(
                                    converted.clone(),
                                    not_lvalue.clone(),
                                ));
                                not_lvalue
                            }

                            _ => {
                                if matches!(save_exp_result, ExpResult::PlainValue(_)) {
                                    if !self.is_lvalue(&left) {
                                        bail!("not lvalue {:?}", left);
                                    }
                                }
                                let op = Self::convert_compound(&token)?;
                                println!(
                                    "===>>op: {:?} left: {:?} {:?}",
                                    op,
                                    left.is_double(),
                                    left
                                );
                                self.check_binary_allowed(&op, &left_type, &right_type)?;

                                let (left_conv, right_conv) = if token == Token::ShiftLeftEquals
                                    || token == Token::ShiftRightEquals
                                {
                                    (left.clone(), right.clone())
                                } else {
                                    let common_type =
                                        Self::get_common_type(&left_type.clone(), &right_type)?;
                                    let left_conv = self.convert_to(&left, &common_type, false)?;
                                    let right_conv =
                                        self.convert_to(&right, &common_type, false)?;
                                    (left_conv, right_conv)
                                };

                                self.instruction(Instruction::Binary(
                                    op,
                                    left_conv.clone(),
                                    right_conv.clone(),
                                    left_conv.clone(),
                                ));
                                // HACK
                                // we need to make sure the the result is *not* an lvalue
                                let dest =
                                    Value::Variable(self.make_temporary(), left_type.clone());
                                let left_final = self.convert_to(&left_conv, &left_type, false)?;
                                self.instruction(Instruction::Copy(
                                    left_final.clone(),
                                    left.clone(),
                                ));
                                self.instruction(Instruction::Copy(
                                    left_final.clone(),
                                    dest.clone(),
                                ));
                                dest
                            }
                        }
                    } else {
                        bail!("Expected variable, got {:?}", left);
                    }
                }
                Token::PlusPlus => {
                    self.next_token()?;
                    if !self.is_lvalue(&left) {
                        bail!("not lvaluea");
                    }
                    let dest_name = self.make_temporary();
                    let ret_dest = Value::Variable(dest_name.clone(), left_type.clone());
                    self.instruction(Instruction::Copy(left.clone(), ret_dest.clone()));
                    self.instruction(Instruction::Binary(
                        BinaryOperator::Add,
                        left.clone(),
                        Value::Int32(1),
                        left.clone(),
                    ));

                    ret_dest
                }
                Token::MinusMinus => {
                    self.next_token()?;
                    if !self.is_lvalue(&left) {
                        bail!("not lvalueb {:?}", left);
                    }
                    let dest_name = self.make_temporary();
                    let ret_dest = Value::Variable(dest_name.clone(), left_type.clone());
                    self.instruction(Instruction::Copy(left.clone(), ret_dest.clone()));
                    self.instruction(Instruction::Binary(
                        BinaryOperator::Subtract,
                        left.clone(),
                        Value::Int32(1),
                        left.clone(),
                    ));

                    ret_dest
                }
                Token::LogicalAnd => {
                    self.next_token()?;
                    let label_false = self.make_label("and_false");
                    let label_end = self.make_label("and_end");
                    self.instruction(Instruction::JumpIfZero(left, label_false.clone()));
                    let right = self.do_expression_and_convert(Self::precedence(&token) + 1)?;
                    self.instruction(Instruction::JumpIfZero(right, label_false.clone()));

                    let dest = Value::Variable(self.make_temporary(), SymbolType::Int32);
                    self.instruction(Instruction::Copy(Value::Int32(1), dest.clone()));
                    self.instruction(Instruction::Jump(label_end.clone()));
                    self.instruction(Instruction::Label(label_false.clone()));
                    self.instruction(Instruction::Copy(Value::Int32(0), dest.clone()));
                    self.instruction(Instruction::Label(label_end.clone()));
                    dest
                }
                Token::LogicalOr => {
                    self.next_token()?;
                    let label_true = self.make_label("or_true");
                    let label_end = self.make_label("or_end");
                    self.instruction(Instruction::JumpIfNotZero(left, label_true.clone()));
                    let right = self.do_expression_and_convert(Self::precedence(&token) + 1)?;
                    self.instruction(Instruction::JumpIfNotZero(right, label_true.clone()));

                    let dest = Value::Variable(self.make_temporary(), SymbolType::Int32);
                    self.instruction(Instruction::Copy(Value::Int32(0), dest.clone()));
                    self.instruction(Instruction::Jump(label_end.clone()));
                    self.instruction(Instruction::Label(label_true.clone()));
                    self.instruction(Instruction::Copy(Value::Int32(1), dest.clone()));
                    self.instruction(Instruction::Label(label_end.clone()));
                    dest
                }
                Token::QuestionMark => {
                    self.next_token()?;
                    let exit_label = self.make_label("ternary_exit");
                    let maybe_true_label = self.make_label("ternary_maybe_true");
                    let false_label = self.make_label("ternary_false");
                    let condition = left;
                    self.instruction(Instruction::JumpIfZero(condition, false_label.clone()));

                    //true
                    let true_exp = self.do_expression_and_convert(0)?;
                    let mut result =
                        Value::Variable(self.make_temporary(), Self::get_type(&true_exp));
                    self.instruction(Instruction::Copy(true_exp.clone(), result.clone()));
                    self.instruction(Instruction::Jump(maybe_true_label.clone()));

                    // false
                    self.instruction(Instruction::Label(false_label.clone()));
                    self.expect(Token::Colon)?;
                    let false_exp = self.do_expression_and_convert(Self::precedence(&token))?;
                    //  let result = Value::Variable(self.make_temporary(), Self::get_type(&true_exp));
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
                        result = Value::Variable(self.make_temporary(), common_type.clone());
                        self.instruction(Instruction::Copy(false_converted, result.clone()));
                        self.instruction(Instruction::Jump(exit_label.clone()));
                        self.instruction(Instruction::Label(maybe_true_label.clone()));
                        let true_converted = self.convert_to(&true_exp, &common_type, false)?;

                        self.instruction(Instruction::Copy(true_converted, result.clone()));
                        // self.instruction(Instruction::Jump(exit_label.clone()));
                    } else {
                        self.instruction(Instruction::Copy(false_exp, result.clone()));
                        self.instruction(Instruction::Label(maybe_true_label.clone()));
                    }
                    self.instruction(Instruction::Label(exit_label.clone()));
                    result
                }
                _ => {
                    let op = self.convert_binop()?;

                    let right = self.do_expression_and_convert(Self::precedence(&token) + 1)?;
                    let right_type = Self::get_type(&right);
                    self.check_binary_allowed(&op, &left_type, &right_type)?;
                    if op == BinaryOperator::ShiftLeft || op == BinaryOperator::ShiftRight {
                        let dest = Value::Variable(self.make_temporary(), left_type.clone());
                        self.instruction(Instruction::Binary(op, left, right, dest.clone()));
                        dest
                    } else {
                        let common_type =
                            if matches!(op, BinaryOperator::Equal | BinaryOperator::NotEqual)
                                && (left.is_pointer() || right.is_pointer())
                            {
                                Self::get_common_pointer_type(&left, &right)
                            } else {
                                Self::get_common_type(&left_type, &right_type)
                            }?;
                        let left = self.convert_to(&left, &common_type, false)?;
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
                        let dest = Value::Variable(self.make_temporary(), dest_type);
                        let inst = Instruction::Binary(op, left, right, dest.clone());
                        self.instruction(inst);
                        dest
                    }
                }
            };
            left = dest;
        }
        self.nest.pop();
        self.nest.pop();
        Ok(ExpResult::PlainValue(left))
    }
    fn is_arithmetic(stype: &SymbolType) -> bool {
        match stype {
            SymbolType::Int32
            | SymbolType::Int64
            | SymbolType::UInt32
            | SymbolType::UInt64
            | SymbolType::Double => true,
            _ => false,
        }
    }

    fn is_integer(stype: &SymbolType) -> bool {
        match stype {
            SymbolType::Int32 | SymbolType::Int64 | SymbolType::UInt32 | SymbolType::UInt64 => true,
            _ => false,
        }
    }

    pub fn is_null_pointer_constant(value: &Value) -> bool {
        match value {
            Value::Int32(0) => true,
            Value::Int64(0) => true,
            Value::UInt32(0) => true,
            Value::UInt64(0) => true,
            _ => false,
        }
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
        bail!("Cannot convert {:?} to {:?}", value, target_type);
    }

    fn do_expression_and_convert(&mut self, precedence: i32) -> Result<Value> {
        let res = self.do_expression(precedence)?;
        self.and_convert(res)
    }

    fn and_convert(&mut self, exp_result: ExpResult) -> Result<Value> {
        match exp_result {
            ExpResult::PlainValue(v) => Ok(v.clone()),
            ExpResult::DereferencedPointer(ptr, stype) => {
                let dest = Value::Variable(self.make_temporary(), stype);
                self.instruction(Instruction::Load(ptr.clone(), dest.clone()));
                Ok(dest)
            }
        }
    }
    fn do_factor_and_convert(&mut self) -> Result<Value> {
        let res = self.do_factor()?;
        self.and_convert(res)
    }
    pub fn get_type(value: &Value) -> SymbolType {
        match value {
            Value::Int32(_) => SymbolType::Int32,
            Value::Int64(_) => SymbolType::Int64,
            Value::UInt32(_) => SymbolType::UInt32,
            Value::UInt64(_) => SymbolType::UInt64,
            Value::Double(_) => SymbolType::Double,
            Value::Variable(_, t) => t.clone(),
        }
    }
    pub fn is_signed(symbol: &SymbolType) -> bool {
        match symbol {
            SymbolType::Int32 | SymbolType::Int64 => true,
            SymbolType::UInt32 | SymbolType::UInt64 => false,
            _ => false,
        }
    }
    pub fn get_size_of_stype(symbol: &SymbolType) -> i32 {
        match symbol {
            SymbolType::Int32 | SymbolType::UInt32 => 4,
            SymbolType::Int64 | SymbolType::UInt64 => 8,
            SymbolType::Double => 8,
            SymbolType::Function(_, _) => 8,
            SymbolType::Pointer(_) => 8,
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
                    let dest_name = self.make_temporary();
                    let dest = Value::Variable(dest_name.clone(), target_type.clone());
                    self.instruction(Instruction::Copy(Value::Int64(0), dest.clone()));
                    return Ok(dest.clone());
                }

                // variables
                _ => {
                    let dest_name = self.make_temporary();
                    let dest = Value::Variable(dest_name.clone(), target_type.clone());
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
    fn do_factor(&mut self) -> Result<ExpResult> {
        let token = self.next_token()?;
        println!("{}factor {:?}", self.nest, token);
        match token {
            Token::Constant(val) => {
                println!("val: {}", val);
                Ok(ExpResult::PlainValue(Value::Int32(val)))
            }
            Token::LongConstant(val) => {
                println!("val: {}", val);
                Ok(ExpResult::PlainValue(Value::Int64(val)))
            }
            Token::UConstant(val) => {
                println!("val: {}", val);
                Ok(ExpResult::PlainValue(Value::UInt32(val)))
            }
            Token::ULongConstant(val) => {
                println!("val: {}", val);
                Ok(ExpResult::PlainValue(Value::UInt64(val)))
            }
            Token::F64Constant(val) => {
                println!("val: {}", val);
                Ok(ExpResult::PlainValue(Value::Double(val)))
            }
            // prefix ++ and --
            Token::PlusPlus => {
                let source = self.do_factor_and_convert()?;
                if !self.is_lvalue(&source) {
                    bail!("not lvalue");
                }
                let dest_name = self.make_temporary();
                let ret_dest = Value::Variable(dest_name.clone(), Self::get_type(&source));
                self.instruction(Instruction::Binary(
                    BinaryOperator::Add,
                    Value::Int32(1),
                    source.clone(),
                    ret_dest.clone(),
                ));
                self.instruction(Instruction::Copy(ret_dest.clone(), source));
                Ok(ExpResult::PlainValue(ret_dest))
            }
            Token::MinusMinus => {
                let source = self.do_factor_and_convert()?;
                if !self.is_lvalue(&source) {
                    bail!("not lvalue");
                }
                let dest_name = self.make_temporary();
                let ret_dest = Value::Variable(dest_name.clone(), Self::get_type(&source));
                self.instruction(Instruction::Binary(
                    BinaryOperator::Subtract,
                    source.clone(),
                    Value::Int32(1),
                    ret_dest.clone(),
                ));
                self.instruction(Instruction::Copy(ret_dest.clone(), source));
                Ok(ExpResult::PlainValue(ret_dest))
            }
            Token::Negate => {
                let source = self.do_factor_and_convert()?;
                if source.is_pointer() {
                    bail!("Cannot use ~ on pointer type");
                }
                let dest_name = self.make_temporary();
                let ret_dest = Value::Variable(dest_name.clone(), Self::get_type(&source));
                let unop = Instruction::Unary(
                    UnaryOperator::Negate,
                    source.clone(),
                    Value::Variable(dest_name, Self::get_type(&source)),
                );
                self.instruction(unop);
                Ok(ExpResult::PlainValue(ret_dest))
            }

            Token::Complement => {
                let source = self.do_factor_and_convert()?;
                if source.is_double() || source.is_pointer() {
                    bail!("Cannot use ~ on double or pointer type");
                }
                let dest_name = self.make_temporary();
                let ret_dest = Value::Variable(dest_name.clone(), Self::get_type(&source));
                let unop = Instruction::Unary(
                    UnaryOperator::Complement,
                    source.clone(),
                    Value::Variable(dest_name, Self::get_type(&source)),
                );
                self.instruction(unop);
                Ok(ExpResult::PlainValue(ret_dest))
            }
            Token::LeftParen => {
                // is this a cast?
                let specifiers = self.parse_specifiers(false)?;

                if specifiers.specified_type.is_some() {
                    //cast

                    let target_type = specifiers.specified_type.unwrap();
                    let (abs, _) = self.parse_abstract_declarator(&target_type)?;
                    let right = self.do_factor_and_convert()?;
                    let dest_name = self.make_temporary();
                    let ret_dest = Value::Variable(dest_name.clone(), abs.clone());
                    let converted = self.convert_to(&right, &abs, true)?;
                    self.instruction(Instruction::Copy(converted, ret_dest.clone()));
                    Ok(ExpResult::PlainValue(ret_dest))
                } else {
                    // expression in parens
                    let ret_dest = self.do_expression_and_convert(0)?;
                    self.expect(Token::RightParen)?;
                    Ok(ExpResult::PlainValue(ret_dest))
                }
            }
            Token::Not => {
                let source = self.do_factor_and_convert()?;
                let dest_name = self.make_temporary();
                let ret_dest = Value::Variable(dest_name.clone(), SymbolType::Int32);
                let unop = Instruction::Unary(UnaryOperator::LogicalNot, source, ret_dest.clone());
                self.instruction(unop);
                Ok(ExpResult::PlainValue(ret_dest))
            }
            // &  = address of
            Token::BitwiseAnd => {
                let source = self.do_factor()?;

                match source {
                    ExpResult::PlainValue(var) => {
                        if !self.is_lvalue(&var) {
                            bail!("not lvalue {:?}", var);
                        }
                        let dest_name = self.make_temporary();
                        let ret_dest = Value::Variable(
                            dest_name.clone(),
                            SymbolType::Pointer(Box::new(Self::get_type(&var))),
                        );

                        self.instruction(Instruction::GetAddress(var, ret_dest.clone()));
                        Ok(ExpResult::PlainValue(ret_dest))
                    }
                    ExpResult::DereferencedPointer(var, stype) => Ok(ExpResult::PlainValue(var)),
                }
            }
            // * = dereference
            Token::Multiply => {
                let source = self.do_factor_and_convert()?;

                let pointed_type = Self::get_pointee_type(&Self::get_type(&source))?;
                let xx = Ok(ExpResult::DereferencedPointer(source.clone(), pointed_type));
                println!("dereference {:?}", xx);
                xx
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

                        let arg = self.do_expression_and_convert(0)?;
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

                    let dest_name = self.make_temporary();
                    let ret_dest = Value::Variable(dest_name.clone(), *return_type.clone()); // TODO
                    self.instruction(Instruction::FunCall(name, args, ret_dest.clone()));
                    Ok(ExpResult::PlainValue(ret_dest))
                } else if let Some((symbol, _)) = self.lookup_symbol(&name) {
                    if symbol.stype.is_function() {
                        bail!("Expected variable, got function {:?}", name);
                    }
                    if symbol.scope_pull {
                        let real_symbol = self.get_global_symbol(&name).clone();

                        Ok(ExpResult::PlainValue(Value::Variable(
                            real_symbol.rename.clone(),
                            real_symbol.stype.clone(),
                        )))
                    } else {
                        Ok(ExpResult::PlainValue(Value::Variable(
                            symbol.rename,
                            symbol.stype,
                        )))
                        // SymbolDetails::ScopePull => {
                        //     let real_symbol = self.get_global_symbol(&name).clone();

                        //     Ok(Value::Variable(
                        //         real_symbol.details.as_variable().unwrap().0.clone(),
                        //         real_symbol.details.as_variable().unwrap().1.clone(),
                        //     ))
                        // }
                    }
                } else {
                    bail!("Variable {} not declared2", name);
                }
            }
            _ => {
                unreachable!("Expected constant, got {:?}", token);
            }
        }
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
