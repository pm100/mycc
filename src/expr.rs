use crate::{
    lexer::Token,
    parser::{Parser, SymbolDetails},
    tacky::{BinaryOperator, Instruction, SymbolType, UnaryOperator, Value},
};
use anyhow::{bail, Result};

impl Parser {
    pub(crate) fn do_expression(&mut self, min_prec: i32) -> Result<Value> {
        let mut left = self.do_factor()?;
        let mut token = self.peek()?;
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
            if Self::precedence(&token) < min_prec {
                break;
            }
            let dest = match token {
                Token::Equals
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
                        if var.starts_with("$temp$") {
                            bail!("not lvalue");
                        }
                        if !var.contains('$') {
                            let lookup = self.lookup_symbol(var);
                            if lookup.is_none() {
                                bail!("Variable {} not declared", var);
                            }
                        }
                        println!("var: {} {:?}", var, token);
                        let right = self.do_expression(Self::precedence(&token))?;
                        let right_type = Self::get_type(&right);
                        if token == Token::Equals {
                            let converted = self.convert_to(&right, &left_type.clone());
                            let inst = Instruction::Copy(converted.clone(), left.clone());
                            self.instruction(inst);
                            right
                        } else {
                            let op = Self::convert_compound(&token)?;
                            let common_type =
                                Self::get_common_type(&left_type.clone(), &right_type);
                            let left_conv = self.convert_to(&left, &common_type);
                            let right_conv = self.convert_to(&right, &common_type);
                            self.instruction(Instruction::Binary(
                                op,
                                left_conv.clone(),
                                right_conv.clone(),
                                left_conv.clone(),
                            ));
                            // HACK
                            // we need to make sure the the result is *not* an lvalue
                            let dest = Value::Variable(self.make_temporary(), left_type.clone());
                            let left_final = self.convert_to(&left_conv, &left_type);
                            self.instruction(Instruction::Copy(left_final.clone(), left.clone()));
                            self.instruction(Instruction::Copy(left_final.clone(), dest.clone()));
                            dest
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
                    let right = self.do_expression(Self::precedence(&token) + 1)?;
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
                    let right = self.do_expression(Self::precedence(&token) + 1)?;
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
                    let true_exp = self.do_expression(0)?;
                    let result = Value::Variable(self.make_temporary(), Self::get_type(&true_exp));
                    self.instruction(Instruction::Copy(true_exp.clone(), result.clone()));
                    self.instruction(Instruction::Jump(maybe_true_label.clone()));

                    // false
                    self.instruction(Instruction::Label(false_label.clone()));
                    self.expect(Token::Colon)?;
                    let false_exp = self.do_expression(Self::precedence(&token))?;
                    //  let result = Value::Variable(self.make_temporary(), Self::get_type(&true_exp));
                    if Self::get_type(&false_exp) != Self::get_type(&true_exp) {
                        let common_type = Self::get_common_type(
                            &Self::get_type(&true_exp),
                            &Self::get_type(&false_exp),
                        );
                        let false_converted = self.convert_to(&false_exp, &common_type);

                        self.instruction(Instruction::Copy(false_converted, result.clone()));
                        self.instruction(Instruction::Jump(exit_label.clone()));
                        self.instruction(Instruction::Label(maybe_true_label.clone()));
                        let true_converted = self.convert_to(&true_exp, &common_type);
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

                    let right = self.do_expression(Self::precedence(&token) + 1)?;
                    if op == BinaryOperator::ShiftLeft || op == BinaryOperator::ShiftRight {
                        let dest = Value::Variable(self.make_temporary(), left_type.clone());
                        self.instruction(Instruction::Binary(op, left, right, dest.clone()));
                        dest
                    } else {
                        let right_type = Self::get_type(&right);
                        let common_type = Self::get_common_type(&left_type, &right_type);
                        let left = self.convert_to(&left, &common_type);
                        let right = self.convert_to(&right, &common_type);
                        let dest = Value::Variable(self.make_temporary(), common_type.clone());
                        let inst = Instruction::Binary(op, left, right, dest.clone());
                        self.instruction(inst);
                        dest
                    }
                }
            };
            left = dest;
            token = self.peek()?;
        }
        self.nest.pop();
        self.nest.pop();
        Ok(left)
    }
    pub fn get_type(value: &Value) -> SymbolType {
        match value {
            Value::Int32(_) => SymbolType::Int32,
            Value::Int64(_) => SymbolType::Int64,
            Value::Variable(_, t) => t.clone(),
        }
    }
    fn get_common_type(a: &SymbolType, b: &SymbolType) -> SymbolType {
        if a == b {
            return a.clone();
        }
        match (a, b) {
            (SymbolType::Int32, SymbolType::Int64) => SymbolType::Int64,
            (SymbolType::Int64, SymbolType::Int32) => SymbolType::Int64,
            _ => SymbolType::Int64,
        }
    }

    pub fn convert_to(&mut self, value: &Value, target_type: &SymbolType) -> Value {
        //        let dest = Value::Variable(to_string(), target_type.clone());
        if Self::get_type(value) == *target_type {
            value.clone()
        } else {
            match value {
                Value::Int32(v) => {
                    if *target_type == SymbolType::Int64 {
                        Value::Int64(*v as i64)
                    } else {
                        value.clone()
                    }
                }
                Value::Int64(v) => {
                    if *target_type == SymbolType::Int32 {
                        Value::Int32(*v as i32)
                    } else {
                        value.clone()
                    }
                }
                _ => {
                    let dest_name = self.make_temporary();
                    let dest = Value::Variable(dest_name.clone(), target_type.clone());
                    match target_type {
                        SymbolType::Int32 => {
                            self.instruction(Instruction::Truncate(value.clone(), dest.clone()));
                        }
                        SymbolType::Int64 => {
                            self.instruction(Instruction::SignExtend(value.clone(), dest.clone()));
                        }

                        _ => {}
                    }
                    dest.clone()
                }
            }
        }
    }
    fn do_factor(&mut self) -> Result<Value> {
        let token = self.next_token()?;
        println!("{}factor {:?}", self.nest, token);
        match token {
            Token::Constant(val) => {
                println!("val: {}", val);
                Ok(Value::Int32(val))
            }
            Token::LongConstant(val) => {
                println!("val: {}", val);
                Ok(Value::Int64(val))
            }
            // prefix ++ and --
            Token::PlusPlus => {
                let source = self.do_factor()?;
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
                Ok(ret_dest)
            }
            Token::MinusMinus => {
                let source = self.do_factor()?;
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
                Ok(ret_dest)
            }
            Token::Negate => {
                let source = self.do_factor()?;
                let dest_name = self.make_temporary();
                let ret_dest = Value::Variable(dest_name.clone(), Self::get_type(&source));
                let unop = Instruction::Unary(
                    UnaryOperator::Negate,
                    source.clone(),
                    Value::Variable(dest_name, Self::get_type(&source)),
                );
                self.instruction(unop);
                Ok(ret_dest)
            }

            Token::Complement => {
                let source = self.do_factor()?;
                let dest_name = self.make_temporary();
                let ret_dest = Value::Variable(dest_name.clone(), Self::get_type(&source));
                let unop = Instruction::Unary(
                    UnaryOperator::Complement,
                    source.clone(),
                    Value::Variable(dest_name, Self::get_type(&source)),
                );
                self.instruction(unop);
                Ok(ret_dest)
            }
            Token::LeftParen => {
                // is this a cast?
                let specifiers = self.load_specifiers()?;
                if specifiers.is_external || specifiers.is_static {
                    bail!("Cannot cast to external or static type");
                }
                if specifiers.specified_type.is_some() {
                    self.expect(Token::RightParen)?;
                    let target_type = specifiers.specified_type.unwrap();
                    let right = self.do_factor()?;
                    let dest_name = self.make_temporary();
                    let ret_dest = Value::Variable(dest_name.clone(), target_type.clone());
                    let converted = self.convert_to(&right, &target_type);
                    self.instruction(Instruction::Copy(converted, ret_dest.clone()));
                    Ok(ret_dest)
                } else {
                    let ret = self.do_expression(0)?;
                    self.expect(Token::RightParen)?;
                    Ok(ret)
                }
            }
            Token::Not => {
                let source = self.do_factor()?;
                let dest_name = self.make_temporary();
                let ret_dest = Value::Variable(dest_name.clone(), Self::get_type(&source));
                let unop = Instruction::Unary(UnaryOperator::LogicalNot, source, ret_dest.clone());
                self.instruction(unop);
                Ok(ret_dest)
            }

            Token::Identifier(name) => {
                // is this a function call?

                let token = self.peek()?;
                if token == Token::LeftParen {
                    // yes it is

                    let (return_type, symargs) =
                        if let Some((symbol, _)) = self.lookup_symbol(&name) {
                            if !matches!(symbol.details, SymbolDetails::Function { .. }) {
                                bail!("Expected function, got {:?}", name);
                            }
                            let (ret, args) = symbol.details.into_function().unwrap();
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
                        let converted_arg = self.convert_to(&arg, &symargs[argidx]);
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
                    let ret_dest = Value::Variable(dest_name.clone(), return_type); // TODO
                    self.instruction(Instruction::FunCall(name, args, ret_dest.clone()));
                    Ok(ret_dest)
                } else if let Some((symbol, _)) = self.lookup_symbol(&name) {
                    match symbol.details {
                        SymbolDetails::Function { .. } => {
                            bail!("Expected variable, got function {:?}", name);
                        }

                        SymbolDetails::Variable {
                            rename,
                            stype,
                            value: _,
                        } => Ok(Value::Variable(rename, stype)),
                        SymbolDetails::ScopePull => {
                            let real_symbol = self.get_global_symbol(&name).clone();

                            Ok(Value::Variable(
                                real_symbol.details.as_variable().unwrap().0.clone(),
                                real_symbol.details.as_variable().unwrap().1.clone(),
                            ))
                        }
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

            Token::Equals => 10,
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
}
