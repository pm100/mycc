use crate::{
    lexer::Token,
    parser::{Parser, SymbolDetails},
    tacky::{BinaryOperator, Instruction, UnaryOperator, Value},
};
use anyhow::{bail, Result};
impl Parser {
    pub(crate) fn do_expression(&mut self, min_prec: i32) -> Result<Value> {

        let mut left = self.do_factor()?;
        let mut token = self.peek()?;

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
                    if let Value::Variable(ref var) = left {
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
                        if token == Token::Equals {
                            let inst = Instruction::Copy(right.clone(), left.clone());
                            self.instruction(inst);
                            right
                        } else {
                            let op = Self::convert_compound(&token)?;
                            self.instruction(Instruction::Binary(
                                op,
                                left.clone(),
                                right.clone(),
                                left.clone(),
                            ));
                            // HACK
                            // we need to make sure the the result is *not* an lvalue
                            let dest = Value::Variable(self.make_temporary());
                            self.instruction(Instruction::Copy(left.clone(), dest.clone()));
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
                    let ret_dest = Value::Variable(dest_name.clone());
                    self.instruction(Instruction::Copy(left.clone(), ret_dest.clone()));
                    self.instruction(Instruction::Binary(
                        BinaryOperator::Add,
                        left.clone(),
                        Value::Int(1),
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
                    let ret_dest = Value::Variable(dest_name.clone());
                    self.instruction(Instruction::Copy(left.clone(), ret_dest.clone()));
                    self.instruction(Instruction::Binary(
                        BinaryOperator::Subtract,
                        left.clone(),
                        Value::Int(1),
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

                    let dest = Value::Variable(self.make_temporary());
                    self.instruction(Instruction::Copy(Value::Int(1), dest.clone()));
                    self.instruction(Instruction::Jump(label_end.clone()));
                    self.instruction(Instruction::Label(label_false.clone()));
                    self.instruction(Instruction::Copy(Value::Int(0), dest.clone()));
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

                    let dest = Value::Variable(self.make_temporary());
                    self.instruction(Instruction::Copy(Value::Int(0), dest.clone()));
                    self.instruction(Instruction::Jump(label_end.clone()));
                    self.instruction(Instruction::Label(label_true.clone()));
                    self.instruction(Instruction::Copy(Value::Int(1), dest.clone()));
                    self.instruction(Instruction::Label(label_end.clone()));
                    dest
                }
                Token::QuestionMark => {
                    self.next_token()?;
                    let true_label = self.make_label("ternary_true");
                    let false_label = self.make_label("ternary_false");
                    let result = Value::Variable(self.make_temporary());
                    let condition = left;
                    self.instruction(Instruction::JumpIfZero(condition, false_label.clone()));

                    //true
                    let true_exp = self.do_expression(0)?;
                    self.instruction(Instruction::Copy(true_exp, result.clone()));
                    self.instruction(Instruction::Jump(true_label.clone()));

                    // false
                    self.instruction(Instruction::Label(false_label.clone()));
                    self.expect(Token::Colon)?;
                    let false_exp = self.do_expression(Self::precedence(&token))?;
                    self.instruction(Instruction::Copy(false_exp, result.clone()));
                    self.instruction(Instruction::Label(true_label.clone()));
                    result
                }
                _ => {
                    let op = self.convert_binop()?;
                    let right = self.do_expression(Self::precedence(&token) + 1)?;
                    let dest = Value::Variable(self.make_temporary());
                    let inst = Instruction::Binary(op, left, right, dest.clone());
                    self.instruction(inst);
                    dest
                }
            };
            left = dest;
            token = self.peek()?;
        }
        self.nest.pop();
        self.nest.pop();
        Ok(left)
    }

    fn do_factor(&mut self) -> Result<Value> {
        let token = self.next_token()?;
        println!("{}factor {:?}", self.nest, token);
        match token {
            Token::Constant(val) => {
                println!("val: {}", val);
                Ok(Value::Int(val))
            } // prefix ++ and --
            Token::PlusPlus => {
                let source = self.do_factor()?;
                if !self.is_lvalue(&source) {
                    bail!("not lvalue");
                }
                let dest_name = self.make_temporary();
                let ret_dest = Value::Variable(dest_name.clone());
                self.instruction(Instruction::Binary(
                    BinaryOperator::Add,
                    Value::Int(1),
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
                let ret_dest = Value::Variable(dest_name.clone());
                self.instruction(Instruction::Binary(
                    BinaryOperator::Subtract,
                    source.clone(),
                    Value::Int(1),
                    ret_dest.clone(),
                ));
                self.instruction(Instruction::Copy(ret_dest.clone(), source));
                Ok(ret_dest)
            }
            Token::Negate => {
                let source = self.do_factor()?;
                let dest_name = self.make_temporary();
                let ret_dest = Value::Variable(dest_name.clone());
                let unop =
                    Instruction::Unary(UnaryOperator::Negate, source, Value::Variable(dest_name));
                self.instruction(unop);
                Ok(ret_dest)
            }

            Token::Complement => {
                let source = self.do_factor()?;
                let dest_name = self.make_temporary();
                let ret_dest = Value::Variable(dest_name.clone());
                let unop = Instruction::Unary(
                    UnaryOperator::Complement,
                    source,
                    Value::Variable(dest_name),
                );
                self.instruction(unop);
                Ok(ret_dest)
            }
            Token::LeftParen => {
                let ret = self.do_expression(0)?;
                self.expect(Token::RightParen)?;
                Ok(ret)
            }
            Token::Not => {
                let source = self.do_factor()?;
                let dest_name = self.make_temporary();
                let ret_dest = Value::Variable(dest_name.clone());
                let unop = Instruction::Unary(UnaryOperator::LogicalNot, source, ret_dest.clone());
                self.instruction(unop);
                Ok(ret_dest)
            }

            Token::Identifier(name) => {
                // is this a function call?

                let token = self.peek()?;
                if token == Token::LeftParen {
                    // yes it is


                    let symargs = if let Some((symbol, _)) = self.lookup_symbol(&name) {
                        if !matches!(symbol.details, SymbolDetails::Function { .. }) {
                            bail!("Expected function, got {:?}", name);
                        }
                        let (_, args) = symbol.details.into_function().unwrap();
                        args
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
                        argidx += 1;
                        args.push(arg);
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
                    let ret_dest = Value::Variable(dest_name.clone());
                    self.instruction(Instruction::FunCall(name, args, ret_dest.clone()));
                    Ok(ret_dest)
                } else if let Some((symbol, _)) = self.lookup_symbol(&name) {
                    match symbol.details {
                        SymbolDetails::Function { .. } => {
                            bail!("Expected variable, got function {:?}", name);
                        }
                        SymbolDetails::Variable {
                            rename,
                            stype: _,
                            value: _,
                        } => Ok(Value::Variable(rename)),
                        SymbolDetails::ScopePull => {
                            let real_symbol = self.get_global_symbol(&name);
                            Ok(Value::Variable(
                                real_symbol.details.as_variable().unwrap().0.clone(),
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
