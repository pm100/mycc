use crate::{
    lexer::{Lexer, Token},
    parser::Parser,
    tacky::{BinaryOperator, Instruction, TackyProgram, UnaryOperator, Value},
};
use anyhow::{bail, Result};
impl Parser {
    pub(crate) fn do_expression(&mut self, min_prec: i32) -> Result<Value> {
        //       println!("{}do_expression {}", self.nest, min_prec);
        let mut left = self.do_factor()?;
        let mut token = self.peek()?;

        // println!(
        //     "{}expleft: {:?} {:?} {}",
        //     self.nest,
        //     left,
        //     token,
        //     Self::precedence(&token)
        // );
        self.nest.push(' ');
        self.nest.push(' ');
        // let mut token = self.peek()?;
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
                        let lookup = self.variables().values().any(|v| v.name == *var);
                        if !lookup {
                            if var.starts_with("temp.") {
                                // puke
                                bail!("not lvalue");
                            }

                            bail!("Variable {} not declared", var);
                        }
                        println!("var: {} {:?}", var, token);
                        let right = self.do_expression(Self::precedence(&token))?;
                        if token == Token::Equals {
                            let inst = Instruction::Copy(right.clone(), left.clone());
                            self.instruction(inst);
                            right
                        } else {
                            let op = Self::convert_compound(&token)?;
                            let dest = Value::Variable(self.make_temporary());
                            self.instruction(Instruction::Binary(
                                op,
                                left.clone(),
                                right.clone(),
                                left.clone(),
                            ));
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
                    //self.instruction(Instruction::Copy(ret_dest.clone(), left));
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
                    //self.instruction(Instruction::Copy(ret_dest.clone(), left));
                    ret_dest
                }
                Token::LogicalAnd => {
                    self.next_token()?;
                    let label_false = self.make_label();
                    let label_end = self.make_label();
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
                    let label_true = self.make_label();
                    let label_end = self.make_label();
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
                    let true_label = self.make_label();
                    let false_label = self.make_label();
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
        //  println!("{}factor {:?}", self.nest, token);
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
                if self.variables().contains_key(&name) {
                    let var = self.variables().get(&name).unwrap().clone();

                    Ok(Value::Variable(var.name.clone()))
                } else {
                    bail!("Variable {} not declared", name);
                }
            }
            _ => {
                //println!("huh? {:?}", token);
                panic!("huh? {:?}", token);
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
}
