use anyhow::{bail, Result};
use std::{
    collections::{HashMap, VecDeque},
    mem::discriminant,
    path::Path,
};

// https://users.rust-lang.org/t/how-to-parameterize-a-function-or-macro-by-enum-variant/126398
macro_rules! expect {
    ($lexer:expr,$token:path) => {
        match $lexer.next_token() {
            Ok($token(data)) => data,
            Err(e) => return Err(e),
            _ => bail!("Expected {:?}, got {:?}", stringify!(token), "x"),
        }
    };
}

use crate::{
    lexer::{Lexer, Token},
    tacky::{BinaryOperator, Instruction, TackyProgram, UnaryOperator, Value},
};

pub struct Parser {
    lexer: Lexer,
    tacky: TackyProgram,
    next_temporary: usize,
    eof_hit: bool,
    peeked_tokens: VecDeque<Token>,
    variables: HashMap<String, String>,
    labels: HashMap<String, String>,
    next_name: usize,
    nest: String,
}

impl Parser {
    pub fn new(path: &Path) -> Self {
        Self {
            lexer: Lexer::new(path),
            tacky: TackyProgram::new(),
            next_temporary: 0,
            eof_hit: false,
            peeked_tokens: VecDeque::new(),
            variables: HashMap::new(),
            labels: HashMap::new(),
            next_name: 0,
            nest: String::new(),
        }
    }

    pub fn parse(&mut self) -> Result<&TackyProgram> {
        self.do_program()?;
        Ok(&self.tacky)
    }

    fn do_program(&mut self) -> Result<()> {
        loop {
            if self.peek()? == Token::Eof {
                break;
            }
            self.do_function()?;
        }
        Ok(())
    }
    fn do_function(&mut self) -> Result<()> {
        self.expect(Token::Int)?;

        let function_name = expect!(self, Token::Identifier);
        println!("Function: {}", function_name);
        self.tacky.add_function(&function_name);
        self.expect(Token::LeftParen)?;
        self.expect(Token::Void)?;
        self.expect(Token::RightParen)?;
        self.do_function_body()?;
        Ok(())
    }

    fn do_function_body(&mut self) -> Result<()> {
        self.expect(Token::LeftBrace)?;
        while self.peek()? != Token::RightBrace {
            self.do_block_item()?;
        }
        self.next_token()?;
        self.instruction(Instruction::Return(Value::Int(0)));
        Ok(())
    }
    fn do_block_item(&mut self) -> Result<()> {
        let token = self.peek()?;
        self.nest.clear();
        match token {
            Token::Int => self.do_declaration()?,
            _ => self.do_statement()?,
        }
        Ok(())
    }
    fn do_declaration(&mut self) -> Result<()> {
        self.next_token()?;

        let name = expect!(self, Token::Identifier);
        let token = self.peek()?;
        let init = if token == Token::Equals {
            self.next_token()?;
            true
        } else {
            false
        };
        self.do_variable_dec(&name, init)?;
        self.expect(Token::SemiColon)?;

        Ok(())
    }

    fn do_variable_dec(&mut self, name: &str, init: bool) -> Result<()> {
        if let Some(var) = self.variables.get(name) {
            panic!("Variable {} already declared", var);
        }
        let new_name = self.make_newname(name);
        println!("new_name: {}=>{}", name, new_name);
        self.variables
            .insert(name.to_string(), new_name.to_string());
        if init {
            let val = self.do_expression(0)?;
            self.instruction(Instruction::Copy(val, Value::Variable(new_name)));
        }
        Ok(())
    }
    fn do_statement(&mut self) -> Result<()> {
        let token = self.peek()?;
        match token {
            Token::Return => {
                self.do_return()?;
                self.expect(Token::SemiColon)?;
            }
            Token::If => {
                self.do_if()?;
            }
            Token::GoTo => {
                self.do_goto()?;
            }
            Token::SemiColon => {
                self.next_token()?;
            }
            _ => {
                if matches!(token, Token::Identifier(_)) && self.peek_n(1)? == Token::Colon {
                    self.do_label()?;
                    self.do_statement()?;
                } else {
                    self.do_expression(0)?;
                    self.expect(Token::SemiColon)?;
                }
            }
        };

        Ok(())
    }
    fn do_goto(&mut self) -> Result<()> {
        self.next_token()?;
        let label = format!("__{}__", expect!(self, Token::Identifier));
        if let Some(_) = self.labels.get(&label) {
        } else {
            self.labels.insert(label.clone(), label.clone());
        }
        self.instruction(Instruction::Jump(label));
        self.expect(Token::SemiColon)?;
        Ok(())
    }
    fn do_return(&mut self) -> Result<()> {
        self.next_token()?;
        let val = self.do_expression(0)?;
        self.instruction(Instruction::Return(val));
        Ok(())
    }
    fn do_label(&mut self) -> Result<()> {
        let label = format!("__{}__", expect!(self, Token::Identifier));
        self.expect(Token::Colon)?;
        self.instruction(Instruction::Label(label));
        Ok(())
    }
    fn do_if(&mut self) -> Result<()> {
        self.next_token()?;
        self.expect(Token::LeftParen)?;
        let cond = self.do_expression(0)?;
        self.expect(Token::RightParen)?;
        let label_false = self.make_label();
        let label_end = self.make_label();
        self.instruction(Instruction::JumpIfZero(cond, label_false.clone()));
        self.do_statement()?;
        self.instruction(Instruction::Jump(label_end.clone()));
        self.instruction(Instruction::Label(label_false.clone()));
        if self.peek()? == Token::Else {
            self.next_token()?;
            self.do_statement()?;
        }
        self.instruction(Instruction::Label(label_end.clone()));
        Ok(())
    }
    fn do_expression(&mut self, min_prec: i32) -> Result<Value> {
        println!("{}do_expression {}", self.nest, min_prec);
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
                        let lookup = self.variables.values().any(|v| v == var);
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
                if self.variables.contains_key(&name) {
                    let var = self.variables.get(&name).unwrap().clone();

                    Ok(Value::Variable(var.clone()))
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
    fn is_lvalue(&self, value: &Value) -> bool {
        match value {
            Value::Variable(var) => self.variables.values().any(|v| v == var),
            _ => false,
        }
    }
    fn expect(&mut self, token: Token) -> Result<Token> {
        let nt = self.next_token()?;
        if discriminant(&nt) != discriminant(&token) {
            bail!("Expected {:?}, got {:?}", token, nt);
        } else {
            Ok(nt)
        }
    }

    fn make_temporary(&mut self) -> String {
        let temp = format!("temp.{}", self.next_temporary);
        self.next_temporary += 1;
        temp
    }

    fn make_newname(&mut self, name: &str) -> String {
        let temp = format!("{}{}", name, self.next_name);
        self.next_name += 1;
        temp
    }
    fn make_label(&mut self) -> String {
        let temp = format!("label_{}", self.next_temporary);
        self.next_temporary += 1;
        temp
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
    fn instruction(&mut self, instruction: Instruction) {
        println!("{}instruction {:?}", self.nest, instruction);
        self.tacky.add_instruction(instruction);
    }
    fn next_token(&mut self) -> Result<Token> {
        let token = if self.peeked_tokens.len() > 0 {
            self.peeked_tokens.pop_front().unwrap()
        } else {
            self.lexer.next_token()?
        };
        if token == Token::Eof {
            self.eof_hit = true;
        }
        Ok(token)
    }

    fn peek(&mut self) -> Result<Token> {
        self.peek_n(0)
    }

    fn peek_n(&mut self, n: usize) -> Result<Token> {
        if n >= self.peeked_tokens.len() {
            for _ in 0..n + 1 {
                let token = self.lexer.next_token()?;
                self.peeked_tokens.push_back(token.clone());
            }
        }
        return Ok(self.peeked_tokens[n].clone());
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
