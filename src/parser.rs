use anyhow::{bail, Result};
use std::{mem::discriminant, path::Path};

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
    peeked_token: Option<Token>,
}

impl Parser {
    pub fn new(path: &Path) -> Self {
        Self {
            lexer: Lexer::new(path),
            tacky: TackyProgram::new(),
            next_temporary: 0,
            eof_hit: false,
            peeked_token: None,
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
        self.do_statement()?;
        self.expect(Token::RightBrace)?;
        Ok(())
    }

    fn do_statement(&mut self) -> Result<()> {
        let token = self.lexer.next_token();
        match token {
            Ok(Token::Return) => {
                self.do_return()?;
            }
            _ => panic!("Expected Return, got {:?}", token),
        }
        self.expect(Token::SemiColon)?;
        Ok(())
    }

    fn do_return(&mut self) -> Result<()> {
        let val = self.do_expression(0)?;
        self.tacky.add_instruction(Instruction::Return(val));
        Ok(())
    }

    fn do_expression(&mut self, min_prec: i32) -> Result<Value> {
        let mut left = self.do_factor()?;
        let mut token = self.peek()?;
        loop {
            if Self::precedence(&token) < min_prec {
                break;
            }
            let op = self.do_binop()?;
            let right = self.do_expression(Self::precedence(&token) + 1)?;
            let dest = Value::Variable(self.make_temporary());
            let inst = Instruction::Binary(op, left, right, dest.clone());
            self.tacky.add_instruction(inst);
            left = dest;
            token = self.peek()?;
        }
        Ok(left)
    }

    fn do_binop(&mut self) -> Result<BinaryOperator> {
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
            Token::LogicalAnd => Ok(BinaryOperator::LogicalAnd),
            Token::LogicalOr => Ok(BinaryOperator::LogicalOr),
            Token::IsEqual => Ok(BinaryOperator::Equal),
            _ => bail!("Expected binop, got {:?}", token),
        }
    }
    fn do_factor(&mut self) -> Result<Value> {
        let token = self.next_token()?;
        match token {
            Token::Constant(val) => {
                println!("val: {}", val);
                Ok(Value::Int(val))
            }

            Token::Negate => {
                let source = self.do_factor()?;
                let dest_name = self.make_temporary();
                let ret_dest = Value::Variable(dest_name.clone());
                let unop =
                    Instruction::Unary(UnaryOperator::Negate, source, Value::Variable(dest_name));
                self.tacky.add_instruction(unop);
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
                self.tacky.add_instruction(unop);
                Ok(ret_dest)
            }
            Token::LeftParen => {
                let ret = self.do_expression(0)?;
                self.expect(Token::RightParen)?;
                Ok(ret)
            }
            _ => {
                println!("huh? {:?}", token);
                bail!("huh? {:?}", token);
            }
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

    fn next_token(&mut self) -> Result<Token> {
        if let Some(peeked) = self.peeked_token.take() {
            return Ok(peeked);
        }
        let token = self.lexer.next_token()?;
        if token == Token::Eof {
            self.eof_hit = true;
        }
        Ok(token)
    }

    fn peek(&mut self) -> Result<Token> {
        if let Some(peeked) = self.peeked_token.as_ref() {
            Ok(peeked.clone())
        } else {
            let next = self.next_token()?;
            self.peeked_token = Some(next.clone());
            Ok(next)
        }
    }

    fn precedence(token: &Token) -> i32 {
        match token {
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

            _ => -1, // indicates this is not a binop
        }
    }
}
