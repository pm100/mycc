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
    tacky::{Instruction, TackyProgram, UnaryOperator, Value},
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
        let val = self.do_expression()?;
        self.tacky.add_instruction(Instruction::Return(val));
        Ok(())
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
    fn do_expression(&mut self) -> Result<Value> {
        let token = self.next_token()?;
        match token {
            Token::Constant(val) => {
                println!("val: {}", val);
                Ok(Value::Int(val))
            }

            Token::Negate => {
                let source = self.do_expression()?;
                let dest_name = self.make_temporary();
                let ret_dest = Value::Variable(dest_name.clone());
                let unop =
                    Instruction::Unary(UnaryOperator::Negate, source, Value::Variable(dest_name));
                self.tacky.add_instruction(unop);
                Ok(ret_dest)
            }

            Token::Complement => {
                let source = self.do_expression()?;
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
                //   let mut pair_iter = pair.into_inner();
                // first is name
                //let pair = pair_iter.next().unwrap()
                let ret = self.do_expression()?;
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
}
