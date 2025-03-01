use std::{f32::consts::E, mem::discriminant, path::PathBuf};

macro_rules! expect {
    ($token:expr) => {
        let function_name = match self.lexer.next_token().unwrap() {
            LexReturn::Token($token) => name,
            LexReturn::Error => panic!("Error: {}", "x"),
            _ => panic!("Expected Identifier, got None"),
        };
    };
}

use crate::{
    lexer::{LexReturn, Lexer, Token},
    tacky::{Instruction, TackyProgram, UnaryOperator, Value},
};

pub struct Parser {
    lexer: Lexer,
    tacky: TackyProgram,
    next_temporary: usize,
}

impl Parser {
    pub fn new(path: PathBuf) -> Self {
        Self {
            lexer: Lexer::new(path),
            tacky: TackyProgram::new(),
            next_temporary: 0,
        }
    }

    pub fn parse(&mut self) {
        self.do_program();
        self.tacky.dump();
    }

    fn do_program(&mut self) {
        self.do_function();
    }
    fn do_function(&mut self) {
        self.expect(Token::Int);
        // expect!(Token::Identifier(name));
        let function_name = match self.lexer.next_token().unwrap() {
            LexReturn::Token(Token::Identifier(name)) => name,
            LexReturn::Error => panic!("Error: {}", "x"),
            _ => panic!("Expected Identifier"),
        };

        //let function_name = self.expect(Token::Identifier("".to_string()));
        println!("Function: {}", function_name);
        self.tacky.add_function(&function_name);
        self.expect(Token::LeftParen);
        self.expect(Token::Void);
        self.expect(Token::RightParen);
        self.do_function_body();
    }

    fn do_function_body(&mut self) {
        self.expect(Token::LeftBrace);
        self.do_statement();
        self.expect(Token::RightBrace);
    }

    fn do_statement(&mut self) {
        let token = self.lexer.next_token().unwrap();
        match token {
            LexReturn::Token(Token::Return) => {
                self.do_return();
            }
            _ => panic!("Expected Return, got {:?}", token),
        }
        self.expect(Token::SemiColon);
    }

    fn do_return(&mut self) {
        let val = self.do_expression();
        self.tacky.add_instruction(Instruction::Return(val));
    }
    fn next_token(&mut self) -> Token {
        let token = self.lexer.next_token().unwrap();
        match token {
            LexReturn::Token(t) => return t,
            _ => panic!("Expected Return, got {:?}", token),
        }
    }
    fn do_expression(&mut self) -> Value {
        let token = self.next_token();
        match token {
            Token::Constant(val) => {
                println!("val: {}", val);
                Value::Int(val)
            }

            Token::Negate => {
                let source = self.do_expression();
                let dest_name = self.make_temporary();
                let ret_dest = Value::Variable(dest_name.clone());
                let unop =
                    Instruction::Unary(UnaryOperator::Negate, source, Value::Variable(dest_name));
                self.tacky.add_instruction(unop);
                ret_dest
            }

            Token::Complement => {
                let source = self.do_expression();
                let dest_name = self.make_temporary();
                let ret_dest = Value::Variable(dest_name.clone());
                let unop = Instruction::Unary(
                    UnaryOperator::Complement,
                    source,
                    Value::Variable(dest_name),
                );
                self.tacky.add_instruction(unop);
                ret_dest
            }
            Token::LeftParen => {
                //   let mut pair_iter = pair.into_inner();
                // first is name
                //let pair = pair_iter.next().unwrap()
                let ret = self.do_expression();
                self.expect(Token::RightParen);
                ret
            }
            _ => {
                println!("huh? {:?}", "x");
                Value::Int(0)
            }
        }
    }
    fn expect(&mut self, token: Token) -> Token {
        match self.lexer.next_token().unwrap() {
            LexReturn::Token(t) => {
                if discriminant(&t) != discriminant(&token) {
                    panic!("Expected {:?}, got {:?}", token, t);
                } else {
                    return t;
                }
            }
            LexReturn::Error => panic!("Error: {}", "x"),
        }
        panic!("Expected {:?}, got None", token);
    }

    fn make_temporary(&mut self) -> String {
        let temp = format!("temp.{}", self.next_temporary);
        self.next_temporary += 1;
        temp
    }
}
