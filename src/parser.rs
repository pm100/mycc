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
#[derive(Debug, Clone)]
pub(crate) struct VariableName {
    pub(crate) name: String,
    pub(crate) is_current: bool,
}
struct LoopLabels {
    cont: String,
    brk: String,
}
pub struct Parser {
    lexer: Lexer,
    tacky: TackyProgram,
    next_temporary: usize,
    eof_hit: bool,
    peeked_tokens: VecDeque<Token>,
    variables: Vec<HashMap<String, VariableName>>,
    labels: HashMap<String, String>,
    next_name: usize,
    loop_labels: Vec<LoopLabels>,
    pub(crate) nest: String,
}

impl Parser {
    pub fn new(path: &Path) -> Self {
        Self {
            lexer: Lexer::new(path),
            tacky: TackyProgram::new(),
            next_temporary: 0,
            eof_hit: false,
            peeked_tokens: VecDeque::new(),
            variables: Vec::new(),
            labels: HashMap::new(),
            next_name: 0,
            nest: String::new(),
            loop_labels: Vec::new(),
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
        //  self.expect(Token::LeftBrace)?;
        self.variables.clear();
        self.variables.push(HashMap::new());
        self.do_block()?;
        self.instruction(Instruction::Return(Value::Int(0)));
        Ok(())
    }
    fn do_block(&mut self) -> Result<()> {
        self.expect(Token::LeftBrace)?;

        while self.peek()? != Token::RightBrace {
            self.do_block_item()?;
        }
        self.variables.pop();
        self.expect(Token::RightBrace)?;
        //  self.next_token()?;
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
        if let Some(var) = self.variables().get(name) {
            if var.is_current {
                bail!("Variable {} already declared", name);
            }
            //bail!("Variable {} already declared", var.name);
        }
        let new_name = self.make_newname(name);
        println!("new_name: {}=>{}", name, new_name);
        self.variables().insert(
            name.to_string(),
            VariableName {
                name: new_name.to_string(),
                is_current: true,
            },
        );
        if init {
            let val = self.do_expression(0)?;
            self.instruction(Instruction::Copy(val, Value::Variable(new_name)));
        }
        Ok(())
    }

    fn do_statement(&mut self) -> Result<()> {
        let token = self.peek()?;
        match token {
            Token::For => {
                self.do_for()?;
            }
            Token::While => {
                self.do_while()?;
            }
            Token::Do => {
                self.do_do_while()?;
            }
            Token::Break => {
                self.do_break()?;
            }
            Token::Continue => {
                self.do_continue()?;
            }
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
            Token::LeftBrace => {
                self.push_new_varmap();
                self.do_block()?;
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
    fn do_for(&mut self) -> Result<()> {
        self.push_new_varmap();
        self.next_token()?;
        let label_end = self.make_label();
        let label_start = self.make_label();
        let label_body = self.make_label();
        let label_inc = self.make_label();
        let loop_labels = LoopLabels {
            cont: label_inc.clone(),
            brk: label_end.clone(),
        };
        self.loop_labels.push(loop_labels);
        self.expect(Token::LeftParen)?;
        let token = self.peek()?;
        // init clause
        match token {
            Token::Int => {
                // eats the semi colon
                self.do_declaration()?;
            }
            Token::SemiColon => {
                self.next_token()?;
            }
            _ => {
                self.do_expression(0)?;
                self.expect(Token::SemiColon)?;
            }
        }
        // ==================== START:
        // condition clause
        self.instruction(Instruction::Label(label_start.clone()));
        let token = self.peek()?;
        let cond = if token == Token::SemiColon {
            Value::Int(1)
        } else {
            self.do_expression(0)?
        };
        self.instruction(Instruction::JumpIfZero(cond, label_end.clone()));
        self.expect(Token::SemiColon)?;
        // ===================== goto BODY
        self.instruction(Instruction::Jump(label_body.clone()));
        // increment clause
        // ===================== INC:
        self.instruction(Instruction::Label(label_inc.clone()));
        let token = self.peek()?;
        if token != Token::RightParen {
            self.do_expression(0)?;
        }
        self.expect(Token::RightParen)?;
        // ===================== goto START
        self.instruction(Instruction::Jump(label_start.clone()));
        // ===================== BODY:
        self.instruction(Instruction::Label(label_body.clone()));
        self.do_statement()?;
        // ===================== goto INC
        self.instruction(Instruction::Jump(label_inc.clone()));
        // ===================== END:
        self.instruction(Instruction::Label(label_end.clone()));
        self.variables.pop();
        self.loop_labels.pop();
        Ok(())
    }
    fn do_while(&mut self) -> Result<()> {
        self.next_token()?;
        let label_start = self.make_label();
        let label_end = self.make_label();
        let loop_labels = LoopLabels {
            cont: label_start.clone(),
            brk: label_end.clone(),
        };
        self.loop_labels.push(loop_labels);
        self.instruction(Instruction::Label(label_start.clone()));
        self.expect(Token::LeftParen)?;
        let cond = self.do_expression(0)?;
        self.instruction(Instruction::JumpIfZero(cond, label_end.clone()));
        self.expect(Token::RightParen)?;
        self.do_statement()?;
        self.instruction(Instruction::Jump(label_start.clone()));
        self.instruction(Instruction::Label(label_end.clone()));
        self.loop_labels.pop();

        Ok(())
    }
    fn do_do_while(&mut self) -> Result<()> {
        self.next_token()?;
        let label_start = self.make_label();
        let label_end = self.make_label();
        let label_cond = self.make_label();
        let loop_labels = LoopLabels {
            cont: label_cond.clone(),
            brk: label_end.clone(),
        };
        self.loop_labels.push(loop_labels);
        self.instruction(Instruction::Label(label_start.clone()));
        self.do_statement()?;
        self.expect(Token::While)?;
        self.expect(Token::LeftParen)?;
        self.instruction(Instruction::Label(label_cond.clone()));
        let cond = self.do_expression(0)?;
        self.instruction(Instruction::JumpIfZero(cond, label_end.clone()));
        self.expect(Token::RightParen)?;
        self.expect(Token::SemiColon)?;
        self.instruction(Instruction::Jump(label_start.clone()));
        self.instruction(Instruction::Label(label_end.clone()));
        self.loop_labels.pop();

        Ok(())
    }
    fn do_break(&mut self) -> Result<()> {
        self.next_token()?;
        if let Some(loop_labels) = self.loop_labels.last() {
            self.instruction(Instruction::Jump(loop_labels.brk.clone()));
        } else {
            bail!("Break outside of loop");
        }

        self.expect(Token::SemiColon)?;
        Ok(())
    }
    fn do_continue(&mut self) -> Result<()> {
        self.next_token()?;
        if let Some(loop_labels) = self.loop_labels.last() {
            self.instruction(Instruction::Jump(loop_labels.cont.clone()));
        } else {
            bail!("continue outside of loop");
        }
        self.expect(Token::SemiColon)?;
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
    pub(crate) fn is_lvalue(&mut self, value: &Value) -> bool {
        match value {
            Value::Variable(var) => self.variables().values().any(|v| v.name == *var),
            _ => false,
        }
    }
    pub(crate) fn expect(&mut self, token: Token) -> Result<Token> {
        let nt = self.next_token()?;
        if discriminant(&nt) != discriminant(&token) {
            bail!("Expected {:?}, got {:?}", token, nt);
        } else {
            Ok(nt)
        }
    }

    pub(crate) fn make_temporary(&mut self) -> String {
        let temp = format!("temp.{}", self.next_temporary);
        self.next_temporary += 1;
        temp
    }

    pub(crate) fn make_newname(&mut self, name: &str) -> String {
        let temp = format!("{}${}", name, self.next_name);
        self.next_name += 1;
        temp
    }
    pub(crate) fn make_label(&mut self) -> String {
        let temp = format!("label_{}", self.next_temporary);
        self.next_temporary += 1;
        temp
    }
    pub(crate) fn convert_binop(&mut self) -> Result<BinaryOperator> {
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
    pub(crate) fn convert_compound(token: &Token) -> Result<BinaryOperator> {
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
    pub(crate) fn instruction(&mut self, instruction: Instruction) {
        println!("{}instruction {:?}", self.nest, instruction);
        self.tacky.add_instruction(instruction);
    }
    pub(crate) fn next_token(&mut self) -> Result<Token> {
        let token = if self.peeked_tokens.len() > 0 {
            self.peeked_tokens.pop_front().unwrap()
        } else {
            self.lexer.next_token()?
        };
        if token == Token::Eof {
            self.eof_hit = true;
        }
        println!(
            "                  next_token {:?} peeks={} blocks={}",
            token,
            self.peeked_tokens.len(),
            self.variables.len()
        );
        Ok(token)
    }
    fn push_new_varmap(&mut self) {
        let newmap = self
            .variables()
            .iter()
            .map(|(k, v)| {
                let mut v = v.clone();
                v.is_current = false;
                (k.clone(), v.clone())
            })
            .collect();
        self.variables.push(newmap);
    }
    pub(crate) fn variables(&mut self) -> &mut HashMap<String, VariableName> {
        let len = self.variables.len();
        self.variables.get_mut(len - 1).unwrap()
    }
    pub(crate) fn peek(&mut self) -> Result<Token> {
        self.peek_n(0)
    }

    pub(crate) fn peek_n(&mut self, n: usize) -> Result<Token> {
        if n >= self.peeked_tokens.len() {
            for _ in 0..n + 1 {
                let token = self.lexer.next_token()?;
                self.peeked_tokens.push_back(token.clone());
            }
        }
        return Ok(self.peeked_tokens[n].clone());
    }
}
