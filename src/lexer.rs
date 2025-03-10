use anyhow::{bail, Result};

pub struct Lexer {
    reader: LineReader,
    current_line: String,
    current_pos: usize,
    current_line_number: usize,
}
#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // punctuation
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    SemiColon,
    Equals,

    // basics
    Identifier(String),
    Constant(i32),

    // operators
    Complement, // ~
    Negate,     // -
    Divide,
    Add,
    Multiply,
    Remainder, // %
    Not,       // !

    LogicalAnd,
    LogicalOr,
    IsEqual,
    IsNotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,

    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    ShiftLeft,
    ShiftRight,

    PlusEquals,
    MinusEquals,
    MultiplyEquals,
    DivideEquals,
    RemainderEquals,
    AndEquals,
    OrEquals,
    XorEquals,
    ShiftLeftEquals,
    ShiftRightEquals,

    // keywords
    Int,
    Void,
    Return,

    // special
    Eof,
}
impl Lexer {
    pub fn new(path: &Path) -> Self {
        let reader = LineReader::open(path).unwrap();
        Self {
            reader,
            current_line: String::new(),
            current_pos: 0,
            current_line_number: 0,
        }
    }

    pub fn next_token(&mut self) -> Result<Token> {
        loop {
            if self.at_end() {
                match self.reader.read_line(&mut self.current_line) {
                    Some(Ok(_)) => (),
                    Some(Err(e)) => panic!("Error reading line: {}", e),
                    None => return Ok(Token::Eof),
                }
                self.current_line = self.current_line.trim().to_string();
                self.current_pos = 0;
                self.current_line_number += 1;
                // blank lines
                if self.at_end() {
                    continue;
                }
            };

            let char = self.advance();
            let token = match char {
                '(' => Token::LeftParen,
                ')' => Token::RightParen,
                '{' => Token::LeftBrace,
                '}' => Token::RightBrace,
                ';' => Token::SemiColon,

                '/' => {
                    if self.match_next('/') {
                        self.current_line.clear();
                        continue;
                    }
                    if self.match_next('*') {
                        while !self.at_end() {
                            if self.match_next('*') && self.match_next('/') {
                                break;
                            }
                            self.advance();
                        }
                        continue;
                    }
                    if self.match_next('=') {
                        Token::DivideEquals
                    } else {
                        Token::Divide
                    }
                }

                '&' => {
                    if self.match_next('&') {
                        Token::LogicalAnd
                    } else {
                        if self.match_next('=') {
                            Token::AndEquals
                        } else {
                            Token::BitwiseAnd
                        }
                    }
                }

                '|' => {
                    if self.match_next('|') {
                        Token::LogicalOr
                    } else {
                        if self.match_next('=') {
                            Token::OrEquals
                        } else {
                            Token::BitwiseOr
                        }
                    }
                }

                '^' => {
                    if self.match_next('=') {
                        Token::XorEquals
                    } else {
                        Token::BitwiseXor
                    }
                }
                '<' => {
                    if self.match_next('=') {
                        Token::LessThanOrEqual
                    } else if self.match_next('<') {
                        if self.match_next('=') {
                            Token::ShiftLeftEquals
                        } else {
                            Token::ShiftLeft
                        }
                    } else {
                        Token::LessThan
                    }
                }
                '>' => {
                    if self.match_next('=') {
                        Token::GreaterThanOrEqual
                    } else if self.match_next('>') {
                        if self.match_next('=') {
                            Token::ShiftRightEquals
                        } else {
                            Token::ShiftRight
                        }
                    } else {
                        Token::GreaterThan
                    }
                }
                '!' => {
                    if self.match_next('=') {
                        Token::IsNotEqual
                    } else {
                        Token::Not
                    }
                }

                '=' => {
                    if self.match_next('=') {
                        Token::IsEqual
                    } else {
                        Token::Equals
                    }
                }

                ' ' | '\t' => continue,

                '~' => Token::Complement,
                '-' => {
                    if self.match_next('=') {
                        Token::MinusEquals
                    } else {
                        Token::Negate
                    }
                }
                '+' => {
                    if self.match_next('=') {
                        Token::PlusEquals
                    } else {
                        Token::Add
                    }
                }
                '*' => {
                    if self.match_next('=') {
                        Token::MultiplyEquals
                    } else {
                        Token::Multiply
                    }
                }
                '%' => {
                    if self.match_next('=') {
                        Token::RemainderEquals
                    } else {
                        Token::Remainder
                    }
                }

                _ => {
                    if char.is_ascii_digit() {
                        let t = self.number();
                        // reject 123foo
                        if self.peek().is_alphabetic() {
                            println!("Error: unexpected character: {}", self.peek());
                            bail!("bad character")
                        }
                        t
                    } else if char.is_alphabetic() || char == '_' {
                        self.identifier()
                    } else {
                        println!("Error: unexpected character: {}", char);
                        bail!("bad character")
                    }
                }
            };
            return Ok(token);
        }
    }

    fn identifier(&mut self) -> Token {
        let start = self.current_pos - 1;
        while self.peek().is_alphanumeric() || self.peek() == '_' {
            self.advance();
        }
        let s = &self.current_line[start..self.current_pos];
        match s {
            "int" => Token::Int,
            "void" => Token::Void,
            "return" => Token::Return,
            _ => Token::Identifier(s.to_string()),
        }
    }
    fn number(&mut self) -> Token {
        let start = self.current_pos - 1;
        while self.peek().is_ascii_digit() {
            self.advance();
        }
        Token::Constant(self.current_line[start..self.current_pos].parse().unwrap())
    }
    fn advance(&mut self) -> char {
        // horifically inefficient!
        let c = self.current_line.chars().nth(self.current_pos);
        self.current_pos += 1;
        c.unwrap()
    }
    fn peek(&self) -> char {
        if self.at_end() {
            return '\0';
        }
        self.current_line.chars().nth(self.current_pos).unwrap()
    }
    fn match_next(&mut self, c: char) -> bool {
        if self.at_end() {
            return false;
        }
        if self.current_line.chars().nth(self.current_pos).unwrap() != c {
            return false;
        }
        self.current_pos += 1;
        true
    }

    fn at_end(&self) -> bool {
        self.current_pos >= self.current_line.len()
    }
}
//https://stackoverflow.com/a/45882510/173397
// fn main() -> std::io::Result<()> {
//     for line in my_reader::BufReader::open("Cargo.toml")? {
//         println!("{}", line?.trim());
//     }

//     Ok(())
// }

use core::panic;
use std::{
    fs::File,
    io::{self, prelude::*},
    path::Path,
};

struct LineReader {
    reader: io::BufReader<File>,
}

impl LineReader {
    pub fn open(path: impl AsRef<std::path::Path>) -> io::Result<Self> {
        let file = File::open(path)?;
        let reader = io::BufReader::new(file);

        Ok(Self { reader })
    }

    pub fn read_line<'buf>(
        &mut self,
        buffer: &'buf mut String,
    ) -> Option<io::Result<&'buf mut String>> {
        buffer.clear();

        self.reader
            .read_line(buffer)
            .map(|u| if u == 0 { None } else { Some(buffer) })
            .transpose()
    }
}
