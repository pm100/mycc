use anyhow::{bail, Result};

pub struct Lexer {
    reader: LineReader,
    pub file: PathBuf,
    pub current_line: String,
    pub current_pos: usize,
    pub current_line_number: usize,
    chars: Vec<char>,
}
#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // punctuation
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    SemiColon,
    Assign,
    QuestionMark,
    Colon,
    Comma,
    LeftBracket,
    RightBracket,

    // basics
    Identifier(String),
    Constant(i32),
    LongConstant(i64),
    UConstant(u32),
    ULongConstant(u64),
    F64Constant(f64),

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

    PlusPlus,
    MinusMinus,

    // keywords
    Int,
    Void,
    Long,
    Return,
    If,
    Else,
    GoTo,
    Do,
    While,
    For,
    Break,
    Continue,
    Case,
    Default,
    Switch,
    Extern,
    Static,
    Signed,
    Unsigned,
    Double,

    // special
    Eof,
}
enum MaybeNumber {
    Number(i128),
    Float(f64),
    None,
}
impl Lexer {
    pub fn new(path: &Path) -> Self {
        let reader = LineReader::open(path).unwrap();
        Self {
            reader,
            file: path.to_path_buf(),
            current_line: String::new(),
            current_pos: 0,
            current_line_number: 0,
            chars: Vec::new(),
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
                println!("{}: {}", self.current_line_number, self.current_line);
                self.current_pos = 0;
                self.current_line_number += 1;
                self.chars = self.current_line.chars().collect();
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
                '?' => Token::QuestionMark,
                ':' => Token::Colon,
                ',' => Token::Comma,

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
                    } else if self.match_next('=') {
                        Token::AndEquals
                    } else {
                        Token::BitwiseAnd
                    }
                }

                '|' => {
                    if self.match_next('|') {
                        Token::LogicalOr
                    } else if self.match_next('=') {
                        Token::OrEquals
                    } else {
                        Token::BitwiseOr
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
                        Token::Assign
                    }
                }

                ' ' | '\t' => continue,

                '~' => Token::Complement,
                '-' => {
                    if self.match_next('=') {
                        Token::MinusEquals
                    } else if self.match_next('-') {
                        Token::MinusMinus
                    } else {
                        Token::Negate
                    }
                }
                '+' => {
                    if self.match_next('=') {
                        Token::PlusEquals
                    } else if self.match_next('+') {
                        Token::PlusPlus
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
                '[' => Token::LeftBracket,
                ']' => Token::RightBracket,
                _ => {
                    if char.is_ascii_digit() || char == '.' {
                        let t = self.number()?;
                        // reject 123foo
                        if !self.peek().is_whitespace()
                            && !"!%&()*+,-/:;<=>?@[\\]^`{|}~\0".contains(self.peek())
                        {
                            //    println!("Error: unexpected character: {}", self.peek());
                            bail!("bad character1 {}", self.peek())
                        }
                        t
                    } else if char.is_alphabetic() || char == '_' {
                        self.identifier()
                    } else {
                        //  println!("Error: unexpected character: {}", char);
                        bail!("bad character2")
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
            "if" => Token::If,
            "else" => Token::Else,
            "goto" => Token::GoTo,
            "do" => Token::Do,
            "while" => Token::While,
            "for" => Token::For,
            "break" => Token::Break,
            "continue" => Token::Continue,
            "case" => Token::Case,
            "default" => Token::Default,
            "switch" => Token::Switch,
            "extern" => Token::Extern,
            "static" => Token::Static,
            "long" => Token::Long,
            "signed" => Token::Signed,
            "unsigned" => Token::Unsigned,
            "double" => Token::Double,

            _ => Token::Identifier(s.to_string()),
        }
    }

    fn try_parse_float(&mut self) -> MaybeNumber {
        let start = self.current_pos - 1;
        let rest_of_line = self.get_slice(start);

        if let Ok((fl, count)) = fast_float::parse_partial::<f64, _>(rest_of_line.as_bytes()) {
            if let Ok(val) = self.current_line[start..start + count].parse::<i128>() {
                if val as f64 == fl {
                    self.current_pos += val.to_string().len() - 1;
                    return MaybeNumber::Number(val);
                } else {
                    self.current_pos += count - 1;
                    return MaybeNumber::Float(fl);
                }
            } else {
                self.current_pos += count - 1;
                return MaybeNumber::Float(fl);
            }
        } else {
            return MaybeNumber::None;
        }
    }
    fn number(&mut self) -> Result<Token> {
        match self.try_parse_float() {
            MaybeNumber::None => bail!("Error: invalid number"),
            MaybeNumber::Float(f) => {
                return Ok(Token::F64Constant(f));
            }
            MaybeNumber::Number(mut val) => {
                // let start = self.current_pos - 1;
                // while self.peek().is_ascii_digit() {
                //     self.advance();
                // }
                //   let mut val: i128 = self.current_line[start..self.current_pos].parse().unwrap();

                let mut unsigned = false;
                let mut long = false;
                loop {
                    match self.peek() {
                        'u' | 'U' => {
                            self.advance();
                            if unsigned {
                                bail!("Error: multiple unsigned specifiers")
                            }
                            unsigned = true;
                        }
                        'l' | 'L' => {
                            self.advance();
                            if long {
                                bail!("Error: multiple long specifiers")
                            }
                            long = true;
                        }
                        _ => break,
                    };
                }

                if long {
                    if unsigned {
                        if val > u64::MAX as i128 {
                            val &= 0xFFFFFFFFFFFFFFFF;
                        }
                        return Ok(Token::ULongConstant(val as u64));
                    }
                    if val > i64::MAX as i128 {
                        val &= 0xFFFFFFFFFFFFFFFF;
                    }
                    return Ok(Token::LongConstant(val as i64));
                }
                if unsigned {
                    if val > u32::MAX as i128 {
                        if val > u64::MAX as i128 {
                            val &= 0xFFFFFFFFFFFFFFFF;
                        }
                        return Ok(Token::ULongConstant(val as u64));
                    }
                    return Ok(Token::UConstant(val as u32));
                }
                if val > i32::MAX as i128 {
                    if val > i64::MAX as i128 {
                        val &= 0xFFFFFFFFFFFFFFFF;
                    }
                    Ok(Token::LongConstant(val as i64))
                } else {
                    Ok(Token::Constant(val as i32))
                }
            }
        }
    }
    fn advance(&mut self) -> char {
        let c = self.chars[self.current_pos];
        self.current_pos += 1;
        c
    }
    fn peek(&self) -> char {
        if self.at_end() {
            return '\0';
        }
        self.chars[self.current_pos]
    }

    fn get_slice(&self, start: usize) -> &str {
        &self.current_line[start..self.current_line.len()]
    }
    fn match_next(&mut self, c: char) -> bool {
        if self.at_end() {
            return false;
        }
        if self.chars[self.current_pos] != c {
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
    path::{Path, PathBuf},
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
