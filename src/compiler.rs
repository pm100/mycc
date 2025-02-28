use anyhow::Result;
use pest::{iterators::Pair, Parser};

use crate::tacky::{Instruction, TackyProgram, UnaryOperator, Value};
#[derive(pest_derive::Parser)]
#[grammar = "c.pest"]
pub struct CParser;

pub struct Compiler {
    tacky: TackyProgram,
    next_temporary: usize,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            tacky: TackyProgram::new(),
            next_temporary: 0,
        }
    }
    pub fn run(&mut self, input: &str) -> Result<bool> {
        let pairs = CParser::parse(Rule::program, input)?;

        for pair in pairs {
            //  println!("{:?}", pair);
            match pair.as_rule() {
                Rule::program => {
                    self.do_program(pair);
                }
                _ => {}
            }
        }
        self.tacky.dump();
        Ok(true)
    }
    fn do_program(&mut self, pair: Pair<Rule>) {
        for pair in pair.into_inner() {
            match pair.as_rule() {
                Rule::function => {
                    self.do_function(pair);
                }
                _ => {}
            }
        }
    }
    fn do_function(&mut self, pair: Pair<Rule>) {
        let mut pair_iter = pair.into_inner();

        let function_name = pair_iter.next().unwrap().as_str();
        println!("function_name: {}", function_name);
        self.tacky.add_function(function_name);
        let statement = pair_iter.next().unwrap();

        self.do_statement(statement);
    }

    fn do_statement(&mut self, pair: Pair<Rule>) {
        let mut pair_iter = pair.into_inner();
        let pair = pair_iter.next().unwrap();
        match pair.as_rule() {
            Rule::expression => {
                let val = self.do_expression(pair);
                self.tacky.add_instruction(Instruction::Return(val));
            }
            _ => {}
        }
    }

    fn do_expression(&mut self, pair: Pair<Rule>) -> Value {
        //        for pair in pair.into_inner() {
        println!("exp {:?}", pair);
        let mut pair_iter = pair.into_inner();
        let pair = pair_iter.next().unwrap();
        match pair.as_rule() {
            Rule::int => {
                let val = pair.as_str().trim().parse::<i32>().unwrap();
                println!("val: {}", val);
                Value::Int(val)
            }

            Rule::unop => {
                let op = pair.as_str();
                println!("op: {}", op);
                //  let mut pair_iter = pair.into_inner();
                let pair = pair_iter.next().unwrap();
                let source = self.do_expression(pair);
                let dest_name = self.make_temporary();
                let ret_dest = Value::Variable(dest_name.clone());
                let unop = match op {
                    "-" => Instruction::Unary(
                        UnaryOperator::Negate,
                        source,
                        Value::Variable(dest_name),
                    ),
                    "~" => Instruction::Unary(
                        UnaryOperator::Complement,
                        source,
                        Value::Variable(dest_name),
                    ),
                    _ => {
                        panic!("unknown unary operator: {}", op);
                    }
                };
                self.tacky.add_instruction(unop);
                ret_dest
            }
            Rule::expression => {
                //   let mut pair_iter = pair.into_inner();
                // first is name
                //let pair = pair_iter.next().unwrap()
                self.do_expression(pair)
            }
            _ => {
                println!("huh? {:?}", pair);
                Value::Int(0)
            }
        }
    }
    fn make_temporary(&mut self) -> String {
        let temp = format!("temp.{}", self.next_temporary);
        self.next_temporary += 1;
        temp
    }
}
