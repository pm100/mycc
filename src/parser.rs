use anyhow::Result;
use pest::{iterators::Pair, Parser};
#[derive(pest_derive::Parser)]
#[grammar = "c.pest"]
pub struct JackParser;

pub fn parse(input: &str) -> Result<bool> {
    let pairs = JackParser::parse(Rule::program, input)?;
    Ok(true)
}
