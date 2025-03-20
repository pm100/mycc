pub mod codegen;
pub mod cpp;
pub mod expr;
pub mod lexer;
pub mod moira;
pub mod parser;
pub mod tacky;

pub mod x64 {
    pub mod moira_inst;
    pub mod moiragen;
    pub mod nasmgen;
}
//use crate::x64::x64gen::X64CodeGenerator;
use crate::codegen::MoiraGenerator;
use crate::x64::nasmgen::X64CodeGenerator;
use anyhow::Result;
use codegen::MoiraCompiler;
use x64::moiragen::X64MoiraGenerator;

use std::path::{Path, PathBuf};
use std::process::ExitCode;

use clap::Parser;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    /// Optional name to operate on
    source: PathBuf,

    #[arg(long)]
    lex: bool,
    #[arg(long)]
    parse: bool,
    #[arg(long)]
    codegen: bool,
    #[arg(long)]
    validate: bool,
    #[arg(short, long)]
    compile_only: bool,
}
fn main() -> ExitCode {
    let mut cli = Cli::parse();

    eprintln!("lex: {:?}", cli.lex);
    eprintln!("parse: {:?}", cli.parse);
    eprintln!("codegen: {:?}", cli.codegen);
    eprintln!("validate: {:?}", cli.validate);
    eprintln!("source: {:?}", &cli.source);
    eprintln!("compile_only: {:?}", cli.compile_only);

    let stub = "mycc_cpp";
    //uuid::Uuid::new_v4()
    let preproc_output = std::env::temp_dir().join(format!("{}.i", stub));

    println!("preprocessing {:?} => {:?}", &cli.source, &preproc_output);
    cpp::preprocess(&cli.source, &preproc_output).unwrap();
    if cli.lex {
        match lex(&preproc_output) {
            Ok(()) => {
                return ExitCode::SUCCESS;
            }
            Err(e) => {
                eprintln!("build error {:?}", e);
                return ExitCode::FAILURE;
            }
        }
    }

    if cli.parse || cli.validate {
        match parse(&preproc_output) {
            Ok(()) => {
                return ExitCode::SUCCESS;
            }
            Err(e) => {
                eprintln!("build error {:?}", e);
                return ExitCode::FAILURE;
            }
        }
    }

    if cli.codegen {
        if codegen(&preproc_output).is_ok() {
            return ExitCode::SUCCESS;
        };
        return ExitCode::FAILURE;
    }
    cli.source.set_extension("exe");
    match build(&preproc_output, &cli.source, cli.compile_only) {
        Ok(()) => {
            return ExitCode::SUCCESS;
        }
        Err(e) => {
            eprintln!("build error {:?}", e);
            return ExitCode::FAILURE;
        }
    }
    ExitCode::FAILURE
}

fn lex(path: &Path) -> Result<()> {
    let mut lexer = lexer::Lexer::new(path);
    loop {
        let token = lexer.next_token()?;
        println!("{:?}", token);
        if token == lexer::Token::Eof {
            break;
        }
    }
    Ok(())
}

fn parse(path: &Path) -> Result<()> {
    let mut parser = parser::Parser::new(path);
    let _ = parser.parse()?;
    Ok(())
}

fn codegen(path: &Path) -> Result<()> {
    let mut parser = parser::Parser::new(path);
    let tackyx = parser.parse();
    match tackyx {
        Ok(tacky) => {
            tacky.dump();
            let mut gen = X64MoiraGenerator::new();
            // let mut moira = MoiraProgram::<Instruction>::new();
            let moira = gen.generate_moira(tacky)?;
            moira.dump();

            let stub = "mycc_cpp";
            let gen_output = std::env::temp_dir().join(format!("{}.s", stub));
            let gen_output = PathBuf::from(format!("{}.asm", stub));
            let mut gen = X64CodeGenerator::new();
            gen.generate_asm(moira, &gen_output)?;
            // crate::cpp::assemble_link(&gen_output, output, compile_only)?;
        }
        Err(e) => {
            println!("Error: {:?}", e);
            return Err(e);
        }
    }
    //tacky.dump();
    Ok(())
}

fn build(source: &Path, output: &Path, compile_only: bool) -> Result<()> {
    let mut parser = parser::Parser::new(source);
    let tackyx = parser.parse();
    match tackyx {
        Ok(tacky) => {
            tacky.dump();
            let mut gen = X64MoiraGenerator::new();
            // let mut moira = MoiraProgram::<Instruction>::new();
            let moira = gen.generate_moira(tacky)?;
            moira.dump();

            let stub = "mycc_cpp";
            let gen_output = std::env::temp_dir().join(format!("{}.s", stub));
            let gen_output = PathBuf::from(format!("{}.asm", stub));
            let mut gen = X64CodeGenerator::new();
            gen.generate_asm(moira, &gen_output)?;
            crate::cpp::assemble_link(&gen_output, output, compile_only)?;
        }
        Err(e) => {
            println!("Error: {:?}", e);
            return Err(e);
        }
    }
    //tacky.dump();
    Ok(())
}
