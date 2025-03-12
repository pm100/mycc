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
    pub mod x64gen;
}
//use crate::x64::x64gen::X64CodeGenerator;
use crate::codegen::MoiraGenerator;
use crate::x64::x64gen::X64CodeGenerator;
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
}
fn main() -> ExitCode {
    let mut cli = Cli::parse();

    println!("lex: {:?}", cli.lex);
    println!("parse: {:?}", cli.parse);
    println!("codegen: {:?}", cli.codegen);
    println!("validate: {:?}", cli.validate);
    println!("source: {:?}", &cli.source);

    let stub = "mycc_cpp";
    //uuid::Uuid::new_v4()
    let preproc_output = std::env::temp_dir().join(format!("{}.i", stub));

    println!("preprocessing {:?} => {:?}", &cli.source, &preproc_output);
    cpp::preprocess(&cli.source, &preproc_output).unwrap();
    if cli.lex {
        if lex(&preproc_output).is_ok() {
            return ExitCode::SUCCESS;
        };
        return ExitCode::FAILURE;
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
    match build(&preproc_output, &cli.source) {
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

fn lex(path: &Path) -> Result<bool> {
    let mut lexer = lexer::Lexer::new(path);
    loop {
        let token = lexer.next_token()?;
        println!("{:?}", token);
        if token == lexer::Token::Eof {
            break;
        }
    }
    Ok(true)
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
            let mut gen = X64MoiraGenerator::new();
            let moira_ast = gen.generate_moira(tacky)?;
            moira_ast.dump();
            // let asm_ast = moira::AsmGenerator::new().generate(&tacky);
            // asm_ast.dump();

            // let stub = "mycc_cpp";
            // let gen_output = std::env::temp_dir().join(format!("{}.s", stub));

            // let mut gen = X64CodeGenerator::new();
            //    gen.generate(&tacky, &gen_output)?;
        }
        Err(e) => {
            println!("Error: {:?}", e);
            return Err(e);
        }
    }
    //tacky.dump();
    Ok(())
}

fn build(source: &Path, output: &Path) -> Result<()> {
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
            crate::cpp::assemble_link(&gen_output, output)?;
        }
        Err(e) => {
            println!("Error: {:?}", e);
            return Err(e);
        }
    }
    //tacky.dump();
    Ok(())
}
