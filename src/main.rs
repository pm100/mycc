pub mod compiler;
pub mod cpp;
pub mod tacky;
use std::path::PathBuf;
use std::process::ExitCode;

use clap::Parser;
use compiler::Compiler;

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
}
fn main() -> ExitCode {
    let cli = Cli::parse();

    println!("lex: {:?}", cli.lex);
    println!("parse: {:?}", cli.parse);
    println!("codegen: {:?}", cli.codegen);
    println!("source: {:?}", cli.source);

    let input = std::fs::read_to_string(&cli.source).unwrap();
    let mut compiler = Compiler::new();
    let parsed = compiler.run(&input);
    if parsed.is_err() {
        eprintln!("{}", parsed.err().unwrap());
        return ExitCode::FAILURE;
    }
    if cli.lex {
        println!("lexing");
        //return std::process::ExitCode::SUCCESS;
        return 0u8.into();
    }

    if cli.parse {
        println!("parsing");
        return ExitCode::SUCCESS;
    }

    if cli.codegen {
        println!("codegening");
        return ExitCode::SUCCESS;
    }

    println!("compiling");
    return ExitCode::SUCCESS;
}
