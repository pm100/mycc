pub mod codegen;
pub mod cpp;
pub mod declarator;
pub mod expr;
pub mod lexer;
pub mod optimizer;
pub mod parser;
pub mod parser_utils;
pub mod symbols;
pub mod tacky;
pub mod x64;

use anyhow::Result;
use clap::Parser;
use codegen::BackEnd;
use log::{info, LevelFilter};
use simplelog::{CombinedLogger, Config, WriteLogger};
use std::env;
use std::fs::File;
use std::path::{Path, PathBuf};
use std::process::ExitCode;
use x64::moiragen::X64BackEnd;

use crate::optimizer::optimize::{OptimizeFlags, Optimizer};

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
    #[arg(short, long)]
    verbose: bool,
    #[arg(short = 's')]
    assembler: bool,
    #[arg(long)]
    fold_constants: bool,
    #[arg(long)]
    propagate_copies: bool,
    #[arg(long)]
    eliminate_unreachable_code: bool,

    #[arg(long)]
    eliminate_dead_stores: bool,
    #[arg(long)]
    optimize: bool,
}
fn main() -> ExitCode {
    let _ = CombinedLogger::init(vec![
        //     TermLogger::new(LevelFilter::Info, Config::default(), TerminalMode::Mixed, ColorChoice::Auto),
        WriteLogger::new(
            LevelFilter::Info,
            Config::default(),
            File::create("my_rust_bin.log").unwrap(),
        ),
    ]);
    let mut cli = Cli::parse();
    let optimize_flags = if cli.optimize {
        OptimizeFlags::CONSTANT_FOLD
            | OptimizeFlags::DEAD_STORE
            | OptimizeFlags::COPY_PROP
            | OptimizeFlags::UNREACHABLE
    } else {
        let mut flags = OptimizeFlags::empty();
        if cli.fold_constants {
            flags |= OptimizeFlags::CONSTANT_FOLD;
        }
        if cli.propagate_copies {
            flags |= OptimizeFlags::COPY_PROP;
        }
        if cli.eliminate_unreachable_code {
            flags |= OptimizeFlags::UNREACHABLE;
        }
        if cli.eliminate_dead_stores {
            flags |= OptimizeFlags::DEAD_STORE;
        }
        flags
    };
    if env::var("USE_MSVC").is_ok() {
        let source = cli.source.display().to_string().replace("/", "\\");
        let source = PathBuf::from(source);
        println!("source: {:?}", &source);
        let output = if cli.compile_only {
            source.with_extension("obj")
        } else {
            source.with_extension("exe")
        };

        match cpp::assemble_link_msvc(&source, &output, cli.compile_only) {
            Ok(()) => {
                return ExitCode::SUCCESS;
            }
            Err(e) => {
                eprintln!("build error {:?}", e);
                return ExitCode::FAILURE;
            }
        }
    }

    let stub = "mycc_cpp";

    let preproc_output = std::env::temp_dir().join(format!("{}.i", stub));

    info!("preprocessing {:?} => {:?}", &cli.source, &preproc_output);
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
        match parse(&preproc_output, &cli.source) {
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
        if codegen(&preproc_output, &cli.source).is_ok() {
            return ExitCode::SUCCESS;
        };
        return ExitCode::FAILURE;
    }
    cli.source.set_extension("exe");
    match build(
        &preproc_output,
        &cli.source,
        &cli.source,
        cli.compile_only,
        cli.assembler,
        optimize_flags,
    ) {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("build error {:?}", e);
            ExitCode::FAILURE
        }
    }
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

fn parse(path: &Path, real_source: &Path) -> Result<()> {
    let mut parser = parser::Parser::new(path, real_source);
    let _ = parser.parse()?;
    Ok(())
}

fn codegen(path: &Path, real_source: &Path) -> Result<()> {
    let mut parser = parser::Parser::new(path, real_source);
    let tackyx = parser.parse();
    match tackyx {
        Ok(tacky) => {
            tacky.dump();
            let mut backend = X64BackEnd::new();
            let stub = "mycc_cpp";
            // let gen_output = std::env::temp_dir().join(format!("{}.s", stub));
            let gen_output = PathBuf::from(format!("{}.asm", stub));
            backend.compile(tacky, &gen_output)?;
        }
        Err(e) => {
            println!("Error: {:?}", e);
            return Err(e);
        }
    }
    //tacky.dump();
    Ok(())
}

fn build(
    source: &Path,
    output: &Path,
    real_source: &Path,
    compile_only: bool,
    assembly: bool,
    optimize_flags: OptimizeFlags,
) -> Result<()> {
    let mut parser = parser::Parser::new(source, real_source);
    parser.parse()?;

    parser.tacky.dump();
    if !optimize_flags.is_empty() {
        Optimizer::optimize(&optimize_flags, &mut parser.tacky)?;
    }

    let mut backend = X64BackEnd::new();
    let stub = "mycc_cpp";
    // let gen_output = std::env::temp_dir().join(format!("{}.s", stub));
    let gen_output = if assembly {
        let mut asm = real_source.to_path_buf();
        if !asm.set_extension("s") {
            return Err(anyhow::anyhow!(
                "Failed to set extension for assembly output"
            ));
        }
        // let x = src_path.parent().join(src_)
        asm
    } else {
        PathBuf::from(format!("{}.asm", stub))
    };
    backend.compile(&parser.tacky, &gen_output)?;
    crate::cpp::assemble_link(&gen_output, output, compile_only | assembly)?;

    //tacky.dump();
    Ok(())
}
