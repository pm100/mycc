use std::{
    path::{Path, PathBuf},
    process::{Command, Stdio},
};

//type Result<T> = std::result::Result<T, Error>;
use anyhow::{anyhow, Result};

lazy_static::lazy_static! {
    static ref COMPILER: Option<Compiler> = match Compiler::find() {
        Some(compiler) => Some(compiler),
        None => {
            eprintln!("Failed to find C preprocessor on this system!!!");
            None
        }
    };
}

pub enum CppError {
    Error(Box<dyn std::error::Error>),
    String(String),
}
impl std::fmt::Debug for CppError {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Error(arg0) => std::fmt::Debug::fmt(arg0, f),
            Self::String(arg0) => std::fmt::Display::fmt(arg0, f),
        }
    }
}
impl std::fmt::Display for CppError {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Error(arg0) => std::fmt::Display::fmt(arg0, f),
            Self::String(arg0) => std::fmt::Display::fmt(arg0, f),
        }
    }
}
impl<E: std::error::Error + 'static> From<E> for CppError {
    #[inline]
    fn from(value: E) -> Self {
        Self::Error(Box::new(value) as _)
    }
}

fn capture(process: std::process::Child) -> Result<Vec<u8>> {
    let output = process.wait_with_output()?;
    // return Err(anyhow!(CppError::String("foo".to_string())));
    if !output.status.success() {
        eprintln!(
            "Error code: {}\n\n{}{}",
            output.status.code().unwrap_or(-1),
            String::from_utf8_lossy(&output.stderr),
            String::from_utf8_lossy(&output.stdout)
        );
        return Err(anyhow!(format!(
            "Error code: {}\n\n{}",
            output.status.code().unwrap_or(-1),
            String::from_utf8_lossy(&output.stderr)
        )));
    }

    Ok(output.stdout)
}

pub enum Compiler {
    GnuClang(PathBuf),
    Msvc(PathBuf),
}
impl Compiler {
    fn find() -> Option<Self> {
        std::env::set_var("OPT_LEVEL", "0");
        std::env::set_var("TARGET", env!("TARGET"));
        std::env::set_var("HOST", env!("HOST"));

        let tool = match cc::Build::new().cargo_metadata(false).try_get_compiler() {
            Ok(tool) => tool,
            Err(_) => return None,
        };

        if tool.is_like_clang() || tool.is_like_gnu() {
            Some(Self::GnuClang(tool.path().to_path_buf()))
        } else if tool.is_like_msvc() {
            Some(Self::Msvc(tool.path().to_path_buf()))
        } else {
            None
        }
    }

    fn preprocess(&self, source: &Path, dest: &Path) -> Result<()> {
        match self {
            Self::GnuClang(path) => Self::gnu_clang(path, source, dest),
            Self::Msvc(path) => Self::msvc(path, source, dest),
        }
    }

    fn assemble_link(&self, source: &Path, dest: &Path, compile_only: bool) -> Result<()> {
        match self {
            Self::GnuClang(path) => Self::gnu_clang(path, source, dest),
            Self::Msvc(path) => {
                let mut path = path.clone();
                path.set_file_name("ml64.exe");
                Self::msvc_asm(&path, source, dest, compile_only)
            }
        }
    }

    // Arguments are the same for Clang and Gnu gcc
    fn gnu_clang(path: &Path, _source: &Path, _dest: &Path) -> Result<()> {
        let process = Command::new(path)
            .args(["-nostdinc", "-P", "-E", "-x", "c", "-"])
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()?;

        // process.stdin.as_mut().unwrap().write_all(code)?;

        capture(process)?;
        Ok(())
    }

    fn msvc(path: &Path, source: &Path, dest: &Path) -> Result<()> {
        capture({
            Command::new(path)
                .args(["/EP", "/P", format!("/Fi{}", dest.display()).as_str()])
                .arg("/DLONG64=long long")
                .arg(source)
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .spawn()?
        })?;
        Ok(())
    }
    fn msvc_asm(path: &Path, source: &Path, dest: &Path, compile_only: bool) -> Result<()> {
        println!("nasm: {:?} {:?} {:?}", path, source, dest);

        let temp_obj = dest.with_extension("obj").display().to_string();
        capture(
            Command::new("nasm")
                .args(["-f", "win64", "-o"])
                .arg(temp_obj.clone())
                .arg(source)
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .spawn()?,
        )?;
        if compile_only {
            return Ok(());
        }
        let args = vec![
            "/subsystem:console",
            "/entry:main",
            "/defaultlib:ucrt.lib",
            "/defaultlib:msvcrt.lib",
            "/defaultlib:legacy_stdio_definitions.lib",
            "/defaultlib:Kernel32.lib",
            "/defaultlib:Shell32.lib",
            "/nologo",
            "/incremental:no",
        ];
        capture({
            Command::new(path.with_file_name("link.exe"))
                .args(args)
                .arg(temp_obj)
                .arg(format!("/out:{}", dest.display()))
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .spawn()?
        })?;
        Ok(())
    }
}

pub fn preprocess(source: &Path, dest: &Path) -> Result<()> {
    let comp = COMPILER.as_ref().unwrap();
    //  .as_ref()
    comp.preprocess(source, dest)
}

pub fn assemble_link(source: &Path, output: &Path, compile_only: bool) -> Result<()> {
    let comp = COMPILER.as_ref().unwrap();

    comp.assemble_link(source, output, compile_only)
}

pub fn assemble_link_msvc(source: &Path, output: &Path, compile_only: bool) -> Result<()> {
    // let comp = COMPILER.as_ref().unwrap();

    let args = if compile_only {
        vec![
            format!("/Fo{}", output.with_extension("obj").display()),
            "/c".to_string(),
        ]
    } else {
        vec![format!("/Fe{}", output.display())]
    };
    //   println!("msvc_asm: {:?} {:?} {:?} {:?}", path, source, dest, args);
    capture({
        Command::new("cl")
            .args(args)
            .arg(source)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()?
    })?;
    Ok(())
}
