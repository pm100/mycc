use crate::tacky::{Function, Instruction, Value};
use crate::{codegen::CodeGenerator, tacky::TackyProgram};
use anyhow::{bail, Result};
use std::io::Write;
use std::{io::BufWriter, path::Path};
pub struct X64CodeGenerator {}

impl CodeGenerator for X64CodeGenerator {
    fn generate(&mut self, program: &TackyProgram, file: &Path) -> Result<()> {
        let mut writer = BufWriter::new(std::fs::File::create(file)?);
        program.dump();
        // writeln!(writer, "INCLUDELIB LIBCMT")?;
        // writeln!(writer, "_TEXT   SEGMENT")?;
        // writeln!(writer, "PUBLIC main")?;
        // writeln!(writer, "main PROC")?;
        // writeln!(writer, "        mov eax, 0")?;
        // writeln!(writer, "        ret")?;
        // writeln!(writer, "main    ENDP")?;
        // writeln!(writer, "_TEXT   ENDS")?;
        // writeln!(writer, "END")?;
        self.gen_program(program, &mut writer)?;
        Ok(())
    }

    fn build(&mut self, source: &Path, output: &Path) -> Result<()> {
        crate::cpp::assemble_link(source, output)
    }
}

impl X64CodeGenerator {
    pub fn new() -> Self {
        Self {}
    }

    fn gen_program(
        &mut self,
        program: &TackyProgram,
        writer: &mut BufWriter<std::fs::File>,
    ) -> Result<()> {
        writeln!(writer, "INCLUDELIB LIBCMT")?;
        writeln!(writer, "_TEXT   SEGMENT")?;

        for function in &program.functions {
            self.gen_function(function, writer)?;
        }

        writeln!(writer, "_TEXT   ENDS")?;
        writeln!(writer, "END")?;
        Ok(())
    }
    fn gen_function(
        &mut self,
        function: &Function,
        writer: &mut BufWriter<std::fs::File>,
    ) -> Result<()> {
        writeln!(writer, "PUBLIC {}", function.name)?;
        writeln!(writer, "{} PROC", function.name)?;
        for instruction in &function.instructions {
            self.gen_instruction(instruction, writer)?;
        }

        writeln!(writer, "{} ENDP", function.name)?;

        Ok(())
    }
    fn gen_instruction(
        &mut self,
        instruction: &Instruction,
        writer: &mut BufWriter<std::fs::File>,
    ) -> Result<()> {
        match instruction {
            Instruction::Return(value) => {
                match value {
                    Value::Int(value) => {
                        writeln!(writer, "        mov eax,{}", value)?;
                    }
                    _ => bail!("Unsupported value"),
                    //     Value::Variable(name) => {
                    //         writeln!(writer, "        mov eax,{}", name)?;
                    //         writeln!(writer, "        ret")?;
                    //     }
                }
                writeln!(writer, "        ret")?;
            }
            _ => bail!("Unsupported instruction"),
        }
        Ok(())
    }
}
