use std::path::Path;

use anyhow::Result;

use crate::{moira::MoiraProgram, tacky::TackyProgram};

pub trait MoiraGenerator {
    type InstructionType;
    // convert tacky to asm_ast
    fn generate_moira(
        &mut self,
        program: &TackyProgram,
        // moira: &mut MoiraProgram<Self::InstructionType>,
    ) -> Result<&MoiraProgram<Self::InstructionType>>;
}
pub trait MoiraCompiler {
    type InstructionType;
    fn generate_asm(
        &mut self,
        moira: &MoiraProgram<Self::InstructionType>,
        file: &Path,
    ) -> Result<()>;
}

pub trait AssembleLink {
    // assemble and link
    fn build(&mut self, source: &Path, output: &Path) -> Result<()>;
}
