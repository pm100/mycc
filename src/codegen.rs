use std::path::Path;

use anyhow::Result;

use crate::tacky::TackyProgram;

// pub trait MoiraGenerator {
//     type InstructionType;
//     // convert tacky to asm_ast
//     fn generate_moira(
//         &mut self,
//         program: &TackyProgram,
//     ) -> Result<&MoiraProgram<Self::InstructionType>>;
// }
// pub trait MoiraCompiler {
//     type InstructionType;
//     fn generate_asm(
//         &mut self,
//         moira: &MoiraProgram<Self::InstructionType>,
//         file: &Path,
//     ) -> Result<()>;
// }

pub trait AssembleLink {
    // assemble and link
    fn build(&mut self, source: &Path, output: &Path) -> Result<()>;
}

pub trait BackEnd {
    fn compile(&mut self, tacky: &TackyProgram, output: &Path) -> Result<()>;
}
