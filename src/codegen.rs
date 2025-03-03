use std::path::Path;

use anyhow::Result;

use crate::tacky::{Instruction, TackyProgram, UnaryOperator, Value};

pub trait CodeGenerator {
    // convert tacky to assembler
    fn generate(&mut self, program: &TackyProgram, file: &Path) -> Result<()>;
    // assemble and link
    fn build(&mut self, source: &Path, output: &Path) -> Result<()>;
}
