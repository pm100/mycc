use anyhow::Result;

use crate::{
    tacky::{StaticInit, SymbolType},
    x64::moira_inst::Instruction,
};

#[derive(Debug, Clone)]
pub struct MoiraProgram<TInst> {
    pub functions: Vec<Function<TInst>>,
    pub top_vars: Vec<StaticVariable>,
    current_function: usize,
}
#[derive(Debug, Clone)]
pub struct Function<TInst> {
    pub name: String,
    pub instructions: Vec<TInst>,
    pub global: bool,
}
#[derive(Debug, Clone)]
pub struct StaticVariable {
    pub name: String,
    pub value: StaticInit,
    pub global: bool,
    pub external: bool,
    pub stype: SymbolType,
}
impl<TInst> Default for MoiraProgram<TInst>
where
    TInst: std::fmt::Debug + Clone,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<TInst> MoiraProgram<TInst>
where
    TInst: std::fmt::Debug + Clone,
{
    pub fn new() -> Self {
        MoiraProgram {
            functions: Vec::new(),
            top_vars: Vec::new(),
            current_function: 0,
        }
    }

    pub fn gen_function(&mut self, func: &crate::tacky::Function) -> Result<()> {
        self.functions.push(Function {
            name: func.name.to_string(),
            instructions: Vec::new(),
            global: func.global,
        });
        self.current_function = self.functions.len() - 1;
        Ok(())
    }

    pub(crate) fn add_instruction(&mut self, instruction: TInst) {
        //  if let Instruction::Mov(a, o1, o2) = instruction {}
        self.functions[self.current_function]
            .instructions
            .push(instruction);
    }

    pub fn dump(&self) {
        println!("Dumping MoiraProgram");
        for top_var in self.top_vars.iter() {
            println!(
                "TopVar: {} = {:?} Ext {}",
                top_var.name, top_var.value, top_var.external
            );
        }
        for function in self.functions.iter() {
            println!("Function: {}", function.name);
            for instruction in function.instructions.iter() {
                println!("  {:?}", instruction);
            }
        }
    }
}
