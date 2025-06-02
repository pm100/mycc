use std::collections::HashMap;

use anyhow::Result;

use crate::tacky::{StaticConstant, StaticVariable, Structure};

use super::moira_inst::Instruction;

#[derive(Debug, Clone)]
pub struct MoiraProgram {
    pub functions: Vec<Function>,
    pub top_vars: Vec<StaticVariable>,
    pub static_constants: HashMap<String, StaticConstant>,
    pub structure_defs: HashMap<String, Structure>,
    current_function: usize,
}
#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub instructions: Vec<Instruction>,
    pub global: bool,
}

impl Default for MoiraProgram {
    fn default() -> Self {
        Self::new()
    }
}

impl MoiraProgram {
    pub fn new() -> Self {
        MoiraProgram {
            functions: Vec::new(),
            top_vars: Vec::new(),
            static_constants: HashMap::new(),
            structure_defs: HashMap::new(),
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

    pub(crate) fn add_instruction(&mut self, instruction: Instruction) {
        //  if let Instruction::Mov(a, o1, o2) = instruction {}
        println!("           {:?}", instruction);
        self.functions[self.current_function]
            .instructions
            .push(instruction);
    }

    pub fn dump(&self) {
        println!("Dumping MoiraProgram");
        for top_var in self.top_vars.iter() {
            println!(
                "TopVar: {} = {:?} Ext {}",
                top_var.name, top_var.init, top_var.external
            );
        }
        for static_constant in self.static_constants.iter() {
            println!(
                "StaticConstant: {} {} = {:?} Align {}",
                static_constant.1.name,
                static_constant.0,
                static_constant.1.init,
                static_constant.1.align
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
