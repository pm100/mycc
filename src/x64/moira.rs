use std::collections::HashMap;

use anyhow::Result;

use crate::{
    symbols::SymbolType,
    tacky::{StaticInit, Value},
};

use super::moira_inst::Instruction;

#[derive(Debug, Clone)]
pub struct MoiraProgram {
    pub functions: Vec<Function>,
    pub top_vars: Vec<StaticVariable>,
    pub static_constants: HashMap<String, StaticConstant>,
    current_function: usize,
}
#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub instructions: Vec<Instruction>,
    pub global: bool,
}
#[derive(Debug, Clone)]
pub struct StaticVariable {
    pub name: String,
    pub values: Vec<Value>,
    pub global: bool,
    pub external: bool,
    pub stype: SymbolType,
}
#[derive(Debug, Clone)]
pub struct StaticConstant {
    pub name: String,
    pub align: u8,
    pub value: StaticInit,
}

impl MoiraProgram {
    pub fn new() -> Self {
        MoiraProgram {
            functions: Vec::new(),
            top_vars: Vec::new(),
            static_constants: HashMap::new(),
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
                top_var.name, top_var.values, top_var.external
            );
        }
        for static_constant in self.static_constants.iter() {
            println!(
                "StaticConstant: {} {} = {:?} Align {}",
                static_constant.1.name,
                static_constant.0,
                static_constant.1.value,
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
