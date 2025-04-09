use std::collections::HashMap;

use anyhow::Result;

use crate::tacky::{StaticInit, SymbolType};

#[derive(Debug, Clone)]
pub struct MoiraProgram<TInst> {
    pub functions: Vec<Function<TInst>>,
    pub top_vars: Vec<StaticVariable>,
    pub static_constants: HashMap<String, StaticConstant>,
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
#[derive(Debug, Clone)]
pub struct StaticConstant {
    pub name: String,
    pub align: u8,
    pub value: StaticInit,
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
    // pub fn make_static_constant(&mut self, value: f64) -> String {
    //     let strval = format!("{:?}", value).replace('-', "_");
    //     let const_label = if let Some(v) = self.static_constants.get(&strval) {
    //         v.name.clone()
    //     } else {
    //         let const_label = format!("__const_{}", strval);
    //         self.static_constants.insert(
    //             strval.clone(),
    //             StaticConstant {
    //                 name: const_label.clone(),
    //                 value: StaticInit::InitDouble(value.clone()),
    //                 align: 16,
    //             },
    //         );
    //         const_label
    //     };
    //     const_label
    // }
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
