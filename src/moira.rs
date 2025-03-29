use anyhow::Result;

// use crate::tacky::{Function, Instruction, TackyProgram};
// pub struct AsmGenerator {
//     asm_program: AsmProgram,
//     current_function: usize,
// }
// impl AsmGenerator {
//     pub fn new() -> Self {
//         AsmGenerator {
//             asm_program: AsmProgram {
//                 functions: Vec::new(),
//             },
//             current_function: 0,
//         }
//     }

//     pub fn generate(&mut self, program: &TackyProgram) -> Result<&AsmProgram> {
//         for function in &program.functions {
//             self.gen_function(function)?;
//         }

//         Ok(&self.asm_program)
//     }

//     fn gen_function(&mut self, function: &Function) -> Result<()> {
//         let asm_function = AsmFunction {
//             name: function.name.clone(),
//             instructions: Vec::new(),
//         };
//         self.asm_program.functions.push(asm_function);
//         self.current_function = self.asm_program.functions.len() - 1;
//         for instruction in &function.instructions {
//             self.gen_instruction(instruction)?;
//         }
//         Ok(())
//     }

//     fn gen_instruction(&mut self, instruction: &Instruction) -> Result<()> {
//         match instruction {
//             Instruction::Return(val) => {
//                 //let asm_val = self.gen_value(val)?;
//                 self.asm_program.functions[self.current_function]
//                     .instructions
//                     .push(AsmInstruction::Ret);
//             }
//             Instruction::Unary(op, src, dest) => {
//                 let asm_src = self.gen_value(src)?;
//                 let asm_dest = self.gen_value(dest)?;
//                 self.asm_program.functions[self.current_function]
//                     .instructions
//                     .push(AsmInstruction::Mov(asm_src, asm_dest));
//             }
//         }
//         Ok(())
//     }

//     pub fn dump(&self) {
//         println!("Dumping AsmProgram");
//         for function in &self.functions {
//             println!("Function: {}", function.name);
//             for instruction in &function.instructions {
//                 match instruction {
//                     AsmInstruction::Ret => {
//                         println!("  Ret");
//                     }
//                     AsmInstruction::Mov(src, dest) => {
//                         println!("  Mov {:?} {:?}", src, dest);
//                     }
//                 }
//             }
//         }
//     }
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
    pub value: i32,
    pub global: bool,
    pub external: bool,
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
        //  let mut functions = self.functions.borrow_mut();
        self.functions.push(Function {
            name: func.name.to_string(),
            instructions: Vec::new(),
            global: func.global,
        });
        self.current_function = self.functions.len() - 1;
        Ok(())
    }

    pub(crate) fn add_instruction(&mut self, instruction: TInst) {
        // let mut functions = self.functions.borrow_mut();
        self.functions[self.current_function]
            .instructions
            .push(instruction);
    }
    // pub fn replace_instruction(&self, instructions: Vec<TInst>, index: usize) {
    //     //let mut functions = self.functions.borrow_mut();
    //     //let mut func_inst = self.functions[self.current_function].instructions;
    //     println!(
    //         "Replacing instruction at index: {} {:?}",
    //         index,
    //         self.functions[self.current_function]
    //             .instructions
    //             .borrow_mut()[index]
    //     );
    //     self.functions[self.current_function]
    //         .instructions
    //         .borrow_mut()[index] = instructions[0].clone();
    //     for instruction in instructions.into_iter().skip(1) {
    //         self.functions[self.current_function]
    //             .instructions
    //             .borrow_mut()
    //             .insert(index, instruction);
    //     }
    // }
    pub fn dump(&self) {
        println!("Dumping MoiraProgram");
        for top_var in self.top_vars.iter() {
            println!(
                "TopVar: {} = {} Ext {}",
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
