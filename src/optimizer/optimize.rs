use anyhow::Result;
use bitflags::bitflags;

use std::collections::HashMap;

use crate::{
    optimizer::graph::CodeGraph,
    tacky::{Function, TackyProgram},
};

bitflags! {
    pub struct OptimizeFlags: u8 {
        const CONSTANT_FOLD = 0b00000001;
        const DEAD_STORE = 0b00000010;
        const COPY_PROP = 0b00000100;
        const UNREACHABLE = 0b00001000;
    }
}

pub struct Optimizer {
    pub graph: CodeGraph,
}
impl Optimizer {
    pub fn optimize(flags: &OptimizeFlags, program: &mut TackyProgram) -> Result<()> {
        let mut optimizer = Self {
            graph: CodeGraph {
                blocks: Vec::new(),
                label_map: HashMap::new(),
            },
        };

        for idx in 0..program.functions.len() {
            let function = &mut program.functions[idx];
            optimizer.optimize_function(flags, function)?;
            //  println!("Function graph for {}: {:?}", function.name, code_graph);
        }

        program.dump();
        //   let x = tacky.clone();
        Ok(())
    }
    fn optimize_function(&mut self, flags: &OptimizeFlags, function: &mut Function) -> Result<()> {
        // Apply optimizations to the function
        // For now, just print the function name
        let mut pass = 1;
        println!("Optimizing function: {}", function.name);

        loop {
            println!("Optimization pass {}", pass);
            pass += 1;
            let mut changed_tacky = false;
            if flags.contains(OptimizeFlags::CONSTANT_FOLD) {
                let changed = self.fold_constants(function)?;

                changed_tacky |= changed;
            }
            self.graph = self.build_function_graph(function);
            // if flags.contains(OptimizeFlags::DEAD_STORE) {
            //     change_count += self.eliminate_dead_stores()?
            // }
            // if flags.contains(OptimizeFlags::COPY_PROP) {
            //     change_count += self.propagate_copies(
            // }
            if flags.contains(OptimizeFlags::UNREACHABLE) {
                changed_tacky |= self.eliminate_unreachable_code()?;
            }
            if !changed_tacky {
                break;
            }
            function.instructions = Self::emit_code(&self.graph);
        }

        Ok(())
    }
}
