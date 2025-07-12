use std::{collections::HashMap, mem};

use crate::{
    optimizer::optimize::Optimizer,
    tacky::{Function, Instruction, StaticVariable, Value},
};
use anyhow::{Ok, Result};
#[derive(Debug, Clone, PartialEq)]
pub struct ReachingCopy {
    src: Value,
    dst: Value,
}
impl Optimizer {
    pub fn propagate_copies(
        &mut self,
        static_vars: &HashMap<String, StaticVariable>,
        function: &mut Function,
    ) -> Result<bool> {
        self.reaching_copies(static_vars)?;
        Ok(false)
    }

    fn transfer_copy(
        &mut self,
        block_id: usize,
        init_copies: &mut Vec<ReachingCopy>,
        static_vars: &HashMap<String, StaticVariable>,
    ) -> Result<()> {
        // Transfer the copy information to the appropriate instruction
        let mut current_copies = mem::take(init_copies);
        for ins_idx in 0..self.graph.blocks[block_id].instructions.len() {
            let inst = &mut self.graph.blocks[block_id].instructions[ins_idx];
            inst.1.reaching_copies = current_copies.clone();
            match inst.0 {
                Instruction::Copy(ref src, ref dst) => {
                    if inst
                        .1
                        .reaching_copies
                        .iter()
                        .find(|c| c.dst == *dst && c.src == *src)
                        .is_some()
                    {
                        // If we already have a copy for this destination, we skip it
                        continue;
                    }
                    current_copies.retain(|c| c.dst != *dst && c.src != *src);
                    current_copies.push(ReachingCopy {
                        src: src.clone(),
                        dst: dst.clone(),
                    });
                }
                Instruction::FunCall(_, _, ref ret) => {
                    let temp_copies = mem::take(&mut current_copies);
                    let mut kill = false;
                    for copy in temp_copies {
                        if Self::is_static_variable(&copy.dst, static_vars)
                            || Self::is_static_variable(&copy.src, static_vars)
                        {
                            // If we have a copy for the return value, we remove it
                            kill = true;
                            //current_copies.retain(|c| *c != copy);
                        }
                        if let Some(ref ret) = ret {
                            if copy.src == *ret || copy.dst == *ret {
                                // If the source is the return value, we remove it
                                kill = true;
                                //current_copies.remove(copy_idx);
                            }
                        }
                        if !kill {
                            current_copies.push(copy);
                        }
                    }
                }
                Instruction::Binary(_, _, _, ref dst) | Instruction::Unary(_, _, ref dst) => {
                    let temp_copies = mem::take(&mut current_copies);
                    let mut kill = false;
                    for copy in temp_copies {
                        if copy.src == *dst || copy.dst == *dst {
                            // If the source is the return value, we remove it
                            kill = true;
                            //current_copies.remove(copy_idx);
                        }

                        if !kill {
                            current_copies.push(copy);
                        }
                    }
                }

                _ => {}
            }
        }
        self.print_graph();
        self.graph.blocks[block_id].reaching_copies = mem::take(&mut current_copies);
        Ok(())
    }

    fn is_static_variable(value: &Value, static_vars: &HashMap<String, StaticVariable>) -> bool {
        match value {
            Value::Variable(name, _) => static_vars.contains_key(name),
            _ => false,
        }
    }
    fn reaching_copies(&mut self, static_vars: &HashMap<String, StaticVariable>) -> Result<bool> {
        let live_list = self
            .graph
            .blocks
            .iter()
            .filter(|block| block.alive)
            .map(|block| block.id)
            .collect::<Vec<_>>();
        let mut init_copies = Vec::new();
        for block_id in &live_list {
            self.transfer_copy(*block_id, &mut init_copies, static_vars)?;
            init_copies = self.graph.blocks[*block_id].reaching_copies.clone();
        }
        Ok(false)
    }

    fn meet(
        &mut self,
        block_id: usize,
        all_copies: Vec<ReachingCopy>,
    ) -> Result<Vec<ReachingCopy>> {
        for pred_id in &self.graph.blocks[block_id].predecessors {
            match *pred_id {
                0 => {
                    return Ok(vec![]);
                }

                usize::MAX => unreachable!(),
                _ => {
                    let pred_copies = self.graph.blocks[*pred_id].reaching_copies.clone();
                }
            }
        }
        Ok(all_copies)
    }
}
