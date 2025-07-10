use crate::{optimizer::optimize::Optimizer, tacky::Instruction};
use anyhow::Result;
impl Optimizer {
    pub fn eliminate_unreachable_code(&mut self) -> Result<bool> {
        let mut modified = false;
        {
            // unrerachable block elimination
            for block_id in 1..self.graph.blocks.len() {
                if !self.graph.blocks[block_id].alive {
                    // If the block is not alive, we skip it
                    continue;
                }
                let mut keep = false;

                for idx in 0..self.graph.blocks[block_id].predecessors.len() {
                    let pred_id = self.graph.blocks[block_id].predecessors[idx];

                    if self.graph.blocks[pred_id].alive {
                        // If the predecessor is alive, we keep this block
                        keep = true;
                        break;
                    }
                }
                if !keep {
                    println!("Block {} is not reachable", block_id);
                    self.kill_block(block_id);
                    modified = true;
                };
            }
            println!("Unreachable block elimination complete");
            self.print_graph();
            let live_list = self
                .graph
                .blocks
                .iter()
                .filter(|block| block.alive)
                .map(|block| block.id)
                .collect::<Vec<_>>();
            for idx in 0..live_list.len() - 1 {
                let block_id = live_list[idx];

                let last = self.graph.blocks[block_id].instructions.last().unwrap();
                if matches!(
                    last.0,
                    Instruction::Jump(_)
                        | Instruction::JumpIfZero(_, _)
                        | Instruction::JumpIfNotZero(_, _)
                ) {
                    let mut keep = false;
                    let next = live_list[idx + 1];

                    for succ in &self.graph.blocks[block_id].successors {
                        if *succ != next {
                            keep = true;
                            break;
                        }
                    }
                    if !keep {
                        // If the next block is not a successor, we remove the jump
                        println!("Removing jump from block {} to block {}", block_id, next);
                        modified = true;
                        self.graph.blocks[block_id].instructions.pop();
                    }
                }
                if self.graph.blocks[block_id].instructions.is_empty() {
                    // If the block has no instructions, we remove it
                    println!("Removing empty block {}", block_id);
                    modified = true;
                    self.kill_block(block_id);
                }
            }
            println!("dead jump removel complete");
            self.print_graph();

            // dead label removal

            /*
            a different algorithm from the book. Loop over the entire function and
            build a list of all referenced labels
            Then loop over each block removing labels that are not referenced
             */
            let live_list = self
                .graph
                .blocks
                .iter()
                .filter(|block| block.alive)
                .map(|block| block.id)
                .collect::<Vec<_>>();
            let mut jump_list = Vec::new();
            for block_id in &live_list {
                for bidx in 0..self.graph.blocks[*block_id].instructions.len() {
                    match self.graph.blocks[*block_id].instructions[bidx].0 {
                        Instruction::Jump(ref label) => {
                            jump_list.push(label.clone());
                        }
                        Instruction::JumpIfZero(_, ref label) => {
                            jump_list.push(label.clone());
                        }
                        Instruction::JumpIfNotZero(_, ref label) => {
                            jump_list.push(label.clone());
                        }
                        _ => {}
                    }
                }
            }
            for block_id in &live_list {
                if let Some(ref label) = self.graph.blocks[*block_id].label {
                    if !jump_list.contains(label) {
                        // If the label is not used in a jump instruction, we remove it
                        println!("Removing label {} from block {}", label, block_id);
                        modified = true;
                        self.graph.blocks[*block_id].label = None;
                        self.graph.blocks[*block_id]
                            .instructions
                            .retain(|ins| !matches!(ins.0, Instruction::Label(_)));
                    }
                }
            }
        }
        println!("Unreachable code elimination complete");
        self.print_graph();

        Ok(modified)
    }
}
