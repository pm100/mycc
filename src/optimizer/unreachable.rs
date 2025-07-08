use core::hash;
use std::collections::HashMap;

use crate::{
    optimizer::optimize::Optimizer,
    tacky::{Function, Instruction},
};
use anyhow::Result;
struct User {
    sname: String,
}
impl Optimizer {
    pub fn eliminate_unreachable_code(&mut self) -> Result<bool> {
        let mut modified = false;
        //  self.print_graph();
        {
            // unrerachable code elimination
            for block_id in 1..self.graph.blocks.len() {
                if !self.graph.blocks[block_id].alive {
                    // If the block is not alive, we skip it
                    continue;
                }
                let mut keep = false;
                //self.graph.nodes[block_id].alive = false;
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
            println!("Unreachable code elimination complete");
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
                    println!("Block {} has a jump instruction: {:?}", block_id, last.0);
                    let mut keep = false;
                    let next = live_list[idx + 1];
                    println!("Checking successors of block {}: ", next);
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
                        // self.graph.blocks[block_id]
                        //     .successors
                        //     .retain(|&s| s != next);
                        // self.graph.blocks[next]
                        //     .predecessors
                        //     .retain(|&p| p != block_id);
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
            let live_list = self
                .graph
                .blocks
                .iter()
                .filter(|block| block.alive)
                .map(|block| block.id)
                .collect::<Vec<_>>();
            let mut jump_list = Vec::new();
            for idx in 0..live_list.len() {
                let block_id = live_list[idx];
                for bidx in 0..self.graph.blocks[block_id].instructions.len() {
                    match self.graph.blocks[block_id].instructions[bidx].0 {
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
            for idx in 0..live_list.len() {
                let block_id = live_list[idx];
                if let Some(ref label) = self.graph.blocks[block_id].label {
                    println!("Block {} has a label instruction: {:?}", block_id, label);
                    if jump_list.contains(label) {
                        // If the label is used in a jump instruction, we keep it
                        println!("Keeping label {} in block {}", label, block_id);
                    } else {
                        // If the label is not used in a jump instruction, we remove it
                        println!("Removing label {} from block {}", label, block_id);
                        modified = true;
                        self.graph.blocks[block_id].label = None;
                        self.graph.blocks[block_id]
                            .instructions
                            .retain(|ins| !matches!(ins.0, Instruction::Label(_)));
                    }
                }
            }
        }
        println!("Unreachable code elimination complete");
        self.print_graph();
        // Ok(modified);

        //         }
        //         1 => {
        //             let prior = live_list[idx - 1];
        //             let first = self.graph.blocks[block_id].predecessors[0];
        //             println!(
        //                 "Block {} has one predecessor: {} prior {}",
        //                 block_id, self.graph.blocks[first].id, prior
        //             );
        //             first != prior
        //         }
        //         _ => {
        //             // If the block has multiple predecessors, we keep the label
        //             true
        //         }
        //     };
        //     // if !keep {
        //     //     // If the next block is not a successor, we remove the jump
        //     //     println!("Removing label from block {} to block {}", block_id, label);
        //     //     modified = true;
        //     //     self.graph.blocks[block_id].instructions.remove(0);
        //     //     self.graph.blocks[block_id].label = None;
        //     // }
        //     if self.graph.blocks[block_id].instructions.is_empty() {
        //         // If the block has no instructions, we remove it
        //         println!("Removing empty block {}", block_id);
        //         modified = true;
        //         self.kill_block(block_id);
        //     }
        // }

        // for idx in 0..live_list.len() - 1 {
        //     let block_id = live_list[idx];
        //     if self.graph.nodes[block_id].instructions.len() == 1 {
        //         if let (Instruction::Label(ref label), _) =
        //             self.graph.nodes[block_id].instructions.last().unwrap()
        //         {
        //             if let Some(ref alias) = self.graph.nodes[live_list[idx + 1]].label {
        //                 for idx2 in 0..live_list.len(){}
        //             }
        //         }
        //     }
        // }

        println!("Unreachable code elimination complete");
        //self.print_graph();
        Ok(modified)
    }
}
