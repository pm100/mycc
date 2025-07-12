use std::collections::HashMap;

use crate::{
    optimizer::{optimize::Optimizer, reaching_copies::ReachingCopy},
    tacky::{Function, Instruction},
};
#[derive(Debug, Clone)]
pub struct CodeGraph {
    pub blocks: Vec<Block>,
    pub label_map: HashMap<String, usize>,
}
#[derive(Debug, Clone)]
pub struct Block {
    pub id: usize,
    pub label: Option<String>,
    pub alive: bool,
    pub instructions: Vec<(Instruction, InstructionGraphData)>,
    pub reaching_copies: Vec<ReachingCopy>,
    pub successors: Vec<usize>,
    pub predecessors: Vec<usize>,
}
#[derive(Debug, Clone)]
pub struct InstructionGraphData {
    pub reaching_copies: Vec<ReachingCopy>,
}

impl Optimizer {
    pub fn build_function_graph(&self, function: &mut Function) -> CodeGraph {
        let mut code_graph = CodeGraph {
            blocks: Vec::new(),
            label_map: HashMap::new(),
            //jump_map: HashMap::new(),
        };

        let mut current_block = Block {
            id: 0,
            label: None,
            alive: true,
            instructions: Vec::new(),
            successors: Vec::new(),
            predecessors: Vec::new(),
            reaching_copies: Vec::new(),
        };
        let mut single_label_block = false;
        let mut jump_aliases = Vec::new();
        for instruction in &function.instructions {
            println!(
                "Processing instruction: {:?} in block {} single={}",
                instruction, current_block.id, single_label_block
            );
            match instruction {
                Instruction::Label(label) => {
                    if single_label_block {
                        // If the current block is a single label block, we add it to the graph
                        println!("new alias for label: {}", label);
                        jump_aliases
                            .push((label.clone(), current_block.label.clone().unwrap().clone()));

                        continue;
                    }
                    if !current_block.instructions.is_empty() {
                        code_graph.blocks.push(current_block);
                    }
                    println!(
                        "starting label block: {:?} id={}",
                        instruction,
                        code_graph.blocks.len()
                    );
                    current_block = Block {
                        id: code_graph.blocks.len(),
                        alive: true,
                        label: Some(label.clone()),
                        instructions: vec![(
                            instruction.clone(),
                            InstructionGraphData {
                                reaching_copies: Vec::new(),
                            },
                        )],
                        successors: Vec::new(),
                        predecessors: Vec::new(),
                        reaching_copies: Vec::new(),
                    };
                    single_label_block = true;
                }
                Instruction::Jump(_)
                | Instruction::JumpIfZero(_, _)
                | Instruction::JumpIfNotZero(_, _)
                | Instruction::Return(_) => {
                    current_block.instructions.push((
                        instruction.clone(),
                        InstructionGraphData {
                            reaching_copies: Vec::new(),
                        },
                    ));
                    println!(
                        "jump pushing block: id={} {:?}",
                        current_block.id, instruction
                    );
                    code_graph.blocks.push(current_block.clone());
                    single_label_block = false;
                    println!("starting  block: {}", code_graph.blocks.len());
                    current_block = Block {
                        id: code_graph.blocks.len(),
                        label: None,
                        alive: true,
                        instructions: vec![],
                        successors: Vec::new(),
                        predecessors: Vec::new(),
                        reaching_copies: Vec::new(),
                    };
                }
                _ => {
                    single_label_block = false;
                    current_block.instructions.push((
                        instruction.clone(),
                        InstructionGraphData {
                            reaching_copies: Vec::new(),
                        },
                    ));
                }
            }
        }
        if !current_block.instructions.is_empty() {
            code_graph.blocks.push(current_block);
        }
        for alias in &jump_aliases {
            // We add the jump aliases to the label map
            println!("jump alias: {} -> {}", alias.0, alias.1);
        }
        for block_idx in 0..code_graph.blocks.len() {
            for idx in 0..code_graph.blocks[block_idx].instructions.len() {
                match &mut code_graph.blocks[block_idx].instructions[idx] {
                    (Instruction::Jump(label), meta) => {
                        if let Some((old, new)) =
                            jump_aliases.iter().find(|(alias, _)| alias == label)
                        {
                            println!("Replacing jump if zero alias: {} -> {}", old, new);
                            // If the label is an alias, we add it to the label map
                            code_graph.blocks[block_idx].instructions[idx] =
                                (Instruction::Jump(new.clone()), meta.clone());
                        }
                    }
                    (Instruction::JumpIfZero(cond, label), meta) => {
                        if let Some((old, new)) =
                            jump_aliases.iter().find(|(alias, _)| alias == label)
                        {
                            println!("Replacing jump if zero alias: {} -> {}", old, new);
                            // If the label is an alias, we add it to the label map
                            code_graph.blocks[block_idx].instructions[idx] = (
                                Instruction::JumpIfZero(cond.clone(), new.clone()),
                                meta.clone(),
                            );
                            // code_graph.label_map.insert(new.clone(), block_idx);
                        }
                    }
                    (Instruction::JumpIfNotZero(cond, label), meta) => {
                        if let Some((old, new)) =
                            jump_aliases.iter().find(|(alias, _)| alias == label)
                        {
                            println!("Replacing jump if not zero alias: {} -> {}", old, new);
                            // If the label is an alias, we add it to the label map
                            code_graph.blocks[block_idx].instructions[idx] = (
                                Instruction::JumpIfNotZero(cond.clone(), new.clone()),
                                meta.clone(),
                            );
                            // code_graph.label_map.insert(new.clone(), block_idx);
                        }
                    }
                    _ => {}
                }
            }
        }
        Self::print_any_graph(&code_graph);
        self.add_edges(&mut code_graph);
        Self::print_any_graph(&code_graph);
        code_graph
    }
    fn find_block_by_label(graph: &CodeGraph, label: &String) -> Option<usize> {
        let ret = graph
            .blocks
            .iter()
            .find(|block| block.label == Some(label.clone()))
            .and_then(|b| Some(b.id));
        println!("find_block_by_label: label: {}, ret: {:?}", label, ret);
        ret
    }
    fn add_edge(graph: &mut CodeGraph, from: usize, to: usize) {
        let from_block = &mut graph.blocks[from];
        if !from_block.successors.contains(&to) {
            from_block.successors.push(to);
        }
        if to == usize::MAX {
            return; // No successors for return instructions
        }
        let to_block = &mut graph.blocks[to];
        if !to_block.predecessors.contains(&from) {
            to_block.predecessors.push(from);
        }
    }
    pub fn kill_block(&mut self, block_id: usize) {
        println!("Killing block {}", block_id);
        for succ_idx in 0..self.graph.blocks[block_id].successors.len() {
            let succ = self.graph.blocks[block_id].successors[succ_idx];
            if succ != usize::MAX {
                // If the successor is not a return instruction
                println!("Removing predecessor {} from successor {}", block_id, succ);
                let succ_block = &mut self.graph.blocks[succ];
                succ_block.predecessors.retain(|&pred| pred != block_id);
            }
        }

        let block = &mut self.graph.blocks[block_id];
        block.alive = false;

        block.successors.clear();
    }
    fn add_edges(&self, code_graph: &mut CodeGraph) {
        for idx in 0..code_graph.blocks.len() {
            let (node_id, last_instr) = {
                let node_id = code_graph.blocks[idx].id;
                let last = code_graph.blocks[idx].instructions.last().unwrap().clone();
                (node_id, last)
            };
            let next_id = if node_id == code_graph.blocks.len() - 1 {
                // Last block, no successors
                usize::MAX
            } else {
                node_id + 1
            };
            match last_instr.0 {
                Instruction::Return(_) => {
                    // Return instructions do not have successors
                    Self::add_edge(code_graph, node_id, usize::MAX);
                }
                Instruction::Jump(ref label) => {
                    let target_id = Self::find_block_by_label(code_graph, label).unwrap();

                    Self::add_edge(code_graph, node_id, target_id);
                }
                Instruction::JumpIfZero(_, ref label) => {
                    let target_id = Self::find_block_by_label(code_graph, label).unwrap();
                    Self::add_edge(code_graph, node_id, target_id);
                    Self::add_edge(code_graph, node_id, next_id);
                }
                Instruction::JumpIfNotZero(_, ref label) => {
                    let target_id = Self::find_block_by_label(code_graph, label).unwrap();
                    Self::add_edge(code_graph, node_id, target_id);
                    Self::add_edge(code_graph, node_id, next_id);
                }
                _ => Self::add_edge(code_graph, node_id, next_id),
            }
        }
    }
    pub fn emit_code(code_graph: &CodeGraph) -> Vec<Instruction> {
        let mut instructions = Vec::new();
        for block in &code_graph.blocks {
            if block.alive {
                for (instruction, _) in &block.instructions {
                    instructions.push(instruction.clone());
                }
            }
        }
        instructions
    }
    pub fn print_graph(&self) {
        Self::print_any_graph(&self.graph);
    }
    pub fn print_any_graph(graph: &CodeGraph) {
        println!("Code Graph:");
        println!(
            "Number of blocks: {}",
            graph.blocks.iter().filter(|b| b.alive).count()
        );
        for block in &graph.blocks {
            if block.alive {
                println!("Block ID: {} Label:{:?}", block.id, block.label);
                for (instruction, meta) in &block.instructions {
                    println!("  Instruction: {:?}", instruction);
                    if meta.reaching_copies.len() > 0 {
                        println!("  Reaching Copies: {:?}", meta.reaching_copies);
                    }
                }
                println!("  Successors: {:?}", block.successors);
                println!("  Predecessors: {:?}", block.predecessors);
            }
        }
    }
}
