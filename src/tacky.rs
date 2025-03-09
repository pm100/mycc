pub struct TackyProgram {
    pub functions: Vec<Function>,
    current_function: usize,
}
#[derive(Debug)]
pub enum Instruction {
    Return(Value),
    Unary(UnaryOperator, Value, Value),
    Binary(BinaryOperator, Value, Value, Value),
    Copy(Value, Value),
    Jump(String),
    JumpIfZero(Value, String),
    JumpIfNotZero(Value, String),
    Label(String),
}
#[derive(Debug)]

pub enum UnaryOperator {
    Negate,
    Complement,
    LogicalNot,
}
#[derive(Debug)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    BitAnd,
    BitOr,
    BitXor,
    ShiftLeft,
    ShiftRight,

    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}
#[derive(Debug, Clone)]

pub enum Value {
    Int(i32),
    Variable(String),
}
#[derive(Debug)]

pub struct Function {
    pub name: String,
    pub instructions: Vec<Instruction>,
}

impl Default for TackyProgram {
    fn default() -> Self {
        Self::new()
    }
}

impl TackyProgram {
    pub fn new() -> Self {
        TackyProgram {
            functions: Vec::new(),
            current_function: 0,
        }
    }

    pub fn add_function(&mut self, name: &str) {
        self.functions.push(Function {
            name: name.to_string(),
            instructions: vec![],
        });
        self.current_function = self.functions.len() - 1;
    }

    pub(crate) fn add_instruction(&mut self, instruction: Instruction) {
        self.functions[self.current_function]
            .instructions
            .push(instruction);
    }

    pub fn dump(&self) {
        println!("Dumping TackyProgram");
        for function in &self.functions {
            println!("Function: {}", function.name);
            for instruction in &function.instructions {
                match instruction {
                    Instruction::Return(val) => {
                        println!("  Return {:?}", val);
                    }
                    Instruction::Unary(op, src, dest) => {
                        println!("  Unary {:?} {:?} {:?}", op, src, dest);
                    }
                    Instruction::Binary(op, src1, src2, dest) => {
                        println!("  Binary {:?} {:?} {:?} {:?}", op, src1, src2, dest);
                    }
                    Instruction::Copy(src, dest) => {
                        println!("  Copy {:?} {:?}", src, dest);
                    }
                    Instruction::Jump(label) => {
                        println!("  Jump {:?}", label);
                    }
                    Instruction::JumpIfZero(val, label) => {
                        println!("  JumpIfZero {:?} {:?}", val, label);
                    }
                    Instruction::JumpIfNotZero(val, label) => {
                        println!("  JumpIfNotZero {:?} {:?}", val, label);
                    }
                    Instruction::Label(label) => {
                        println!("  Label {:?}", label);
                    }
                }
            }
        }
    }
}
