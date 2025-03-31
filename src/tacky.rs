use std::collections::HashMap;

pub struct TackyProgram {
    pub functions: Vec<Function>,
    pub static_variables: HashMap<String, StaticVariable>,
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
    FunCall(String, Vec<Value>, Value),
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
#[derive(Debug, Clone, PartialEq)]

pub enum Value {
    Int(i32),
    Variable(String),
}
#[derive(Debug)]

pub struct Function {
    pub name: String,
    pub parameters: Vec<String>,
    pub instructions: Vec<Instruction>,
    pub global: bool,
}
#[derive(Debug)]
pub struct StaticVariable {
    pub name: String,

    pub global: bool,
    pub external: bool,
    pub value: Option<Value>,
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
            static_variables: HashMap::new(),
        }
    }
    pub fn add_static_variable(
        &mut self,
        name: &str,
        value: Option<Value>,
        global: bool,
        external: bool,
    ) -> Option<StaticVariable> {
        self.static_variables.insert(
            name.to_string(),
            StaticVariable {
                name: name.to_string(),
                value,
                global,
                external,
            },
        )
    }
    pub fn add_function(&mut self, name: &str, params: &Vec<String>, global: bool) {
        self.functions.push(Function {
            name: name.to_string(),
            parameters: params.clone(),
            instructions: vec![],
            global,
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
        for static_variable in self.static_variables.values() {
            println!(
                "Static Variable: {} = {:?} Global: {} External: {}",
                static_variable.name,
                static_variable.value,
                static_variable.global,
                static_variable.external
            );
        }
        for function in &self.functions {
            println!(
                "Function: {} {:?} global:{}",
                function.name, function.parameters, function.global
            );
            for instruction in &function.instructions {
                match instruction {
                    Instruction::Return(val) => {
                        println!("      Return {:?}", val);
                    }
                    Instruction::Unary(op, src, dest) => {
                        println!("      Unary {:?} {:?} {:?}", op, src, dest);
                    }
                    Instruction::Binary(op, src1, src2, dest) => {
                        println!("      Binary {:?} {:?} {:?} {:?}", op, src1, src2, dest);
                    }
                    Instruction::Copy(src, dest) => {
                        println!("      Copy {:?} {:?}", src, dest);
                    }
                    Instruction::Jump(label) => {
                        println!("      Jump {:?}", label);
                    }
                    Instruction::JumpIfZero(val, label) => {
                        println!("      JumpIfZero {:?} {:?}", val, label);
                    }
                    Instruction::JumpIfNotZero(val, label) => {
                        println!("      JumpIfNotZero {:?} {:?}", val, label);
                    }
                    Instruction::Label(label) => {
                        println!("  Label {:?}", label);
                    }
                    Instruction::FunCall(name, args, dest) => {
                        println!("      FunCall {:?} {:?} {:?}", name, args, dest);
                    }
                }
            }
        }
    }
}
