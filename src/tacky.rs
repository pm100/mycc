pub struct TackyProgram {
    pub function: Function,
}

pub enum Instruction {
    Return(Value),
    Unary(UnaryOperator, Value, Value),
}
pub enum UnaryOperator {
    Negate,
    Complement,
}

pub enum Value {
    Int(i32),
    Variable(String),
}

pub struct Function {
    pub name: String,
    pub instructions: Vec<Instruction>,
}
