#[derive(Debug, Clone)]
pub enum Instruction {
    Ret,
    Mov(Operand, Operand),
    Unary(UnaryOperator, Operand),
    Binary(BinaryOperator, Operand, Operand),
    Idiv(Operand),
    Cdq,
    AllocateStack(i32),
}
#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Neg,
    Not,
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mult,
}
#[derive(Debug, Clone)]
pub enum Operand {
    Register(Register),
    Immediate(i32),
    Pseudo(String),
    Stack(i32),
}
#[derive(Debug, Clone)]
pub enum Register {
    AX,
    DX,
    R10,
    R11,
}
