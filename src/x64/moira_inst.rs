#[derive(Debug, Clone)]
pub enum Instruction {
    Ret,
    Mov(Operand, Operand),
    Unary(UnaryOperator, Operand),
    AllocateStack(i32),
}
#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Neg,
    Not,
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
    R10,
}
