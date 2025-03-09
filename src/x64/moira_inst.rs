#[derive(Debug, Clone)]
pub enum Instruction {
    Ret,
    Mov(Operand, Operand),
    Unary(UnaryOperator, Operand),
    Binary(BinaryOperator, Operand, Operand),
    Idiv(Operand),
    Cdq,
    Cmp(Operand, Operand),
    Jmp(String),
    JmpCC(CondCode, String),
    SetCC(CondCode, Operand),
    Label(String),
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
    BitAnd,
    BitOr,
    BitXor,
    ShiftLeft,
    ShiftRight,
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
    CL,
}
#[derive(Debug, Clone)]
pub enum CondCode {
    E,
    NE,
    G,
    GE,
    L,
    LE,
}
