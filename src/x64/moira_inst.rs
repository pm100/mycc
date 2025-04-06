#[derive(Debug, Clone)]
pub enum Instruction {
    Ret,
    Mov(AssemblyType, Operand, Operand),
    Unary(UnaryOperator, AssemblyType, Operand),
    Binary(BinaryOperator, AssemblyType, Operand, Operand),
    Idiv(AssemblyType, Operand),
    Cdq(AssemblyType),
    Cmp(AssemblyType, Operand, Operand),
    Jmp(String),
    JmpCC(CondCode, String),
    SetCC(CondCode, Operand),
    Label(String),
    Call(String),
    Push(Operand),
    DeallocateStack(i32),
    AllocateStack(i32),
    SignExtend(Operand, Operand),
    //Truncate(Operand, Operand),
}
#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Neg,
    Not,
}
#[derive(Debug, Clone, PartialEq)]
pub enum AssemblyType {
    LongWord,
    QuadWord,
    Byte,
    Word,
}
#[derive(Debug, Clone, PartialEq)]
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
    ImmediateI32(i32),
    ImmediateI64(i64),
    Pseudo(String),
    Stack(i32),
    Data(String),
}
#[derive(Debug, Clone)]
pub enum Register {
    RAX,
    RDX,
    RCX,
    R8,
    R9,
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
