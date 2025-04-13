#[derive(Debug, Clone)]
pub enum Instruction {
    Ret,
    Mov(AssemblyType, Operand, Operand),
    Unary(UnaryOperator, AssemblyType, Operand),
    Binary(BinaryOperator, AssemblyType, Operand, Operand),
    Idiv(AssemblyType, Operand),
    Div(AssemblyType, Operand),
    Cdq(AssemblyType),
    Cmp(AssemblyType, Operand, Operand),
    FCmp(AssemblyType, Operand, Operand),
    Jmp(String),
    JmpCC(CondCode, String),
    SetCC(CondCode, Operand),
    Label(String),
    Call(String),
    Push(Operand),
    DeallocateStack(i32),
    AllocateStack(i32),
    SignExtend(Operand, Operand),
    Cvttsdsi(AssemblyType, Operand, Operand),
    Cvtsi2sd(AssemblyType, Operand, Operand),
    Lea(Operand, Operand),
}
#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Neg,
    Not,
    FNeg,
}
#[derive(Debug, Clone, PartialEq)]
pub enum AssemblyType {
    LongWord,
    QuadWord,
    Double,
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
    ShiftRightArith,
    FAdd,
    FSub,
    FMul,
    FDiv,
}
#[derive(Debug, Clone)]
pub enum Operand {
    Register(Register),
    ImmediateI32(i32),
    ImmediateI64(i64),
    ImmediateU32(u32),
    ImmediateU64(u64),
    ImmediateF64(f64),
    Pseudo(String),
    Memory(Register, i32),
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
    RBP,
    XMM0,
    XMM1,
    XMM2,
    XMM3,
    XMM4,
    XMM5,
    XMM6,
    XMM7,
    XMM14,
    XMM15,
}
#[derive(Debug, Clone)]
pub enum CondCode {
    E,
    NE,
    G,
    GE,
    L,
    LE,
    A,
    AE,
    B,
    BE,
    P,
}
