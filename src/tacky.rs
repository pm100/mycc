use std::collections::HashMap;

use enum_as_inner::EnumAsInner;

use crate::{parser::Parser, symbols::SymbolType, x64::moira_inst::AssemblyType};

pub struct TackyProgram {
    pub functions: Vec<Function>,
    pub static_variables: HashMap<String, StaticVariable>,
    pub static_constants: HashMap<String, StaticConstant>,
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
    SignExtend(Value, Value),
    ZeroExtend(Value, Value),
    Truncate(Value, Value),
    DoubleToInt(Value, Value),
    DoubleToUInt(Value, Value),
    IntToDouble(Value, Value),
    UIntToDouble(Value, Value),
    GetAddress(Value, Value),
    Load(Value, Value),
    Store(Value, Value),
    AddPtr(Value, Value, isize, Value),
    CopyToOffset(Value, Value, usize),
}
#[derive(Debug, PartialEq)]

pub enum UnaryOperator {
    Negate,
    Complement,
    LogicalNot,
}
#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, Clone, PartialEq, EnumAsInner)]

pub enum Value {
    Int32(i32),
    Int64(i64),
    UInt32(u32),
    UInt64(u64),
    Double(f64),
    Char(i8),
    UChar(u8),
    String(String),
    Variable(String, SymbolType),
}
#[derive(Debug, Clone, PartialEq)]
pub enum PendingResult {
    PlainValue(Value),
    Dereference(Value),
}
impl PendingResult {
    pub fn is_pointer(&self) -> bool {
        match self {
            PendingResult::PlainValue(v) => v.is_pointer() || v.is_array(),
            PendingResult::Dereference(v) => v.is_pointer() || v.is_array(),
        }
    }
    pub fn is_array(&self) -> bool {
        match self {
            PendingResult::PlainValue(v) => v.is_array(),
            PendingResult::Dereference(v) => v.is_array(),
        }
    }
    pub fn is_string(&self) -> bool {
        match self {
            PendingResult::PlainValue(v) => matches!(v, Value::String(_)),
            PendingResult::Dereference(v) => matches!(v, Value::String(_)),
        }
    }
}
impl Value {
    pub fn is_pointer(&self) -> bool {
        matches!(self, Value::Variable(_, SymbolType::Pointer(_)))
    }
    pub fn is_array(&self) -> bool {
        matches!(self, Value::Variable(_, SymbolType::Array(_, _)))
    }
    pub fn stype(&self) -> SymbolType {
        Parser::get_type(self)
    }
    pub fn is_constant(&self) -> bool {
        !matches!(self, Value::Variable(_, _))
    }
}
#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub parameters: Vec<(String, SymbolType)>,
    pub return_type: SymbolType,
    pub instructions: Vec<Instruction>,
    pub global: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StaticInit {
    InitI32(i32),
    InitI64(i64),
    InitU32(u32),
    InitU64(u64),
    InitDouble(f64),
    InitString(String, bool),
    InitChar(i8),
    InitUChar(u8),
    PointerInit(String),
    InitNone,
}
#[derive(Debug, Clone)]
pub struct StaticVariable {
    pub name: String,
    pub stype: SymbolType,
    pub global: bool,
    pub external: bool,
    pub init: Vec<StaticInit>,
}
#[derive(Debug, Clone)]
pub struct StaticConstant {
    pub name: String,
    pub stype: SymbolType,
    pub global: bool,
    pub external: bool,
    pub init: Vec<StaticInit>,
    pub align: usize,
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
            static_constants: HashMap::new(),
        }
    }
    pub fn add_static_variable(
        &mut self,
        name: &str,
        values: Vec<StaticInit>,
        global: bool,
        external: bool,
        stype: &SymbolType,
    ) -> Option<StaticVariable> {
        self.static_variables.insert(
            name.to_string(),
            StaticVariable {
                name: name.to_string(),
                stype: stype.clone(),
                init: values,
                global,
                external,
            },
        )
    }
    pub fn add_static_constant(
        &mut self,
        name: &str,
        values: Vec<StaticInit>,
        global: bool,
        external: bool,
        stype: &SymbolType,
    ) -> Option<StaticConstant> {
        self.static_constants.insert(
            name.to_string(),
            StaticConstant {
                name: name.to_string(),
                stype: stype.clone(),
                init: values,
                global,
                external,
                align: 0,
            },
        )
    }
    pub fn add_function(
        &mut self,
        name: &str,
        params: &[(String, SymbolType)],
        global: bool,
        return_type: SymbolType,
    ) {
        self.functions.push(Function {
            name: name.to_string(),
            parameters: params.to_owned(),
            instructions: vec![],
            global,
            return_type,
        });
        self.current_function = self.functions.len() - 1;
    }
    fn get_assembly_type(value: &Value) -> AssemblyType {
        match value {
            Value::Int32(_) => AssemblyType::LongWord,
            Value::Int64(_) => AssemblyType::QuadWord,
            Value::UInt32(_) => AssemblyType::LongWord,
            Value::UInt64(_) => AssemblyType::QuadWord,
            Value::Double(_) => AssemblyType::QuadWord,
            Value::Char(_) => AssemblyType::Byte,
            Value::UChar(_) => AssemblyType::Byte,
            Value::String(_) => todo!(),
            Value::Variable(_, stype) => match stype {
                SymbolType::Int32 | SymbolType::UInt32 => AssemblyType::LongWord,
                SymbolType::Int64 | SymbolType::UInt64 => AssemblyType::QuadWord,
                SymbolType::Double => AssemblyType::QuadWord,
                SymbolType::Char => AssemblyType::Byte,
                SymbolType::SChar => AssemblyType::Byte,
                SymbolType::UChar => AssemblyType::Byte,
                SymbolType::Function(_, _) => AssemblyType::QuadWord, // TODO
                SymbolType::Pointer(_) => AssemblyType::QuadWord,
                SymbolType::Array(_, _) => todo!(), // TODO
            },
        }
    }
    pub(crate) fn add_instruction(&mut self, instruction: Instruction) {
        if let Instruction::Copy(v1, v2) = &instruction {
            assert!(
                Self::get_assembly_type(v1) == Self::get_assembly_type(v2),
                "Copy instruction types do not match: {:?} {:?}",
                v1,
                v2
            );
        }
        self.functions[self.current_function]
            .instructions
            .push(instruction);
    }

    pub fn dump(&self) {
        println!("Dumping TackyProgram");
        for static_variable in self.static_variables.values() {
            println!(
                "Static Variable: {} = {:?} Global: {} External: {} Stype: {:?}",
                static_variable.name,
                static_variable.init,
                static_variable.global,
                static_variable.external,
                static_variable.stype,
            );
        }
        for static_constant in self.static_constants.values() {
            println!(
                "Static Constant: {} = {:?} Global: {} External: {} Stype: {:?}",
                static_constant.name,
                static_constant.init,
                static_constant.global,
                static_constant.external,
                static_constant.stype,
            );
        }
        for function in &self.functions {
            println!(
                "Function: {:?} {}({:?}) global:{}",
                function.return_type, function.name, function.parameters, function.global
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

                    Instruction::SignExtend(src, dest) => {
                        println!("      SignExtend {:?} {:?}", src, dest);
                    }
                    Instruction::Truncate(src, dest) => {
                        println!("      Truncate {:?} {:?}", src, dest);
                    }
                    Instruction::ZeroExtend(src, dest) => {
                        println!("      ZeroExtend {:?} {:?}", src, dest);
                    }
                    Instruction::DoubleToInt(src, dest) => {
                        println!("      DoubleToInt {:?} {:?}", src, dest);
                    }
                    Instruction::DoubleToUInt(src, dest) => {
                        println!("      DoubleToUInt {:?} {:?}", src, dest);
                    }
                    Instruction::IntToDouble(src, dest) => {
                        println!("      IntToDouble {:?} {:?}", src, dest);
                    }
                    Instruction::UIntToDouble(src, dest) => {
                        println!("      UIntToDouble {:?} {:?}", src, dest);
                    }
                    Instruction::GetAddress(src, dest) => {
                        println!("      GetAddress {:?} {:?}", src, dest);
                    }
                    Instruction::Load(src, dest) => {
                        println!("      Load {:?} {:?}", src, dest);
                    }
                    Instruction::Store(src, dest) => {
                        println!("      Store {:?} {:?}", src, dest);
                    }
                    Instruction::AddPtr(src, dest, offset, result) => {
                        println!(
                            "      AddPtr {:?} {:?} {:?} {:?}",
                            src, dest, offset, result
                        );
                    }
                    Instruction::CopyToOffset(src, dest, offset) => {
                        println!("      CopyToOffset {:?} {:?} {:?}", src, dest, offset);
                    }
                }
            }
        }
    }
}
