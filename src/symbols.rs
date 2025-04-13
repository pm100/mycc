use anyhow::{bail, Result};
use enum_as_inner::EnumAsInner;

use crate::{parser::Parser, tacky::Value};

#[derive(Debug, Clone)]
pub(crate) struct VariableName {
    pub(crate) name: String,
    pub(crate) is_current: bool,
}
#[derive(Debug, Clone)]
pub struct Specifiers {
    pub is_static: bool,
    pub is_external: bool,
    pub specified_type: Option<SymbolType>,
}
#[derive(Debug, Clone, PartialEq, EnumAsInner)]
pub enum SymbolType {
    Int32,
    Int64,
    UInt32,
    UInt64,
    Double,
    Function(Vec<SymbolType>, Box<SymbolType>),
    Pointer(Box<SymbolType>),
}
// #[derive(Debug, Clone, PartialEq, EnumAsInner)]
// pub(crate) enum SymbolDetails {
//     Function {
//         return_type: SymbolType,
//         args: Vec<SymbolType>,
//     },
//     //
//     Variable {
//         rename: String,
//         stype: SymbolType,
//         // value: Option<Value>, //TODO - remove this
//     },
//     ScopePull,
// }
#[derive(Debug, Clone, PartialEq)]
pub enum SymbolState {
    Defined,
    Declared,
    Tentative,
}
#[derive(Debug, Clone, PartialEq)]
pub enum SymbolLinkage {
    External,

    Internal,
    None, //loalc stack var
}
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Symbol {
    pub name: String,
    pub state: SymbolState,
    pub rename: String,
    pub stype: SymbolType,
    pub linkage: SymbolLinkage,
    pub explicit_external: bool,
    pub scope_pull: bool,
}
#[derive(Debug, Clone)]
pub struct Extern {
    pub name: String,
    pub linkage: SymbolLinkage,
    pub state: SymbolState,
    pub value: Option<Value>,
    pub stype: SymbolType,
}
impl Parser {
    pub fn get_pointee_type(stype: &SymbolType) -> Result<SymbolType> {
        match stype {
            SymbolType::Pointer(t) => Ok(*t.clone()),
            _ => bail!("Not a pointer type"),
        }
    }
}
