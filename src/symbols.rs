use std::collections::HashMap;

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
pub struct Symbol {
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
    pub(crate) fn lookup_symbol(&self, name: &str) -> Option<(Symbol, bool)> {
        for i in (0..self.symbol_stack.len()).rev() {
            if let Some(sym) = self.symbol_stack[i].get(name) {
                return Some((sym.clone(), i == self.symbol_stack.len() - 1));
            }
        }
        None
    }
    pub(crate) fn get_global_symbol(&mut self, name: &str) -> Symbol {
        self.symbol_stack[0].get(name).unwrap().clone()
    }
    pub(crate) fn lookup_global_symbol(&self, name: &str) -> Option<(Symbol, bool)> {
        if let Some(sym) = self.symbol_stack[0].get(name) {
            return Some((sym.clone(), self.symbol_stack.len() == 1));
        }
        None
    }
    pub fn insert_global_symbol(&mut self, name: &str, symbol: Symbol) {
        println!("insert_global_symbol {:?}", symbol);
        //   debug_assert!(!self.symbol_stack[0].contains_key(name));
        self.symbol_stack
            .get_mut(0)
            .unwrap()
            .insert(name.to_string(), symbol);
    }
    pub fn insert_symbol(&mut self, name: &str, symbol: Symbol) {
        println!("insert_symbol {:?}", symbol);
        self.symbol_stack
            .last_mut()
            .unwrap()
            .insert(name.to_string(), symbol);
    }

    pub fn pop_symbols(&mut self) {
        //  self.dump_symbols();
        println!("pop_symbols {:?}", self.symbol_stack.len());
        self.symbol_stack.pop();
    }
    pub fn push_symbols(&mut self) {
        println!("push_symbols {:?}", self.symbol_stack.len());
        self.symbol_stack.push(HashMap::new());
    }
    pub fn dump_symbols(&self) {
        println!("=======symbol table dump=======");
        for (i, sym) in self.symbol_stack.iter().enumerate() {
            let pad = format!("{empty:>width$}", empty = "", width = i * 4);
            if sym.is_empty() {
                println!("{} empty", pad);
            }
            for (_, sym) in sym.iter() {
                println!("{} {:?}", pad, sym);
            }
        }
        for (sym, top) in self.externs.iter() {
            println!("extern {} {:?}", sym, top);
        }
        println!("=======end symbol table dump=======");
    }
}
