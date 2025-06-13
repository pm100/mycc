use std::{collections::HashMap, fmt};

use anyhow::{bail, Result};
use enum_as_inner::EnumAsInner;

use crate::{
    parser::Parser,
    tacky::{StaticInit, StructurePtr},
};

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
#[derive(Clone, EnumAsInner)]
pub enum SymbolType {
    Int32,
    Int64,
    UInt32,
    UInt64,
    Double,
    Char,
    SChar,
    UChar,
    Void,
    Function(Vec<SymbolType>, Box<SymbolType>),
    Pointer(Box<SymbolType>),
    Array(Box<SymbolType>, usize),
    Struct(StructurePtr), // true means arg_ptr
                          //StructArg(StructurePtr),
}

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
    None, //local stack var
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
    pub value: Vec<StaticInit>,
    pub stype: SymbolType,
}

impl PartialEq for SymbolType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (SymbolType::Int32, SymbolType::Int32) => true,
            (SymbolType::Int64, SymbolType::Int64) => true,
            (SymbolType::UInt32, SymbolType::UInt32) => true,
            (SymbolType::UInt64, SymbolType::UInt64) => true,
            (SymbolType::Double, SymbolType::Double) => true,
            (SymbolType::Char, SymbolType::Char) => true,
            (SymbolType::SChar, SymbolType::SChar) => true,
            (SymbolType::UChar, SymbolType::UChar) => true,
            (SymbolType::Void, SymbolType::Void) => true,
            (SymbolType::Function(args1, ret1), SymbolType::Function(args2, ret2)) => {
                args1 == args2 && ret1 == ret2
            }
            (SymbolType::Pointer(t1), SymbolType::Pointer(t2)) => t1 == t2,
            (SymbolType::Array(t1, sz1), SymbolType::Array(t2, sz2)) => t1 == t2 && sz1 == sz2,
            (SymbolType::Struct(s1), SymbolType::Struct(s2)) => {
                s1.borrow().unique_name == s2.borrow().unique_name
            }

            _ => false,
        }
    }
}
impl fmt::Debug for SymbolType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SymbolType::Int32 => write!(f, "int32"),
            SymbolType::Int64 => write!(f, "int64"),
            SymbolType::UInt32 => write!(f, "uint32"),
            SymbolType::UInt64 => write!(f, "uint64"),
            SymbolType::Double => write!(f, "double"),
            SymbolType::Char => write!(f, "char"),
            SymbolType::SChar => write!(f, "schar"),
            SymbolType::UChar => write!(f, "uchar"),
            SymbolType::Void => write!(f, "void"),
            SymbolType::Function(args, ret) => {
                write!(f, "function({:?}) -> {:?}", args, ret)
            }
            SymbolType::Pointer(t) => write!(f, "*{:?}", t),
            SymbolType::Array(t, sz) => write!(f, "[{:?}; {}]", t, sz),
            SymbolType::Struct(s) => {
                write!(f, "struct {} ({})", s.borrow().unique_name, s.borrow().name)
            }
        }
    }
}
impl SymbolType {
    pub fn is_integer(&self) -> bool {
        Parser::is_integer(self)
    }
    pub fn is_character(&self) -> bool {
        matches!(
            self,
            SymbolType::Char | SymbolType::UChar | SymbolType::SChar
        )
    }
    pub fn get_inner_type(&self) -> Result<SymbolType> {
        Parser::get_inner_type(self)
    }
    pub fn is_void_pointer(&self) -> bool {
        matches!(self, SymbolType::Pointer(t) if t.is_void())
    }
    pub fn is_scalar(&self) -> bool {
        Parser::is_scalar(self)
    }
}
impl Parser {
    pub fn get_pointee_type(stype: &SymbolType) -> Result<SymbolType> {
        match stype {
            SymbolType::Pointer(t) => Ok(*t.clone()),
            _ => bail!("Not a pointer type"),
        }
    }
    pub fn get_array_type(stype: &SymbolType) -> Result<SymbolType> {
        match stype {
            SymbolType::Array(t, _) => Ok(*t.clone()),
            _ => bail!("Not an array"),
        }
    }

    pub fn get_inner_type(stype: &SymbolType) -> Result<SymbolType> {
        match stype {
            SymbolType::Pointer(t) => Ok(*t.clone()),
            SymbolType::Array(t, _) => Ok(*t.clone()),
            SymbolType::Function(_, t) => Ok(*t.clone()),

            _ => bail!("Not a pointer or array type"),
        }
    }
    pub fn get_target_type(stype: &SymbolType) -> Result<SymbolType> {
        match stype {
            SymbolType::Pointer(t) => Ok(*t.clone()),
            SymbolType::Array(t, _) => Ok(*t.clone()),
            _ => bail!("Not a pointer or array type"),
        }
    }
    pub fn get_inner_array_type(stype: &SymbolType) -> Result<SymbolType> {
        let mut stype = stype;
        loop {
            match stype {
                SymbolType::Array(t, _) => {
                    if let SymbolType::Array(_, _) = &**t {
                        stype = t;
                    } else {
                        return Ok(*t.clone());
                    }
                }
                _ => bail!("Not an array"),
            }
        }
    }
    pub fn get_total_object_size(stype: &SymbolType) -> Result<usize> {
        let size = match stype {
            SymbolType::Array(_, _) => {
                let dim = Self::get_array_size(stype)?;

                let inner = Self::get_array_type(stype)?;
                let size = Self::get_total_object_size(&inner)?;
                dim * size
            }
            SymbolType::Struct(_) => {
                let size = stype.as_struct().unwrap().borrow().size;
                if size == 0 {
                    bail!("Structure {:?} is incomplete", stype);
                }
                size
            } // SymbolType::StructArg(_) => stype.as_struct_arg().unwrap().borrow().size,
            _ => {
                
                Self::get_size_of_stype(stype)
            }
        };
        Ok(size)
    }

    pub fn get_array_count_and_type(stype: &SymbolType) -> Result<(usize, SymbolType)> {
        if stype.is_array() {
            let dim = Self::get_array_size(stype)?;
            let inner = Self::get_array_type(stype)?;
            let (idim, stype) = Self::get_array_count_and_type(&inner)?;
            Ok((idim * dim, stype))
        } else {
            Ok((1, stype.clone()))
        }
    }
    pub fn get_array_size(stype: &SymbolType) -> Result<usize> {
        match stype {
            SymbolType::Array(_, sz) => Ok(*sz),
            _ => bail!("Not an array"),
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
