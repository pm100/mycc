use std::mem::discriminant;

use anyhow::{bail, Result};

use crate::{lexer::Token, parser::Parser, symbols::SymbolType, tacky::Value};

impl Parser {
    pub fn is_arithmetic(stype: &SymbolType) -> bool {
        match stype {
            SymbolType::Int32
            | SymbolType::Int64
            | SymbolType::UInt32
            | SymbolType::UInt64
            | SymbolType::Double => true,
            _ => false,
        }
    }

    pub fn is_integer(stype: &SymbolType) -> bool {
        match stype {
            SymbolType::Int32 | SymbolType::Int64 | SymbolType::UInt32 | SymbolType::UInt64 => true,
            _ => false,
        }
    }
    pub fn get_integer(value: &Value) -> Result<usize> {
        match value {
            Value::Int32(v) => Ok(*v as usize),
            Value::Int64(v) => Ok(*v as usize),
            Value::UInt32(v) => Ok(*v as usize),
            Value::UInt64(v) => Ok(*v as usize),
            _ => bail!("Expected integerxxx, got {:?}", value),
        }
    }

    // pub fn deref_pointer(value: &Value) -> Result<Value> {
    //     assert!(value.is_pointer(), "Expected pointer, got {:?}", value);
    //     let (name, stype) = value.as_variable().unwrap();
    //     let ptype = Self::get_pointee_type(&stype).unwrap();

    //     assert!(name.starts_with('*'));
    //     let name = &name[1..];

    //     Ok(Value::Variable(name.to_string(), ptype.clone()))
    // }
    // pub fn make_deref_pointer(value: &Value) -> Value {
    //     let name = value.as_variable().unwrap().0.clone();
    //     let name = format!("*{}", name);
    //     let stype = Self::get_type(&value);
    //     let ptype = Self::get_pointee_type(&stype).unwrap();
    //     Value::Variable(name, SymbolType::Pointer(Box::new(ptype.clone())))
    // }

    pub fn is_null_pointer_constant(value: &Value) -> bool {
        match value {
            Value::Int32(0) => true,
            Value::Int64(0) => true,
            Value::UInt32(0) => true,
            Value::UInt64(0) => true,
            _ => false,
        }
    }
    pub(crate) fn expect(&mut self, token: Token) -> Result<Token> {
        let nt = self.next_token()?;
        if discriminant(&nt) != discriminant(&token) {
            bail!("Expected {:?}, got {:?}", token, nt);
        } else {
            Ok(nt)
        }
    }

    pub(crate) fn make_temporary_name(&mut self) -> String {
        let temp = format!("$temp${}", self.next_temporary);
        self.next_temporary += 1;
        temp
    }
    pub fn make_temporary(&mut self, stype: &SymbolType) -> Value {
        let temp_name = self.make_temporary_name();
        Value::Variable(temp_name.clone(), stype.clone())
    }
    pub(crate) fn make_newname(&mut self, name: &str) -> String {
        let temp = format!("{}${}", name, self.next_name);
        self.next_name += 1;
        temp
    }
    pub(crate) fn make_label(&mut self, name: &str) -> String {
        let temp = format!("_{}_{}", name, self.next_temporary);
        self.next_temporary += 1;
        temp
    }
    pub(crate) fn is_lvalue(&mut self, value: &Value) -> bool {
        match value {
            Value::Variable(var, _) => {
                self.local_variables().values().any(|v| v.name == *var)
                    || self.lookup_symbol(var).is_some()
            }
            _ => false,
        }
    }
}
