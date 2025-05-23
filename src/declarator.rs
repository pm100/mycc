use crate::{
    lexer::Token,
    parser::Parser,
    symbols::{Specifiers, SymbolType},
};
use anyhow::{bail, Result};
use enum_as_inner::EnumAsInner;
#[derive(Debug, Clone)]
struct Parameter {
    pub stype: SymbolType,
    pub decl: Declarator,
}
pub fn indent(str: &str) {
    //let msg = format!("{:>width$}", str, width=INDENT as usize);
    let pad = unsafe { format!("{empty:>width$}", empty = "", width = INDENT * 4) };
    println!("{}{}", pad, str);
}
pub static mut INDENT: usize = 0;
enum Suffix {
    ParamList(Vec<Parameter>),
    Index(Vec<usize>),
    Nothing,
}
#[derive(Debug, Clone, EnumAsInner)]
enum Declarator {
    Identifier(String),
    Pointer(Box<Declarator>),
    Array(Box<Declarator>, usize),
    Function(Vec<Parameter>, Box<Declarator>),
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum AbstractDeclarator {
    Pointer(Box<AbstractDeclarator>),
    Array(Box<AbstractDeclarator>, usize),
    AbstractBase,
}
impl Parser {
    pub fn parse_declaration(&mut self) -> Result<(Specifiers, String, SymbolType, Vec<String>)> {
        unsafe { INDENT = 0 };
        let specifiers = self.parse_specifiers(true)?;
        if specifiers.specified_type.is_none() {
            // its a statement , return effectively null
            return Ok((specifiers, String::new(), SymbolType::Int32, vec![]));
        }
        let base_type = specifiers.specified_type.clone().unwrap();
        let decl = self.parse_declarator()?;
        println!("declaration {:?}", decl);
        let res = Self::process_declarator(&decl, &base_type)?;
        println!("declaration {:?}", res);
        Ok((specifiers, res.0, res.1, res.2))
    }

    fn process_declarator(
        decl: &Declarator,
        base_type: &SymbolType,
    ) -> Result<(String, SymbolType, Vec<String>)> {
        match decl {
            Declarator::Identifier(name) => Ok((name.clone(), base_type.clone(), vec![])),
            Declarator::Pointer(pdecl) => {
                let stype = SymbolType::Pointer(Box::new(base_type.clone()));
                Self::process_declarator(pdecl, &stype)
            }
            Declarator::Array(pdecl, size) => {
                if base_type.is_void() {
                    bail!("Array type cannot be void");
                }
                let stype = SymbolType::Array(Box::new(base_type.clone()), *size);
                Self::process_declarator(pdecl, &stype)
            }
            Declarator::Function(params, decl) => {
                let unbox = *decl.clone();
                let s = match unbox {
                    Declarator::Identifier(name) => name.clone(),
                    _ => bail!("bad function"),
                };
                //  bail!("Function type cannot be a function type");

                let mut pnames = Vec::new();
                let mut ptypes = Vec::new();
                for param in params.iter() {
                    let (name, ptype, _) = Self::process_declarator(&param.decl, &param.stype)?;
                    if pnames.contains(&name) {
                        bail!("Duplicate parameter name: {}", name);
                    }
                    pnames.push(name);
                    let ptype = Self::decay_arg(&ptype);
                    ptypes.push(ptype);
                }
                if base_type.is_array() {
                    bail!("Function type cannot be an array type");
                }
                let stype = SymbolType::Function(ptypes, Box::new(base_type.clone()));
                Ok((s, stype, pnames))
            }
        }
    }
    pub fn decay_arg(stype: &SymbolType) -> SymbolType {
        match stype {
            SymbolType::Array(stype, _) => SymbolType::Pointer(stype.clone()),
            _ => stype.clone(),
        }
    }
    pub fn parse_abstract_declarator(
        &mut self,
        //base_type: &SymbolType,
    ) -> Result<AbstractDeclarator> {
        indent("parse_abstract_declarator");
        unsafe { INDENT += 1 };
        let decl = if self.peek()? == Token::Multiply {
            // * followed by optionally another absdecl
            self.next_token()?;

            let token = self.peek()?;
            let inner = if token == Token::LeftParen
                || token == Token::Multiply
                || token == Token::LeftBracket
            {
                self.parse_abstract_declarator()?
            } else {
                AbstractDeclarator::AbstractBase
            };
            AbstractDeclarator::Pointer(Box::new(inner))
        } else {
            self.parse_direct_abstract_declarator()?
        };
        indent(&format!("parse_abstract_declarator {:?}", decl));
        unsafe { INDENT += 1 };
        Ok(decl)
    }
    fn parse_direct_abstract_declarator(
        &mut self,
        //base_type: &SymbolType,
    ) -> Result<AbstractDeclarator> {
        let token = self.peek()?;
        if token == Token::LeftParen {
            self.next_token()?;
            let decl = self.parse_abstract_declarator()?;
            self.expect(Token::RightParen)?;
            let token = self.peek()?;
            if token == Token::LeftBracket {
                let arr_decl = self.parse_abs_suffix(&decl)?;
                Ok(arr_decl)
            } else {
                Ok(decl)
            }
        } else {
            let decl = AbstractDeclarator::AbstractBase;
            let arr_decl = self.parse_abs_suffix(&decl)?;
            Ok(arr_decl)
        }
    }
    fn parse_abs_suffix(&mut self, base_type: &AbstractDeclarator) -> Result<AbstractDeclarator> {
        self.next_token()?;
        let index = self.do_rvalue_expression()?;
        let index = Self::get_integer(&index)?;
        let arr_decl = AbstractDeclarator::Array(Box::new(base_type.clone()), index);
        self.expect(Token::RightBracket)?;
        if self.peek()? == Token::LeftBracket {
            return self.parse_abs_suffix(&arr_decl);
        }

        Ok(arr_decl)
    }

    pub fn process_abstract_declarator(
        //    &mut self,
        decl: &AbstractDeclarator,
        base_type: &SymbolType,
    ) -> Result<SymbolType> {
        match decl {
            AbstractDeclarator::Pointer(pdecl) => {
                let stype = SymbolType::Pointer(Box::new(base_type.clone()));
                Self::process_abstract_declarator(pdecl, &stype)
            }
            AbstractDeclarator::Array(pdecl, size) => {
                let stype = SymbolType::Array(Box::new(base_type.clone()), *size);
                Self::process_abstract_declarator(pdecl, &stype)
            }
            AbstractDeclarator::AbstractBase => Ok(base_type.clone()),
        }
    }
    fn parse_declarator(&mut self) -> Result<Declarator> {
        unsafe { INDENT += 1 };
        indent("parse_declarator base_type ");
        let ret = if self.peek()? == Token::Multiply {
            self.next_token()?;
            let pdecl = self.parse_declarator()?;
            Declarator::Pointer(Box::new(pdecl))
        } else {
            self.parse_direct_declarator()?
        };
        unsafe { INDENT -= 1 };
        Ok(ret)
    }

    fn parse_direct_declarator(&mut self) -> Result<Declarator> {
        // simple declarator
        let simp = self.parse_simple()?;
        indent(&format!("direct_declarator after simp  {:?}", simp));

        Ok(match self.parse_suffix()? {
            Suffix::ParamList(params) => {
                if simp.is_function() {
                    bail!("Function type cannot be a function type");
                }
                // let names = params.iter().map(|p| p.name.clone()).collect::<Vec<_>>();
                // let stypes = params.iter().map(|p| p.stype.clone()).collect::<Vec<_>>();
                // let stype = SymbolType::Function(stypes, Box::new(new_type));
                Declarator::Function(params, Box::new(simp))
            }
            Suffix::Index(indexes) => {
                let mut adecl = simp; //              let mut atype = new_type.clone();
                for index in indexes.iter() {
                    adecl = Declarator::Array(Box::new(adecl), *index);
                }
                adecl
            }
            Suffix::Nothing => simp,
        })
    }
    fn parse_simple(&mut self) -> Result<Declarator> {
        indent("parse_simple ");
        match self.peek()? {
            Token::Identifier(id) => {
                self.next_token()?;
                Ok(Declarator::Identifier(id))
            }
            Token::LeftParen => {
                self.next_token()?;
                let decl = self.parse_declarator()?;
                self.expect(Token::RightParen)?;
                Ok(decl)
            }
            _ => bail!("Expected identifierxx got{:?}", self.peek()?),
        }
    }

    fn parse_suffix(&mut self) -> Result<Suffix> {
        let token = self.peek()?;
        Ok(match token {
            Token::LeftParen => {
                self.next_token()?;
                let params = self.parse_param_list()?;
                //self.expect(Token::RightParen)?;
                Suffix::ParamList(params)
            }
            Token::LeftBracket => {
                let mut array_sizes = Vec::new();
                loop {
                    self.next_token()?;
                    let index = self.do_rvalue_expression()?;
                    self.expect(Token::RightBracket)?;
                    let index = Self::get_integer(&index)?;
                    array_sizes.push(index);
                    if self.peek()? != Token::LeftBracket {
                        break;
                    }
                }
                Suffix::Index(array_sizes)
            }
            _ => Suffix::Nothing,
        })
    }
    fn parse_param_list(&mut self) -> Result<Vec<Parameter>> {
        let mut parameters = Vec::new();

        // special case -- int x()
        if self.peek()? == Token::RightParen {
            self.next_token()?;
            return Ok(parameters);
        }

        // special case -- int x(void)
        if self.peek()? == Token::Void && self.peek_n(1)? != Token::Multiply {
            self.next_token()?;
            self.expect(Token::RightParen)?;
            return Ok(parameters);
        }
        // deal with the fact that the first param is not preceded by a comma

        let first = self.parse_one_param()?;
        parameters.push(first);

        loop {
            if self.peek()? == Token::RightParen {
                self.next_token()?;
                return Ok(parameters);
            }
            self.expect(Token::Comma)?;
            // parse one param
            let param = self.parse_one_param()?;

            // if parameters.iter().any(|p| p.name == param.name) {
            //     bail!("Duplicate parameter name: {}", param.name);
            // }
            parameters.push(param);
            if self.peek()? == Token::RightParen {
                self.next_token()?;
                break;
            }
        }
        Ok(parameters)
    }

    fn parse_one_param(&mut self) -> Result<Parameter> {
        let specifiers = self.parse_specifiers(false)?;
        let base_type = specifiers.specified_type.clone().unwrap();
        let param = self.parse_declarator()?;
        indent(&format!("{:?}", param));
        Ok(Parameter {
            stype: base_type,
            decl: param,
        })
    }

    pub fn parse_specifiers(&mut self, allow_storage: bool) -> Result<Specifiers> {
        let mut specifiers = Specifiers {
            is_static: false,
            is_external: false,
            specified_type: None, // SymbolType::Int,
        };
        let mut long = false;
        let mut long_long = false;
        let mut int = false;
        let mut signed = false;
        let mut unsigned = false;
        let mut double = false;
        let mut char = false;
        let mut void = false;

        loop {
            let token = self.peek()?;
            match token {
                Token::Double => {
                    if int || long || long_long || char || signed || unsigned {
                        bail!("integer types and double cannot be used together");
                    }
                    self.next_token()?;
                    if double {
                        bail!("Duplicate double specifier");
                    }
                    double = true;
                }
                Token::Int => {
                    if char || double {
                        bail!("integer types and char cannot be used together");
                    }
                    self.next_token()?;
                    if int {
                        bail!("Duplicate int specifier");
                    }
                    int = true;
                }
                Token::Long => {
                    if char || double {
                        bail!("integer types and char cannot be used together");
                    }
                    // this code assumes that all the 'long's are together
                    if long_long {
                        bail!("Duplicate long specifier");
                    }
                    if long {
                        long_long = true;
                    } else {
                        long = true;
                    }
                    self.next_token()?;
                }
                Token::Char => {
                    if int || double {
                        bail!("integer types and char cannot be used together");
                    }
                    self.next_token()?;
                    if char {
                        bail!("Duplicate char specifier");
                    }

                    char = true;
                }
                Token::Signed => {
                    if double {
                        bail!("double is always signed");
                    }
                    self.next_token()?;
                    if signed {
                        bail!("Duplicate signed specifier");
                    }
                    if unsigned {
                        bail!("signed and unsigned cannot be used together");
                    }
                    signed = true;
                }
                Token::Unsigned => {
                    if double {
                        bail!("double is always signed");
                    }
                    self.next_token()?;
                    if unsigned {
                        bail!("Duplicate unsigned specifier");
                    }
                    if signed {
                        bail!("signed and unsigned cannot be used together");
                    }
                    unsigned = true;
                }
                Token::Void => {
                    self.next_token()?;
                    if int || long || long_long || char || signed || unsigned || double {
                        bail!("integer types and void cannot be used together");
                    }
                    if void {
                        bail!("Duplicate void specifier");
                    }
                    void = true;
                }
                Token::Extern => {
                    self.next_token()?;
                    if !allow_storage {
                        bail!("extern specifier not allowed here");
                    }
                    if specifiers.is_external {
                        bail!("Duplicate extern specifier");
                    }
                    specifiers.is_external = true;
                }
                Token::Static => {
                    self.next_token()?;
                    if !allow_storage {
                        bail!("static specifier not allowed here");
                    }
                    if specifiers.is_static {
                        bail!("Duplicate static specifier");
                    }
                    specifiers.is_static = true;
                }
                _ => break,
            };
        }
        if void {
            if int || long || long_long || char || signed || unsigned || double {
                bail!("integer types and void cannot be used together");
            }
            specifiers.specified_type = Some(SymbolType::Void);
            return Ok(specifiers);
        }
        if char {
            if signed {
                specifiers.specified_type = Some(SymbolType::SChar);
            } else if unsigned {
                specifiers.specified_type = Some(SymbolType::UChar);
            } else {
                specifiers.specified_type = Some(SymbolType::Char);
            }
            return Ok(specifiers);
        }

        if double {
            specifiers.specified_type = Some(SymbolType::Double);
            return Ok(specifiers);
        }
        if long_long {
            if signed {
                specifiers.specified_type = Some(SymbolType::Int64);
            } else if unsigned {
                specifiers.specified_type = Some(SymbolType::UInt64);
            } else {
                specifiers.specified_type = Some(SymbolType::Int64);
            }
            return Ok(specifiers);
        }
        if long {
            if signed {
                specifiers.specified_type = Some(SymbolType::Int64);
            } else if unsigned {
                specifiers.specified_type = Some(SymbolType::UInt64);
            } else {
                specifiers.specified_type = Some(SymbolType::Int32);
            }
            specifiers.specified_type = Some(SymbolType::Int32);
            return Ok(specifiers);
        }
        if signed || int {
            specifiers.specified_type = Some(SymbolType::Int32);
        }
        if unsigned {
            specifiers.specified_type = Some(SymbolType::UInt32);
        }
        println!("specifiers {:?}", specifiers);
        Ok(specifiers)
    }
}
