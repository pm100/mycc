use crate::{
    expect,
    lexer::Token,
    parser::Parser,
    symbols::{Specifiers, SymbolType},
};
use anyhow::{bail, Result};
pub struct Parameter {
    pub name: String,
    pub stype: SymbolType,
}
impl Parser {
    pub fn parse_declaration(&mut self) -> Result<(Specifiers, String, SymbolType, Vec<String>)> {
        let specifiers = self.parse_specifiers(true)?;
        if specifiers.specified_type.is_none() {
            // its a statement , return effectively null
            return Ok((specifiers, String::new(), SymbolType::Int32, vec![]));
        }
        let base_type = specifiers.specified_type.clone().unwrap();
        let (name, stype, params) = self.parse_declarator(&base_type)?;
        println!("declaration {} {:?}", name, stype);
        Ok((specifiers, name, stype, params))
    }

    pub fn parse_abstract_declarator(
        &mut self,
        base_type: &SymbolType,
    ) -> Result<(SymbolType, Vec<String>)> {
        if self.peek()? == Token::Multiply {
            self.next_token()?;
            let (pstype, pargs) = self.parse_abstract_declarator(base_type)?;
            let stype = SymbolType::Pointer(Box::new(pstype));
            Ok((stype, pargs))
        } else {
            if self.peek()? == Token::RightParen {
                self.next_token()?;
                return Ok((base_type.clone(), vec![]));
            }
            return self.parse_direct_abstract_declarator(base_type);
        }
    }
    fn parse_direct_abstract_declarator(
        &mut self,
        base_type: &SymbolType,
    ) -> Result<(SymbolType, Vec<String>)> {
        self.expect(Token::LeftParen)?;
        let (stype, params) = self.parse_abstract_declarator(base_type)?;
        self.expect(Token::RightParen)?;
        Ok((stype, params))
    }
    fn parse_declarator(
        &mut self,
        base_type: &SymbolType,
    ) -> Result<(String, SymbolType, Vec<String>)> {
        println!("parse_declarator base_type {:?}", base_type);
        if self.peek()? == Token::Multiply {
            self.next_token()?;
            let (pname, pstype, pargs) = self.parse_declarator(base_type)?;
            match &pstype {
                SymbolType::Function(args, ret) => {
                    let ret_type = SymbolType::Pointer(ret.clone());
                    let stype = SymbolType::Function(args.clone(), Box::new(ret_type));
                    return Ok((pname, stype, pargs));
                }
                _ => {
                    let stype = SymbolType::Pointer(Box::new(pstype));
                    return Ok((pname, stype, pargs));
                }
            }
        } else {
            return self.parse_direct_declarator(base_type);
        }
    }

    fn parse_direct_declarator(
        &mut self,
        base_type: &SymbolType,
    ) -> Result<(String, SymbolType, Vec<String>)> {
        // simple declarator
        let (identifier, stype_args) = self.parse_simple(base_type)?;
        println!(
            "direct_declarator {} {:?} {:?}",
            identifier, stype_args, base_type
        );
        let (new_type, _) = if let Some((stype, params)) = stype_args {
            (stype, params)
        } else {
            (base_type.clone(), vec![])
        };
        if self.peek()? == Token::LeftParen {
            self.next_token()?;
            // a function

            let params = self.parse_param_list()?;
            let names = params.iter().map(|p| p.name.clone()).collect::<Vec<_>>();
            let stypes = params.iter().map(|p| p.stype.clone()).collect::<Vec<_>>();
            if new_type.is_function() {
                bail!("Function type cannot be a function type");
            }
            let stype = SymbolType::Function(stypes, Box::new(new_type.clone()));
            Ok((identifier, stype, names))
        } else {
            // a variable

            Ok((identifier, new_type.clone(), vec![]))
        }
    }
    fn parse_simple(
        &mut self,
        base_type: &SymbolType,
    ) -> Result<(String, Option<(SymbolType, Vec<String>)>)> {
        println!("parse_simple base_type {:?}", base_type);
        match self.peek()? {
            Token::Identifier(id) => {
                self.next_token()?;
                return Ok((id, None));
            }
            Token::LeftParen => {
                self.next_token()?;
                let (identifier, stype, params) = self.parse_declarator(base_type)?;
                self.expect(Token::RightParen)?;
                Ok((identifier, Some((stype, params))))
            }
            _ => bail!("Expected identifierxx got{:?}", self.peek()?),
        }
    }
    fn parse_param_list(&mut self) -> Result<Vec<Parameter>> {
        let mut parameters = Vec::new();

        // special case -- int x()
        if self.peek()? == Token::RightParen {
            self.next_token()?;
            return Ok(parameters);
        }

        // special case -- int x(void)
        if self.peek()? == Token::Void {
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

            if parameters.iter().any(|p| p.name == param.name) {
                bail!("Duplicate parameter name: {}", param.name);
            }
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
        let (name, stype, _) = self.parse_declarator(&base_type)?;
        println!("param {} {:?} {:?}", name, specifiers, stype);
        Ok(Parameter { name, stype })
    }

    pub fn parse_specifiers(&mut self, allow_storage: bool) -> Result<Specifiers> {
        let mut specifiers = Specifiers {
            is_static: false,
            is_external: false,
            specified_type: None, // SymbolType::Int,
        };
        let mut long_count = 0;
        let mut int_count = 0;
        let mut signed_count = 0;
        let mut unsigned_count = 0;
        let mut double_count = 0;

        loop {
            let token = self.peek()?;
            match token {
                Token::Double => {
                    self.next_token()?;
                    if double_count > 0 {
                        bail!("Duplicate double specifier");
                    }
                    double_count += 1;
                }
                Token::Int => {
                    self.next_token()?;
                    int_count += 1;
                }
                Token::Long => {
                    if long_count > 0 {
                        bail!("Duplicate long specifier");
                    }
                    long_count += 1;
                    self.next_token()?;
                    loop {
                        let token = self.peek()?;
                        if token == Token::Long {
                            self.next_token()?;
                            long_count += 1;
                        } else {
                            break;
                        }
                    }
                }
                Token::Signed => {
                    self.next_token()?;
                    if signed_count > 0 {
                        bail!("Duplicate signed specifier");
                    }
                    if unsigned_count > 0 {
                        bail!("signed and unsigned cannot be used together");
                    }
                    signed_count += 1;
                }
                Token::Unsigned => {
                    self.next_token()?;
                    if unsigned_count > 0 {
                        bail!("Duplicate unsigned specifier");
                    }
                    if signed_count > 0 {
                        bail!("signed and unsigned cannot be used together");
                    }
                    unsigned_count += 1;
                }
                Token::Void => {
                    self.next_token()?;
                    if specifiers.specified_type.is_some() {
                        bail!("Duplicate void specifier");
                    }
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
        if double_count > 0 {
            if long_count > 0 {
                bail!("long and double cannot be used together");
            }
            if int_count > 0 {
                bail!("int and double cannot be used together");
            }
            if signed_count > 0 {
                bail!("signed and double cannot be used together");
            }
            if unsigned_count > 0 {
                bail!("unsigned and double cannot be used together");
            }
            specifiers.specified_type = Some(SymbolType::Double);
            return Ok(specifiers);
        }
        if unsigned_count > 0 {
            match (long_count, int_count) {
                (0, 0) => specifiers.specified_type = Some(SymbolType::UInt32),
                (1, 1) => specifiers.specified_type = Some(SymbolType::UInt32),
                (1, 0) => specifiers.specified_type = Some(SymbolType::UInt32),
                (2, 0) => specifiers.specified_type = Some(SymbolType::UInt64),
                (2, 1) => specifiers.specified_type = Some(SymbolType::UInt64),
                (0, 1) => specifiers.specified_type = Some(SymbolType::UInt32),
                _ => bail!("Invalid type specifier"),
            }
        } else {
            match (long_count, int_count) {
                (0, 0) => {
                    specifiers.specified_type = if signed_count > 0 {
                        Some(SymbolType::Int32)
                    } else {
                        None
                    }
                }
                (1, 1) => specifiers.specified_type = Some(SymbolType::Int32),
                (1, 0) => specifiers.specified_type = Some(SymbolType::Int32),
                (2, 0) => specifiers.specified_type = Some(SymbolType::Int64),
                (2, 1) => specifiers.specified_type = Some(SymbolType::Int64),
                (0, 1) => specifiers.specified_type = Some(SymbolType::Int32),
                _ => bail!("Invalid type specifier"),
            }
        }
        Ok(specifiers)
    }
}
