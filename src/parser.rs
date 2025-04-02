use anyhow::{bail, Result};
use backtrace::{Backtrace, BacktraceFrame, BacktraceSymbol};
use enum_as_inner::EnumAsInner;
use std::{
    collections::{HashMap, HashSet, VecDeque},
    mem::discriminant,
    path::{Path, PathBuf},
};

// https://users.rust-lang.org/t/how-to-parameterize-a-function-or-macro-by-enum-variant/126398

macro_rules! expect {
    ($lexer:expr,$token:path) => {
        match $lexer.next_token() {
            Ok($token(data)) => data,
            Err(e) => return Err(e),
            t => bail!("Expected {e:?}, got {t:?}", e = stringify!($token)),
        }
    };
}
// macro_rules! fatal {
//     ($msg:literal $(,)?) => {
//         let message = format!(
//             "{}:{} {}",
//             self.lexer.file.display(),
//             self.lexer.current_line_number,
//             $msg
//         );
//         bail!(message);
//     };
// }
use crate::{
    lexer::{Lexer, Token},
    tacky::{BinaryOperator, Instruction, TackyProgram, Value},
};
#[derive(Debug, Clone)]
pub(crate) struct VariableName {
    pub(crate) name: String,
    pub(crate) is_current: bool,
}
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum SymbolType {
    Int,
    //  Void,
}
#[derive(Debug, Clone, PartialEq)]
enum Specifier {
    Type(SymbolType),
    // Void,
    Extern,
    Static,
}
// pub(crate) struct ArgumentDescriptor {
//     pub(crate) name: String,
//     pub(crate) tipe: SymbolType,
// }
#[derive(Debug, Clone, PartialEq, EnumAsInner)]
pub(crate) enum SymbolDetails {
    Function {
        return_type: SymbolType,
        args: Vec<SymbolType>,
    },
    //
    Variable {
        rename: String,
        stype: SymbolType,
        value: Option<Value>,
    },
    ScopePull,
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
    None, //loalc stack var
}
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Symbol {
    pub(crate) name: String,
    pub(crate) state: SymbolState,
    pub(crate) details: SymbolDetails,
    linkage: SymbolLinkage,
    explicit_external: bool,
}

struct SwitchContext {
    cases: Vec<Value>,
    value: Value,
    before_first_case: bool,
    default_statement_seen: bool,

    next_case_label: String,
    next_drop_thru_label: String,
}
#[derive(Debug, Clone)]
struct Extern {
    name: String,
    linkage: SymbolLinkage,
    state: SymbolState,
    value: Option<Value>,
    stype: SymbolType,
}
pub struct Parser {
    lexer: Lexer,
    source_file: PathBuf,
    tacky: TackyProgram,
    next_temporary: usize,
    eof_hit: bool,
    peeked_tokens: VecDeque<Token>,
    // original name => new name
    local_variables: Vec<HashMap<String, VariableName>>,
    labels: HashMap<String, bool>,
    next_name: usize,
    continue_label_stack: Vec<String>,
    switch_context_stack: Vec<SwitchContext>,
    break_label_stack: Vec<String>,
    pub(crate) symbol_stack: Vec<HashMap<String, Symbol>>,
    pub(crate) nest: String,
    current_function_name: String,
    in_function_body: bool,
    externs: HashMap<String, Extern>,
}

impl Parser {
    pub fn new(path: &Path, real_source: &Path) -> Self {
        Self {
            lexer: Lexer::new(path),
            source_file: real_source.to_path_buf(),
            tacky: TackyProgram::new(),
            next_temporary: 0,
            eof_hit: false,
            peeked_tokens: VecDeque::new(),
            local_variables: Vec::new(),
            labels: HashMap::new(),
            next_name: 0,
            nest: String::new(),
            continue_label_stack: Vec::new(),
            switch_context_stack: Vec::new(),
            break_label_stack: Vec::new(),
            symbol_stack: vec![HashMap::new()],
            current_function_name: String::new(),
            in_function_body: false,
            externs: HashMap::new(),
        }
    }
    pub(crate) fn cc_fatal_error(&self, msg: &str) {
        let message = format!(
            "{}:{} {}",
            self.lexer.file.display(),
            self.lexer.current_line_number,
            msg
        );
    }
    pub fn parse(&mut self) -> Result<&TackyProgram> {
        self.do_program()?;
        Ok(&self.tacky)
    }

    fn do_program(&mut self) -> Result<()> {
        loop {
            if self.peek()? == Token::Eof {
                break;
            }
            // top level is either a function or a variable
            // but we cant tell easily which is which
            // so a common routine parses both

            match self.do_function_or_variable(true, true) {
                Ok(true) => {}
                Ok(false) => {
                    // its a statement, thats an error here
                }
                Err(e) => {
                    let message = format!(
                        "{}:{} {}",
                        self.source_file.display(),
                        self.lexer.current_line_number,
                        e
                    );
                    bail!(message);
                }
            }
        }
        for ext in self.externs.values() {
            //  if ext.linkage != SymbolLinkage::None {
            self.tacky.add_static_variable(
                &ext.name,
                ext.value.clone(),
                ext.linkage == SymbolLinkage::External,
                ext.state == SymbolState::Declared,
            );
            //  }
        }

        Ok(())
    }
    fn do_function_or_variable(&mut self, func_allowed: bool, body_allowed: bool) -> Result<bool> {
        // three possible normal outcomes
        // this is a function, the heavy lifting is done here => true
        // this is a variable declartion, passed to do_variable_dec => true
        // this is a statment => false

        // scan the specifiers first
        let mut already = HashSet::new();
        // let specifiers = {
        let mut specifiers = Vec::new();
        loop {
            let token = self.peek()?;

            let specifier = match token {
                Token::Int => {
                    self.next_token()?;
                    Specifier::Type(SymbolType::Int)
                    //   specifiers.insert(Specifier::Type(SymbolType::Int));
                }

                Token::Extern => {
                    self.next_token()?;
                    Specifier::Extern
                }
                Token::Static => {
                    self.next_token()?;
                    Specifier::Static
                }
                _ => {
                    break;
                    //specifiers;
                }
            };
            if !already.insert(format!("{:?}", token)) {
                bail!("already");
            }
            specifiers.push(specifier);
        }
        //  };

        if specifiers.is_empty() {
            // its a statement, only allowed in functions
            if !self.in_function_body {
                bail!("expected function or variable");
            }
            return Ok(false);
        }
        let func_dec_name = expect!(self, Token::Identifier);
        println!("Function or dec: {}", func_dec_name);

        // if next token is "(" then its a function, otherwise its a variable
        let token = self.peek()?;
        if token != Token::LeftParen {
            self.do_variable_dec(&func_dec_name, &specifiers)?;
        } else {
            if !func_allowed {
                bail!("Function declaration not allowed here");
            }
            self.do_function(&func_dec_name, &specifiers, body_allowed)?;
        }
        Ok(true)
    }
    fn do_function(
        &mut self,
        name: &str,
        specifiers: &Vec<Specifier>,
        body_allowed: bool,
    ) -> Result<()> {
        let explicit_external = specifiers.contains(&Specifier::Extern);
        // table 10.2
        let mut linkage = match (
            specifiers.contains(&Specifier::Extern),
            specifiers.contains(&Specifier::Static),
            self.in_function_body,
        ) {
            (true, true, _) => bail!("Function cannot be both extern and static"),
            (true, _, _) => SymbolLinkage::External,
            (_, true, false) => SymbolLinkage::Internal,
            (_, true, true) => bail!("Function cannot be static"),
            (false, false, false) => SymbolLinkage::None,

            (false, false, true) => SymbolLinkage::External,
        };
        self.current_function_name = name.to_string();

        let mut symbol_det = SymbolDetails::Function {
            return_type: SymbolType::Int, // always at the moment
            args: Vec::new(),
        };
        let arg_types = &mut symbol_det.as_function_mut().unwrap().1;
        self.expect(Token::LeftParen)?;

        let mut arg_names = Vec::new();

        loop {
            let token = self.next_token()?;
            if token == Token::RightParen {
                break;
            }

            match token {
                Token::Int => {
                    arg_types.push(SymbolType::Int);
                }
                Token::Void => {
                    self.expect(Token::RightParen)?;
                    break;
                }
                _ => bail!("Expected Int or Void, got {:?}", token),
            }

            let token = self.peek()?;
            if let Token::Identifier(name) = token {
                if arg_names.contains(&name) {
                    bail!("Duplicate argument name {}", name);
                }
                arg_names.push(name);
                self.next_token()?;
            }
            let token = self.next_token()?;
            match token {
                Token::RightParen => {
                    break;
                }
                Token::Comma => {
                    if self.peek()? == Token::RightParen {
                        bail!("redundant comma in function declaration");
                    }
                    continue;
                }
                _ => {
                    bail!("Expected Comma or RightParen, got {:?}", token);
                }
            }
        }
        let token = self.peek()?;

        let definition = if token == Token::SemiColon {
            self.next_token()?;
            false
        } else {
            true
        };

        // have we already seen this function before
        let new_func = if let Some((ref mut found, top)) = self.lookup_symbol(name) {
            if linkage == SymbolLinkage::None {
                linkage = found.linkage.clone();
            }

            if found.linkage != linkage {
                match (&found.linkage, &linkage, explicit_external, top) {
                    (SymbolLinkage::Internal, SymbolLinkage::External, true, _) => {}
                    (SymbolLinkage::External, SymbolLinkage::Internal, _, _) => {
                        bail!(
                            "Function {} linkage mismatch {:?} {:?} {:?} {:?}",
                            name,
                            found,
                            top,
                            linkage,
                            explicit_external
                        )
                    }
                    (_, _, _, false) => {}
                    _ => bail!(
                        "Function {} linkage mismatch {:?} {:?} {:?} {:?}",
                        name,
                        found,
                        top,
                        linkage,
                        explicit_external
                    ),
                }
            }
            match &found.details {
                SymbolDetails::Function { return_type, args } => {
                    // we know this function already exists, so check the args and return type
                    if *return_type != SymbolType::Int {
                        bail!(
                            "Function {} already declared with different return type",
                            name
                        );
                    }
                    if args != *arg_types {
                        bail!("Function {} already declared with different args", name);
                    }
                    if found.state == SymbolState::Defined && top {
                        bail!("Function {} already defined", name);
                    };
                    if definition {
                        found.state = SymbolState::Defined;
                    } else {
                        found.state = SymbolState::Declared;
                    }
                    false
                }
                SymbolDetails::Variable {
                    rename: _,
                    stype: _,
                    value: _,
                } => {
                    println!("{:?} {:?}", top, found);
                    if linkage == SymbolLinkage::External
                        && found.linkage == SymbolLinkage::External
                    {
                        bail!("Function {} already declared as external var", name);
                    }
                    if (found.linkage == SymbolLinkage::External
                        || found.linkage == SymbolLinkage::None)
                        && top
                    {
                        bail!("Function {} already declared as variable1", name);
                    }
                    true
                }
                _ => {
                    if top {
                        bail!("duplicated name in same scope");
                    } else {
                        true
                    }
                }
            }
        } else {
            true
        };
        // None was ued to indicate that nothing was specified in the source
        // now tidy it up if its still none
        if linkage == SymbolLinkage::None {
            linkage = SymbolLinkage::External;
        }
        let symbol = Symbol {
            name: name.to_string(),
            state: if definition {
                SymbolState::Defined
            } else {
                SymbolState::Declared
            },
            details: symbol_det.clone(),
            linkage: linkage.clone(),
            explicit_external,
        };
        if new_func {
            // new function, so add it to the symbol table

            self.insert_global_symbol(name, symbol.clone());
        }
        // if its a declaration we are done

        if !definition {
            match self.lookup_symbol(name) {
                Some((_, true)) => {}
                _ => {
                    self.insert_symbol(name, symbol);
                }
            }
            return Ok(());
        }
        if !body_allowed {
            bail!("Function {} definition not allowed here", name);
        }
        let mut new_names = Vec::new();
        self.local_variables.clear();
        self.local_variables.push(HashMap::new());
        self.in_function_body = true;
        self.push_symbols();
        for arg in arg_names {
            let new_name = self.make_newname(&arg);
            new_names.push(new_name.clone());
            self.local_variables().insert(
                arg.clone(),
                VariableName {
                    name: new_name.clone(),
                    is_current: true,
                },
            );
            let symbol = Symbol {
                name: arg.clone(),
                state: SymbolState::Defined,
                linkage: SymbolLinkage::None,
                details: SymbolDetails::Variable {
                    rename: new_name.to_string(),
                    stype: SymbolType::Int,
                    value: None,
                },
                explicit_external,
            };
            self.insert_symbol(&arg, symbol);
        }
        self.tacky
            .add_function(name, &new_names, linkage == SymbolLinkage::External);

        self.do_function_body()?;
        self.in_function_body = false;
        //
        Ok(())
    }

    fn do_function_body(&mut self) -> Result<()> {
        self.do_block()?;
        if self.labels.iter().any(|l| !l.1) {
            bail!("Undefined label");
        }
        self.instruction(Instruction::Return(Value::Int(0)));
        Ok(())
    }
    fn do_block(&mut self) -> Result<()> {
        self.expect(Token::LeftBrace)?;

        while self.peek()? != Token::RightBrace {
            self.do_block_item()?;
        }
        self.pop_var_map();
        self.pop_symbols();
        self.expect(Token::RightBrace)?;

        Ok(())
    }
    fn do_block_item(&mut self) -> Result<()> {
        self.nest.clear();
        if !self.do_function_or_variable(true, false)? {
            self.do_statement()?;
        }
        Ok(())
    }

    fn do_variable_dec(&mut self, name: &str, specifiers: &Vec<Specifier>) -> Result<()> {
        let token = self.peek()?;
        let spec_ext = specifiers.contains(&Specifier::Extern);
        let spec_static = specifiers.contains(&Specifier::Static);
        let explicit_external = spec_ext;
        if !specifiers.iter().any(|f| matches!(f, Specifier::Type(_))) {
            bail!("missing type");
        }
        let init = if token == Token::Equals {
            self.next_token()?;
            true
        } else {
            false
        };
        #[derive(Debug, Clone, PartialEq)]
        enum Linkage {
            Internal,      // static keyword
            External,      //
            MaybeExternal, // extern keyword
            None,
        }
        // encapulates the table 10-1 first part => linkage
        let mut linkage = match (self.in_function_body, spec_ext, spec_static) {
            (_, true, true) => bail!("Variable cannot be both extern and static"),
            // file scope
            (false, false, false) => Linkage::External,
            (false, false, true) => Linkage::Internal,
            (false, true, false) => Linkage::MaybeExternal,
            // block scope
            (true, false, true) => Linkage::None,
            (true, false, false) => Linkage::None,
            (true, true, false) => Linkage::MaybeExternal,
        };
        // 10-1 second part => state
        let state = match (self.in_function_body, &linkage, init) {
            // file scope
            (false, Linkage::External, true) => SymbolState::Defined,
            (false, Linkage::External, false) => SymbolState::Tentative,
            (false, Linkage::Internal, true) => SymbolState::Defined,
            (false, Linkage::Internal, false) => SymbolState::Tentative,
            (false, Linkage::MaybeExternal, true) => SymbolState::Defined,
            (false, Linkage::MaybeExternal, false) => SymbolState::Declared,
            //block scope
            (true, Linkage::None, _) => SymbolState::Defined,
            (true, Linkage::Internal, _) => SymbolState::Defined,
            (true, Linkage::MaybeExternal, true) => {
                unreachable!("external variable cannot be initialized")
            }
            (true, Linkage::MaybeExternal, false) => SymbolState::Declared,
            _ => unreachable!(),
        };
        // the only bit of storage duration we care about, all else is statc
        let is_auto = self.in_function_body && !spec_ext && !spec_static;
        let is_body_static = self.in_function_body && spec_static;
        println!(
            "state = {:?} linkage = {:?} auto = {:?}",
            state, &linkage, is_auto
        );
        // reniming of local variables
        let rename = if self.in_function_body && (is_auto || spec_static) {
            if let Some(var) = self.local_variables().get(name) {
                if var.is_current {
                    bail!("Variable {} already declared", name);
                }
            }
            let new_name = self.make_newname(name);
            self.local_variables().insert(
                name.to_string(),
                VariableName {
                    name: new_name.to_string(),
                    is_current: true,
                },
            );
            new_name
        } else {
            name.to_string()
        };

        // did we already see a symbol with this name
        let sym_look = self.lookup_symbol(name);
        // fix the maybe external case
        let sym_look = if linkage == Linkage::MaybeExternal {
            if let Some((existing, top)) = &sym_look {
                // detect the
                //    int x;
                //    extern int x;
                // case
                if self.in_function_body && *top && existing.linkage != SymbolLinkage::External
                //&& existing.state != SymbolState::Declared
                {
                    bail!("Variable {} already declared in function", name);
                }

                //  now the
                //   int x;
                //   {
                //      extern int x;
                //    }
                // case
                // its valid but the 'x' refers to a global symbol
                let glob = self.lookup_global_symbol(name);
                if let Some((g, __)) = &glob {
                    if g.linkage == SymbolLinkage::External {
                        linkage = Linkage::External;
                    } else {
                        linkage = Linkage::Internal;
                    }
                } else {
                    linkage = Linkage::External;
                }
                glob
            } else {
                linkage = Linkage::External;
                sym_look
            }
        } else {
            sym_look
        };
        // now convert the linkage here to the proper symbol linkage
        let linkage = match linkage {
            Linkage::Internal => SymbolLinkage::Internal,
            Linkage::External => SymbolLinkage::External,
            Linkage::MaybeExternal => {
                unreachable!("MaybeExternal should not be here")
            }
            Linkage::None => SymbolLinkage::None,
        };

        println!("do_variable_dec {:?} {:?}", sym_look, linkage);
        if !(is_auto || is_body_static) {
            if let Some(ext) = self.externs.get(name) {
                if ext.linkage != linkage {
                    bail!("Variable {} linkage mismatch ", name);
                }
            }
        }
        let (mut symbol, top) = if let Some((symbol, top)) = sym_look {
            if symbol.details.is_function() && top {
                bail!("Variable {} already declared as function", name);
            }
            if symbol.details.is_function()
                && symbol.linkage == SymbolLinkage::External
                && explicit_external
            {
                bail!("Variable {} already declared as external function", name);
            }
            if symbol.state == SymbolState::Defined && top && !explicit_external {
                bail!("Variable {} already defined {:?}", name, symbol);
            }
            if symbol.linkage != linkage && top && symbol.linkage != SymbolLinkage::Internal {
                bail!("Variable {} linkage mismatch", name);
            }

            (Some(symbol), top)
        } else {
            (None, false)
        };
        println!(
            "do_variable_dec {:?} existing = {:?} {}",
            name,
            symbol.clone(),
            top
        );
        let _this_symbol = match (&mut symbol, &linkage) {
            // simple case, we have a new variable, so add it to the symbol table
            (None, _) => {
                let new_sym = Symbol {
                    name: name.to_string(),
                    state: state.clone(),
                    linkage: linkage.clone(),
                    details: SymbolDetails::Variable {
                        rename: rename.to_string(),
                        stype: SymbolType::Int,
                        value: None,
                    },
                    explicit_external,
                };
                if linkage == SymbolLinkage::External && !self.in_function_body {
                    self.insert_global_symbol(name, new_sym.clone());
                } else {
                    self.insert_symbol(name, new_sym.clone());
                };
                new_sym
            }
            (Some(existing), SymbolLinkage::External) => {
                let x = match existing.linkage {
                    SymbolLinkage::External => {
                        if top && linkage != SymbolLinkage::External {
                            bail!("Variable {} already declared as external", name);
                        }
                        existing.clone()
                    }
                    SymbolLinkage::Internal => {
                        if explicit_external {
                        } else if top {
                            bail!("Variable {} already declared as static", name);
                        }
                        existing.clone()
                    }
                    SymbolLinkage::None => {
                        let new_sym = Symbol {
                            name: name.to_string(),
                            state: if existing.state == SymbolState::Defined {
                                SymbolState::Defined
                            } else {
                                state.clone()
                            },
                            linkage: linkage.clone(),
                            details: SymbolDetails::Variable {
                                rename: rename.to_string(),
                                stype: SymbolType::Int,
                                value: None,
                            },
                            explicit_external,
                        };
                        self.insert_global_symbol(name, new_sym.clone());
                        new_sym.clone()
                    }
                };
                if self.in_function_body {
                    let pull_sym = Symbol {
                        state: SymbolState::Declared,
                        name: name.to_string(),
                        linkage: linkage.clone(),
                        details: SymbolDetails::ScopePull,
                        explicit_external,
                    };

                    self.insert_symbol(name, pull_sym.clone());
                    pull_sym.clone()
                } else {
                    x
                }
            }
            (Some(_symbol), SymbolLinkage::None) => {
                let new_sym = Symbol {
                    state: state.clone(),
                    name: name.to_string(),
                    linkage: linkage.clone(),
                    details: SymbolDetails::Variable {
                        rename: rename.clone(),
                        stype: SymbolType::Int,
                        value: None,
                    },
                    explicit_external,
                };
                self.insert_symbol(name, new_sym.clone());
                self.dump_symbols();
                new_sym.clone()
            }

            (Some(symbol), _) => {
                symbol.state = SymbolState::Declared;
                symbol.clone()
            }
        };

        if init {
            let val = self.do_expression(0)?;
            if self.in_function_body && is_auto {
                self.instruction(Instruction::Copy(val, Value::Variable(rename)));
            } else if let Value::Int(_) = val {
                if !is_auto {
                    self.externs
                        .entry(rename.to_string())
                        .and_modify(|e| {
                            e.state = SymbolState::Defined;
                            e.value = Some(val.clone());
                        })
                        .or_insert(Extern {
                            name: rename.to_string(),
                            linkage: linkage.clone(),
                            state: SymbolState::Defined,
                            value: Some(val),
                            stype: SymbolType::Int,
                        });
                }
            } else {
                bail!("Static variable must be initialized to a constant");
            }
        } else if !is_auto {
            self.externs
                .entry(rename.to_string())
                .and_modify(|e| {
                    if e.state != SymbolState::Defined && state != SymbolState::Declared {
                        e.state = SymbolState::Defined
                    }
                })
                .or_insert(Extern {
                    name: rename.to_string(),
                    linkage: linkage.clone(),
                    state, //SymbolState::Defined,
                    value: None,
                    stype: SymbolType::Int,
                });
        }
        self.dump_symbols();

        self.expect(Token::SemiColon)?;
        Ok(())
    }

    fn do_statement(&mut self) -> Result<()> {
        let token = self.peek()?;
        match token {
            Token::For => self.do_for()?,
            Token::While => self.do_while()?,
            Token::Do => self.do_do_while()?,
            Token::Break => self.do_break()?,
            Token::Continue => self.do_continue()?,
            Token::Return => self.do_return()?,
            Token::If => self.do_if()?,
            Token::GoTo => self.do_goto()?,
            Token::Switch => self.do_switch()?,
            Token::Case => self.do_case()?,
            Token::Default => self.do_default()?,

            Token::SemiColon => {
                self.next_token()?;
            }

            Token::LeftBrace => {
                self.push_new_varmap();
                self.push_symbols();
                self.do_block()?;
            }
            _ => {
                if matches!(token, Token::Identifier(_)) && self.peek_n(1)? == Token::Colon {
                    self.do_label()?;
                    self.do_statement()?;
                } else {
                    self.do_expression(0)?;
                    self.expect(Token::SemiColon)?;
                }
            }
        };
        Ok(())
    }
    fn do_for(&mut self) -> Result<()> {
        self.push_new_varmap();
        self.push_symbols();
        self.next_token()?;

        let label_end = self.make_label("end_for");
        let label_start = self.make_label("start_for");
        let label_body = self.make_label("body_for");
        let label_inc = self.make_label("inc_for");

        self.break_label_stack.push(label_end.clone());
        self.continue_label_stack.push(label_inc.clone());

        self.expect(Token::LeftParen)?;
        let token = self.peek()?;

        // init clause?
        match token {
            Token::Int => {
                // eats the semi colon
                self.do_function_or_variable(false, false)?;
            }
            Token::SemiColon => {
                self.next_token()?;
            }
            _ => {
                self.do_expression(0)?;
                self.expect(Token::SemiColon)?;
            }
        }
        // ==================== START:
        // condition clause
        self.instruction(Instruction::Label(label_start.clone()));
        let token = self.peek()?;
        let cond = if token == Token::SemiColon {
            Value::Int(1)
        } else {
            self.do_expression(0)?
        };
        self.instruction(Instruction::JumpIfZero(cond, label_end.clone()));
        self.expect(Token::SemiColon)?;
        // ===================== goto BODY
        self.instruction(Instruction::Jump(label_body.clone()));
        // increment clause
        // ===================== INC:
        self.instruction(Instruction::Label(label_inc.clone()));
        let token = self.peek()?;
        if token != Token::RightParen {
            self.do_expression(0)?;
        }
        self.expect(Token::RightParen)?;
        // ===================== goto START
        self.instruction(Instruction::Jump(label_start.clone()));
        // ===================== BODY:
        self.instruction(Instruction::Label(label_body.clone()));
        self.do_statement()?;
        // ===================== goto INC
        self.instruction(Instruction::Jump(label_inc.clone()));
        // ===================== END:
        self.instruction(Instruction::Label(label_end.clone()));
        self.pop_var_map();
        self.pop_symbols();
        self.continue_label_stack.pop();
        self.break_label_stack.pop();
        Ok(())
    }
    fn do_while(&mut self) -> Result<()> {
        self.next_token()?;

        let label_start = self.make_label("start_while");
        let label_end = self.make_label("end_while");
        self.break_label_stack.push(label_end.clone());
        self.continue_label_stack.push(label_start.clone());

        self.instruction(Instruction::Label(label_start.clone()));
        self.expect(Token::LeftParen)?;
        let cond = self.do_expression(0)?;
        self.instruction(Instruction::JumpIfZero(cond, label_end.clone()));
        self.expect(Token::RightParen)?;
        self.do_statement()?;
        self.instruction(Instruction::Jump(label_start.clone()));
        self.instruction(Instruction::Label(label_end.clone()));
        self.continue_label_stack.pop();
        self.break_label_stack.pop();

        Ok(())
    }
    fn do_do_while(&mut self) -> Result<()> {
        self.next_token()?;

        let label_start = self.make_label("start_do_while");
        let label_end = self.make_label("end_do_while");
        let label_cond = self.make_label("cond_do_while");
        self.continue_label_stack.push(label_cond.clone());
        self.break_label_stack.push(label_end.clone());

        self.instruction(Instruction::Label(label_start.clone()));
        self.do_statement()?;
        self.expect(Token::While)?;
        self.expect(Token::LeftParen)?;
        self.instruction(Instruction::Label(label_cond.clone()));
        let cond = self.do_expression(0)?;
        self.instruction(Instruction::JumpIfZero(cond, label_end.clone()));
        self.expect(Token::RightParen)?;
        self.expect(Token::SemiColon)?;
        self.instruction(Instruction::Jump(label_start.clone()));
        self.instruction(Instruction::Label(label_end.clone()));
        self.continue_label_stack.pop();
        self.break_label_stack.pop();

        Ok(())
    }
    fn do_break(&mut self) -> Result<()> {
        self.next_token()?;
        if let Some(break_label) = self.break_label_stack.last() {
            self.instruction(Instruction::Jump(break_label.clone()));
        } else {
            bail!("Break outside of loop/switch");
        }

        self.expect(Token::SemiColon)?;
        Ok(())
    }
    fn do_continue(&mut self) -> Result<()> {
        self.next_token()?;
        if let Some(cont_label) = self.continue_label_stack.last() {
            self.instruction(Instruction::Jump(cont_label.clone()));
        } else {
            bail!("continue outside of loop");
        }
        self.expect(Token::SemiColon)?;
        Ok(())
    }
    fn do_goto(&mut self) -> Result<()> {
        self.next_token()?;
        let label = expect!(self, Token::Identifier);

        let label = self.gen_label(&label);
        if self.labels.get(&label).is_some() {
        } else {
            self.labels.insert(label.clone(), false);
        }

        self.instruction(Instruction::Jump(label));
        self.expect(Token::SemiColon)?;
        Ok(())
    }
    fn do_return(&mut self) -> Result<()> {
        self.next_token()?;
        let val = self.do_expression(0)?;
        self.instruction(Instruction::Return(val));
        self.expect(Token::SemiColon)?;

        Ok(())
    }
    fn do_label(&mut self) -> Result<()> {
        let label = expect!(self, Token::Identifier);
        let label = self.gen_label(&label);
        self.expect(Token::Colon)?;
        self.instruction(Instruction::Label(label.clone()));

        let mut dup = false;
        self.labels
            .entry(label.clone())
            .and_modify(|l| {
                if *l {
                    // bail!("duplicate label");
                    dup = true;
                }
                *l = true
            })
            .or_insert(true);
        if dup {
            bail!("duplicate label");
        }
        Ok(())
    }

    fn gen_label(&self, name: &str) -> String {
        let label = format!("__{}${}__", self.current_function_name, name);
        label
    }
    fn do_if(&mut self) -> Result<()> {
        self.next_token()?;
        self.expect(Token::LeftParen)?;
        let cond = self.do_expression(0)?;
        self.expect(Token::RightParen)?;
        let label_false = self.make_label("if_false");
        let label_end = self.make_label("if_end");
        self.instruction(Instruction::JumpIfZero(cond, label_false.clone()));
        self.do_statement()?;
        self.instruction(Instruction::Jump(label_end.clone()));
        self.instruction(Instruction::Label(label_false.clone()));
        if self.peek()? == Token::Else {
            self.next_token()?;
            self.do_statement()?;
        }
        self.instruction(Instruction::Label(label_end.clone()));
        Ok(())
    }
    fn do_switch(&mut self) -> Result<()> {
        self.next_token()?;
        self.expect(Token::LeftParen)?;

        // where 'break' will go
        let label_end = self.make_label("switch_end");
        self.break_label_stack.push(label_end.clone());

        // evaluate the condition once

        let value = self.do_expression(0)?;
        self.expect(Token::RightParen)?;

        let next_drop_thru_label = self.make_label("next_drop_thru");
        let next_case_label = self.make_label("next_case");
        // emit here otherwise the 'before first case' suppression gets it
        self.instruction(Instruction::Jump(next_case_label.clone()));
        self.switch_context_stack.push(SwitchContext {
            cases: Vec::new(),
            value,
            default_statement_seen: false,
            before_first_case: true,

            // bootstrap the label chain
            next_drop_thru_label: next_drop_thru_label.clone(),
            next_case_label: next_case_label.clone(),
        });
        // the entire body of the switch statement is handled by this call
        // including the cases and breaks
        self.do_statement()?;

        // at the end of the switch, add the exit label
        // plus any pending labels
        self.instruction(Instruction::Label(label_end.clone()));
        let last_case_label = self
            .switch_context_stack
            .last()
            .unwrap()
            .next_case_label
            .clone();
        if !last_case_label.is_empty() {
            self.instruction(Instruction::Label(last_case_label));
        }

        self.switch_context_stack.pop();
        self.break_label_stack.pop();
        Ok(())
    }

    fn do_case(&mut self) -> Result<()> {
        self.next_token()?;
        self.do_case_or_default(true)
    }
    fn do_case_or_default(&mut self, is_case: bool) -> Result<()> {
        // case and default are almost identical
        // just default has no compare in it

        // a case generates
        //  goto this_drop_thru;
        //  label this_case_label;
        //  if (case_value == switch_value) goto next_case_label;
        //  label this_drop_thru_label;
        // then update thisxx labels to nextxx

        let case_value = if is_case {
            let cv = self.do_expression(0)?;
            if !matches!(cv, Value::Int(_)) {
                bail!("Switch value must be an integer");
            }
            Some(cv)
        } else {
            None
        };
        self.expect(Token::Colon)?;

        let next_case_label = self.make_label("next_case");
        let next_drop_thru_label = self.make_label("next_drop_thru");
        let (switch_value, this_case_label, this_drop_thru_label) =
            if let Some(context) = self.switch_context_stack.last_mut() {
                // check for duplicate case values
                if let Some(ref cv) = case_value {
                    if context.cases.contains(cv) {
                        bail!("Duplicate case value");
                    }
                    context.cases.push(cv.clone());
                }
                context.before_first_case = false;

                let this_case_label = context.next_case_label.clone();
                let this_drop_thru_label = context.next_drop_thru_label.clone();
                context.next_case_label = next_case_label.clone();
                context.next_drop_thru_label = next_drop_thru_label.clone();
                (context.value.clone(), this_case_label, this_drop_thru_label)
            } else {
                bail!("Case outside of switch");
            };
        self.instruction(Instruction::Jump(this_drop_thru_label.clone()));
        self.instruction(Instruction::Label(this_case_label.clone()));

        // condition or not? ie case vs default
        if let Some(cv) = case_value {
            let dest = Value::Variable(self.make_temporary());
            self.instruction(Instruction::Binary(
                BinaryOperator::Equal,
                cv,
                switch_value,
                dest.clone(),
            ));
            self.instruction(Instruction::JumpIfZero(dest, next_case_label));
        }
        self.instruction(Instruction::Label(this_drop_thru_label.clone()));
        self.do_statement()?;
        Ok(())
    }
    fn do_default(&mut self) -> Result<()> {
        self.next_token()?;

        if let Some(context) = self.switch_context_stack.last_mut() {
            if context.default_statement_seen {
                bail!("Duplicate default statement");
            }
            context.default_statement_seen = true;
        } else {
            bail!("Default outside of switch");
        };
        self.do_case_or_default(false)?;
        Ok(())
    }
    pub(crate) fn is_lvalue(&mut self, value: &Value) -> bool {
        match value {
            Value::Variable(var) => self.local_variables().values().any(|v| v.name == *var),
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

    pub(crate) fn make_temporary(&mut self) -> String {
        let temp = format!("$temp${}", self.next_temporary);
        self.next_temporary += 1;
        temp
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

    pub(crate) fn instruction(&mut self, instruction: Instruction) {
        let skip_before_case = if let Some(context) = self.switch_context_stack.last() {
            !matches!(instruction, Instruction::Label(_)) && context.before_first_case
        } else {
            false
        };
        if skip_before_case {
            println!("skip_before_case {:?}", instruction);
            return;
        }
        println!("{}instruction {:?}", self.nest, instruction);
        self.tacky.add_instruction(instruction);
    }
    pub(crate) fn next_token(&mut self) -> Result<Token> {
        let token = if !self.peeked_tokens.is_empty() {
            self.peeked_tokens.pop_front().unwrap()
        } else {
            self.lexer.next_token()?
        };
        if token == Token::Eof {
            self.eof_hit = true;
        }

        Ok(token)
    }
    fn dump_var_map(&self) {
        println!("=======var map dump=======");
        for (i, map) in self.local_variables.iter().enumerate() {
            let pad = format!("{empty:>width$}", empty = "", width = i * 4);
            for (name, var) in map.iter() {
                println!("{} {:?} {:?}", pad, name, var);
            }
        }
        println!("=======end var map dump=======");
    }
    fn push_new_varmap(&mut self) {
        let newmap = self
            .local_variables()
            .iter()
            .map(|(k, v)| {
                let mut v = v.clone();
                v.is_current = false;
                (k.clone(), v.clone())
            })
            .collect();
        self.local_variables.push(newmap);
    }
    fn pop_var_map(&mut self) {
        println!("pop_var_map {:?}", self.local_variables.len());
        self.dump_var_map();
        self.local_variables.pop();
    }
    pub(crate) fn local_variables(&mut self) -> &mut HashMap<String, VariableName> {
        let len = self.local_variables.len();
        self.local_variables.get_mut(len - 1).unwrap()
    }
    pub(crate) fn peek(&mut self) -> Result<Token> {
        self.peek_n(0)
    }
    pub(crate) fn lookup_symbol(&mut self, name: &str) -> Option<(Symbol, bool)> {
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
    pub(crate) fn lookup_global_symbol(&mut self, name: &str) -> Option<(Symbol, bool)> {
        if let Some(sym) = self.symbol_stack[0].get(name) {
            return Some((sym.clone(), self.symbol_stack.len() == 1));
        }
        None
    }
    fn insert_global_symbol(&mut self, name: &str, symbol: Symbol) {
        println!("insert_global_symbol {:?}", symbol);
        //   debug_assert!(!self.symbol_stack[0].contains_key(name));
        self.symbol_stack
            .get_mut(0)
            .unwrap()
            .insert(name.to_string(), symbol);
    }
    fn insert_symbol(&mut self, name: &str, symbol: Symbol) {
        println!("insert_symbol {:?}", symbol);
        self.symbol_stack
            .last_mut()
            .unwrap()
            .insert(name.to_string(), symbol);
    }

    fn pop_symbols(&mut self) {
        //  self.dump_symbols();
        println!("pop_symbols {:?}", self.symbol_stack.len());
        self.symbol_stack.pop();
    }
    fn push_symbols(&mut self) {
        println!("push_symbols {:?}", self.symbol_stack.len());
        self.symbol_stack.push(HashMap::new());
    }
    pub(crate) fn peek_n(&mut self, n: usize) -> Result<Token> {
        if n >= self.peeked_tokens.len() {
            for _ in 0..n + 1 {
                let token = self.lexer.next_token()?;
                self.peeked_tokens.push_back(token.clone());
            }
        }
        Ok(self.peeked_tokens[n].clone())
    }
    fn dump_symbols(&self) {
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
    //https://www.reddit.com/r/rust/comments/6ojuxz/comment/dki11oe/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
    fn _previous_symbol(level: u32) -> Option<BacktraceSymbol> {
        let (trace, _curr_file, _curr_line) = (Backtrace::new(), file!(), line!());
        let frames = trace.frames();
        frames
            .iter()
            .flat_map(BacktraceFrame::symbols)
            // .skip_while(|s| {
            //     s.filename()
            //         .map(|p| !p.ends_with(curr_file))
            //         .unwrap_or(true)
            //         || s.lineno() != Some(curr_line)
            // })
            .nth(1 + level as usize)
            .cloned()
    }
}
