use crate::{declarator::indent, tacky::StaticInit};
use anyhow::{bail, Result};
use backtrace::{Backtrace, BacktraceFrame, BacktraceSymbol};
use enum_as_inner::EnumAsInner;
use std::{
    collections::{HashMap, VecDeque},
    path::{Path, PathBuf},
    vec,
};

use crate::{
    declarator::INDENT,
    expect,
    lexer::{Lexer, Token},
    symbols::{Extern, Specifiers, Symbol, SymbolLinkage, SymbolState, SymbolType, VariableName},
    tacky::{BinaryOperator, Instruction, TackyProgram, Value},
};

struct SwitchContext {
    cases: Vec<Value>,
    value: Value,
    before_first_case: bool,
    default_statement_seen: bool,

    next_case_label: String,
    next_drop_thru_label: String,
}

pub struct Parser {
    lexer: Lexer,
    source_file: PathBuf,
    pub tacky: TackyProgram,
    pub next_temporary: usize,
    eof_hit: bool,
    peeked_tokens: VecDeque<Token>,
    // original name => new name
    local_variables: Vec<HashMap<String, VariableName>>,
    labels: HashMap<String, bool>,
    pub next_name: usize,
    continue_label_stack: Vec<String>,
    switch_context_stack: Vec<SwitchContext>,
    break_label_stack: Vec<String>,
    pub(crate) symbol_stack: Vec<HashMap<String, Symbol>>,
    current_function_name: String,
    in_function_body: bool,
    pub externs: HashMap<String, Extern>,
    pub static_init: bool,
}
#[derive(Debug, Clone, PartialEq, EnumAsInner)]
enum Initializer {
    SingleInit(Value),
    CompoundInit(Vec<Initializer>),
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

            continue_label_stack: Vec::new(),
            switch_context_stack: Vec::new(),
            break_label_stack: Vec::new(),
            symbol_stack: vec![HashMap::new()],
            current_function_name: String::new(),
            in_function_body: false,
            externs: HashMap::new(),
            static_init: false,
        }
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
                    println!("{:?}", e);
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
                &ext.stype,
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

        let (specifiers, func_dec_name, stype, param_names) = self.parse_declaration()?;

        if !specifiers.is_static && !specifiers.is_external && specifiers.specified_type.is_none() {
            // its a statement, only allowed in functions
            if !self.in_function_body {
                bail!("expected function or variable");
            }
            return Ok(false);
        }
        if !func_allowed && (specifiers.is_external || specifiers.is_static) {
            bail!("static or external not allowed here");
        }

        println!("Function or dec: {}", func_dec_name);
        if specifiers.specified_type.is_none() {
            bail!("type not specified for {}", func_dec_name);
        }

        if !stype.is_function() {
            self.do_variable_dec(&func_dec_name, &specifiers, &stype)?;
        } else {
            if !func_allowed {
                bail!("Function declaration not allowed here");
            }
            self.do_function(
                &func_dec_name,
                &specifiers,
                body_allowed,
                &stype,
                param_names,
            )?;
        }
        Ok(true)
    }
    fn do_function(
        &mut self,
        name: &str,
        specifiers: &Specifiers,
        body_allowed: bool,
        stype: &SymbolType,
        param_names: Vec<String>,
    ) -> Result<()> {
        let explicit_external = specifiers.is_external;
        // table 10.2
        let mut linkage = match (
            specifiers.is_external,
            specifiers.is_static,
            self.in_function_body,
        ) {
            (true, true, _) => bail!("Function cannot be both extern and static"),
            (true, _, _) => SymbolLinkage::External,
            (_, true, false) => SymbolLinkage::Internal,
            (_, true, true) => bail!("Function cannot be static"),
            (false, false, false) => SymbolLinkage::None,

            (false, false, true) => SymbolLinkage::External,
        };

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
            match &found.stype {
                SymbolType::Function(args, return_type) => {
                    // we know this function already exists, so check the args and return type
                    if **return_type != **stype.as_function().unwrap().1 {
                        bail!(
                            "Function {}  already declared with different return type",
                            name
                        );
                    }
                    if args != stype.as_function().unwrap().0 {
                        bail!("Function {} already declared with different args", name);
                    }
                    if found.state == SymbolState::Defined && top && definition {
                        bail!("Function {} already defined", name);
                    };
                    if definition {
                        found.state = SymbolState::Defined;
                    } else {
                        found.state = SymbolState::Declared;
                    }
                    false
                }
                &SymbolType::Pointer(_) => {
                    bail!("Function {} already declared as pointer", name);
                }
                SymbolType::Double
                | SymbolType::Int32
                | SymbolType::Int64
                | SymbolType::UInt32
                | SymbolType::Char
                | SymbolType::SChar
                | SymbolType::UChar
                | SymbolType::UInt64 => {
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
                SymbolType::Array(_, _) => {
                    bail!("Function {} already declared as array", name);
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
            stype: stype.clone(),
            rename: name.to_string(),
            linkage: linkage.clone(),
            explicit_external,
            scope_pull: false,
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
        self.current_function_name = name.to_string();
        let mut new_names = Vec::new();
        self.local_variables.clear();
        self.local_variables.push(HashMap::new());
        self.in_function_body = true;
        self.push_symbols();
        for (arg_name, arg_type) in param_names
            .iter()
            .zip(symbol.stype.into_function().unwrap().0.iter())
        {
            let new_name = self.make_newname(arg_name);
            new_names.push((new_name.clone(), arg_type.clone()));
            self.local_variables().insert(
                arg_name.to_string(),
                VariableName {
                    name: new_name.clone(),
                    is_current: true,
                },
            );
            let symbol = Symbol {
                name: arg_name.to_string(),
                state: SymbolState::Defined,
                linkage: SymbolLinkage::None,

                rename: new_name.to_string(),
                stype: arg_type.clone(),
                explicit_external,
                scope_pull: false,
            };
            self.insert_symbol(arg_name, symbol);
        }

        self.tacky.add_function(
            name,
            &new_names,
            linkage == SymbolLinkage::External,
            *stype.as_function().unwrap().1.clone(),
        );

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
        self.instruction(Instruction::Return(Value::Int32(0)));
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
        if !self.do_function_or_variable(true, false)? {
            self.do_statement()?;
        }
        Ok(())
    }

    fn do_variable_dec(
        &mut self,
        name: &str,
        specifiers: &Specifiers,
        symbol_type: &SymbolType,
    ) -> Result<()> {
        let spec_ext = specifiers.is_external;
        let spec_static = specifiers.is_static;
        let explicit_external = spec_ext;
        if specifiers.specified_type.is_none() {
            bail!("missing type");
        }
        let symbol_type = symbol_type.clone(); //specifiers.specified_type.clone().unwrap().clone();
        let token = self.peek()?;
        let init = if token == Token::Assign {
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
        let (mut found_symbol, top) = if let Some((symbol, top)) = sym_look {
            if symbol.stype.is_function() && top {
                bail!("Variable {} already declared as function", name);
            }
            if symbol.stype.is_function()
                && symbol.linkage == SymbolLinkage::External
                && explicit_external
            {
                bail!("Variable {} already declared as external function", name);
            }
            if symbol.state == SymbolState::Defined && top && !explicit_external {
                if symbol.stype != symbol_type {
                    bail!("Variable {} already defined with different type", name);
                }
                if init {
                    bail!("Variable {} already defined {:?}", name, symbol);
                }
            }
            if symbol.linkage != linkage && top && symbol.linkage != SymbolLinkage::Internal {
                bail!("Variable {} linkage mismatch", name);
            }

            if symbol.stype != symbol_type && (top || explicit_external) {
                bail!(
                    "Variable {} type mismatch {:?} {:?}",
                    name,
                    symbol.stype,
                    symbol_type
                );
            }

            (Some(symbol), top)
        } else {
            (None, false)
        };
        println!(
            "do_variable_dec {:?} existing = {:?} {}",
            name,
            found_symbol.clone(),
            top
        );
        let _this_symbol = match (&mut found_symbol, &linkage) {
            // simple case, we have a new variable, so add it to the symbol table
            (None, _) => {
                let new_sym = Symbol {
                    name: name.to_string(),
                    state: state.clone(),
                    linkage: linkage.clone(),

                    rename: rename.to_string(),
                    stype: symbol_type.clone(),
                    //  value: None,
                    explicit_external,
                    scope_pull: false,
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

                            rename: rename.to_string(),
                            stype: symbol_type.clone(),
                            // value: None,
                            explicit_external,
                            scope_pull: false,
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

                        explicit_external,
                        rename: rename.to_string(),
                        stype: symbol_type.clone(),
                        scope_pull: true,
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

                    rename: rename.clone(),
                    stype: symbol_type.clone(),
                    explicit_external,
                    scope_pull: false,
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

        // get the initializer value  - if any

        let init_value: Vec<StaticInit> = if init {
            self.do_initializer(&rename, &symbol_type, is_auto)?
        } else {
            vec![]
        };

        // update the static / externs table

        if !is_auto {
            self.externs
                .entry(rename.to_string())
                // already exists - update it
                .and_modify(|e| {
                    println!("update extern {:?} {:?} {:?}", rename, e, init_value);
                    match (init_value.is_empty(), e.state.clone()) {
                        (true, SymbolState::Tentative | SymbolState::Declared) => {
                            if state != SymbolState::Declared {
                                e.state = SymbolState::Defined
                            };
                        }
                        (false, _) => {
                            e.state = SymbolState::Defined;
                            e.value = init_value.clone();
                        }
                        (_, _) => {}
                    }
                })
                // new extern - insert it
                .or_insert_with(|| {
                    let new = Extern {
                        name: rename.to_string(),
                        linkage: linkage.clone(),
                        state: if init_value.is_empty() {
                            state
                        } else {
                            SymbolState::Defined
                        },
                        value: init_value,
                        stype: symbol_type.clone(),
                    };
                    println!("insert extern {:?} ", new);
                    new
                });
        }
        self.dump_symbols();

        self.expect(Token::SemiColon)?;
        Ok(())
    }

    pub fn make_static_string(&mut self, value: &Value) -> Result<String> {
        let str = value.as_string().unwrap().clone();
        let sname = format!("string${}", self.tacky.static_constants.len());
        self.tacky.add_static_constant(
            &sname,
            vec![StaticInit::InitString(str.to_owned(), true)],
            false,
            false,
            &SymbolType::Array(Box::new(SymbolType::Char), str.len() + 1),
        );
        Ok(sname)
    }

    fn do_initializer(
        &mut self,
        name: &str,
        dest_type: &SymbolType,
        is_auto: bool,
    ) -> Result<Vec<StaticInit>> {
        let init = self.parse_initializer(is_auto, dest_type)?;
        // simple initializer or compound
        // assign via static or auto
        println!("do_initializer {:?} {:?} {:?}", name, init, dest_type);

        let vals = match (is_auto, &init) {
            (true, Initializer::SingleInit(v)) => {
                // int x = 42;
                if v.is_string() {
                    if dest_type.is_pointer() {
                        if !dest_type.get_inner_type()?.is_char() {
                            bail!("variable is not char type");
                        }
                        // char *str = "hello";

                        let sname = self.make_static_string(v)?;
                        self.instruction(Instruction::GetAddress(
                            Value::Variable(sname, SymbolType::Pointer(Box::new(SymbolType::Char))),
                            Value::Variable(name.to_owned(), dest_type.clone()),
                        ));
                    } else {
                        // char str[6] = "hello";
                        let str = v.as_string().unwrap().clone();
                        if !dest_type.get_inner_type()?.is_character() {
                            bail!("variable is not char type");
                        }
                        let mut offset = 0;
                        let size = dest_type.as_array().unwrap().1;
                        if str.len() > *size {
                            bail!("String initializer too long")
                        }
                        for ch in str.chars() {
                            self.instruction(Instruction::CopyToOffset(
                                Value::Char(ch as i8),
                                Value::Variable(name.to_owned(), dest_type.clone()),
                                offset,
                            ));
                            offset += 1;
                        }
                        if str.len() < *size {
                            self.instruction(Instruction::CopyToOffset(
                                Value::Char(0_i8),
                                Value::Variable(name.to_owned(), dest_type.clone()),
                                offset,
                            ));
                        }
                    }
                } else {
                    let converted = self.convert_by_assignment(v, &dest_type.clone())?;
                    self.instruction(Instruction::Copy(
                        converted.clone(),
                        Value::Variable(name.to_owned(), dest_type.clone()),
                    ));
                }
                vec![]
            }
            (false, Initializer::SingleInit(v)) => {
                // static int = 42;
                if v.is_variable() {
                    bail!("Static variable must be initialized to a constant");
                }
                if v.is_string() {
                    if dest_type.is_pointer() {
                        // static char *str = "hello";
                        if !dest_type.get_inner_type()?.is_char() {
                            bail!("variable is not char type");
                        }

                        let sname = self.make_static_string(v)?;
                        let ptr = StaticInit::PointerInit(sname);
                        vec![ptr]
                    } else {
                        // static char str[6] = "hello";
                        let str = v.as_string().unwrap().clone();
                        if !dest_type.get_inner_type()?.is_character() {
                            bail!("variable is not char type");
                        }
                        let mut res = vec![StaticInit::InitString(str.to_owned(), false)];
                        let size = dest_type.as_array().unwrap().1;
                        if str.len() > *size {
                            bail!("String initializer too long")
                        }
                        for _ in str.len()..*size {
                            res.push(StaticInit::InitChar(0_i8));
                        }
                        res
                    }
                } else {
                    let converted = if dest_type.is_pointer() && Self::is_null_pointer_constant(v) {
                        StaticInit::InitU32(0)
                    } else {
                        let con = self.convert_by_assignment(v, dest_type)?;
                        self.value_to_staticinit(&con)?
                    };

                    vec![converted]
                }
            }
            (true, Initializer::CompoundInit(_)) => {
                //  {
                //      int x[3] = {1,2,3};
                //  }
                if !dest_type.is_array() {
                    bail!("Compound initializer not allowed here")
                }

                unsafe { INDENT = 0 }
                let values = Self::unwind_nested(Some(init), dest_type)?;
                let elem_type = Self::get_inner_array_type(dest_type)?;
                let mut offset = 0;
                for v in &values {
                    let converted = if v.is_string() {
                        if !elem_type.is_pointer() {
                            bail!("String initializer not allowed here")
                        }
                        if !elem_type.get_inner_type()?.is_char() {
                            bail!("variable is not char type");
                        }
                        let sname = self.make_static_string(v)?;
                        let straddr =
                            self.make_temporary(&SymbolType::Pointer(Box::new(SymbolType::Char)));
                        self.instruction(Instruction::GetAddress(
                            Value::Variable(sname, SymbolType::Pointer(Box::new(SymbolType::Char))),
                            straddr.clone(),
                        ));
                        straddr
                    } else {
                        self.convert_by_assignment(v, &elem_type)?
                    };
                    self.instruction(Instruction::CopyToOffset(
                        converted.clone(),
                        Value::Variable(name.to_owned(), dest_type.clone()),
                        offset,
                    ));
                    let size = Self::get_size_of_stype(&elem_type);
                    offset += size;
                }
                vec![]
            }
            (false, Initializer::CompoundInit(_)) => {
                // static int x[3] = {1,2,3};

                if !dest_type.is_array() {
                    bail!("Compound initializer not allowed here")
                }

                let elem_type = Self::get_inner_array_type(dest_type)?;
                let init_values = Self::unwind_nested(Some(init), dest_type)?;
                let mut values = Vec::new();
                for v in init_values {
                    let converted = if v.is_string() {
                        if !elem_type.is_pointer() {
                            bail!("String initializer not allowed here")
                        }
                        if !elem_type.get_inner_type()?.is_char() {
                            bail!("variable is not char type");
                        }
                        let sname = self.make_static_string(&v)?;
                        StaticInit::PointerInit(sname)
                    } else if dest_type.is_pointer() && Self::is_null_pointer_constant(&v) {
                        StaticInit::InitU32(0)
                    } else {
                        let con = self.convert_by_assignment(&v, &elem_type)?;
                        self.value_to_staticinit(&con)?
                    };
                    values.push(converted);
                }

                values
            }
        };
        Ok(vals)
    }
    fn value_to_staticinit(&mut self, value: &Value) -> Result<StaticInit> {
        match value {
            Value::Int32(v) => Ok(StaticInit::InitI32(*v)),
            Value::Int64(v) => Ok(StaticInit::InitI64(*v)),
            Value::UInt32(v) => Ok(StaticInit::InitU32(*v)),
            Value::UInt64(v) => Ok(StaticInit::InitU64(*v)),
            Value::Char(v) => Ok(StaticInit::InitChar(*v)),
            Value::UChar(v) => Ok(StaticInit::InitUChar(*v)),
            Value::String(s) => Ok(StaticInit::InitString(s.clone(), true)),
            Value::Double(v) => Ok(StaticInit::InitDouble(*v)),
            _ => bail!("Expected integer, got {:?}", value),
        }
    }
    fn unwind_nested(compound_init: Option<Initializer>, stype: &SymbolType) -> Result<Vec<Value>> {
        indent(&format!("unwind_nested {:?} {:?} ", compound_init, stype));
        let compound_init = if let Some(ci) = compound_init {
            if let Initializer::SingleInit(v) = ci {
                if v.is_string() && stype.is_array() {
                    let s = v.as_string().unwrap().clone();
                    let chars = s
                        .chars()
                        .map(|ch| Initializer::SingleInit(Value::Char(ch as i8)))
                        .collect::<Vec<_>>();
                    Some(Initializer::CompoundInit(chars))
                } else {
                    Some(Initializer::SingleInit(v.clone()))
                }
            } else {
                Some(ci.clone())
            }
        } else {
            compound_init
        };
        let ret = match compound_init {
            Some(Initializer::SingleInit(v)) => Ok(vec![v.clone()]),
            Some(Initializer::CompoundInit(v)) => {
                let mut result = Vec::new();
                let inner = Self::get_inner_type(stype).unwrap();
                let mut this_array_size = Self::get_array_size(stype).unwrap();
                indent(&format!(
                    "unwinding arr size {} v.len{} ",
                    this_array_size,
                    v.len()
                ));
                for init in v {
                    if this_array_size == 0 {
                        bail!("Array size mismatch")
                    }
                    this_array_size -= 1;
                    indent(&format!("unwind this size {}", this_array_size));
                    unsafe { INDENT += 1 };
                    result.extend(Self::unwind_nested(Some(init), &inner)?);
                    unsafe { INDENT -= 1 };
                }
                for i in 0..this_array_size {
                    indent(&format!("padding 0 to unwind {:?}", i));
                    unsafe { INDENT += 1 };

                    result.extend(Self::unwind_nested(None, &inner)?);
                    unsafe { INDENT -= 1 };
                }

                Ok(result)
            }
            None => {
                if stype.is_array() {
                    let mut result = Vec::new();
                    let inner = Self::get_inner_type(stype).unwrap();
                    let this_array_size = Self::get_array_size(stype).unwrap();
                    indent(&format!("unwind this size {}", this_array_size));
                    for _ in 0..this_array_size {
                        unsafe { INDENT += 1 };

                        result.extend(Self::unwind_nested(None, &inner)?);
                        unsafe { INDENT -= 1 };
                    }
                    Ok(result)
                } else {
                    Ok(vec![Value::Int32(0)])
                }
            }
        };
        indent(&format!("unwind_nested result {:?}", ret));
        ret
    }
    fn parse_initializer(&mut self, is_auto: bool, stype: &SymbolType) -> Result<Initializer> {
        let token = self.peek()?;
        println!("parse_initializer {:?}", token);
        match token {
            Token::LeftBrace => {
                self.next_token()?;
                let mut values = Vec::new();
                let inner = Self::get_inner_type(stype)?;
                while self.peek()? != Token::RightBrace {
                    values.push(self.parse_initializer(is_auto, &inner)?);
                    if self.peek()? == Token::Comma {
                        self.next_token()?;
                    } else {
                        break;
                    }
                }
                self.expect(Token::RightBrace)?;
                if values.is_empty() {
                    bail!("Empty initializer list");
                }
                Ok(Initializer::CompoundInit(values))
            }
            _ => {
                let val = self.do_init_expression(is_auto, stype)?;
                Ok(Initializer::SingleInit(val))
            }
        }
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
                    self.do_rvalue_expression()?;
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

        if token != Token::SemiColon {
            let is_dec = self.do_function_or_variable(false, false)?;
            if is_dec {
            } else {
                self.do_rvalue_expression()?;
                self.expect(Token::SemiColon)?;
            }
        } else {
            self.next_token()?;
        }
        //}
        // ==================== START:
        // condition clause
        self.instruction(Instruction::Label(label_start.clone()));
        let token = self.peek()?;
        let cond = if token == Token::SemiColon {
            Value::Int32(1)
        } else {
            self.do_rvalue_expression()?
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
            self.do_rvalue_expression()?;
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
        let cond = self.do_rvalue_expression()?;
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
        let cond = self.do_rvalue_expression()?;
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
        if !self.labels.contains_key(&label) {
            self.labels.insert(label.clone(), false);
        }

        self.instruction(Instruction::Jump(label));
        self.expect(Token::SemiColon)?;
        Ok(())
    }
    fn do_return(&mut self) -> Result<()> {
        self.next_token()?;
        let func_name = &self.current_function_name;
        let (func_sym, _) = self.lookup_global_symbol(func_name).unwrap();
        let ret_type = *func_sym.stype.as_function().unwrap().1.clone();

        let val = self.do_rvalue_expression()?;
        let converted_val = self.convert_by_assignment(&val, &ret_type)?;
        self.instruction(Instruction::Return(converted_val));
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
        let cond = self.do_rvalue_expression()?;
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

        let mut this = scopeguard::guard(&mut *self, |x| {
            x.break_label_stack.pop();
        });
        // evaluate the condition once

        let value = this.do_rvalue_expression()?;
        if !value.stype().is_integer() {
            bail!("Switch value must be an integer");
        }
        let value = if value.stype().is_character() {
            this.convert_to(&value, &SymbolType::Int32, false)?
        } else {
            value.clone()
        };
        this.expect(Token::RightParen)?;

        let next_drop_thru_label = this.make_label("next_drop_thru");
        let next_case_label = this.make_label("next_case");
        // emit here otherwise the 'before first case' suppression gets it
        this.instruction(Instruction::Jump(next_case_label.clone()));
        this.switch_context_stack.push(SwitchContext {
            cases: Vec::new(),
            value,
            default_statement_seen: false,
            before_first_case: true,

            // bootstrap the label chain
            next_drop_thru_label: next_drop_thru_label.clone(),
            next_case_label: next_case_label.clone(),
        });
        // defer!(self.switch_context_stack.pop(););
        // the entire body of the switch statement is handled by this call
        // including the cases and breaks
        this.do_statement()?;

        // at the end of the switch, add the exit label
        // plus any pending labels
        this.instruction(Instruction::Label(label_end.clone()));
        let last_case_label = this
            .switch_context_stack
            .last()
            .unwrap()
            .next_case_label
            .clone();
        if !last_case_label.is_empty() {
            this.instruction(Instruction::Label(last_case_label));
        }

        this.switch_context_stack.pop();
        // self.break_label_stack.pop();
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
            let cv = self.do_rvalue_expression()?;
            if !matches!(cv, Value::Int32(_))
                && !matches!(cv, Value::Int64(_))
                && !matches!(cv, Value::UInt32(_))
                && !matches!(cv, Value::UInt64(_))
            {
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
        if let Some(mut cv) = case_value {
            let dest = Value::Variable(self.make_temporary_name(), SymbolType::Int32);
            if Self::get_type(&switch_value) != Self::get_type(&cv) {
                cv = self.convert_to(&cv, &Self::get_type(&switch_value), false)?;
            }
            let context = self.switch_context_stack.last_mut().unwrap();
            if context.cases.contains(&cv) {
                bail!("Duplicate case value");
            }
            context.cases.push(cv.clone());

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
        println!("   instruction {:?}", instruction);
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
        // println!("next_token {:?}", token);
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
        let token = self.peek_n(0);
        //  println!("peek {:?}", token);
        token
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
