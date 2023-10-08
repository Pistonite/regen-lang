use std::collections::{HashMap, HashSet};
use std::fmt::Write;
use std::path::PathBuf;

use codize::{block, codeln, dynblock, Code, Codize};
use heck::{ToSnakeCase, ToUpperCamelCase};

use crate::core::{
    Hook, Language, Param, ParamDataType, ParamDecorType, ParamType, Rule, RuleValue, TokenDef,
    TokenRule,
};
use crate::emit::{get_include_contents, Emitter};

use super::emitter::EmitterError;

pub struct RustEmitter {
    is_self: bool,
    path: PathBuf,
    extract_token_rules: HashSet<String>,
    head_block: Vec<Code>,
    main_block: Vec<Code>,
    token_block: Vec<Code>,
    token_rule_block: Vec<Code>,
    semantic_block: Vec<Code>,
    ast_block: Vec<Code>,
    pt_block: Vec<Code>,

    /// Cache for the AST type names without the ast:: namespace
    ast_name_cache: HashMap<String, String>,
    /// Cache for the PT type names without the pt:: namespace
    pt_name_cache: HashMap<String, String>,
}

impl RustEmitter {
    pub fn new(is_self: bool, path: PathBuf) -> Self {
        Self {
            is_self,
            path,
            extract_token_rules: HashSet::new(),
            head_block: Vec::new(),
            main_block: Vec::new(),
            token_block: Vec::new(),
            token_rule_block: Vec::new(),
            semantic_block: Vec::new(),
            ast_block: Vec::new(),
            pt_block: Vec::new(),
            ast_name_cache: HashMap::new(),
            pt_name_cache: HashMap::new(),
        }
    }
    fn crate_name(&self) -> &'static str {
        if self.is_self {
            "crate"
        } else {
            "regen"
        }
    }
    fn token_name(&self, name: &str) -> String {
        format!("T{}", name.to_upper_camel_case())
    }
    fn semantic_name(&self, name: &str) -> String {
        format!("S{}", name.to_upper_camel_case())
    }
    /// The name of the Abstract Syntax Tree struct/type
    fn ast(&mut self, rule: &str, include_ns: bool) -> String {
        let name = if let Some(name) = self.ast_name_cache.get(rule) {
            name.clone()
        } else {
            let n = rule.to_upper_camel_case();
            self.ast_name_cache.insert(rule.to_owned(), n.clone());
            n
        };
        if include_ns {
            format!("ast::{name}")
        } else {
            name
        }
    }
    /// The inner (unhooked) name of the Parse Tree struct/type
    fn pt_inner_type(&mut self, rule: &str, include_ns: bool) -> String {
        let name = if let Some(name) = self.pt_name_cache.get(rule) {
            name.clone()
        } else {
            let n = rule.to_upper_camel_case();
            self.pt_name_cache.insert(rule.to_owned(), n.clone());
            n
        };
        if include_ns {
            format!("pt::{name}")
        } else {
            name
        }
    }
    /// The outer (hooked) name of the Parse Tree node struct/type
    fn pt_outer_type(&mut self, lang: &Language, rule: &str) -> String {
        match &lang.rules.get(rule).unwrap().hook {
            Some(hook) => {
                let name = hook.return_type.clone();
                // Always use the namespaced name for the inner type
                let inner_type = self.pt_inner_type(rule, true);
                format!("ParseHook<{name}, {inner_type}<'p>>")
            }
            None => format!("{}<'p>", self.pt_inner_type(rule, true)),
        }
    }
    /// The name of a member (field) of the Abstract Syntax Tree or Parse Tree
    fn member_name(&mut self, name: &str) -> String {
        format!("m_{name}").to_snake_case()
    }
    /// The name of a member (field) of the Abstract Syntax Tree or Parse Tree, with an index
    fn member_name_with_idx(&mut self, name: &str, i: usize) -> String {
        format!("m_{name}_{i}").to_snake_case()
    }
    /// The type of a member (field) of the Abstract Syntax Tree
    fn ast_param_type(&mut self, param: &Param) -> String {
        match &param.param_type.data {
            ParamDataType::Rule => {
                let ast_t = self.ast(&param.type_name, false);
                match &param.param_type.decor {
                    ParamDecorType::None => format!("Box<{ast_t}>"),
                    ParamDecorType::Optional => format!("Option<Box<{ast_t}>>"),
                    ParamDecorType::Vec(_) => format!("Vec<{ast_t}>"),
                }
            }
            _ => match &param.param_type.decor {
                ParamDecorType::None => "Token".to_owned(),
                ParamDecorType::Optional => "Option<Token>".to_owned(),
                ParamDecorType::Vec(_) => "Vec<Token>".to_owned(),
            },
        }
    }
    fn pt_parse_func_name(&self, rule: &Rule) -> &'static str {
        match &rule.hook {
            Some(_) => "from_ast_internal",
            None => "from_ast",
        }
    }
    /// The type of a member (field) of the Parse Tree
    fn pt_var_type(&mut self, lang: &Language, var: &Param) -> Option<String> {
        let t = match &var.param_type.data {
            ParamDataType::Rule => {
                let pt_type = self.pt_outer_type(lang, &var.type_name);
                match &var.param_type.decor {
                    ParamDecorType::None => format!("Box<{pt_type}>"),
                    ParamDecorType::Optional => format!("Option<Box<{pt_type}>>"),
                    ParamDecorType::Vec(_) => format!("Vec<{pt_type}>"),
                }
            }
            ParamDataType::String => match &var.param_type.decor {
                ParamDecorType::None => "String".to_owned(),
                ParamDecorType::Optional => "Option<String>".to_owned(),
                ParamDecorType::Vec(_) => "Vec<String>".to_owned(),
            },
            ParamDataType::Flag(_) => {
                match &var.param_type.decor {
                    ParamDecorType::None => return None, // not in pt
                    ParamDecorType::Optional => "bool".to_owned(),
                    ParamDecorType::Vec(_) => "usize".to_owned(),
                }
            }
        };
        Some(t)
    }

    fn emit_ast(&mut self, _: &Language, rule: &Rule) {
        match &rule.value {
            RuleValue::Union(subrules) => {
                self.emit_ast_union(rule, subrules);
            }
            RuleValue::Function(params) => {
                self.emit_ast_func(rule, params);
            }
        }
    }

    fn emit_ast_func(&mut self, rule: &Rule, params: &[Param]) {
        let ast_t = self.ast(&rule.name, false);
        let ast_ns_t = self.ast(&rule.name, true);
        let member_names = params
            .iter()
            .enumerate()
            .map(|(i, p)| {
                if p.is_in_pt() {
                    self.member_name(&p.name)
                } else {
                    // May have duplicates if not in pt
                    self.member_name_with_idx(&p.name, i)
                }
            })
            .collect::<Vec<_>>();

        // Generate struct type
        let struct_block = {
            let mut body = Vec::new();
            for (param, member) in params.iter().zip(member_names.iter()) {
                let param_t = self.ast_param_type(param);
                body.push(codeln!(f "pub {member}: {param_t},"));
            }
            dynblock!(format!("#[derive(Debug)] pub struct {ast_t} {{"), body, "}")
        };
        self.ast_block.push(struct_block);
        let impl_block = block!(
            format!("impl {ast_ns_t} {{"),
            [
                self.get_ast_func_parse(params, &member_names),
                self.get_ast_func_apply_semantic(params, member_names),
            ],
            "}"
        );

        self.main_block.push(impl_block);
    }

    fn get_ast_func_parse(&mut self, params: &[Param], member_names: &[String]) -> Code {
        let mut struct_body = Vec::new();

        // Rust struct expressions have guaranteed eval order, so we can inline the evals into the return
        for (param, member) in params.iter().zip(member_names.iter()) {
            let type_name = &param.type_name;
            let mut t = match &param.param_type.data {
                ParamDataType::Rule => {
                    let t = self.ast(type_name, false);
                    format!("ast::{t}::parse(ts)")
                }
                ParamDataType::String => {
                    let t = self.token_name(type_name);
                    format!("token!({t}::parse(ts))")
                }
                ParamDataType::Flag(literal) => {
                    let t = self.token_name(type_name);
                    format!("token!({t}::\"{literal}\"(ts))")
                }
            };
            match &param.param_type.decor {
                ParamDecorType::None => {
                    t = format!("required!(ts, {t})?");
                }
                ParamDecorType::Optional => {
                    t = format!("optional!(ts, {t})");
                }
                ParamDecorType::Vec(optional) => {
                    if *optional {
                        t = format!("{{ let mut v = vec![]; list!(ts, v, {t}) }}");
                    } else {
                        t = format!(
                            "{{ let mut v = vec![required!(ts, {t})?]; list!(ts, v, {t}) }}"
                        );
                    }
                }
            }

            let code = match &param.param_type {
                ParamType {
                    data: ParamDataType::Rule,
                    decor: ParamDecorType::None,
                } => {
                    codeln!(f "{member}: Box::new({t}),")
                }
                ParamType {
                    data: ParamDataType::Rule,
                    decor: ParamDecorType::Optional,
                } => {
                    codeln!(f "{member}: ({t}).map(Box::new),")
                }
                _ => {
                    codeln!(f "{member}: {t},")
                }
            };

            struct_body.push(code);
        }
        block!(
            "pub fn parse(ts: &mut TokenStream<Tok>) -> Option<Self> {",
            [dynblock!("Some(Self {", struct_body, "})")],
            "}"
        )
    }
    fn get_ast_func_apply_semantic(&self, params: &[Param], member_names: Vec<String>) -> Code {
        let mut body = Vec::new();

        for (param, member) in params.iter().zip(member_names.iter()) {
            let code = match &param.param_type.data {
                ParamDataType::Rule => {
                    let semantic_expr = match &param.semantic {
                        Some(s) => {
                            let s = self.semantic_name(s);
                            format!("&Some(Tok::{s})")
                        }
                        None => "_ovr".to_owned(),
                    };
                    match &param.param_type.decor {
                        ParamDecorType::None => {
                            codeln!(f "self.{member}.apply_semantic(si, {semantic_expr});")
                        }
                        ParamDecorType::Optional => {
                            codeln!(f "if let Some(m) = &self.{member} {{ m.apply_semantic(si, {semantic_expr}); }}")
                        }
                        ParamDecorType::Vec(_) => {
                            codeln!(f "for m in &self.{member} {{ m.apply_semantic(si, {semantic_expr}); }}")
                        }
                    }
                }
                _ => {
                    let semantic_expr = match &param.semantic {
                        Some(s) => {
                            let s = self.semantic_name(s);
                            format!("_ovr.as_ref().cloned().unwrap_or(Tok::{s})")
                        }
                        None => "o.clone()".to_owned(),
                    };

                    let inner = match &param.param_type.decor {
                        ParamDecorType::None => {
                            format!("si.set(&self.{member}, {semantic_expr});")
                        }
                        ParamDecorType::Optional => {
                            format!(
                                "if let Some(m) = &self.{member} {{ si.set(m, {semantic_expr}); }}"
                            )
                        }
                        ParamDecorType::Vec(_) => {
                            format!("for m in &self.{member} {{ si.set(m, {semantic_expr}); }}")
                        }
                    };

                    if param.semantic.is_none() {
                        codeln!(f "if let Some(o) = _ovr {{ {inner} }}")
                    } else {
                        Code::Line(inner)
                    }
                }
            };

            body.push(code);
        }
        dynblock!(
            "pub fn apply_semantic(&self, si: &mut TokenBlocks<Tok>, _ovr: &Option<Tok>) {",
            body,
            "}"
        )
    }

    fn emit_ast_union(&mut self, rule: &Rule, subrules: &Vec<String>) {
        let ast_t = self.ast(&rule.name, false);
        let mut enum_block = Vec::new();
        for subrule in subrules {
            let ast_t = self.ast(subrule, false);
            enum_block.push(codeln!(f "{ast_t}(Box<{ast_t}>),"));
        }
        self.ast_block.push(dynblock!(
            format!("#[derive(Debug)] pub enum {ast_t} {{"),
            enum_block,
            "}"
        ));
    }

    fn emit_pt(&mut self, lang: &Language, rule: &Rule) {
        match &rule.value {
            RuleValue::Union(type_defs) => {
                self.emit_pt_union(lang, rule, type_defs);
            }
            RuleValue::Function(params) => {
                self.emit_pt_func(lang, rule, params);
            }
        }
    }

    fn emit_pt_func(&mut self, lang: &Language, rule: &Rule, params: &[Param]) {
        let member_names = params
            .iter()
            .enumerate()
            .map(|(i, p)| {
                if p.is_in_pt() {
                    self.member_name(&p.name)
                } else {
                    // May have duplicates if not in pt
                    self.member_name_with_idx(&p.name, i)
                }
            })
            .collect::<Vec<_>>();

        let pt_t = self.pt_inner_type(&rule.name, false);
        let ast_ns_t = self.ast(&rule.name, true);

        // Struct declaration
        let struct_block = {
            let mut body = Vec::new();
            // Every PT node have a reference to its AST node
            body.push(codeln!(f "pub ast: &'p {ast_ns_t},"));
            for (member, param) in member_names.iter().zip(params.iter()) {
                //let var_type = param_types.get(var).unwrap();
                let var_type = self.pt_var_type(lang, param);
                if let Some(var_type) = var_type {
                    body.push(codeln!(f "pub {member}: {var_type},"));
                }
            }
            dynblock!(
                format!("#[derive(Debug)] pub struct {pt_t}<'p> {{"),
                body,
                "}"
            )
        };
        self.pt_block.push(struct_block);

        let parse_block = {
            let func_name = self.pt_parse_func_name(rule);
            let mut body = vec![codeln!("ast,")];

            // Parse each member from its AST node
            for (member, param) in member_names.iter().zip(params.iter()) {
                let l = match &param.param_type.data {
                    ParamDataType::Rule => {
                        let pt_ns_t = self.pt_inner_type(&param.type_name, true);
                        Some(match &param.param_type.decor {
                            ParamDecorType::None => {
                                codeln!(f "{member}: Box::new({pt_ns_t}::from_ast(ast.{member}.as_ref(), _ctx)),")
                            }
                            ParamDecorType::Optional => {
                                codeln!(f "{member}: ast.{member}.as_ref().map(|x| Box::new({pt_ns_t}::from_ast(x, _ctx))),")
                            }
                            ParamDecorType::Vec(_) => {
                                codeln!(f "{member}: ast.{member}.iter().map(|x| {pt_ns_t}::from_ast(x, _ctx)).collect::<Vec<_>>(),")
                            }
                        })
                    }
                    ParamDataType::String => Some(match &param.param_type.decor {
                        ParamDecorType::None => {
                            codeln!(f "{member}: ast.{member}.value.clone(),")
                        }
                        ParamDecorType::Optional => {
                            codeln!(f "{member}: ast.{member}.as_ref().map(|t| t.value.clone()),")
                        }
                        ParamDecorType::Vec(_) => {
                            codeln!(f "{member}: ast.{member}.iter().map(|t| t.value.clone()).collect::<Vec<_>>(),")
                        }
                    }),
                    ParamDataType::Flag(_) => {
                        match &param.param_type.decor {
                            ParamDecorType::None => {
                                // not in PT
                                None
                            }
                            ParamDecorType::Optional => {
                                Some(codeln!(f "{member}: ast.{member}.is_some(),"))
                            }
                            ParamDecorType::Vec(_) => {
                                Some(codeln!(f "{member}: ast.{member}.len() as usize,"))
                            }
                        }
                    }
                };
                if let Some(code) = l {
                    body.push(code);
                }
            }

            block!(
                format!("fn {func_name}(ast: &'p {ast_ns_t}, _ctx: &mut Ctx) -> Self {{"),
                [dynblock!("Self {", body, "}")],
                "}"
            )
        };
        let mut functions = vec![parse_block];
        // Add hook if needed
        if let Some(hook) = &rule.hook {
            let hook_block = self.get_pt_hook_func(lang, rule, hook);
            functions.push(hook_block);
        }
        let pt_ns_t = self.pt_inner_type(&rule.name, true);
        self.main_block.push(dynblock!(
            format!("impl<'p> {pt_ns_t}<'p> {{"),
            functions,
            "}"
        ));
    }

    fn emit_pt_union(&mut self, lang: &Language, rule: &Rule, type_defs: &[String]) {
        let (enum_discriminants, impl_block) = {
            let func_name = self.pt_parse_func_name(rule);
            let ast_t = self.ast(&rule.name, false);
            // The union type might not have all PT discriminants if multiple hooks
            // returns the same type. So we use a vector to keep track of the unique names
            // Note that vector is used because we want to keep the order of the discriminants
            // It's ok because there shouldn't be too many variants in a union
            let mut enum_discriminants = Vec::new();
            let crate_name = self.crate_name();
            let body = type_defs
                .iter()
                .map(|type_def| {
                    if !enum_discriminants.contains(type_def) {
                        enum_discriminants.push(type_def.clone());
                    }
                    let t = self.ast(type_def, false);
                    codeln!(f "{t},")
                })
                .collect::<Vec<_>>();
            (
                enum_discriminants,
                dynblock!(
                    format!("{crate_name}::impl_union!({func_name}, {ast_t}, {{"),
                    body,
                    "});"
                ),
            )
        };
        self.main_block.push(impl_block);

        let enum_block = {
            let pt_t = self.pt_inner_type(&rule.name, false);
            let body = enum_discriminants
                .iter()
                .map(|type_def| {
                    let pt_type = self.pt_outer_type(lang, type_def);
                    let pt_discriminant = self.pt_inner_type(type_def, false);
                    codeln!(f "{pt_discriminant}(Box<{pt_type}>),")
                })
                .collect::<Vec<_>>();
            dynblock!(
                format!("#[derive(Debug)] pub enum {pt_t}<'p> {{ "),
                body,
                "}"
            )
        };
        self.pt_block.push(enum_block);

        if let Some(hook) = &rule.hook {
            let hook_block = self.get_pt_hook_func(lang, rule, hook);
            let pt_ns_t = self.pt_inner_type(&rule.name, true);
            self.main_block.push(block!(
                format!("impl<'p> {pt_ns_t}<'p> {{"),
                [hook_block],
                "}"
            ));
        }
    }
    fn get_pt_hook_func(&mut self, lang: &Language, rule: &Rule, hook: &Hook) -> Code {
        let ast_ns_t = self.ast(&rule.name, true);
        let pt_t = self.pt_outer_type(lang, &rule.name);
        let func = &hook.name;

        block!(format!("#[inline] #[allow(clippy::unnecessary_mut_passed)] fn from_ast(ast: &'p {ast_ns_t}, ctx: &mut Ctx) -> {pt_t} {{"), [
      codeln!("let mut pt = Self::from_ast_internal(ast, ctx);"),
      codeln!(f "ParseHook {{ val: {func}(&mut pt, ctx), pt }}"),
    ], "}")
    }

    fn regex_to_literal(&self, regex: &str) -> String {
        // Escape the regex literal
        let mut escape = String::new();
        while regex.contains(&format!("\"{}", escape)) {
            escape.push('#');
        }
        format!("r{escape}\"^{regex}\"{escape}")
    }
}

impl Emitter for RustEmitter {
    fn start(&mut self, lang: &Language) -> Result<(), EmitterError> {
        for token in &lang.tokens {
            if token.is_extract {
                self.extract_token_rules.insert(token.name.clone());
            }
        }
        self.head_block.push(codeln!("#![allow(dead_code)]"));
        self.head_block
            .push(codeln!("#![cfg_attr(rustfmt, rustfmt_skip)]"));

        self.head_block
            .push(block!("/*", [codeln!(f "Generated with regen-lang")], "*/"));
        Ok(())
    }
    fn emit_include(&mut self, _: &Language, path: &str) -> Result<(), EmitterError> {
        let target = if self.main_block.is_empty() {
            &mut self.head_block
        } else {
            &mut self.main_block
        };
        target.push(codeln!(f "// /* {path} */"));
        target.push(Code::Line(get_include_contents(&self.path, path)?));
        target.push(codeln!(f "// /* {path} */"));
        Ok(())
    }
    fn emit_token(&mut self, _: &Language, token: &TokenDef) -> Result<(), EmitterError> {
        self.token_block
            .push(codeln!(f "{},", self.token_name(&token.name)));
        Ok(())
    }
    fn emit_token_rule(&mut self, _: &Language, rule: &TokenRule) -> Result<(), EmitterError> {
        match rule {
            TokenRule::IgnoreRegExp(regex) => {
                let regex = self.regex_to_literal(regex);
                self.token_rule_block.push(codeln!(
                  f "lex::Rule::Regex(Regex::new({regex}).unwrap(),lex::Target::Ignore),"
                ));
            }
            TokenRule::RegExp(name, regex) => {
                let should_extract = if self.extract_token_rules.contains(name) {
                    "Extract"
                } else {
                    "Keep"
                };
                let regex = self.regex_to_literal(regex);
                let name = self.token_name(name);
                self.token_rule_block.push(codeln!(
            f "lex::Rule::Regex(Regex::new({regex}).unwrap(), lex::Target::{should_extract}(Tok::{name})),"
          ));
            }
            TokenRule::IgnoreLiteral(literal) => {
                self.token_rule_block.push(codeln!(
                  f "lex::Rule::Literal(\"{literal}\".to_owned(), lex::Target::Ignore],"
                ));
            }
            TokenRule::Literal(name, literal) => {
                let should_extract = if self.extract_token_rules.contains(name) {
                    "Extract"
                } else {
                    "Keep"
                };
                let name = self.token_name(name);
                self.token_rule_block.push(codeln!(
            f "lex::Rule::Literal(\"{literal}\".to_owned(), lex::Target::{should_extract}(Tok::{name})), "
          ));
            }
        }
        Ok(())
    }
    fn emit_semantic(&mut self, _: &Language, semantic: &str) -> Result<(), EmitterError> {
        self.semantic_block
            .push(codeln!(f "{},", self.semantic_name(semantic)));
        Ok(())
    }

    fn emit_rule(&mut self, lang: &Language, rule: &Rule) -> Result<(), EmitterError> {
        self.emit_ast(lang, rule);
        self.emit_pt(lang, rule);
        Ok(())
    }

    fn done(mut self, lang: &Language) -> Result<String, EmitterError> {
        let codize = Codize::indent(2);

        let mut output = String::new();
        let crate_name = self.crate_name();
        for code in self.head_block {
            let s: String = code.to_string_with(&codize);
            writeln!(output, "{}", s)?;
        }

        let sdk_block = block!(
            format!("{crate_name}::sdk!("),
            [
                codeln!(f "context: {t};", t = lang.context.as_ref().cloned().unwrap_or("()".to_owned())),
                codeln!(f "target: {t};", t = &lang.target),
                dynblock!("tokens: [", self.token_block, "];"),
                dynblock!("rules: [", self.token_rule_block, "];"),
                dynblock!("semantics: [", self.semantic_block, "];"),
            ],
            ");"
        );

        let s: String = sdk_block.to_string_with(&codize);
        writeln!(output, "{}", s)?;

        let ast_mod_block = dynblock!(
            "pub mod ast {",
            {
                let mut v = vec![codeln!("use super::*;")];
                v.append(&mut self.ast_block);
                v
            },
            "}"
        );

        let s: String = ast_mod_block.to_string_with(&codize);
        writeln!(output, "{}", s)?;

        let pt_mod_block = dynblock!(
            "pub mod pt {",
            {
                let mut v = vec![codeln!("use super::*;")];
                v.append(&mut self.pt_block);
                v
            },
            "}"
        );

        let s: String = pt_mod_block.to_string_with(&codize);
        writeln!(output, "{}", s)?;

        for code in self.main_block {
            let s: String = code.to_string_with(&codize);
            writeln!(output, "{}", s)?;
        }

        Ok(output)
    }
}
