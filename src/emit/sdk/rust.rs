use std::collections::{HashMap, HashSet};
use std::env::var;
use std::fmt::{format, Write};
use std::path::PathBuf;

use clap::crate_version;
use heck::{ToSnakeCase, ToUpperCamelCase};

use crate::core::{
  Expr, Hook, Language, Param, ParamType, RetType, Rule, RuleValue, TokenDef, TokenRule,
};
use crate::emit::{get_include_contents, Code, Emitter};

pub struct RustEmitter {
  is_self: bool,
  indent: usize,
  path: PathBuf,
  extract_token_rules: HashSet<String>,
  head_block: Vec<Code>,
  main_block: Vec<Code>,
  token_block: Vec<Code>,
  regex_block: Vec<Code>,
  token_rule_block: Vec<Code>,
  semantic_block: Vec<Code>,
  ast_block: Vec<Code>,
  pt_block: Vec<Code>,

  token_rule_idx: usize,
  /// Cache for the AST type names without the ast:: namespace
  ast_name_cache: HashMap<String, String>,
  /// Cache for the PT type names without the pt:: namespace
  pt_name_cache: HashMap<String, String>,
}

impl RustEmitter {
  pub fn new(is_self: bool, indent: usize, path: PathBuf) -> Self {
    Self {
      is_self,
      indent,
      path,
      extract_token_rules: HashSet::new(),
      head_block: Vec::new(),
      main_block: Vec::new(),
      token_block: Vec::new(),
      regex_block: Vec::new(),
      token_rule_block: Vec::new(),
      semantic_block: Vec::new(),
      ast_block: Vec::new(),
      pt_block: Vec::new(),
      token_rule_idx: 0,
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
    let type_name = &param.type_name;
    if !param.is_token {
      let ast_t = self.ast(type_name, false);
      if param.is_optional {
        format!("Box<Option<{ast_t}>>")
      } else {
        format!("Box<{ast_t}>")
      }
    } else {
      if param.is_optional {
        "Option<Token>".to_owned()
      } else {
        "Token".to_owned()
      }
    }
  }
  fn pt_parse_func_name(&self, rule: &Rule) -> &'static str {
    match &rule.hook {
      Some(_) => "from_ast_internal",
      None => "from_ast",
    }
  }
  /// The type of a member (field) of the Parse Tree
  fn pt_var_type(&mut self, lang: &Language, var_type: &ParamType) -> String {
    match var_type {
      ParamType::Item(optional, name) => {
        let pt_type = self.pt_outer_type(lang, name);
        if *optional {
          format!("Box<Option<{pt_type}>>")
        } else {
          format!("Box<{pt_type}>")
        }
      }
      ParamType::String(optional) => {
        if *optional {
          "Option<String>".to_owned()
        } else {
          "String".to_owned()
        }
      }
      ParamType::Bool => "bool".to_owned(),
    }
  }
  /// The type of member vectors of the Parse Tree
  /// Returns the type of the AST vector and the value vector
  fn pt_vec_type(&mut self, lang: &Language, item_ret_type: &RetType) -> (String, String) {
    let mut asts_type = String::from("VecDeque<");
    let mut vals_type = String::from("VecDeque<");
    let mut nest_level = 1;

    let mut next_item = item_ret_type;
    loop {
      match next_item {
        RetType::Vec(item) => {
          asts_type.push_str("VecDeque<");
          vals_type.push_str("VecDeque<");
          nest_level += 1;
          next_item = item.as_ref();
        }
        RetType::Unit(item) => {
          match item {
            ParamType::Item(optional, name) => {
              let ast_ns_t = self.ast(name, true);
              let pt_t = self.pt_outer_type(lang, name);

              if *optional {
                asts_type.push_str(&format!("Option<&'p {ast_ns_t}>"));
                vals_type.push_str(&format!("Option<{pt_t}>"));
              } else {
                asts_type.push_str(&format!("&'p {ast_ns_t}"));
                vals_type.push_str(&pt_t);
              }
            }
            ParamType::String(optional) => {
              if *optional {
                asts_type.push_str("Option<&'p Token>");
                vals_type.push_str("Option<String>");
              } else {
                asts_type.push_str("&'p Token");
                vals_type.push_str("String");
              }
            }
            ParamType::Bool => {
              asts_type.push_str("&'p Token<Tokens>");
              vals_type.push_str("bool");
            }
          }

          break;
        }
        _ => unreachable!(),
      }
    }

    for _ in 0..nest_level {
      asts_type.push('>');
      vals_type.push('>');
    }
    (asts_type, vals_type)
  }
  fn emit_ast(&mut self, _: &Language, rule: &Rule) -> Result<(), Box<dyn std::error::Error>> {
    match &rule.value {
      RuleValue::Union(subrules) => {
        self.emit_ast_union(rule, subrules)?;
      }
      RuleValue::Function(params, _) => {
        self.emit_ast_func(rule, params);
      }
    }
    Ok(())
  }

  fn emit_ast_func(&mut self, rule: &Rule, params: &Vec<Param>) {
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
      let start = format!("#[derive(Debug)] pub struct {ast_t} {{");
      let mut body = Vec::new();
      for (param, member) in params.iter().zip(member_names.iter()) {
        let param_t = self.ast_param_type(&param);
        body.push(Code::Line(format!("pub {member}: {param_t},")));
      }
      let end = "}".to_owned();
      Code::Block {
        indent: self.indent,
        inline: params.len() <= 2,
        start,
        body,
        end,
      }
    };
    self.ast_block.push(struct_block);
    let impl_block = Code::Block {
      indent: self.indent,
      inline: false,
      start: format!("impl {ast_ns_t} {{"),
      body: vec![
        self.get_ast_func_parse(params, &member_names),
        self.get_ast_func_apply_semantic(params, member_names),
      ],
      end: "}".to_owned(),
    };
    self.main_block.push(impl_block);
  }

  fn get_ast_func_parse(&mut self, params: &Vec<Param>, member_names: &Vec<String>) -> Code {
    let func_start = "fn parse(ts: &mut TokenStream<Tok>) -> Option<Self> {".to_owned();
    let self_start = "Some(Self {".to_owned();

    let mut struct_expr_body = Vec::new();

    // Rust struct expressions have guaranteed eval order, so we can inline the evals into the return
    for (param, member) in params.iter().zip(member_names.iter()) {
      let type_name = &param.type_name;
      let mut t = if !param.is_token {
        let t = self.ast(&type_name, false);
        format!("ast::{t}::parse(ts)")
      } else {
        let mut t = self.token_name(type_name);
        t.push_str("::");
        match &param.match_literal {
          None => {
            t.push_str("parse(ts)");
          }
          Some(literal) => {
            t.push('"');
            t.push_str(literal);
            t.push('"');
            t.push_str("(ts)");
          }
        }
        format!("token!({t})")
      };

      if param.is_optional {
        t = format!("optional!(ts, {t})");
      } else {
        t.push('?');
      }

      if !param.is_token {
        struct_expr_body.push(Code::Line(format!("{member}: Box::new({t}),")));
      } else {
        struct_expr_body.push(Code::Line(format!("{member}: {t},")));
      }
    }

    Code::Block {
      indent: self.indent,
      inline: false,
      start: func_start,
      body: vec![Code::Block {
        indent: self.indent,
        inline: struct_expr_body.len() <= 2,
        start: self_start,
        body: struct_expr_body,
        end: "})".to_owned(),
      }],
      end: "}".to_owned(),
    }
  }
  fn get_ast_func_apply_semantic(&self, params: &Vec<Param>, member_names: Vec<String>) -> Code {
    let start = "fn apply_semantic(&self, _si: &mut SemInfo) {".to_owned();
    let end = "}".to_owned();
    let mut body = Vec::new();

    for (param, member) in params.iter().zip(member_names.iter()) {
      if !param.is_token {
        if param.is_optional {
          body.push(Code::Line(format!(
            "if let Some(m) = self.{member}.as_ref() {{ m.apply_semantic(_si); }}"
          )));
        } else {
          body.push(Code::Line(format!("self.{member}.apply_semantic(_si);")));
        }
      } else {
        if let Some(s) = &param.semantic {
          let s = self.semantic_name(s);
          if param.is_optional {
            body.push(Code::Line(format!(
              "if let Some(m) = &self.{member} {{ _si.set(m, Sem::{s}); }}"
            )));
          } else {
            body.push(Code::Line(format!("_si.set(&self.{member}, Sem::{s});")));
          }
        }
      }
    }

    Code::Block {
      indent: self.indent,
      inline: false,
      start,
      body,
      end,
    }
  }

  fn emit_ast_union(
    &mut self,
    rule: &Rule,
    subrules: &Vec<String>,
  ) -> Result<(), Box<dyn std::error::Error>> {
    let ast_t = self.ast(&rule.name, false);
    let enum_block_start = format!("#[derive(Debug)] pub enum {ast_t} {{");
    let mut enum_block = Vec::new();
    for subrule in subrules {
      let ast_t = self.ast(subrule, false);
      enum_block.push(Code::Line(format!("{ast_t}(Box<{ast_t}>),")));
    }
    let enum_block_end = "}".to_owned();
    self.ast_block.push(Code::Block {
      indent: self.indent,
      inline: false,
      start: enum_block_start,
      body: enum_block,
      end: enum_block_end,
    });
    Ok(())
  }
  fn emit_pt(&mut self, lang: &Language, rule: &Rule) -> Result<(), Box<dyn std::error::Error>> {
    match &rule.value {
      RuleValue::Union(type_defs) => {
        self.emit_pt_union(lang, rule, type_defs);
      }
      RuleValue::Function(params, body) => {
        self.emit_pt_func(lang, rule, params, body);
      }
    }
    Ok(())
  }

  fn emit_pt_func(&mut self, lang: &Language, rule: &Rule, params: &Vec<Param>, body_expr: &Expr) {
    // Bind parameters to their types and the AST member names
    // This is needed because body expression can refer to the parameters in any order
    // so we can't just put the names in a vec
    let (param_members, param_types) = {
      let var_to_member = params
        .iter()
        .enumerate()
        .map(|(i, p)| {
          let member_name = if p.is_in_pt() {
            self.member_name(&p.name)
          } else {
            self.member_name_with_idx(&p.name, i)
          };
          (p.name.clone(), member_name)
        })
        .collect::<HashMap<_, _>>();
      let mut param_types = HashMap::new();
      for param in params {
        if let Some(t) = param.get_type() {
          param_types.insert(param.name.clone(), t);
        }
      }
      (var_to_member, param_types)
    };

    let pt_t = self.pt_inner_type(&rule.name, false);
    let ast_ns_t = self.ast(&rule.name, true);
    
    let ret_type = lang.ret_types.get(&rule.name).unwrap();

    // Struct declaration
    let struct_block = {
      let start = format!("#[derive(Debug)] pub struct {pt_t}<'p> {{");
      let mut body = Vec::new();
      // All PT nodes have a reference to its AST node
      body.push(Code::Line(format!("pub ast: &'p {ast_ns_t},")));

      let vars = body_expr.get_vars();

      if let RetType::Vec(item) = ret_type {
        // if return type is a vector, store the vector of AST nodes and the vector of PT values
        let (asts_type, vals_type) = self.pt_vec_type(lang, item.as_ref());
        body.push(Code::Line(format!("pub asts: {asts_type},")));
        body.push(Code::Line(format!("pub vals: {vals_type},")));
      } else {
        // if returning values, store the respective members
        for var in &vars {
          let var_type = param_types.get(var).unwrap();
          let var_type = self.pt_var_type(lang, var_type);
          let member = param_members.get(var).unwrap();
          body.push(Code::Line(format!("pub {member}: {var_type},")));
        }
      }
      Code::Block {
        indent: self.indent,
        inline: body.len() <= 2,
        start,
        body,
        end: "}".to_owned(),
      }
    };
    self.pt_block.push(struct_block);
    

    
    let parse_block = {
      let func_name = self.pt_parse_func_name(rule);
      let start = format!("fn {func_name}(ast: &'p {ast_ns_t}, _si: &mut SemInfo, _errors: &mut Vec<Error>) -> Self {{");
      let mut body = vec![];
      let vars = body_expr.get_vars();
      
      // Parse members

    for var in vars {
      let member = param_members.get(&var).unwrap();
      let var_type = param_types.get(&var).unwrap();
      let l = match var_type {
        ParamType::Item(optional, name) => {
          let pt_ns_t = self.pt_inner_type(name, true);
          if *optional {
            format!("let {member} = if let Some(v) = ast.{member}.as_ref() {{ Box::new(Some({pt_ns_t}::from_ast(v, _si, _errors))) }} else {{ Box::new(None) }};")
          } else {
            format!("let {member} = Box::new({pt_ns_t}::from_ast(&ast.{member}.as_ref(), _si, _errors));")
          }
        }
        ParamType::String(optional) => {
          if *optional {
            format!("let {member} = ast.{member}.as_ref().map(|t| t.value.clone());")
          } else {
            format!("let {member} = ast.{member}.value.clone();")
          }
        }
        ParamType::Bool => {
          format!("let {member} = !ast.{member}.is_none();")
        }
      };
      body.push(Code::Line(l));
    }
      // Execute body expression

    match &body_expr {
      Expr::Concat(vars) => {
        // return a concatenation
        // implementation: move the asts and vals over, push_front the rest
        let last = vars.last().unwrap();
        let last_type = param_types.get(last).unwrap();
        let last_member = param_members.get(last).unwrap();

        let move_macro = if last_type.is_optional() {
          "move_pt_vec_optional"
        } else {
          "move_pt_vec"
        };
        body.push(Code::Line(format!("let (mut asts, mut vals) = {move_macro}!({last_member});")));

        // push the vars in reverse order to the front of the vec
        for var in vars.iter().rev().skip(1) {
          let var_type = param_types.get(var).unwrap();
          let member = param_members.get(var).unwrap();

          match var_type {
            ParamType::Item(optional, _) => {
              body.push(Code::Line(format!("vals.push_front(*{member});")));
              if *optional {
                body.push(Code::Line(format!("asts.push_front(ast.{member}.as_ref());")));
              } else {
                body.push(Code::Line(format!("asts.push_front(&ast.{member});")));
              }
            }
            ParamType::String(optional) => {
              body.push(Code::Line(format!("vals.push_front({member});")));
              if *optional {
                body.push(Code::Line(format!("asts.push_front(ast.{member}.as_ref());")));
              } else {
                body.push(Code::Line(format!("asts.push_front(&ast.{member});")));
              }
            }
            ParamType::Bool => {
              body.push(Code::Line(format!("vals.push_front({member});")));
              body.push(Code::Line(format!("asts.push_front(ast.{member}.as_ref());")));
            }
          }
        }
        body.push(Code::Line("Self { ast, asts, vals }".to_owned()));
      }
      Expr::Var(var) => {
        let member = param_members.get(var).unwrap();
        match ret_type {
          RetType::Unit(_) => {
            //return a single non-vec value
        //    implementation: move the val over
        body.push(Code::Line(format!("Self {{ ast, {member} }}")));
          }
          RetType::Vec(_) => {
            //// 2. return a single vec value VecType = Vec, RetType = Param, Expr = Var
        //    implementation: move the asts and vals over
        let move_macro = if param_types.get(var).unwrap().is_optional() {
          "move_pt_vec_optional"
        } else {
          "move_pt_vec"
        };
        body.push(Code::Line(format!("let (asts, vals) = {move_macro}!({member});")));
           
        body.push(Code::Line("Self { ast, asts, vals }".to_owned()));

          }
          _ => unreachable!(),
        }
      }
      Expr::Dict(vars) => {
        // 3. return a struct 
        //    implementation: create the struct and move the val over
        let mut struct_body = vec![Code::Line("ast,".to_owned())];
        for var in vars {
          let member = param_members.get(var).unwrap();
          struct_body.push(Code::Line(format!("{member},")));
        }
        body.push(Code::Block { indent: self.indent, inline: struct_body.len() <= 3, start: "Self {".to_owned(), body: struct_body, end: "}".to_owned() });
      }
    }
      Code::Block {
        indent: self.indent,
        inline: false,
        start,
        body,
        end: "}".to_owned(),
      }
    };
    let mut functions = vec![parse_block];
    // Add hook if needed
    if let Some(hook) = &rule.hook {
      let hook_block = self.get_pt_hook_func(lang, rule, hook);
      functions.push(hook_block);
    }
    let pt_ns_t = self.pt_inner_type(&rule.name, true);
      let impl_block = Code::Block {
        indent: self.indent,
        inline: false,
        start: format!("impl<'p> {pt_ns_t}<'p> {{"),
        body: functions,
        end: "}".to_owned(),
      };
      self.main_block.push(impl_block);
  }

  fn emit_pt_union(&mut self, lang: &Language, rule: &Rule, type_defs: &Vec<String>) {
    let (enum_discriminants, impl_block) = {
      let func_name = self.pt_parse_func_name(rule);
      let ast_t = self.ast(&rule.name, false);
      // The union type might not have all PT discriminants if multiple hooks
      // returns the same type. So we use a vector to keep track of the unique names
      // Note that vector is used because we want to keep the order of the discriminants
      // It's ok because there shouldn't be too many variants in a union
      let mut enum_discriminants = Vec::new();
      let crate_name = self.crate_name();
      let start = format!("{crate_name}::impl_union!({func_name}, {ast_t}, {{");
      let body = type_defs
        .iter()
        .map(|type_def| {
          if !enum_discriminants.contains(type_def) {
            enum_discriminants.push(type_def.clone());
          }
          let t = self.ast(&type_def, false);
          Code::Line(format!("{t},"))
        })
        .collect::<Vec<_>>();
      let end = "});".to_owned();
      (
        enum_discriminants,
        Code::Block {
          indent: self.indent,
          inline: body.len() <= 2,
          start,
          body,
          end,
        },
      )
    };
    self.main_block.push(impl_block);

    let enum_block = {
      let pt_t = self.pt_inner_type(&rule.name, false);
      let start = format!("#[derive(Debug)] pub enum {pt_t}<'p> {{ ");
      let body = enum_discriminants
        .iter()
        .map(|type_def| {
          let pt_type = self.pt_outer_type(lang, type_def);
          let pt_discriminant = self.pt_inner_type(type_def, false);
          Code::Line(format!("{pt_discriminant}(Box<{pt_type}>),"))
        })
        .collect::<Vec<_>>();
      Code::Block {
        indent: self.indent,
        inline: body.len() <= 2,
        start,
        body,
        end: "}".to_owned(),
      }
    };
    self.pt_block.push(enum_block);

    if let Some(hook) = &rule.hook {
      let hook_block = self.get_pt_hook_func(lang, rule, hook);
      let pt_ns_t = self.pt_inner_type(&rule.name, true);
      let impl_block = Code::Block {
        indent: self.indent,
        inline: false,
        start: format!("impl<'p> {pt_ns_t}<'p> {{"),
        body: vec![hook_block],
        end: "}".to_owned(),
      };
      self.main_block.push(impl_block);
    }
  }
  fn get_pt_hook_func(&mut self, lang: &Language, rule: &Rule, hook: &Hook) -> Code {
    let ast_ns_t = self.ast(&rule.name, true);
    let pt_t = self.pt_outer_type(lang, &rule.name);
    let func = &hook.name;
    let start = format!("#[inline] fn from_ast(ast: &'p {ast_ns_t}, si: &mut SemInfo, err: &mut Vec<Error>) -> {pt_t} {{");
    let body = vec![
      Code::Line("let mut pt = Self::from_ast_internal(ast, si, err);".to_owned()),
      Code::Line(format!("ParseHook {{ val: {func}(&mut pt, si, err), pt }}")),
    ];
    let end = "}".to_owned();
    Code::Block {
      indent: self.indent,
      inline: false,
      start,
      body,
      end,
    }
  }
}

impl Emitter for RustEmitter {
  fn start(&mut self, lang: &Language) -> Result<(), Box<dyn std::error::Error>> {
    for token in &lang.tokens {
      if token.is_extract {
        self.extract_token_rules.insert(token.name.clone());
      }
    }
    self.head_block.push(Code::Block {
      indent: self.indent,
      inline: false,
      start: "/*".to_owned(),
      body: vec![
        Code::Line(format!("Generated with regen-lang v{v}", v = crate_version!())),
      ],
      end: "*/".to_owned(),
    
    });
    Ok(())
  }
  fn emit_include(&mut self, _: &Language, path: &str) -> Result<(), Box<dyn std::error::Error>> {
    let target = if self.main_block.is_empty() {
      &mut self.head_block
    } else {
      &mut self.main_block
    };
    target.push(Code::Line(format!("//// /* {path} */")));
    target
      .push(Code::Line(get_include_contents(&self.path, path)?));
    target.push(Code::Line(format!("//// /* {path} */")));
    Ok(())
  }
  fn emit_token(
    &mut self,
    _: &Language,
    token: &TokenDef,
  ) -> Result<(), Box<dyn std::error::Error>> {
    self
      .token_block
      .push(Code::Line(format!("{},", self.token_name(&token.name))));
    Ok(())
  }
  fn emit_token_rule(
    &mut self,
    _: &Language,
    rule: &TokenRule,
  ) -> Result<(), Box<dyn std::error::Error>> {
    if let Some(literal) = rule.get_regex() {
      // Get a new identifier for the compiled regex variable
      let re_ident = format!("re{}", self.token_rule_idx);
      self.token_rule_idx += 1;
      // Escape the regex literal
      let mut escape = String::new();
      while literal.contains(&format!("\"{}", escape)) {
        escape.push_str("#");
      }
      let literal = literal.replace(r"\/", "/");
      self.regex_block.push(Code::Line(format!(
        "{re_ident} = r{escape}\"^{literal}\"{escape},"
      )));

      // Emit the token rule
      match rule {
        TokenRule::IgnoreRegExp(_) => {
          self.token_rule_block.push(Code::Line(format!("[{re_ident}],")));
        }
        TokenRule::RegExp(name, _) => {
          let should_extract = self.extract_token_rules.contains(name);
          let name = self.token_name(name);
          self.token_rule_block.push(Code::Line(format!(
            "[{should_extract}, {name}, {re_ident}],"
          )));
        }
        _ => unreachable!(),
      }
    } else {
      match rule {
        TokenRule::IgnoreLiteral(literal) => {
          self
            .token_rule_block
            .push(Code::Line(format!("[\"{literal}\", {}],", literal.len())));
        }
        TokenRule::Literal(name, literal) => {
          let should_extract = self.extract_token_rules.contains(name);
          let name = self.token_name(name);
          self.token_rule_block.push(Code::Line(format!(
            "[{should_extract}, {name}, \"{literal}\", {}],",
            literal.len()
          )));
        }
        _ => unreachable!(),
      }
    }
    Ok(())
  }
  fn emit_semantic(
    &mut self,
    _: &Language,
    semantic: &str,
  ) -> Result<(), Box<dyn std::error::Error>> {
    self
      .semantic_block
      .push(Code::Line(format!("{},", self.semantic_name(semantic))));
    Ok(())
  }
  fn emit_rule(&mut self, lang: &Language, rule: &Rule) -> Result<(), Box<dyn std::error::Error>> {
    self.emit_ast(lang, rule)?;
    self.emit_pt(lang, rule)
  }
  fn done(mut self, lang: &Language) -> Result<String, Box<dyn std::error::Error>> {
    let mut output = String::new();
    let crate_name = self.crate_name();
    for code in self.head_block {
      let s: String = code.into();
      writeln!(output, "{}", s)?;
    }
    
    let sdk_block = Code::Block {
      indent: self.indent,
      inline: false,
      start: format!("{crate_name}::sdk!("),
      body: vec![
        Code::Line(format!("{crate_name};")),
        Code::Line(format!("target: {t};", t=&lang.target)),
        Code::Block {
          indent: self.indent,
          inline: false,
          start: "tokens: [".to_owned(),
          body: self.token_block,
          end: "];".to_owned(),
        },
        Code::Block {
          indent: self.indent,
          inline: false,
          start: "regex: [".to_owned(),
          body: self.regex_block,
          end: "];".to_owned(),
        },
        Code::Block {
          indent: self.indent,
          inline: false,
          start: "rules: [".to_owned(),
          body: self.token_rule_block,
          end: "];".to_owned(),
        },
        Code::Block {
          indent: self.indent,
          inline: false,
          start: "semantics: [".to_owned(),
          body: self.semantic_block,
          end: "];".to_owned(),
        },
      ],
      end: ");".to_owned(),
    };
    let s: String = sdk_block.into();
    writeln!(output, "{}", s)?;

    let ast_mod_block = Code::Block {
      indent: self.indent,
      inline: false,
      start: "pub mod ast {".to_owned(),
      body: {
        let mut v = vec![
          Code::Line("use super::*;".to_owned()),
        ];
        v.append(&mut self.ast_block);
        v
      },
      end: "}".to_owned(),
    };
    let s: String = ast_mod_block.into();
    writeln!(output, "{}", s)?;

    let pt_mod_block = Code::Block {
      indent: self.indent,
      inline: false,
      start: "pub mod pt {".to_owned(),
      body: {
        let mut v = vec![
          Code::Line("use super::*;".to_owned()),
        ];
        v.append(&mut self.pt_block);
        v
      },
      end: "}".to_owned(),
    };
    let s: String = pt_mod_block.into();
    writeln!(output, "{}", s)?;

    for code in self.main_block {
      let s: String = code.into();
      writeln!(output, "{}", s)?;
    }

    Ok(output)

  }
}
