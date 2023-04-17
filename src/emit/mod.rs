use heck::{ToSnakeCase, ToUpperCamelCase};
use std::collections::HashMap;
use std::path::Path;
use std::fs;
mod html;
pub use html::{to_prismjs, emit_html};
mod visitor;
mod code;
pub use code::Code;
mod sdk;
pub use sdk::{emit_sdk, RustEmitter};

use crate::core::{
  Param, ParamType, RetType, 
  Language,
  TokenDef,
  TokenRule,
  Rule,
};

pub trait Emitter {
  fn start(&mut self, lang: &Language) -> Result<(), Box<dyn std::error::Error>>;
  fn emit_include(&mut self, lang: &Language, path: &str)-> Result<(), Box<dyn std::error::Error>>;
  fn emit_token(&mut self, lang: &Language, token: &TokenDef)-> Result<(), Box<dyn std::error::Error>>;
  fn emit_token_rule(&mut self, lang: &Language, rule: &TokenRule)-> Result<(), Box<dyn std::error::Error>>;
  fn emit_semantic(&mut self, lang: &Language, semantic: &str)-> Result<(), Box<dyn std::error::Error>>;
  fn emit_rule(&mut self, lang: &Language, rule: &Rule)-> Result<(), Box<dyn std::error::Error>>;
  fn done(self, lang: &Language) -> Result<String, Box<dyn std::error::Error>>;
}

pub fn get_include_contents(from: &Path, path: &str) -> Result<String, Box<dyn std::error::Error>> {
  let p = from.join(path);
  println!("{}, {}, {}", from.display(), path, p.display());
  Ok(fs::read_to_string(p).unwrap())
}

pub trait NameContext {
  /// The namespace of Abstract Syntax Tree struct/types
  fn ast_ns(&self) -> String;
  /// The name of the Abstract Syntax Tree struct/type
  fn ast(&mut self, rule: &str, in_main_namespace: bool) -> String;
  /// The namespace of the Parse Tree struct/types
  fn pt_ns(&self) -> String;
  /// The inner (unhooked) name of the Parse Tree struct/type
  fn pt_inner_type(&mut self, rule: &str, in_main_namespace: bool) -> String;
  /// The outer (hooked) name of the Parse Tree node struct/type
  fn pt_outer_type(&mut self, rule: &str) -> String;
  /// The name of a member (field) of the Abstract Syntax Tree or Parse Tree
  fn member_name(&mut self, name: &str) -> String;
  /// The name of a member (field) of the Abstract Syntax Tree or Parse Tree
  fn member_name_with_idx(&mut self, name: &str, i: usize) -> String;
  /// The type of a member (field) of the Abstract Syntax Tree
  fn ast_param_type(&mut self, param: &Param) -> String;
  /// The type of a member (field) of the Parse Tree
  fn pt_var_type(&mut self, var_type: &ParamType) -> String;
  /// The type of member vectors of the Parse Tree
  /// Returns the type of the AST vector and the value vector
  fn pt_vec_type(&mut self, item_ret_type: &RetType) -> (String, String);
}

pub struct RustNameContext<'a> {
  rules: HashMap<String, &'a Rule>,
  ast_name_cache: HashMap<String, String>,
  pt_inner_name_cache: HashMap<String, String>,
}

impl<'a> RustNameContext<'a> {
  pub fn new(rules: &'a HashMap<String, Rule>) -> Self {
    Self {
      rules: rules.iter().map(|(k, v)| (k.clone(), v)).collect(),
      ast_name_cache: HashMap::new(),
      pt_inner_name_cache: HashMap::new(),
    }
  }
}

impl<'a> NameContext for RustNameContext<'a> {
  fn ast_ns(&self) -> String {
    "ast".to_owned()
  }

  fn ast(&mut self, rule: &str, in_main_namespace: bool) -> String {
    let name = if let Some(name) = self.ast_name_cache.get(rule) {
      name.clone()
    } else {
      let n = rule.to_upper_camel_case();
      self.ast_name_cache.insert(rule.to_owned(), n.clone());
      n
    };
    if in_main_namespace {
      format!("{}::{}", self.ast_ns(), name)
    } else {
      name
    }
  }

  fn pt_ns(&self) -> String {
    "pt".to_owned()
  }

  fn pt_inner_type(&mut self, rule: &str, in_main_namespace: bool) -> String {
    let name = if let Some(name) = self.pt_inner_name_cache.get(rule) {
      name.clone()
    } else {
      let n = rule.to_upper_camel_case();
      self.pt_inner_name_cache.insert(rule.to_owned(), n.clone());
      n
    };
    if in_main_namespace {
      format!("{}::{}", self.pt_ns(), name)
    } else {
      name
    }
  }

  fn pt_outer_type(&mut self, rule: &str) -> String {
    match &self.rules.get(rule).unwrap().hook {
      Some(hook) => {
        let name = hook.return_type.clone();
        // Always use the namespaced name for the inner type
        let inner_type = self.pt_inner_type(rule, true);
        format!("ParseHook<{name}, {inner_type}<'p>>")
      }
      None => format!("{}<'p>", self.pt_inner_type(rule, true)),
    }
  }

  fn member_name(&mut self, name: &str) -> String {
    format!("m_{name}").to_snake_case()
  }

  fn member_name_with_idx(&mut self, name: &str, i: usize) -> String {
    format!("m_{name}_{i}").to_snake_case()
  }

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

  fn pt_var_type(&mut self, var_type: &ParamType) -> String {
    match var_type {
      ParamType::Item(optional, name) => {
        let pt_type = self.pt_outer_type(name);
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

  fn pt_vec_type(&mut self, item_ret_type: &RetType) -> (String, String) {
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
              let pt_t = self.pt_outer_type(name);

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
}
