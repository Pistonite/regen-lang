use std::fs;
use std::path::Path;
mod html;
pub use html::{emit_html, to_prismjs};
mod code;
mod visitor;
pub use code::Code;
mod sdk;
pub use sdk::{emit_sdk, RustEmitter};

use crate::core::{Language, Rule, TokenDef, TokenRule};

pub trait Emitter {
  fn start(&mut self, lang: &Language) -> Result<(), Box<dyn std::error::Error>>;
  fn emit_include(&mut self, lang: &Language, path: &str)
    -> Result<(), Box<dyn std::error::Error>>;
  fn emit_token(
    &mut self,
    lang: &Language,
    token: &TokenDef,
  ) -> Result<(), Box<dyn std::error::Error>>;
  fn emit_token_rule(
    &mut self,
    lang: &Language,
    rule: &TokenRule,
  ) -> Result<(), Box<dyn std::error::Error>>;
  fn emit_semantic(
    &mut self,
    lang: &Language,
    semantic: &str,
  ) -> Result<(), Box<dyn std::error::Error>>;
  fn emit_rule(&mut self, lang: &Language, rule: &Rule) -> Result<(), Box<dyn std::error::Error>>;
  fn done(self, lang: &Language) -> Result<String, Box<dyn std::error::Error>>;
}

pub fn get_include_contents(from: &Path, path: &str) -> Result<String, Box<dyn std::error::Error>> {
  let p = from.join(path);
  match fs::read_to_string(p) {
    Ok(r) => Ok(r),
    Err(e) => Err(format!("Error reading file {path}: {e}").into()),
  }
}
