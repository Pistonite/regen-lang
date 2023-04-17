use crate::core::{Language, Rule, TokenDef, TokenRule};
use std::error::Error;

pub trait Emitter {
  fn emit_include(&mut self, lang: &Language, path: &str);
  fn emit_token(&mut self, lang: &Language, token: &TokenDef);
  fn emit_token_rule(&mut self, lang: &Language, rule: &TokenRule);
  fn emit_semantic(&mut self, lang: &Language, semantic: &str);
  fn emit_rule(&mut self, lang: &Language, rule: &Rule);
  fn done(&mut self) -> Result<(), Box<dyn Error>>;
}
