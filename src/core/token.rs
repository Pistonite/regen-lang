use crate::sdk::generated::{pt, Sem, SemInfo};
use crate::sdk::Error;

/// Definition for a token type used by the tokenizer
///
/// Each token definiton has a name and a flag to indicate if it should be extracted.
/// Extracted tokens are not used in AST generation (for example, comments), while ignored tokens are completely removed
/// from the rest of the parsing process.
///
/// Note that this should not be confused with the Token in the SDK, which is the token data structure at runtime when parsing.
#[derive(Debug)]
pub struct TokenDef {
  /// Token type name
  pub name: String,
  /// If the token type should be extracted
  pub is_extract: bool,
}

/// Definition for a token rule used by the tokenizer
///
/// On each tokenization step, the tokenizer will try to match the input against the token rules.
/// The longest token matched will be the next token produced.
///
/// If a token cannot be matched, one character is skipped and added to the unmatchable token list.
#[derive(Debug)]
pub enum TokenRule {
  /// Ignore rule matching a literal
  IgnoreLiteral(String /* literal */),
  /// Ignore rule matching a regular expression
  IgnoreRegExp(String /* regex */),
  /// Token rule matching a literal
  Literal(String /* token_type */, String),
  /// Token rule matching a regular expression
  RegExp(String /* token_type */, String),
}

impl TokenRule {
  pub fn get_regex(&self) -> Option<&str> {
    match self {
      TokenRule::IgnoreLiteral(_) => None,
      TokenRule::IgnoreRegExp(regex) => Some(regex),
      TokenRule::Literal(_, _) => None,
      TokenRule::RegExp(_, regex) => Some(regex),
    }
  }
}

pub fn parse_token_def(
  pt: &pt::DefineTokenTypeStatement,
  si: &mut SemInfo,
  _errors: &mut Vec<Error>,
) -> Option<TokenDef> {
  if pt.m_kw_extract {
    // Extracted tokens are not used in AST generation. Tag it to indicate that
    si.set(
      &pt.ast.m_token_type,
      Sem::Tag("unused".to_owned(), Box::new(Sem::SToken)),
    )
  }
  Some(TokenDef {
    name: pt.m_token_type.clone(),
    is_extract: pt.m_kw_extract,
  })
}

pub fn parse_token_ignore_rule(
  pt: &pt::DefineIgnoreTokenRuleStatement,
  _si: &mut SemInfo,
  _errors: &mut Vec<Error>,
) -> Option<TokenRule> {
  match pt.m_value.as_ref() {
    pt::LiteralOrRegExp::TokenLiteral(literal) => {
      Some(TokenRule::IgnoreLiteral(super::strip_quotes(&literal.m_t)))
    }
    pt::LiteralOrRegExp::TokenRegExp(regexp) => {
      Some(TokenRule::IgnoreRegExp(super::strip_slashes(&regexp.m_t)))
    }
  }
}

pub fn parse_token_rule(
  pt: &pt::DefineTokenRuleStatement,
  _si: &mut SemInfo,
  _errors: &mut Vec<Error>,
) -> Option<TokenRule> {
  match pt.m_value.as_ref() {
    pt::LiteralOrRegExp::TokenLiteral(literal) => Some(TokenRule::Literal(
      pt.m_token_type.clone(),
      super::strip_quotes(&literal.m_t),
    )),
    pt::LiteralOrRegExp::TokenRegExp(regexp) => Some(TokenRule::RegExp(
      pt.m_token_type.clone(),
      super::strip_slashes(&regexp.m_t),
    )),
  }
}
