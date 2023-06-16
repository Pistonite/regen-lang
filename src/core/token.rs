//! Core logic of tokens and token rules
use crate::core::ExtractFromLiteral;
use crate::grammar::{pt, Ctx, Tok};
use crate::sdk::Error;

/// Definition for a token type used by the tokenizer
///
/// Each token definiton has a name and a flag to indicate if it should be extracted.
/// Extracted tokens are not used in AST generation (for example, comments), while ignored tokens are completely removed
/// from the rest of the parsing process.
///
/// Note that this should not be confused with the Token in the SDK, which is the token data structure at runtime when parsing.
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
pub enum TokenRule {
  /// Ignore rule matching a literal
  IgnoreLiteral(String /* literal */),
  /// Ignore rule matching a regular expression
  IgnoreRegExp(String /* regex */),
  /// Token rule matching a literal
  Literal(String /* token_type */, String /* literal */),
  /// Token rule matching a regular expression
  RegExp(String /* token_type */, String /* regex */),
}

pub fn parse_token_def(pt: &pt::DefineTokenTypeStatement, ctx: &mut Ctx) -> Option<()> {
  if pt.m_kw_extract {
    // Extracted tokens are not used in AST generation. Tag it to indicate that
    ctx.tbs.set(
      &pt.ast.m_token_type,
      Tok::Decor {
        tag: "unused".to_owned(),
        base: Box::new(Tok::SToken),
      },
    )
  }

  if !ctx.val.add_token(TokenDef {
    name: pt.m_token_type.clone(),
    is_extract: pt.m_kw_extract,
  }) {
    let name = &pt.m_token_type;
    let msg = format!("Duplicate token definition: {name}");
    let help = "Remove or rename the duplicate definition".to_owned();
    ctx
      .err
      .push(Error::from_token(&pt.ast.m_token_type, msg, help));
  }
  None
}

pub fn parse_token_ignore_rule(
  pt: &pt::DefineIgnoreTokenRuleStatement,
  ctx: &mut Ctx,
) -> Option<()> {
  let token_rule = match pt.m_value.as_ref() {
    pt::LiteralOrRegExp::TokenLiteral(literal) => {
      TokenRule::IgnoreLiteral(literal.m_t.strip_quotes())
    }
    pt::LiteralOrRegExp::TokenRegExp(regexp) => {
      TokenRule::IgnoreRegExp(regexp.m_t.strip_and_escape_regex())
    }
  };

  ctx.val.add_token_rule(token_rule);
  None
}

pub fn parse_token_rule(pt: &pt::DefineTokenRuleStatement, ctx: &mut Ctx) -> Option<()> {
  let token_rule = match pt.m_value.as_ref() {
    pt::LiteralOrRegExp::TokenLiteral(literal) => {
      TokenRule::Literal(pt.m_token_type.clone(), literal.m_t.strip_quotes())
    }
    pt::LiteralOrRegExp::TokenRegExp(regexp) => {
      TokenRule::RegExp(pt.m_token_type.clone(), regexp.m_t.strip_and_escape_regex())
    }
  };

  ctx.val.add_token_rule(token_rule);
  None
}
