use crate::sdk::grammar::{pt, Ctx};
use crate::sdk::Error;

#[derive(Default)]
pub struct Context {
  pub context_type: Option<String>,
}

pub fn parse_context(pt: &pt::DefineContextStatement, ctx: &mut Ctx) -> Option<()> {
  if ctx.val.context_type.is_some() {
    ctx.err.push(Error::from_token(
      &pt.ast.m_context_type,
      "Context already defined".to_owned(),
      "Remove this duplicate definition".to_owned(),
    ));
  } else {
    ctx.val.context_type = Some(super::strip_quotes(&pt.m_context_type));
  }
  None
}
