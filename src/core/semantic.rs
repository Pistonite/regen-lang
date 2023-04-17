use crate::sdk::grammar::{pt, Ctx};

/// Parser hook for semantic annotations
pub fn parse_semantic(pt: &pt::DefineSemanticStatement, _: &mut Ctx) -> Option<String> {
  Some(pt.m_id.clone())
}
