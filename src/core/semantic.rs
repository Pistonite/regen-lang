use crate::sdk::generated::{PTDefineSemanticStatement, SemInfo};
use crate::sdk::Error;
pub use String;

/// Parser hook for semantic annotations
pub fn parse_semantic(pt: &PTDefineSemanticStatement, _: &mut SemInfo, _: &mut Vec<Error>) -> Option<String> {
    Some(pt.m_id.clone())
}