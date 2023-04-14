use crate::sdk::generated::{PTDefineSemanticStatement, SemInfo};
use crate::sdk::RegenError;
pub use String;

/// Parser hook for semantic annotations
pub fn parse_semantic(pt: &PTDefineSemanticStatement, _si: &mut SemInfo, _errors: &mut Vec<RegenError>) -> Option<String> {
    Some(pt.val.clone())
}