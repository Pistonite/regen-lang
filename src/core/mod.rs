//! Core definition of the domain objects that are used to represent the Regen language
//!
//! Contains the domain objects like [`Rule`], the highest of which is [`Language`],
//! which is what the compiler creates at runtime based on the input grammar file.

use crate::grammar::{pt, Ctx};
use crate::sdk::Error;

pub mod hook;
pub use hook::Hook;
pub mod lang;
pub use lang::{LangBuilder, Language};
pub mod param;
pub use param::{Param, ParamDataType, ParamDecorType, ParamType};
pub mod rule;
pub use rule::{Rule, RuleValue};
pub mod token;
pub use token::{TokenDef, TokenRule};

pub trait ExtractFromLiteral {
    fn strip_quotes(&self) -> String;
    fn strip_and_escape_regex(&self) -> String;
}

impl ExtractFromLiteral for String {
    fn strip_quotes(&self) -> String {
        if !self.starts_with('"') || !self.ends_with('"') {
            panic!("Expected string to start and end with quotes");
        }
        self[1..self.len() - 1].to_string()
    }

    fn strip_and_escape_regex(&self) -> String {
        if !self.starts_with('/') || !self.ends_with('/') {
            panic!("Expected string to start and end with slashes");
        }
        self[1..self.len() - 1].replace(r"\/", "/")
    }
}

// parser hooks
/// Parser hook for context definition
pub fn parse_context(pt: &pt::DefineContextStatement, ctx: &mut Ctx) -> Option<()> {
    let context_type = pt.m_context_type.strip_quotes();
    if !ctx.val.set_context(context_type) {
        ctx.err.push(Error::from_token(
            &pt.ast.m_context_type,
            "Context already defined".to_owned(),
            "Remove this duplicate definition".to_owned(),
        ));
    }
    None
}

/// Parser hook for semantic annotations
pub fn parse_semantic(pt: &pt::DefineSemanticStatement, ctx: &mut Ctx) -> Option<()> {
    if !ctx.val.add_semantic(pt.m_id.clone()) {
        let name = &pt.m_id;
        let msg = format!("Duplicate semantic definition: {name}");
        let help = "Remove or rename the duplicate definition".to_owned();
        ctx.err.push(Error::from_token(&pt.ast.m_id, msg, help));
    }
    None
}

/// Parser hook for include statements
pub fn parse_include(pt: &pt::DefineIncludeStatement, ctx: &mut Ctx) -> Option<()> {
    ctx.val.add_include(pt.m_path.strip_quotes());
    None
}
