//! Core logic for parser hooks
use crate::core::ExtractFromLiteral;
use crate::grammar::{pt, Ctx};

/// Definition of a Parser Hook
///
/// After generating the Abstract Syntax Tree (AST), we still need to convert it to the internal objects.
/// This is done by the Parse Tree (PT) and Parser Hooks.
/// The PT uses the body of the rules to prelimilarily convert the AST to a simpler form
/// and the hooks can be used to inject custom validation logic and types to the PT.
/// The goal of the hooks is that the PT will produce the final object ready to be used by the application.
/// Therefore there is no need to do another round of traversing.
///
/// The hooks are extern and need to be imported, therefore 3 things need to be defined
/// - The module name as a string literal
/// - The function name
/// - The return type of the function (used to generate appropriate structs higher up in the PT)
///
/// The generated import statement depends on the target language:
/// - Rust: `use {module}::{name};`
/// - Python: `from {module} import {name}`
/// - TypeScript: `import { {name} } from "{module}";`
///
/// The hooks are implemented as extern functions. The exact implementation depends on the target language.
/// The inputs are:
/// - The PT node that is being processed
/// - The context object, which contains semantic information, errors, and optionally a custom context
///
/// The hook will wrap the PT node with a custom object returned by the hook
#[derive(Debug, Clone)]
pub struct Hook {
  /// Function name
  pub name: String,
  /// Return type
  pub return_type: String,
}

pub fn parse_hook(pt: &pt::HookAttribute, _: &mut Ctx) -> Option<Hook> {
  Some(Hook {
    name: pt.m_hook_name.strip_quotes(),
    return_type: pt.m_hook_type.strip_quotes(),
  })
}
