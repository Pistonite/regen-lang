use crate::sdk::grammar::{pt, Ctx};

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
/// - The AST node that is being processed
/// - The PT node that is being processed
/// - The semantic information (i.e. the syntax highlighter)
/// - The list of errors
///
/// The hook will take ownership of the PT node, and return the custom object in the place of it.
#[derive(Debug)]
pub struct Hook {
  /// Function name
  pub name: String,
  /// Return type
  pub return_type: String,
}

pub fn parse_hook(pt: &pt::HookAttribute, _: &mut Ctx) -> Option<Hook> {
  Some(Hook {
    name: super::strip_quotes(&pt.m_hook_name),
    return_type: super::strip_quotes(&pt.m_hook_type),
  })
}
