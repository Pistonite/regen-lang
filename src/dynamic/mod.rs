//! Utils for dynamically generate AST for source code based on the Language object.
//! Note that this only includes up to the AST, since parse tree hooks cannot be executed dynamically.

mod dyn_lang;
pub use dyn_lang::*;
mod dyn_node;
pub use dyn_node::*;

use crate::sdk::TokenImpl;

/// Dynamic token type.
///
/// The name of the token or semantic is used directly as the type.
/// Tokens are prefixed with `T` and Semantics are prefixed with `S`.
pub type DynTok = String;
/// Dynamic token implementation
pub type DynToken = TokenImpl<DynTok>;
