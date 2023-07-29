//! Generic implementations of functions and objects that are used to parse plain text into application specific objects.
//!
//! The SDK module is the base module of Regen that provides shared implementation for all language environment.
//! The rust code generated by the Regen compiler depends on this module to provide functionality.
//!
//! This module should not depend on the other Regen modules.

mod util;
pub use util::*;
mod token;
pub use token::*;
mod ast_parser;
/// Module with macros to implement the generated language environment.
pub mod gen;
pub use ast_parser::*;
mod parse_tree;
pub use parse_tree::*;

pub use heck::AsKebabCase;
pub use regex::Regex;

/// Convenience macro for casting enum variants for AST and PT.
#[macro_export]
macro_rules! tree_cast {
  (($($t:tt)*) $var:expr ) => {
    match $var {
      $($t)*(x) => x,
      _ => panic!(
        "Unable to tree_cast: Expected {}, got {:?}",
        stringify!($enum_variant),
        $var
      ),
    }
  };
}
