//! Functionality to generate output from the [`Language`](crate::core::Language) struct.
//!
//! The Emit module includes implementation for the environment to generate code such as semantic analysis in HTML,
//! or target language environment in Rust, TypeScript (TODO), Python (TODO), etc.

use std::fs;
use std::path::Path;
mod rust;
pub use rust::RustEmitter;
mod emitter;
pub use emitter::Emitter;

pub fn get_include_contents(from: &Path, path: &str) -> Result<String, Box<dyn std::error::Error>> {
  let p = from.join(path);
  match fs::read_to_string(&p) {
    Ok(r) => Ok(r),
    Err(e) => Err(format!("Error reading file {p}: {e}", p = p.display()).into()),
  }
}
