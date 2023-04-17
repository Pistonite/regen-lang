mod rust;
use crate::core::Language;
use crate::emit::Emitter;
use crate::sdk::generated::Env;
use crate::sdk::Environment;
pub use rust::RustEmitter;

pub fn emit_sdk<E>(env: &mut Env, emitter: E) -> Result<Option<String>, Box<dyn std::error::Error>>
where
  E: Emitter,
{
  // Parse the input file and convert it to language
  let lang = match env.parse_pts_then(Language::try_from) {
    Ok(r) => r,
    Err(_) => {
      return Ok(None);
    }
  };
  // Emit the language
  let output = lang.emit_sdk(emitter)?;
  Ok(Some(output))
}
