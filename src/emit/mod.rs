mod html;
pub use html::emit_html;

use crate::sdk::Error;

pub fn emit_sdk(
    template_sdk: &str,
    source: &str,
    stack_size: usize,
) -> (String, Vec<Error> 
) {
    todo!()
}