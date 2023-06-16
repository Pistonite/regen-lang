use html_escape;

mod token_type;
pub use token_type::TokenType;
mod token_stream;
pub use token_stream::TokenStream;
mod token_blocks;
pub use token_blocks::TokenBlocks;
pub mod lex;

/// Generic Token implementation
///
/// A Token is anything that contains a type, a value, and a position in the input text
#[derive(Debug, Clone)]
pub struct TokenImpl<T>
where
  T: TokenType,
{
  /// Token type
  pub token_type: T,
  /// Token value
  pub value: String,
  /// Token absolute position in the input text
  /// (start, end) - start is inclusive, end is exclusive
  pub pos: (usize, usize),
}

impl<T> TokenImpl<T>
where
  T: TokenType,
{
  /// Convert to another token type, with the value and position untouched
  #[inline]
  pub fn into<TO>(self) -> TokenImpl<TO>
  where
    TO: From<T> + TokenType,
  {
    TokenImpl {
      token_type: self.token_type.into(),
      value: self.value,
      pos: self.pos,
    }
  }
  #[inline]
  /// Convert the token to a raw html-escaped text without applying any style.
  pub fn to_html_text(&self) -> String {
    html_escape::encode_text(&self.value).to_string()
  }

  /// Convert the token to an html string with `class` attributes provided by the token type (may be `None`)
  ///
  /// This may return a `<span>` tag, or a raw string if the class name is `None`.
  pub fn to_html(&self) -> String {
    let text = self.to_html_text();
    match self.token_type.html_class() {
      None => text,
      Some(class_name) => {
        format!(r#"<span class="{class_name}">{text}</span>"#)
      }
    }
  }
}
