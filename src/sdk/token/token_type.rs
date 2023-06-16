/// The trait for token type
///
/// This trait is used to provide a way to convert a token type to a string that can be used as a class name in HTML.
/// By default, the generated language SDK contains a `Tok` enum that implements this trait. (See [`crate::grammar::Tok`] for the one used internally by Regen)
pub trait TokenType: Clone {
  /// Get the content to put inside an HTMl class attribute (excluding the quotes)
  ///
  /// The returned string should be a valid class name in whatever context is used.
  /// If [`None`] is returned, it means that the text should not be wrapped with a `<span>`
  fn html_class(&self) -> Option<String>;
}

/// [`String`]s can be converted to HTML class names directly. Empty strings are mapped to [`None`]
impl TokenType for String {
  fn html_class(&self) -> Option<String> {
    if self.is_empty() {
      None
    } else {
      Some(self.clone())
    }
  }
}

/// [`Option`]s are mapped to [`None`] if [`None`], otherwise the inner value is mapped
impl<T> TokenType for Option<T>
where
  T: TokenType,
{
  fn html_class(&self) -> Option<String> {
    self.as_ref().and_then(|t| t.html_class())
  }
}
