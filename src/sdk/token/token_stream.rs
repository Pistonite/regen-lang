use crate::sdk::{TokenImpl, TokenType};

/// A generic implementation of a stream of tokens with type `T`
///
/// The TokenStream implementation does not own the Tokens.
/// It returns references to the tokens on demand. The caller can clone them if needed.
///
/// The token streams are used internally to generate the AST.
/// They are not meant to be used independently.
///
pub struct TokenStream<'t, T>
where
  T: TokenType,
{
  /// Token data
  tokens: &'t [TokenImpl<T>],
  /// Current token index
  index: usize,
  /// Position stack
  stack: Vec<usize>,
  /// Max stack size
  max_stack_size: usize,
  /// Best guess at which token is causing a syntax error
  best_error_guess: usize,
}

impl<'t, T> TokenStream<'t, T>
where
  T: TokenType,
{
  /// Create a new TokenStream with the given tokens
  pub fn new(tokens: &'t [TokenImpl<T>], max_stack_size: usize) -> Self {
    Self {
      tokens,
      index: 0,
      stack: Vec::new(),
      max_stack_size,
      best_error_guess: 0,
    }
  }

  /// Returns if there is no token left after the current position
  #[inline]
  pub fn is_exhausted(&self) -> bool {
    self.index >= self.tokens.len()
  }

  /// Get the best guess at which token is causing a syntax error
  pub fn get_guess_err_token(&self) -> Option<&'t TokenImpl<T>> {
    self
      .tokens
      .get(self.best_error_guess)
      .or(self.tokens.get(self.index))
  }

  /// Set the error guess at the current index
  pub fn set_error(&mut self, force: bool) {
    if force || self.index > self.best_error_guess {
      self.best_error_guess = self.index;
    }
  }

  /// Returns the next token if available, and advance the position
  /// A reference is returned to avoid copying the token
  pub fn consume(&mut self) -> Option<&'t TokenImpl<T>> {
    match self.tokens.get(self.index) {
      Some(token) => {
        self.index += 1;
        Some(token)
      }
      None => None,
    }
  }

  /// Push the current position to stack so it can be restored
  pub fn push(&mut self) -> bool {
    if self.stack.len() >= self.max_stack_size {
      return false;
    }
    self.stack.push(self.index);
    true
  }

  /// Pop position stack without restoring the position
  #[inline]
  pub fn pop(&mut self) {
    self.stack.pop();
  }

  /// Restore the position to be the index on the stack top.
  /// This does not pop the stack.
  #[inline]
  pub fn restore(&mut self) {
    self.index = *self.stack.last().unwrap();
  }
}

/// Helper for parsing an optional parameter using a TokenStream
///
/// This will save the current position (push), try to execute the parse (which returns an [`Option`]),
/// and restore the position if the parse failed.
///
/// See source code for [`crate::grammar`] for examples.
#[macro_export]
macro_rules! optional {
  ( $ts:ident, $inner_optional:expr ) => {
    // save the pos to restore in case of failure
    if !$ts.push() {
      None
    } else {
      let inner = $inner_optional;
      if inner.is_none() {
        // restore if failure
        $ts.restore();
      }
      // remove the saved pos
      $ts.pop();
      inner
    }
  };
}

/// Helper for parsing a required parameter using a TokenStream
///
/// This will try to execute the parse (which returns an [`Option`]).
/// If the parse failed, it will mark an error on the TokenStream.
///
/// Note that this does not change the control flow. The caller should
/// still use the `?` operator for early return if needed. This is intentional
/// to make the early return more obvious.
#[macro_export]
macro_rules! required {
  ( $ts:ident, $inner_required:expr ) => {{
    let inner = $inner_required;
    if inner.is_none() {
      $ts.set_error(false);
    }
    inner
  }};
}

/// helper for parsing a list of parameters using a TokenStream
///
/// Optional list would be
/// ```nocompile
/// let mut v = vec![];
/// list!(ts, v, $inner)
/// ```
///
/// Required (at least one) would be
/// ```nocompile
/// let mut v = vec![required!(ts, $inner)?];
/// list!(ts, v, $inner)
/// ```
#[macro_export]
macro_rules! list {
  ( $ts:ident, $list:ident, $inner:expr ) => {{
    loop {
      if !$ts.push() {
        break;
      }
      match $inner {
        Some(item) => {
          $list.push(item);
          $ts.pop();
        }
        None => {
          // restore if failure
          $ts.restore();
          $ts.pop();
          break;
        }
      }
    }
    $list
  }};
}
