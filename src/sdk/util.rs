use crate::sdk::{token::TokenType, TokenImpl};
use std::fmt::Write;

/// TODO doc needed
#[derive(Debug)]
pub struct ParseHook<H, P> {
  pub pt: P,
  pub val: Option<H>,
}

impl<H, P> ParseHook<H, P> {
  pub fn take_unchecked(&mut self) -> H {
    self.val.take().unwrap_or_else(||panic!("ParseHook::take() called on None. Make sure your parse hook function is not taking the value twice."))
  }
}

/// Struct that represents result of [`merge_list_tail`] and [`merge_list_tail_optional_first`] macro
pub struct MergedListTail<A, P> {
  /// If the first member is present.
  ///
  /// This will always be true from [`merge_list_tail`] macro.
  pub has_first: bool,
  /// The merged list of values.
  pub vals: Vec<P>,
  /// The merged list of ast node references.
  pub asts: Vec<A>,
}

impl<A, P> IntoIterator for MergedListTail<A, P> {
  type Item = (A, P);
  type IntoIter = std::iter::Zip<std::vec::IntoIter<A>, std::vec::IntoIter<P>>;

  /// Convert into a parallel iterator of ast node references and values.
  fn into_iter(self) -> Self::IntoIter {
    self.asts.into_iter().zip(self.vals.into_iter())
  }
}

/// Util macro to merge `first` and `rest` in a parse tree.
///
/// The language definition syntax sometimes forces a list to be defined as a first element and a tail.
/// This macro makes it easier to parse such lists.
///
/// See [`MergedListTail`] for more details of the merged result.
///
/// # Example
/// ```nocompile
/// // Example definition of a list of numbers separated with "+" sign
/// // rule NumberList(first: token Number, rest: optional NumberListTail+);
/// // rule NumberListTail(_: token Symbol"+", n: token Number);
///
/// let pt = /* instance of the generated parse tree */;
/// The processor function takes a &String, which is the type of `token Number` in the parse tree
/// let merged = merge_list_tail!(pt, m_first, m_rest, m_n);
#[macro_export]
macro_rules! merge_list_tail {
  ($pt:ident, $first:ident, $rest:ident, $rest_member:ident) => {{
    let mut vals = vec![$pt.$first.as_ref()];
    let mut asts = vec![&$pt.ast.$first];
    for x in &mut $pt.$rest {
      vals.push(x.$rest_member.as_ref());
      asts.push(&x.ast.$rest_member);
    }
    $crate::sdk::MergedListTail {
      has_first: true,
      vals,
      asts,
    }
  }};
  (mut $pt:ident, $first:ident, $rest:ident, $rest_member:ident) => {{
    let mut vals = vec![$pt.$first.as_mut()];
    let mut asts = vec![&$pt.ast.$first];
    for x in &mut $pt.$rest {
      vals.push(x.$rest_member.as_mut());
      asts.push(&x.ast.$rest_member);
    }
    $crate::sdk::MergedListTail {
      has_first: true,
      vals,
      asts,
    }
  }};
}

/// Util macro to merge `first` and `rest` in a parse tree.
///
/// Like [`merge_list_tail`], but the first member is optional.
///
/// # Example
/// ```nocompile
/// // Example definition of a list of numbers separated with "+" sign
/// // rule NumberList(first: optional token Number, rest: optional NumberListTail+);
/// // rule NumberListTail(_: token Symbol"+", n: token Number);
///
/// let pt = /* instance of the generated parse tree */;
/// The processor function takes a &String, which is the type of `token Number` in the parse tree
/// let merged = merge_list_tail_optional_first!(pt, m_first, m_rest, m_n, |x: &String| x.parse::<i32>());
/// ```
#[macro_export]
macro_rules! merge_list_tail_optional_first {
  ($pt:ident, $first:ident, $rest:ident, $rest_member:ident) => {{
    let (has_first, mut vals, mut asts) = match &$pt.$first {
      Some(x) => (
        true,
        vec![x.as_ref()],
        vec![$pt.ast.$first.as_ref().unwrap()],
      ),
      None => (false, vec![], vec![]),
    };
    for x in &$pt.$rest {
      vals.push(x.$rest_member.as_ref());
      asts.push(&x.ast.$rest_member);
    }
    $crate::sdk::MergedListTail {
      has_first,
      vals,
      asts,
    }
  }};
  (mut $pt:ident, $first:ident, $rest:ident, $rest_member:ident) => {{
    let (has_first, mut vals, mut asts) = match &mut $pt.$first {
      Some(x) => (
        true,
        vec![x.as_mut()],
        vec![$pt.ast.$first.as_ref().unwrap()],
      ),
      None => (false, vec![], vec![]),
    };
    for x in &mut $pt.$rest {
      vals.push(x.$rest_member.as_mut());
      asts.push(&x.ast.$rest_member);
    }
    $crate::sdk::MergedListTail {
      has_first,
      vals,
      asts,
    }
  }};
}

/// TODO doc needed
#[derive(Debug, Clone)]
pub struct Error {
  pub msg: String,
  pub help: Option<String>,
  pub pos: (usize, usize),
}

impl Error {
  pub fn global(msg: String, help: String) -> Self {
    Self {
      msg,
      help: Some(help),
      pos: (0, 1),
    }
  }

  pub fn from_token<T>(token: &TokenImpl<T>, msg: String, help: String) -> Self
  where
    T: TokenType,
  {
    Self {
      msg,
      help: Some(help),
      pos: token.pos,
    }
  }

  pub fn from_token_without_help<T>(token: &TokenImpl<T>, msg: String) -> Self
  where
    T: TokenType,
  {
    Self {
      msg,
      help: None,
      pos: token.pos,
    }
  }

  pub fn pretty(&self, source: &str, context: usize) -> Result<String, std::fmt::Error> {
    let mut output = String::new();
    let (l, c) = self.to_line_col(source);
    let m = &self.msg;
    writeln!(output, "error: {m}")?;
    writeln!(output, "   --> {l}:{c}")?;
    writeln!(output, "    |")?;

    let start = if l > context { l - context } else { 1 };
    for (i, line_str) in source
      .lines()
      .skip(start - 1)
      .take(context * 2 + 1)
      .enumerate()
    {
      let current = i + start;
      writeln!(output, "{current:3} | {line_str}")?;
      let help = self
        .help
        .as_ref()
        .map(|s| format!(" help: {}", s))
        .unwrap_or_default();
      if current == l {
        writeln!(
          output,
          "{:3} | {:>width$}{t}{help}",
          "",
          "",
          width = c - 1,
          t = "^".repeat(self.pos.1 - self.pos.0)
        )?;
      }
    }

    Ok(output)
  }

  fn to_line_col(&self, content: &str) -> (usize, usize) {
    let (start, _) = self.pos;
    let mut cur = 0;
    let mut l = 1;
    for line in content.split('\n') {
      let line_len = line.len() + 1;
      if cur + line_len > start {
        return (l, start - cur + 1);
      }
      cur += line_len;
      l += 1;
    }
    (l, 0)
  }
}
