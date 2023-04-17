use super::TokenImpl;
use std::fmt::Write;
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

  pub fn from_token<T>(token: &TokenImpl<T>, msg: String, help: String) -> Self {
    Self {
      msg,
      help: Some(help),
      pos: token.pos,
    }
  }

  pub fn from_token_without_help<T>(token: &TokenImpl<T>, msg: String) -> Self {
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
