use crate::sdk::{TokenImpl, TokenType};

/// Semantic information that is stored and updated during the parsing process
///
/// The difference between `TokenBlocks` and [`TokenStream`](crate::sdk::TokenStream)
/// is that `TokenBlocks` is used for overwriting semantic type for tokens in any position, while `TokenStream`
/// is used for consuming one token at a time. The `get_spans` and `get_html` methods can
/// be used to get the semantic tokens.
pub struct TokenBlocks<S>
where
  S: TokenType,
{
  /// Source code.
  ///
  /// This is needed because the blocks may not cover the entire source code
  src: String,
  /// Semantic token blocks
  blocks: Vec<TokenImpl<S>>,
}

impl<S> AsRef<[TokenImpl<S>]> for TokenBlocks<S>
where
  S: TokenType,
{
  /// Get the semantic blocks currently stored as a slice.
  ///
  /// The returned semantic tokens may not be contiguous (i.e. maybe have gaps).
  /// Use one of the `get_spans` method to compute contiguous semantic tokens that cover the entire source code.
  fn as_ref(&self) -> &[TokenImpl<S>] {
    &self.blocks
  }
}

impl<S> TokenBlocks<S>
where
  S: TokenType,
{
  /// Create a new instance with the given source code
  pub fn new(src: &str) -> Self {
    Self {
      src: src.to_string(),
      blocks: Vec::new(),
    }
  }

  /// Insert all tokens and mark them with the associated token semantic
  pub fn insert_all<T>(&mut self, tokens: &[TokenImpl<T>])
  where
    T: TokenType + Into<S>,
  {
    self
      .blocks
      .extend(tokens.iter().cloned().map(|t| TokenImpl {
        pos: t.pos,
        value: t.value,
        token_type: t.token_type.into(),
      }));
    self.blocks.sort_by(|a, b| a.pos.0.cmp(&b.pos.0))
  }

  /// Set the semantic of a token
  ///
  /// This replaces the semantic type of the current token if there exists one that starts at the same position (and assumes it ends at the same position, without additional checks).
  /// If there is no token that starts at the same position, it inserts a new token without checking for overlapping.
  ///
  /// It also assumes the token in the parameter and the existing token stored have the same content
  pub fn set<T>(&mut self, token: &TokenImpl<T>, semantic_type: S)
  where
    T: TokenType,
    S: From<T>,
  {
    let result = self
      .blocks
      .binary_search_by(|probe| probe.pos.0.cmp(&token.pos.0));
    match result {
      Ok(index) => {
        self.blocks[index].token_type = semantic_type;
      }
      Err(index) => {
        self.blocks.insert(
          index,
          TokenImpl {
            token_type: semantic_type,
            value: token.value.clone(),
            pos: token.pos,
          },
        );
      }
    }
  }

  /// Get the semantic spans.
  ///
  /// The difference between `as_ref` and `get_spans` is that `as_ref` returns the semantic tokens as they are stored,
  /// while `get_spans` returns a continuous list of spans that cover the entire source code.
  /// If you don't care about the gaps, use `as_ref` instead to avoid extra computation.
  ///
  /// The `converter` parameter can be used to convert the stored token type to any custom type that implements the `TokenType` trait.
  pub fn get_spans<T, F>(&self, converter: F) -> Vec<TokenImpl<Option<T>>>
  where
    F: Fn(S) -> T,
    T: TokenType,
  {
    // TODO: we can combine adjacent blocks with the same semantic
    let mut code_blocks = Vec::new();
    let mut cur = 0;
    for semantic_token in &self.blocks {
      let (start, end) = semantic_token.pos;
      if start > cur {
        code_blocks.push(TokenImpl {
          token_type: None,
          value: self.src[cur..start].to_owned(),
          pos: (cur, start),
        });
      }
      cur = cur.max(start);
      if end > cur {
        code_blocks.push(TokenImpl {
          token_type: Some(converter(semantic_token.token_type.clone())),
          value: self.src[cur..end].to_owned(),
          pos: (cur, start),
        });
        cur = end;
      }
    }
    if cur < self.src.len() {
      code_blocks.push(TokenImpl {
        token_type: None,
        value: self.src[cur..].to_owned(),
        pos: (cur, self.src.len()),
      });
    }
    code_blocks
  }

  /// Get the semantic spans as html
  ///
  /// This is a wrapper that calls [`get_spans`](TokenBlocks::get_spans) and then converts the semantic tokens to html.
  /// The converter function is be used to convert the stored token type to another custom type.
  pub fn get_html<T, F>(&self, converter: F) -> String
  where
    F: Fn(S) -> T,
    T: TokenType,
  {
    self
      .get_spans(converter)
      .iter()
      .map(|s| s.to_html())
      .collect::<Vec<String>>()
      .join("")
  }
}
