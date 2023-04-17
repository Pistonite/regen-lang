use super::{Error, SemInfoImpl, Semantic, TokenImpl, TokenStream};

pub trait RootParser {
  type T: Clone;
  type S: Semantic<T = Self::T>;
  type A;
  type C;
  type P<'p>;
  fn do_tokenize(src: &str, si: &mut SemInfoImpl<Self::S>) -> Vec<TokenImpl<Self::T>>;
  fn parse_ast_root(
    ts: &mut TokenStream<Self::T>,
    si: &mut SemInfoImpl<Self::S>,
  ) -> Option<Self::A>;
  fn parse_pt_root<'a>(ast: &'a Self::A, ctx: &mut ContextImpl<Self::C, Self::S>) -> Self::P<'a>;
}

pub trait Environment {
  type T: Clone;
  type S: Semantic<T = Self::T>;
  type C;
  fn as_ctx(&self) -> &ContextImpl<Self::C, Self::S>;
  fn as_ctx_mut(&mut self) -> &mut ContextImpl<Self::C, Self::S>;
  fn tokenize(&mut self);
  fn parse_asts(&mut self);
  fn parse_pts_then<'s, 'p, P, F, R>(&'s mut self, consumer: F) -> Result<R, &[Error]>
  where
    F: FnOnce(Vec<<Self as RootParser>::P<'p>>, &<Self as RootParser>::C) -> Result<R, Vec<Error>>,
    Self: RootParser<P<'p> = P>,
    's: 'p;
}

pub struct ContextImpl<C, S>
where
  S: Semantic,
{
  pub si: SemInfoImpl<S>,
  pub err: Vec<Error>,
  pub val: Box<C>,
}

pub enum EnvMode {
  One,
  All,
}

#[derive(PartialEq, PartialOrd)]
enum EnvState {
  Init,
  Tokenized,
  ASTParsed,
}

pub struct EnvImpl<T, S, A, C>
where
  T: Clone,
  S: Semantic<T = T> + Clone,
{
  /// state
  state: EnvState,
  /// mode
  mode: EnvMode,
  /// source code
  pub src: String,
  /// tokens
  tokens: Vec<TokenImpl<T>>,
  /// context
  ctx: ContextImpl<C, S>,
  /// stack size for ast parsing
  stack_size: usize,

  /// ast
  asts: Vec<A>,
}

impl<T, S, A, C> From<EnvImpl<T, S, A, C>> for ContextImpl<C, S>
where
  T: Clone,
  S: Semantic<T = T> + Clone,
{
  fn from(env: EnvImpl<T, S, A, C>) -> Self {
    env.ctx
  }
}

impl<T, S, A> EnvImpl<T, S, A, ()>
where
  T: Clone,
  S: Semantic<T = T> + Clone,
  Self: RootParser<T = T, A = A, S = S>,
{
  pub fn new(source_code: &str, mode: EnvMode, stack_size: usize) -> Self {
    Self::new_ctx(source_code, mode, stack_size, ())
  }
}

impl<T, S, A, C> EnvImpl<T, S, A, C>
where
  T: Clone,
  S: Semantic<T = T> + Clone,
  Self: RootParser<T = T, A = A, S = S>,
{
  /// Create new environment for parsing the source code
  pub fn new_ctx(source_code: &str, mode: EnvMode, stack_size: usize, context: C) -> Self {
    let si = SemInfoImpl::new(source_code);
    Self {
      state: EnvState::Init,
      mode,
      src: source_code.to_owned(),
      ctx: ContextImpl {
        si,
        err: vec![],
        val: Box::new(context),
      },
      tokens: vec![],
      asts: vec![],
      stack_size,
    }
  }
}

impl<T, S, A, C> Environment for EnvImpl<T, S, A, C>
where
  T: Clone,
  S: Semantic<T = T> + Clone,
  Self: RootParser<T = T, A = A, S = S, C = C>,
{
  type T = T;
  type S = S;
  type C = C;
  fn as_ctx(&self) -> &ContextImpl<C, S> {
    &self.ctx
  }

  fn as_ctx_mut(&mut self) -> &mut ContextImpl<C, S> {
    &mut self.ctx
  }

  fn tokenize(&mut self) {
    if self.state == EnvState::Init {
      self.tokens = Self::do_tokenize(&self.src, &mut self.ctx.si);
      self.state = EnvState::Tokenized;
    }
  }

  fn parse_asts(&mut self) {
    if self.state >= EnvState::ASTParsed {
      return;
    }
    self.tokenize();

    let mut ts = TokenStream::new(&self.tokens, self.stack_size);
    let mut last_failed = false;
    let mut asts = Vec::new();

    loop {
      match Self::parse_ast_root(&mut ts, &mut self.ctx.si) {
        Some(ast) => {
          last_failed = false;
          asts.push(ast);
          if let EnvMode::One = self.mode {
            break;
          }
          if ts.is_exhausted() {
            break;
          }
        }
        None => {
          if let Some(token) = ts.consume() {
            if !last_failed {
              last_failed = true;
              if let Some(err_token) = ts.get_guess_err_token() {
                self.ctx.err.push(Error::from_token_without_help(
                  err_token,
                  "Syntax error near this location".to_owned(),
                ));
              } else {
                self.ctx.err.push(Error::from_token_without_help(
                  token,
                  "Syntax error".to_owned(),
                ));
              }
            }
          } else {
            break;
          }
        }
      }
    }
    self.state = EnvState::ASTParsed;
    self.asts = asts;
  }

  fn parse_pts_then<'s, 'p, P, F, R>(&'s mut self, consumer: F) -> Result<R, &[Error]>
  where
    F: FnOnce(Vec<<Self as RootParser>::P<'p>>, &<Self as RootParser>::C) -> Result<R, Vec<Error>>,
    's: 'p,
  {
    self.parse_asts();
    if !self.ctx.err.is_empty() {
      return Err(&self.ctx.err);
    }

    let mut pts = Vec::new();

    for ast in &self.asts {
      let pt = Self::parse_pt_root(ast, &mut self.ctx);
      pts.push(pt);
    }
    if !self.ctx.err.is_empty() {
      return Err(&self.ctx.err);
    }
    let result = consumer(pts, &self.ctx.val);
    //self.asts.append(&mut asts);
    match result {
      Ok(r) => Ok(r),
      Err(mut errs) => {
        self.ctx.err.append(&mut errs);
        Err(&self.ctx.err)
      }
    }
  }
}
