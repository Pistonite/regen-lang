use crate::sdk::{TokenBlocks, TokenImpl, TokenType};

mod base;
use base::State;
pub use base::{ContextImpl, Environment, Mode, RootParser};
mod env_environment;

/// Environment implementation
pub struct EnvImpl<T, A, C>
where
  T: TokenType,
{
  /// state
  state: State,
  /// mode
  mode: Mode,
  /// source code
  src: String,
  /// tokens
  tokens: Vec<TokenImpl<T>>,
  /// context
  pub ctx: ContextImpl<C, T>,
  /// stack size for ast parsing
  stack_size: usize,
  /// ast
  asts: Vec<A>,
}

impl<T, A> EnvImpl<T, A, ()>
where
  T: TokenType,
  Self: RootParser<T = T, A = A, C = ()>,
{
  /// Create new environment with a unit context for parsing the source code.
  pub fn new(source_code: &str, mode: Mode, stack_size: usize) -> Self {
    Self::new_ctx(source_code, mode, stack_size, ())
  }
}

impl<T, A, C> EnvImpl<T, A, C>
where
  T: TokenType,
  C: Default,
  Self: RootParser<T = T, A = A, C = C>,
{
  /// Create new environment with a default context for parsing the source code.
  pub fn new_default(source_code: &str, mode: Mode, stack_size: usize) -> Self {
    Self::new_ctx(source_code, mode, stack_size, Default::default())
  }
}

impl<T, A, C> EnvImpl<T, A, C>
where
  T: TokenType,
  Self: RootParser<T = T, A = A, C = C>,
{
  /// Create new environment with the given context for parsing the source code.
  pub fn new_ctx(source_code: &str, mode: Mode, stack_size: usize, context: C) -> Self {
    Self {
      state: State::Init,
      mode,
      src: source_code.to_owned(),
      ctx: ContextImpl {
        tbs: TokenBlocks::new(source_code),
        err: vec![],
        val: Box::new(context),
      },
      tokens: vec![],
      asts: vec![],
      stack_size,
    }
  }
}
