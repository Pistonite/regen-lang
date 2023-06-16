use crate::sdk::{Error, TokenBlocks, TokenImpl, TokenStream, TokenType};

/// The part of the language environment that interacts with the language
///
/// RootParser provides methods for parsing the language in 3 steps:
/// 1. From plain text to tokens (Lexer/Tokenizer)
/// 2. From tokens to Abstract Syntax Tree (AST)
/// 3. From AST to Parse Tree (PT), which is the final result of parsing
///
/// The generated language environment contains an implementation of RootParser
/// for the `Env` object exported. For example, the one for Regen is [`crate::grammar::Env`]
pub trait RootParser {
  /// Token type
  ///
  /// This type is generated in the environment. See [`crate::grammar::Tok`]
  type T: TokenType;
  /// AST root type
  ///
  /// This type is generated in the environment. See [`crate::grammar::ast::TopLevelStatement`].
  /// Note that the first rule in the grammar file becomes the root.
  type A;
  /// Context type
  ///
  /// This type is defined by the grammar file with the `context` keyword.
  type C;
  /// Parse Tree root type
  ///
  /// This type is generated in the environment. See [`crate::grammar::pt::TopLevelStatement`].
  /// Note that the first rule in the grammar file becomes the root.
  type P<'p>;
  /// Tokenize the input source code
  ///
  /// This method tokenizes `src` based on the grammar rules, and returns a vector of tokens.
  /// It also stores the token information in the `TokenBlocks` in the `ctx` object.
  fn do_tokenize(src: &str, ctx: &mut ContextImpl<Self::C, Self::T>) -> Vec<TokenImpl<Self::T>>;
  /// Parse AST root
  ///
  /// Parse one AST root from the [`TokenStream`], and apply semantic if successful.
  fn parse_ast_root(
    ts: &mut TokenStream<Self::T>,
    ctx: &mut ContextImpl<Self::C, Self::T>,
  ) -> Option<Self::A>;
  /// Parse PT root
  ///
  /// Creates a Parse Tree root from the AST reference. Note that Parse Tree nodes contain references
  /// to the AST, so the AST must be kept in memory until the Parse Tree is dropped.
  fn parse_pt_root<'a>(ast: &'a Self::A, ctx: &mut ContextImpl<Self::C, Self::T>) -> Self::P<'a>;
}
/// The part of the language environment that provides interface to the language
///
/// Methods in `Environment` manages parsing the language incrementally and handles internal shared
/// logic for parsing more than one root. Since the language-specific parsing part is delegated to the [`RootParser`],
/// the implementation of `Environment` can be shared. See [`crate::sdk::EnvImpl`]
pub trait Environment: RootParser {
  /// Run the tokenizer
  ///
  /// This should save the tokens so subsequent calls simply returns
  fn tokenize(&mut self);
  /// Parse the ASTs
  ///
  /// This should either parse one AST or continue to parse until the token stream is exhausted.
  /// See [`Mode`] for more details. This should save the result so subsequent calls simply returns
  fn parse_asts(&mut self);
  /// Parse the PTs
  ///
  /// This will generate new PTs from the ASTs every call, and give them to the `consumer` function
  /// along with the context. The consumer function is meant to generate the application-specific object,
  /// and either return it or store it inside the context (for example, for results across multiple source code files)
  fn parse_pts_then<'s, 'p, P, F, R>(&'s mut self, consumer: F) -> Result<R, &[Error]>
  where
    F: FnOnce(Vec<Self::P<'p>>, &mut Self::C) -> Result<R, Vec<Error>>,
    Self: RootParser<P<'p> = P>,
    's: 'p;
}

/// The context object in the language environment
///
/// The context stores information across the whole parsing process.
/// This includes the token blocks, errors, and the application-specific context obect
/// defined with the `context` keyword in the grammar file.
///
/// The context object is also passed to parse tree hooks.
pub struct ContextImpl<C, T>
where
  T: TokenType,
{
  /// The semantic information stored as token blocks
  pub tbs: TokenBlocks<T>,
  /// The errors encountered during parsing
  pub err: Vec<Error>,
  /// The application-specific context object.
  ///
  /// For Regen, this is [`crate::core::Language`].
  pub val: Box<C>,
}

/// Parse mode
pub enum Mode {
  /// Parse one AST
  One,
  /// Keep parsing ASTs until the token stream is exhausted
  All,
}

#[derive(PartialEq, PartialOrd)]
pub enum State {
  Init,
  Tokenized,
  ASTParsed,
}
