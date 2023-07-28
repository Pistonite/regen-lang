use crate::sdk::{Error, TokenBlocks, TokenStream, TokenType};

/// The main trait for using the generated language
///
/// ASTParser provides methods for parsing the language from source code to:
/// 1. List of tokens
/// 2. Abstract Syntax Tree (AST) (either one or until the token stream is exhausted)
///
/// The generated language SDK will include implementation of this trait
pub trait ASTParser {
    /// Token type
    ///
    /// This type is generated in the environment. See [`crate::grammar::Tok`]
    type T: TokenType;
    /// AST root type
    ///
    /// This type is generated in the environment. See [`crate::grammar::ast::TopLevelStatement`].
    /// Note that the first rule in the grammar file becomes the root.
    type A;

    /// Parse one AST root from the [`TokenStream`]
    ///
    /// Returns `None` when there's syntax error
    fn parse_ast(&self, ts: &mut TokenStream<Self::T>) -> Option<Self::A>;

    /// Create all ASTs from the source code, until the token stream is exhausted
    ///
    /// This method is useful for parsing multiple roots in a single source code,
    /// for example, many statements where each statement is the target.
    ///
    /// On success, it returns a vector of ASTs. If there's any error, it returns an `Err` variant
    /// with the ASTs that have been succesfully parsed, along with the errors.
    fn parse_ast_all(&self, ts: &mut TokenStream<Self::T>) -> Result<Vec<Self::A>, (Vec<Self::A>, Vec<Error>)> {
        let mut last_failed = false;
        let mut asts = vec![];
        let mut errors = vec![];

        loop {
            match self.parse_ast(ts) {
                Some(ast) => {
                    last_failed = false;
                    asts.push(ast);
                    if ts.is_exhausted() {
                        break;
                    }
                }
                None => {
                    if let Some(token) = ts.consume() {
                        if !last_failed {
                            last_failed = true;
                            if let Some(err_token) = ts.get_guess_err_token() {
                                errors.push(Error::from_token_without_help(
                                    err_token,
                                    "Syntax error near this location".to_owned(),
                                ));
                            } else {
                                errors.push(Error::from_token_without_help(
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
        if errors.is_empty() {
            Ok(asts)
        } else {
            Err((asts, errors))
        }
    }
}

/// Trait used for converting AST to Parse Tree
///
/// This trait is implemented by the root of the generated AST. Use this to generate a Parse Tree
/// from the AST
pub trait CreateParseTree {
    type T: TokenType;
    type C;
    type P<'p> where Self: 'p;
    /// Create parse tree
    ///
    /// Creates a Parse Tree root from the AST reference. Note that Parse Tree nodes contain references
    /// to the AST, so the AST must be kept in memory until the Parse Tree is dropped.
    fn parse_pt<'a>(&'a self, ctx: &mut ParseTreeContext<Self::C, Self::T>) -> Self::P<'a>;
}

/// Trait used for converting AST to Parse Tree
///
/// This trait is implemented by the root of the generated AST. Use this to generate a Parse Tree
/// from the AST
pub trait CreateParseTree {
    type T: TokenType;
    type C;
    type P<'p> where Self: 'p;
    /// Create parse tree
    ///
    /// Creates a Parse Tree root from the AST reference. Note that Parse Tree nodes contain references
    /// to the AST, so the AST must be kept in memory until the Parse Tree is dropped.
    fn parse_pt<'a>(&'a self, ctx: &mut ParseTreeContext<Self::C, Self::T>) -> Self::P<'a>;
}

#[derive(Default)]
pub struct ParseTreeContext<C, T>
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
