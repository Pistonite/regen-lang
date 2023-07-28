use super::{TokenType, TokenStream, Error};

/// ASTParser provides methods for parsing the language from token stream to:
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
