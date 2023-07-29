//! # Regen
//!
//! Regen is a language that defines a language. See [README](https://github.com/Pistonite/regen-lang) on GitHub

use crate::core::Language;

use sdk::{ASTParser, CreateParseTree, Error, ParseTreeResult, TokenStream};

pub mod core;
pub mod dynamic;
pub mod emit;
pub mod grammar;
pub mod sdk;

/// Create a language from a grammar string
///
/// This is a convienience wrapper for the underlying lexer and parser.
/// It is used by the CLI and does not have many options.
///
/// If you need more control over the language creation process, see the source code
/// and use the underlying parser directly.
pub fn parse_language_from_grammar(
    grammar: &str,
    stack_size: usize,
) -> Result<Language, Vec<Error>> {
    // Parse the language from the grammar
    let lex_output = grammar::tokenize(grammar);
    let mut ts = TokenStream::new(&lex_output.tokens, stack_size);
    let ast_parser = grammar::Parser;
    let asts = match ast_parser.parse_ast_all(&mut ts) {
        Err((_, errors)) => {
            // if errors occur when parsing AST, ignore the ASTs generated and return
            // the errors
            return Err(errors);
        }
        Ok(asts) => asts,
    };
    // create language builder
    let mut lang_builder = Box::default();
    let mut errors = vec![];
    let mut pts = vec![];
    for ast in &asts {
        match ast.parse_pt(lang_builder) {
            ParseTreeResult::Err { ctx, err, .. } => {
                errors.extend(err);
                lang_builder = ctx;
            }
            ParseTreeResult::Ok { pt, ctx } => {
                pts.push(pt);
                lang_builder = ctx;
            }
        }
    }
    if !errors.is_empty() {
        return Err(errors);
    }
    lang_builder.build(&pts)
}
