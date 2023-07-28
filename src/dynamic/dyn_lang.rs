//! Dynamic language implementation

use std::collections::HashSet;

use regex::Regex;

use crate::sdk::{ASTParser, TokenStream};
use crate::sdk::lex::{self, TokenizerOutput};
use crate::core::{Language, RuleValue, TokenRule};

use super::{DynTok, DynAstNode};

impl Language {
    /// Dynamically tokenize a source code string based on this language
    pub fn dyn_tokenize(&self, src: &str) -> TokenizerOutput<DynTok> {
        // Generate token rules dynamically
        let mut extract_token_rules = HashSet::new();
        for token in &self.tokens {
            if token.is_extract {
                extract_token_rules.insert(token.name.clone());
            }
        }
        let token_rules = &self
            .token_rules
            .iter()
            .map(|r| match r {
                TokenRule::IgnoreLiteral(literal) => {
                    lex::Rule::Literal(literal.to_string(), lex::Target::Ignore)
                }
                TokenRule::IgnoreRegExp(regex) => {
                    let regex = format!("^{regex}");
                    lex::Rule::Regex(Regex::new(&regex).unwrap(), lex::Target::Ignore)
                }
                TokenRule::Literal(token_type, literal) => {
                    if extract_token_rules.contains(token_type) {
                        lex::Rule::Literal(
                            literal.to_string(),
                            lex::Target::Extract(format!("T{token_type}")),
                        )
                    } else {
                        lex::Rule::Literal(
                            literal.to_string(),
                            lex::Target::Keep(format!("T{token_type}")),
                        )
                    }
                }
                TokenRule::RegExp(token_type, regex) => {
                    let regex = format!("^{regex}");
                    if extract_token_rules.contains(token_type) {
                        lex::Rule::Regex(
                            Regex::new(&regex).unwrap(),
                            lex::Target::Extract(format!("T{token_type}")),
                        )
                    } else {
                        lex::Rule::Regex(
                            Regex::new(&regex).unwrap(),
                            lex::Target::Keep(format!("T{token_type}")),
                        )
                    }
                }
            })
            .collect::<Vec<_>>();

        lex::run_tokenizer(src, "".to_owned(), token_rules)
    }
}

/// Generate a dynamic AST from a token stream based on this language
impl ASTParser for Language {
    type T = DynTok;
    type A = DynAstNode;

    fn parse_ast(&self, ts: &mut TokenStream<Self::T>) -> Option<Self::A> {
        DynAstNode::parse_rule(ts, &self, &self.target)
    }
}
