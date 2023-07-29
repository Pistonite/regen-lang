//! Core logic for language

use crate::core::{Rule, TokenDef, TokenRule};
use crate::grammar::pt;
use crate::sdk::Error;
use std::collections::BTreeMap;

mod validate;

/// Definition of a language
///
/// A language consists of:
/// - a set of tokens, defined with the `token` keyword
/// - a set of tokenizer rules, defined with `TokenType "literal"` or `TokenType /regex/`.
///   The syntax of the regex depends on the target language
/// - a set of semantics, defined with the `semantics` keyword
/// - a set of derivations, defined with the `rule` keyword
/// - a context object, defined with the `context` keyword
/// - a list of include files in the output
#[derive(Debug, Clone, Default)]
pub struct Language {
    /// The context type name.
    ///
    /// If this is [`None`], it will be set to the reasonable "None" in the target language. (For example, `()` in Rust
    pub context: Option<String>,
    /// The target rule name
    pub target: String,
    /// The token definitions
    pub tokens: Vec<TokenDef>,
    /// The token rules
    pub token_rules: Vec<TokenRule>,
    /// The semantic definitions
    pub semantics: Vec<String>,
    /// The rule definitions
    pub rules: BTreeMap<String, Rule>,
    /// The files to include (relative path from the grammar file)
    pub includes: Vec<String>,
}

/// Builder for the language
///
/// This is used as the context in the parse process to collect definitions from the parse tree.
/// This serves as the "partial" definition of a language, and [`Language`] is the result when all definitions are collected and validated.
#[derive(Default)]
pub struct LangBuilder {
    context: Option<String>,
    target: Option<String>,
    tokens: Vec<TokenDef>,
    token_rules: Vec<TokenRule>,
    semantics: Vec<String>,
    rules: BTreeMap<String, Rule>,
    includes: Vec<String>,
}

impl LangBuilder {
    /// Create new builder
    pub fn new() -> Self {
        Default::default()
    }

    /// Set the context type
    ///
    /// Returns false if the context type is already set
    #[inline]
    pub fn set_context(&mut self, context: String) -> bool {
        if self.context.is_some() {
            false
        } else {
            self.context = Some(context);
            true
        }
    }

    /// Add a token definition
    ///
    /// Returns false if a token is already defined with the same name
    #[inline]
    pub fn add_token(&mut self, token: TokenDef) -> bool {
        if self.tokens.iter().any(|t| t.name == token.name) {
            false
        } else {
            self.tokens.push(token);
            true
        }
    }

    /// Add a token rule
    #[inline]
    pub fn add_token_rule(&mut self, token_rule: TokenRule) {
        self.token_rules.push(token_rule);
    }

    /// Add a semantic type
    ///
    /// Returns false if a semantic type is already defined with the same name
    #[inline]
    pub fn add_semantic(&mut self, semantic: String) -> bool {
        if self.semantics.iter().any(|s| s == &semantic) {
            false
        } else {
            self.semantics.push(semantic);
            true
        }
    }

    /// Add a rule
    ///
    /// Returns false if a rule is already defined with the same name
    #[inline]
    pub fn add_rule(&mut self, rule: Rule) -> bool {
        if self.rules.is_empty() {
            self.target = Some(rule.name.clone());
        }
        if self.rules.contains_key(&rule.name) {
            false
        } else {
            self.rules.insert(rule.name.clone(), rule);
            true
        }
    }

    /// Add a file to include
    #[inline]
    pub fn add_include(&mut self, path: String) {
        self.includes.push(path);
    }

    /// Build the language
    ///
    /// This will take out the current stored definitions and return a [`Language`].
    pub fn build(&mut self, pt: &[pt::TopLevelStatement]) -> Result<Language, Vec<Error>> {
        if self.rules.is_empty() {
            return Err(vec![Error::global(
                "No rule defined.".to_owned(),
                "Define a rule with the \"rule\" keyword.".to_owned(),
            )]);
        }
        let lang = Language {
            context: self.context.take(),
            target: self.target.take().unwrap(),
            tokens: std::mem::take(&mut self.tokens),
            token_rules: std::mem::take(&mut self.token_rules),
            semantics: std::mem::take(&mut self.semantics),
            rules: std::mem::take(&mut self.rules),
            includes: std::mem::take(&mut self.includes),
        };

        // Validate the language
        validate::validate_references(pt, lang)
    }
}
