use crate::core::{Language, Rule, TokenDef, TokenRule};

/// Language SDK Emitter
///
///
pub trait Emitter {
    /// Emit the SDK for the language into a [`String`].
    fn emit(mut self, lang: &Language) -> Result<String, EmitterError>
    where
        Self: Sized,
    {
        self.start(lang)?;
        for path in &lang.includes {
            self.emit_include(lang, path)?;
        }

        for token in &lang.tokens {
            self.emit_token(lang, token)?;
        }

        for token_rule in &lang.token_rules {
            self.emit_token_rule(lang, token_rule)?;
        }

        for semantic in &lang.semantics {
            self.emit_semantic(lang, semantic)?;
        }

        for rule in lang.rules.values() {
            self.emit_rule(lang, rule)?;
        }

        self.done(lang)
    }

    fn start(&mut self, lang: &Language) -> Result<(), EmitterError>;
    fn emit_include(&mut self, lang: &Language, path: &str) -> Result<(), EmitterError>;
    fn emit_token(&mut self, lang: &Language, token: &TokenDef) -> Result<(), EmitterError>;
    fn emit_token_rule(&mut self, lang: &Language, rule: &TokenRule) -> Result<(), EmitterError>;
    fn emit_semantic(&mut self, lang: &Language, semantic: &str) -> Result<(), EmitterError>;
    fn emit_rule(&mut self, lang: &Language, rule: &Rule) -> Result<(), EmitterError>;
    fn done(self, lang: &Language) -> Result<String, EmitterError>;
}

#[derive(Debug, thiserror::Error)]
pub enum EmitterError {
    #[error("Format error")]
    Fmt(#[from] std::fmt::Error),
    #[error("Io error")]
    Io(#[from] std::io::Error),
    #[error("Unknown error")]
    Other(#[from] Box<dyn std::error::Error>),
    #[error("Unknown error")]
    Unknown,
}
