use crate::core::{Language, Rule, TokenDef, TokenRule};

/// Language SDK Emitter
///
///
pub trait Emitter {
    /// Emit the SDK for the language into a [`String`].
    fn emit(mut self, lang: &Language) -> Result<String, Box<dyn std::error::Error>>
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

    fn start(&mut self, lang: &Language) -> Result<(), Box<dyn std::error::Error>>;
    fn emit_include(
        &mut self,
        lang: &Language,
        path: &str,
    ) -> Result<(), Box<dyn std::error::Error>>;
    fn emit_token(
        &mut self,
        lang: &Language,
        token: &TokenDef,
    ) -> Result<(), Box<dyn std::error::Error>>;
    fn emit_token_rule(
        &mut self,
        lang: &Language,
        rule: &TokenRule,
    ) -> Result<(), Box<dyn std::error::Error>>;
    fn emit_semantic(
        &mut self,
        lang: &Language,
        semantic: &str,
    ) -> Result<(), Box<dyn std::error::Error>>;
    fn emit_rule(&mut self, lang: &Language, rule: &Rule)
        -> Result<(), Box<dyn std::error::Error>>;
    fn done(self, lang: &Language) -> Result<String, Box<dyn std::error::Error>>;
}
