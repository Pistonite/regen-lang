//! Implementation of a lexer/tokenizer

use crate::sdk::{TokenImpl, TokenType};
use regex::Regex;

/// Action the tokenizer can take at each step
pub enum Action<T>
where
    T: TokenType,
{
    /// Panic when a token cannot be matched.
    ///
    /// The tokenizer will skip the next character, mark it as unrecognized and try to match again.
    Panic,
    /// Keep the token
    Keep(TokenImpl<T>),
    /// Extract the token
    ///
    /// Extracted tokens are not used in AST generation (for example, comments)
    Extract(TokenImpl<T>),
    /// Discard the token (for example, whitespaces)
    Ignore,
}

/// The target of a token rule
///
/// Targets are blue prints of the tokenizer. [`Action`]s are generated from targets.
pub enum Target<T>
where
    T: TokenType,
{
    Ignore,
    Extract(T),
    Keep(T),
}

impl<T> Target<T>
where
    T: TokenType,
{
    #[inline]
    fn get_action(&self, val: &str, len: usize, index: usize) -> Action<T>
    where
        T: TokenType,
    {
        match self {
            Target::Ignore => Action::Ignore,
            Target::Extract(t) => Action::Extract(TokenImpl {
                token_type: t.clone(),
                value: val.to_owned(),
                pos: (index, index + len),
            }),
            Target::Keep(t) => Action::Keep(TokenImpl {
                token_type: t.clone(),
                value: val.to_owned(),
                pos: (index, index + len),
            }),
        }
    }
}
/// Token rule
pub enum Rule<T>
where
    T: TokenType,
{
    /// Regex rule
    Regex(Regex, Target<T>),
    /// Literal match rule
    Literal(String, Target<T>),
}

impl<T> Rule<T>
where
    T: TokenType,
{
    #[inline]
    fn run(&self, rest: &str, index: usize, current_len: usize) -> Option<(usize, Action<T>)> {
        match self {
            Rule::Regex(re, target) => {
                if let Some(m) = re.find(rest) {
                    let m_str = m.as_str();
                    let len = m_str.len();
                    if len > current_len {
                        let action = target.get_action(m_str, len, index);
                        return Some((len, action));
                    }
                }
            }
            Rule::Literal(literal, target) => {
                let len = literal.len();
                if len > current_len && rest.starts_with(literal) {
                    let action = target.get_action(literal, len, index);
                    return Some((len, action));
                }
            }
        }
        None
    }
}

/// Run the tokenizer based on the rules
///
/// This returns 3 lists of tokens:
/// - Recognized tokens
/// - Extracted tokens
/// - Unrecognized tokens, which are a list of tokens with type `unknown_token`
pub fn run_tokenizer<T>(
    input: &str,
    unknown_token: T,
    rules: &[Rule<T>],
) -> (Vec<TokenImpl<T>>, Vec<TokenImpl<T>>, Vec<TokenImpl<T>>)
where
    T: TokenType,
{
    let mut tokens = Vec::new();
    let mut extracted_tokens = Vec::new();
    let mut unrecognized_tokens = Vec::new();
    let mut index = 0;
    while index < input.len() {
        // Get the current slice
        let rest = &input[index..];
        let mut current_action = Action::Panic;
        let mut current_len = 0;
        // Run rules
        for rule in rules {
            if let Some((len, action)) = rule.run(rest, index, current_len) {
                current_len = len;
                current_action = action;
            }
        }
        // Process action
        match current_action {
            Action::Panic => {
                // Unrecognized token, skip one character
                unrecognized_tokens.push(TokenImpl {
                    token_type: unknown_token.clone(),
                    value: rest[0..1].to_owned(),
                    pos: (index, index + 1),
                });
                current_len = 1;
            }
            Action::Keep(token) => {
                tokens.push(token);
            }
            Action::Extract(token) => {
                extracted_tokens.push(token);
            }
            Action::Ignore => {
                // Ignore token
            }
        }
        index += current_len;
    }
    (tokens, extracted_tokens, unrecognized_tokens)
}
