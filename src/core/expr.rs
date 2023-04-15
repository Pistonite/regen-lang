use crate::sdk::generated::{PTExpression, SemInfo};
use crate::sdk::Error;

/// Expression used in a function rule derivation body.
#[derive(Debug)]
pub enum Expr {
    /// Concatenation (i.e. a | b | c ...)
    /// The last value in the list must be a subrule that returns a vector (also through concatenation)
    /// If the last value is an optional parameter, empty vec is returned
    /// It's possible to cause infinite recursion by not having optional parameters at the end to return empty vectors.
    Concat(Vec<String>),
    /// Variable reference.
    /// Returning the value of a parameter
    Var(String /* identifier */),
    /// Dictionary (i.e. { a, b, ... })
    /// Returning the rule as an object.
    /// The implementation depends on the target language:
    /// - In Rust/TypeScript, the data is stored as members of the parse tree struct
    Dict(Vec<String>),
}

impl Expr {
    /// Get the variables in the expression, in the order of appearance from left to right
    pub fn get_vars(&self) -> Vec<String> {
        match self {
            Expr::Concat(vars) => vars.clone(),
            Expr::Var(identifier) => vec![identifier.clone()],
            Expr::Dict(vars) => vars.clone(),
        }
    }
}

/// Parser hook for Expr
pub fn parse_expr(pt: &PTExpression, _si: &mut SemInfo, errors: &mut Vec<Error>) -> Option<Expr> {
    match pt {
        PTExpression::PTConcatExpression(pt) => {
            // Make sure we have at least 1 item
            let vars = &pt.vals;
            // Vars cannot be empty because of language
            assert!(!vars.is_empty());
            if vars.len() == 1 {
                // If only 1 item, we treat it as a variable
                return Some(Expr::Var(vars[0].clone()));
            }
            Some(Expr::Concat(vars.clone().into()))
        }
        PTExpression::PTDictExpression(pt) => {
            // Make sure we have at least 1 item
            let vars = &pt.vals;
            if vars.is_empty() {
                let msg = "Cannot make empty dictionary.".to_owned();
                let help = "If this rule is optional when deriving, consider making that parameter optional (i.e. foo: optional Bar).".to_owned();
                errors.push(Error::from_token(&pt.ast.m___0, msg, help));
                return None;
            }
            Some(Expr::Dict(vars.clone().into()))
        }
    }
}

macro_rules! expr {
    ($t:ident) => {
        Expr::Var(stringify!($t).to_owned())
    };
    ($( $t:ident)|* ) => {
        Expr::Concat(vec![ $( stringify!($t).to_owned() ),*])
    };
    {$( $t:ident),* } => {
        Expr::Dict(vec![ $( stringify!($t).to_owned() ),*])
    };

}
pub(crate) use expr;
