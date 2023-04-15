use crate::sdk::generated::{PTExpression, SemInfo};
use crate::sdk::RegenError;
use crate::err;

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
            Expr::Dict(vars) => vars.clone()
        }
    }
}

/// Parser hook for Expr
pub fn parse_expr(pt: &PTExpression, _si: &mut SemInfo, errors: &mut Vec<RegenError>) -> Option<Expr> {
    match pt {
        PTExpression::PTConcatExpression(pt) => {
            // Make sure we have at least 1 item
            let vars = &pt.val;
            if vars.is_empty() {
                let msg = "Concat expression must have at least two items".to_owned();
                errors.push(err!(pt.ast.m_first_0, msg));
                return None;
            }
            if vars.len() == 1{
                // If only 1 item, we treat it as a variable
                return Some(Expr::Var(vars[0].clone()));
            }
            Some(Expr::Concat(vars.clone().into()))
        },
        PTExpression::PTDictExpression(pt) => {
            // Make sure we have at least one item
            match pt.val.as_ref().as_ref().filter(|x| !x.val.is_empty()) {
                None => {
                    let msg = "Cannot make empty dictionary. Consider using an optional rule."
                        .to_owned();
                    errors.push(err!(pt.ast.m___0, msg));
                    return None;
                }
                Some(x) => Some(Expr::Dict(x.val.clone().into())),
            }
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