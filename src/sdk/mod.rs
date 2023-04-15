pub mod generated;
pub use generated::{
    tokenize
};

mod util;
pub use util::{
    Error,
    ParseHook,
};
mod token;
pub use token::{
    Token,
    TokenizerAction,
    TokenStream
};
mod semantic;
pub use semantic::{
    Span,
    ISemantics,
    SemInfoImpl
};
mod context;
pub use context::{
    IASTTarget,
    IPTTarget,
    TokenizeResult,
    AbstractSyntaxTree
};


pub mod gen;
#[macro_export]
macro_rules! tree_cast {
    ($enum_type:ident :: $enum_variant:ident $var:expr )=> {
        match $var {
            $enum_type::$enum_variant(x) => x,
            _ => panic!("Unable to tree_cast: Expected {}, got {:?}", stringify!($match_type), $var)
        }
    };
}

