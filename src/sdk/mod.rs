pub mod generated;

mod util;
pub use util::{Error, ParseHook};
mod token;
pub use token::{TokenImpl, TokenStream, Tokenizer, TokenizerAction};
mod semantic;
pub use semantic::{SemInfoImpl, Semantic, Span};
mod env;
pub use env::{ContextImpl, EnvImpl, EnvMode, Environment, RootParser};

pub mod gen;
#[macro_export]
macro_rules! tree_cast {
    (($($t:tt)*) $var:expr ) => {
        match $var {
            $($t)*(x) => x,
            _ => panic!(
                "Unable to tree_cast: Expected {}, got {:?}",
                stringify!($enum_variant),
                $var
            ),
        }
    };
}
