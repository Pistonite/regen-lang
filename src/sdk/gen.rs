//! Module with macros to implement the generated language environment.

/// Macro that implements the language SDK.
///
/// This is used in the generated code to avoid code duplication.
/// It also creates convenience macros for tree parsing.
#[macro_export]
macro_rules! sdk {
    (
    context: $context:ty;
    target: $target:ident;
    tokens: [ $( $token_type:ident ),* , ];
    rules: [ $( $token_rule:expr ),* , ];
    semantics: [ $( $sem_type:ident ),* , ];
) => {
    use $crate::sdk::{
        ParseHook,
        TokenType,
        TokenImpl,
        TokenStream,
        TokenBlocks,
        ASTParser,
        Regex,
        AsKebabCase,
        CreateParseTree,
        ParseTreeContext,
        lex
    };
    use $crate::{optional, required, list};

    // Type aliases

    /// GENERATED Token type alias
    ///
    /// See [`TokenImpl`] for more details.
    pub type Token = TokenImpl<Tok>;

    /// GENERATED Context type alias
    ///
    /// See [`ContextImpl`] for more details.
    pub type Ctx = ParseTreeContext<$context, Tok>;

    /// GENERATED Token type enum
    #[derive(PartialEq, Default, Debug, Clone)]
    pub enum Tok {
        /// Internal token type used to mark unrecognized tokens
        #[default]
        Unknown,
        /// Semantic type with extra decorators (tags)
        /// The tags are dynamic strings that can be used to add extra information to the token,
        /// such as `unused` or `deprecated`.
        Decor{
            tag: String,
            base:Box<Tok>},
        $( $token_type, )*

        $( $sem_type, )*
    }

    // Trait implementations

    impl TokenType for Tok {
        fn html_class(&self) -> Option<String> {
            Some(match self {
                Tok::Unknown => "token unknown".to_string(),
                Tok::Decor{tag, base} => format!("{tag}{}", base.html_class().map(|s| format!(" {}", s)).unwrap_or_default()),
                $( Tok::$token_type => format!("token {}",AsKebabCase(stringify!($token_type))) ,)*
                $( Tok::$sem_type => format!("semantic token {}",AsKebabCase(stringify!($sem_type))) ),*
            })
        }
    }

    /// Tokenize the input source code
    ///
    /// This method tokenizes `src` based on the grammar rules, and returns the tokens
    pub fn tokenize(src: &str) -> lex::TokenizerOutput<Tok>{
        let rules = vec![
            $( $token_rule ),*
        ];
        lex::run_tokenizer(src, Tok::Unknown, &rules)
    }

    impl CreateParseTree for ast::$target {
        type T = Tok;
        type C = $context;
        type P<'p> = pt::$target<'p>;

        fn parse_pt_with_context_internal<'a>(
            &'a self,
            ctx: &mut Ctx
        ) -> Self::P<'a> {
            pt::$target::from_ast(self, ctx)
        }
    }

    pub struct Parser;

    impl ASTParser for Parser {
        type T = Tok;
        type A = ast::$target;

        fn parse_ast(&self, ts: &mut TokenStream<Self::T>) -> Option<Self::A>{
            ast::$target::parse(ts)
        }
    }

    /// Convinience macro for parsing a token in a functional derivation
    macro_rules! token {
        ($param_type_name:ident :: parse ( $ts:ident ) ) => {
            {
                $ts.consume().filter(|token| {
                    match &token.token_type {
                        Tok::$param_type_name => true,
                        _ => false,
                    }
                }).cloned()
            }
        };
        ($param_type_name:ident :: $lit:literal ( $ts:ident ) ) => {
            {
                $ts.consume().filter(|token| {
                    if let Tok::$param_type_name = &token.token_type {
                        if &token.value == $lit {
                            return true;
                        }
                    }
                    false
                }).cloned()
            }
        };
    }
};
}

/// Generate union implementation for a union rule. Used in generated code.
#[macro_export]
macro_rules! impl_union {
    (
    $from_ast:ident,  $type_name:ident,
    { $( $derivation_type_name:ident, )* }
) => {
    /// GENERATED AST Union Rule Implementation
    impl ast::$type_name {
        fn parse(ts: & mut TokenStream<Tok>) -> Option<Self> {
            if !ts.push() { return None; };
            $(
                if let Some(r) = ast::$derivation_type_name::parse(ts) {
                    ts.pop();
                    return Some(Self::$derivation_type_name(Box::new(r)))
                }
                    ts.restore();
            )*
            ts.pop();
            return None;
        }
        fn apply_semantic(&self, si: &mut TokenBlocks<Tok>, ovr: &Option<Tok>) {
            match self {
                $( Self::$derivation_type_name(r) => r.apply_semantic(si, ovr), )*
            }
        }
    }
    impl<'p> pt::$type_name<'p> {
        fn $from_ast(ast: &'p ast::$type_name, ctx: &mut Ctx) -> Self {
            match ast {
                $(
                    ast::$type_name::$derivation_type_name(ast) => {
                        return Self::$derivation_type_name(Box::new(pt::$derivation_type_name::from_ast(ast, ctx)));
                        }
                )*
            }
        }
    }
};
}
