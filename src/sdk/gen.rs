// Macros used to generate SDK code

/// Macro that implements the language SDK.
/// This is used in the generated code to avoid code duplication.
/// It also creates convenience macros for tree parsing.
#[macro_export]
macro_rules! sdk {
    (
        target: ($pt_target:ident, $ast_target:ident);
        tokens: [ $( $token_type:ident ),* , ];
        regex: [ $( $token_regex:ident = $token_regex_literal:literal ),* , ];
        rules: [ $( [ $( $token_rule:tt ),* ] ),* , ];
        semantics: [ $( $sem_type:ident ),* , ];
    ) => {
        use regen::sdk::util::Error;
        use regen::sdk::{
            ParseHook,
            Token,
            TokenStream,
            TokenizerAction,
            ISemantics,
            SemInfoImpl,
            IASTTarget,
            IPTTarget,
            TokenizeResult
        };
        use std::collections::VecDeque;
        use regex::Regex;
        pub type SemInfo = SemInfoImpl<Semantics>;

        /// Entry point for the language.
        /// 
        /// This function takes a string and tokenize it based on the language rules.
        /// The returned TokenizeResult can be used to generate the Abstract Syntax Tree (AST), which can then be used to generate the Parse Tree (PT).
        pub fn tokenize(content: &str) -> TokenizeResult<Semantics> {
            let mut si = SemInfo::new();
            let (tokens, extract_tokens, unrec_tokens) = tokenize_internal(content);
            si.insert_all(&tokens);
            si.insert_all(&extract_tokens);
            si.insert_all(&unrec_tokens);
            TokenizeResult {
                si,
                tokens,
            }
        }

        macro_rules! move_pt_vec {
            ( $pt:ident ) => {
                {
                    let pt = *$pt;
                    (pt.asts, pt.vals)
                }
            };
        }
        macro_rules! move_pt_vec_optional {
            ( $pt:ident ) => {
                match *$pt {
                    Some(pt) => (pt.asts, pt.vals),
                    None => (VecDeque::new(), VecDeque::new()),
                }
            };
        }


        /// Convinience macro for parsing an optional parameter in a functional derivation
        macro_rules! optional {
            ( $ts:ident, $inner:expr ) => {
                // save the pos to restore in case of failure
                if !$ts.push() {
                    None
                } else {
                    let ast = $inner;
                    if ast.is_none() {
                        // restore if failure
                        $ts.set_error(false);
                        $ts.restore();
                    }
                    // remove the saved pos
                    $ts.pop();
                    ast
                }
            };

        }

        /// Convinience macro for parsing a token in a functional derivation
        macro_rules! token {
            ($param_type_name:ident :: parse ( $ts:ident ) ) => {
                {
                    $ts.consume().filter(|token| {
                        match &token.token_type {
                            Tokens::$param_type_name => true,
                            _ => false,
                        }
                    }).cloned().or_else(|| {
                        $ts.set_error(false);
                        None
                    })
                }

            };
            ($param_type_name:ident :: $lit:literal ( $ts:ident ) ) => {
                {
                    $ts.consume().filter(|token| {
                        if let Tokens::$param_type_name = &token.token_type {
                            if &token.value == $lit {
                                return true;
                            }
                        }
                        false
                    }).cloned()
                    .or_else(|| {
                        $ts.set_error(false);
                        None
                    })
                }
            };
        }

        /// Token type enum
        #[derive(Debug, Clone)]
        pub enum Tokens { 
            /// Internal token type used to mark unrecognized tokens
            Unknown, 
            $( $token_type ),*
        }

        /// Macro that defines a tokenizer rule. This is used inside the tokenizer function
        /// 
        /// A tokenizer rule matches a literal or a regex pattern at the start of the input.
        /// Then it sets `$current_len` and `$action` accordingly.
        /// The rule will only be applied if the length of the matched string is greater than `$current_len`.
        macro_rules! tokenizer_rule {
            // Literal match rule
            ($current_len:ident, $action:ident, $rest:ident, $index:ident, $should_extract:expr, $match_token_type:ident, $literal:literal, $len:expr,) => {
                if $len > $current_len && $rest.starts_with($literal) {
                    $action = TokenizerAction::Keep($should_extract, Token {
                        token_type: Tokens :: $match_token_type,
                        value: $literal.to_owned(),
                        pos: ( $index, $index + $len ),
                    });
                    $current_len = $len;
                }
            };
            // Regex match rule
            // Note that regexes are precompiled before the loop, so an identity is used
            ($current_len:ident, $action:ident, $rest:ident, $index:ident, $should_extract:expr, $match_token_type:ident, $re:ident,) => {
                if let Some(m) = $re.find($rest) {
                    let m_str = m.as_str();
                    let len = m_str.len();
                    if len > $current_len {
                        $action = TokenizerAction::Keep($should_extract, Token {
                            token_type: Tokens :: $match_token_type,
                            value: m_str.to_owned(),
                            pos: ( $index, $index + len ),
                        });
                        $current_len = len;
                    }
                }
            };
            // Literal ignore rule
            ($current_len:ident, $action:ident, $rest:ident, $index:ident, $literal:literal, $len:expr,) => {
                if $len > $current_len && $rest.starts_with($literal) {
                    $action = TokenizerAction::Ignore;
                    $current_len = $len;
                }
            };
            // Regex ignore rule
            ($current_len:ident, $action:ident, $rest:ident, $index:ident, $re:ident,) => {
                if let Some(m) = $re.find($rest) {
                    let len = m.end();
                    if len > $current_len {
                        $action = TokenizerAction::Ignore;
                        $current_len = len;
                    }
                }
            };
        }
    
        /// The tokenizer function
        fn tokenize_internal(input: &str) -> (Vec<Token<Tokens>>, Vec<Token<Tokens>>, Vec<Token<Tokens>>) {
            // Compile regex rules
            $(
                let $token_regex = Regex::new($token_regex_literal).unwrap();
            )*
            let mut tokens = Vec::new();
            let mut extracted_tokens = Vec::new();
            let mut unrecognized_tokens = Vec::new();
            let mut index = 0;
            while index < input.len() {
                // Get the current slice
                let rest = &input[index..];
                let mut action = TokenizerAction::Panic;
                let mut current_len = 0;
                // Run rules
                $(
                    tokenizer_rule!(
                        current_len,
                        action,
                        rest,
                        index,
                        $(
                            $token_rule,
                        )*
                    );
                )*
                // Process action
                match action {
                    TokenizerAction::Panic => {
                        // Unrecognized token, skip one character
                        unrecognized_tokens.push(Token {
                            token_type: Tokens::Unknown,
                            value: rest[0..1].to_owned(),
                            pos: (index, index+1),
                        });
                        index+=1;
                    },
                    TokenizerAction::Keep(should_extract, token) => {
                        // Recognized token
                        if should_extract {
                            extracted_tokens.push(token);
                        } else {
                            tokens.push(token);
                        }
                        index += current_len;
                    },
                    TokenizerAction::Ignore => {
                        // Ignore token
                        index += current_len;
                    }
                }
            }
            (tokens, extracted_tokens, unrecognized_tokens)
        }
    
    
        /// Semantic type enum
        #[derive(Debug, Clone)]
        pub enum Semantics {
            /// Built-in semantic type that corresponds to a token
            Token(Tokens),
            /// Semantic type with extra tags.
            /// The tags are strings and are directly converted to HTMl classes when generating html code blocks.
            Tag(String, Box<Semantics>),
            $( $sem_type ),*
        }

        impl ISemantics for Semantics{
            type Tokens = Tokens;
            fn to_html_class(&self) -> String {
                match self {
                    Semantics::Token(token_type) => format!("token {:?}", token_type),
                    Semantics::Tag(tag, underlying) => format!("{tag} {}", underlying.to_html_class()),
                    _ => format!("semantic token {:?}", self),
                }
            }
            
        }

        impl From<Tokens> for Semantics {
            fn from(token_type: Tokens) -> Self {
                Semantics::Token(token_type)
            }
        }

        impl IASTTarget for $ast_target {
            type Semantics = Semantics;
            type Tokens = Tokens;
            fn parse_root(ts: &mut TokenStream<Tokens>, si: &mut SemInfo) -> Option<Self> {
                let ast = Self::parse(ts);
                if let Some(ast) = &ast {
                    ast.apply_semantic(si);
                }
                ast
            }
        }

        impl<'a> IPTTarget<'a, $ast_target, Semantics> for $pt_target<'a> {
            fn parse_root(ast: &'a $ast_target, si: &mut SemInfo, err: &mut Vec<Error>) -> Self {
                Self::from_ast(ast, si, err)
            }
        }
    };
}


/// Generate union implementation for a union rule. Used in generated code.
#[macro_export]
macro_rules! impl_union {
    (
        $from_ast:ident,
        [ $ast_type_name:ident, $( $ast_derivation_type_name:ident, )* ],
        [ $pt_type_name:ty, $( $pt_hooked_type_name:ty, $pt_derivation_type_name:ident, )* ],
    ) => {
        #[derive(Debug)]
        pub enum $ast_type_name {
            $( $ast_derivation_type_name(Box<$ast_derivation_type_name>), )*
        }
        impl $ast_type_name {
            fn parse(ts: & mut TokenStream<Tokens>) -> Option<Self> {
                if !ts.push() { return None; };
                $(
                    if let Some(r) = $ast_derivation_type_name::parse(ts) {
                        ts.pop();
                        return Some($ast_type_name::$ast_derivation_type_name(Box::new(r)))
                    }
                    ts.set_error(false);
                    ts.restore();
                )*
                ts.pop();
                return None;
            }
            fn apply_semantic(&self, si: &mut SemInfo) {
                match self {
                    $( $ast_type_name::$ast_derivation_type_name(r) => r.apply_semantic(si), )*
                }
            }
        }
        impl<'p> $pt_type_name {
            fn $from_ast(ast: &'p $ast_type_name, si: &mut SemInfo, err: &mut Vec<Error>) -> Self {
                match ast {
                    $(
                        $ast_type_name::$ast_derivation_type_name(ast) => {
                            return Self::$pt_derivation_type_name(Box::new($pt_derivation_type_name::from_ast(ast, si, err)));
                        }
                    )*
                }
            }
        }
    };
}