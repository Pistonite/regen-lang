//! Utils for dynamically generate AST for source code based on the Language object.
//! Note that this only includes up to the AST, since parse tree hooks cannot be executed dynamically.
use std::collections::HashSet;

use regex::Regex;

use crate::core::{Language, ParamDataType, ParamDecorType, RuleValue, TokenRule};
use crate::sdk::{lex, ContextImpl, EnvImpl, RootParser, TokenImpl, TokenStream};
use crate::{list, optional, required};

/// Dynamic token type.
///
/// The name of the token or semantic is used directly as the type.
/// Tokens are prefixed with `T` and Semantics are prefixed with `S`.
pub type DynTok = String;
/// Dynamic token implementation
pub type DynToken = TokenImpl<DynTok>;
/// Dynamic context implementation
///
/// The context type is [`Language`](crate::core::Language) because the language
/// definition is needed to generate the AST dynamically
pub type DynCtx = ContextImpl<Language, DynTok>;
/// Dynamic environment implementation
pub type DynEnv = EnvImpl<DynTok, DynAstNode, Language>;

/// Dynamic AST node
pub enum DynAstNode {
    /// Exterior (leaf) AST node
    ///
    /// This is a token node
    Exterior(DynToken),
    /// Interior AST node
    ///
    /// This corresponds to a rule. The children nodes are the members of the rule
    Interior(String /* rule_name */, Vec<DynAstNode>),
    /// Vector AST node
    ///
    /// This corresponds to a vector parameter. The children nodes are the rule type and are in the vector.
    InteriorVec(String /* rule_name */, Vec<DynAstNode>),
}

macro_rules! impl_dyn_ast_parse_param {
    ($param:ident, $ts:ident, $children:ident, $parse_expr:expr) => {
        match &$param.param_type.decor {
            ParamDecorType::None => $children.push(required!($ts, $parse_expr)?),
            ParamDecorType::Optional => {
                if let Some(n) = optional!($ts, $parse_expr) {
                    $children.push(n);
                }
            }
            ParamDecorType::Vec(optional) => {
                let v = if *optional {
                    let mut v = vec![];
                    list!($ts, v, $parse_expr)
                } else {
                    let mut v = vec![required!($ts, $parse_expr)?];
                    list!($ts, v, $parse_expr)
                };
                $children.push(DynAstNode::InteriorVec($param.type_name.clone(), v))
            }
        }
    };
}

impl DynAstNode {
    /// Parse a rule from the token stream.
    ///
    /// `rule_name` is the name of the rule to parse
    fn parse_rule(ts: &mut TokenStream<DynTok>, lang: &Language, rule_name: &str) -> Option<Self> {
        let rule = lang.rules.get(rule_name).unwrap();
        match &rule.value {
            RuleValue::Union(rules) => {
                if !ts.push() {
                    return None;
                };
                for rule in rules {
                    let n = Self::parse_rule(ts, lang, rule);
                    if n.is_some() {
                        ts.pop();
                        return n;
                    }
                    ts.restore();
                }
                ts.pop();
                None
            }
            RuleValue::Function(params) => {
                let mut children = Vec::new();
                for param in params {
                    let type_name = &param.type_name;

                    match &param.param_type.data {
                        ParamDataType::Rule => {
                            impl_dyn_ast_parse_param!(
                                param,
                                ts,
                                children,
                                Self::parse_rule(ts, lang, type_name)
                            )
                        }
                        ParamDataType::String => {
                            impl_dyn_ast_parse_param!(
                                param,
                                ts,
                                children,
                                Self::parse_token(ts, type_name)
                            )
                        }
                        ParamDataType::Flag(lit) => impl_dyn_ast_parse_param!(
                            param,
                            ts,
                            children,
                            Self::parse_token_lit(ts, type_name, lit)
                        ),
                    }
                }
                Some(DynAstNode::Interior(rule_name.to_string(), children))
            }
        }
    }

    /// Parse a token from the token stream.
    fn parse_token(ts: &mut TokenStream<DynTok>, token_name: &str) -> Option<Self> {
        let token_type = format!("T{}", token_name);
        ts.consume()
            .filter(|token| token.token_type == token_type)
            .cloned()
            .map(DynAstNode::Exterior)
    }

    /// Parse a token with literal match from the token stream.
    fn parse_token_lit(
        ts: &mut TokenStream<DynTok>,
        token_name: &str,
        literal: &str,
    ) -> Option<Self> {
        let token_type = format!("T{}", token_name);
        ts.consume()
            .filter(|token| token.token_type == token_type && token.value == literal)
            .cloned()
            .map(DynAstNode::Exterior)
    }

    fn apply_semantic(&self, ctx: &mut DynCtx, ovr: &Option<DynTok>) {
        match self {
            DynAstNode::Exterior(token) => {
                if let Some(ovr) = ovr {
                    ctx.tbs.set(token, ovr.clone());
                }
            }
            DynAstNode::Interior(rule_name, children)
            | DynAstNode::InteriorVec(rule_name, children) => {
                let rule = ctx.val.rules.get(rule_name).unwrap();
                // param and children has 1:1 mapping
                let params = match &rule.value {
                    RuleValue::Union(_) => panic!("Union rules should not be in dynamic AST"),
                    RuleValue::Function(params) => params,
                };
                let semantic_override = params
                    .iter()
                    .map(|param| match &param.semantic {
                        None => ovr.clone(),
                        Some(s) => Some(format!("S{}", s)),
                    })
                    .collect::<Vec<_>>();

                for (ovr, child) in semantic_override.iter().zip(children.iter()) {
                    child.apply_semantic(ctx, ovr);
                }
            }
        }
    }
}

impl RootParser for DynEnv {
    type T = DynTok;
    type C = Language;
    type A = DynAstNode;
    type P<'p> = ();

    fn do_tokenize(src: &str, ctx: &mut DynCtx) -> Vec<DynToken> {
        // Generate token rules dynamically
        let mut extract_token_rules = HashSet::new();
        let lang = ctx.val.as_ref();
        for token in &lang.tokens {
            if token.is_extract {
                extract_token_rules.insert(token.name.clone());
            }
        }
        let token_rules = &lang
            .token_rules
            .iter()
            .map(|r| match r {
                TokenRule::IgnoreLiteral(literal) => {
                    lex::Rule::Literal(literal.to_string(), lex::Target::Ignore)
                }
                TokenRule::IgnoreRegExp(regex) => {
                    let regex = format!("^{regex}");
                    lex::Rule::Regex(Regex::new(&regex).unwrap(), lex::Target::Ignore)
                }
                TokenRule::Literal(token_type, literal) => {
                    if extract_token_rules.contains(token_type) {
                        lex::Rule::Literal(
                            literal.to_string(),
                            lex::Target::Extract(format!("T{token_type}")),
                        )
                    } else {
                        lex::Rule::Literal(
                            literal.to_string(),
                            lex::Target::Keep(format!("T{token_type}")),
                        )
                    }
                }
                TokenRule::RegExp(token_type, regex) => {
                    let regex = format!("^{regex}");
                    if extract_token_rules.contains(token_type) {
                        lex::Rule::Regex(
                            Regex::new(&regex).unwrap(),
                            lex::Target::Extract(format!("T{token_type}")),
                        )
                    } else {
                        lex::Rule::Regex(
                            Regex::new(&regex).unwrap(),
                            lex::Target::Keep(format!("T{token_type}")),
                        )
                    }
                }
            })
            .collect::<Vec<_>>();

        // Run tokenizer
        let (tokens, extract_tokens, unrec_tokens) =
            lex::run_tokenizer(src, "".to_owned(), token_rules);
        ctx.tbs.insert_all(&tokens);
        ctx.tbs.insert_all(&extract_tokens);
        ctx.tbs.insert_all(&unrec_tokens);
        tokens
    }

    fn parse_ast_root(ts: &mut TokenStream<Self::T>, ctx: &mut DynCtx) -> Option<Self::A> {
        let lang = &ctx.val;
        let target = &lang.target;
        let ast = DynAstNode::parse_rule(ts, lang, target);
        if let Some(ast) = &ast {
            ast.apply_semantic(ctx, &None);
        }
        ast
    }

    fn parse_pt_root<'a>(_: &'a Self::A, _: &mut DynCtx) -> Self::P<'a> {
        // empty implementation because dynamic parse tree is not supported
        // (just return ())
    }
}
