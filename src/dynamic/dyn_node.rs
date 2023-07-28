use super::{DynTok, DynToken};
use crate::core::{Language, ParamDataType, ParamDecorType, RuleValue};
use crate::sdk::{TokenBlocks, TokenStream};
use crate::{list, optional, required};

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
    pub fn parse_rule(
        ts: &mut TokenStream<DynTok>,
        lang: &Language,
        rule_name: &str,
    ) -> Option<Self> {
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

    /// Apply semantic to the AST node based on the language
    pub fn apply_semantic(
        &self,
        lang: &Language,
        tbs: &mut TokenBlocks<DynTok>,
        ovr: &Option<DynTok>,
    ) {
        match self {
            DynAstNode::Exterior(token) => {
                if let Some(ovr) = ovr {
                    tbs.set(token, ovr.clone());
                }
            }
            DynAstNode::Interior(rule_name, children)
            | DynAstNode::InteriorVec(rule_name, children) => {
                let rule = lang.rules.get(rule_name).unwrap();
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
                    child.apply_semantic(lang, tbs, ovr);
                }
            }
        }
    }
}
