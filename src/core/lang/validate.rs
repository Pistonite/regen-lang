//! Utils for validating the language parse tree.
use crate::core::{ParamDataType, RuleValue};
use crate::grammar::{pt, Token};
use crate::sdk::{Error, MergedListTail};
use crate::{merge_list_tail_optional_first, tree_cast};

use super::Language;

pub fn validate_references(
    pt: &[pt::TopLevelStatement],
    lang: Language,
) -> Result<Language, Vec<Error>> {
    let mut errors = Vec::new();

    // validate that referenced names exist
    for pt in pt {
        match pt.m_body.as_ref() {
            pt::TopLevelDefine::DefineTokenRuleStatement(token_rule) => {
                let name = &token_rule.pt.m_token_type;
                if !lang.tokens.iter().any(|t| &t.name == name) {
                    let msg = format!("Undefined token type: {}", name);
                    errors.push(Error::from_token_without_help(
                        &token_rule.pt.ast.m_token_type,
                        msg,
                    ));
                }
            }
            pt::TopLevelDefine::DefineRuleStatement(rule_pt) => {
                // Make sure all referenced rules exist
                let name = &rule_pt.pt.m_rule_name;
                let rule = lang.rules.get(name).expect("rule not found");

                let rule_body_pt = &rule_pt.as_ref().pt.m_body.as_ref().pt;
                match &rule.value {
                    RuleValue::Union(subrules) => {
                        let pt = tree_cast!((pt::RuleDefineBody::UnionRuleBody) rule_body_pt);
                        let merged_pt: MergedListTail<&Token, &str> =
                            merge_list_tail_optional_first!(pt, m_first, m_rest, m_r);
                        for (token, next_rule) in merged_pt.asts.into_iter().zip(subrules) {
                            if !lang.rules.contains_key(next_rule) {
                                let msg = format!("Undefined rule: {}.", next_rule);
                                errors.push(Error::from_token_without_help(token, msg));
                            }
                        }
                    }
                    RuleValue::Function(params) => {
                        let pt = tree_cast!((pt::RuleDefineBody::FunctionalRuleBody) rule_body_pt);
                        let merged_pt =
                            merge_list_tail_optional_first!(pt, m_first_param, m_rest_params, m_p);

                        for (ast, param) in merged_pt.asts.into_iter().zip(params) {
                            if matches!(param.param_type.data, ParamDataType::Rule) {
                                if !lang.rules.contains_key(&param.type_name) {
                                    let msg = format!("Undefined rule: {}.", param.type_name);
                                    let help = format!("Rule \"{}\" is undefined.", param.name);
                                    errors.push(Error::from_token(
                                        &ast.m_type.as_ref().as_ref().unwrap().m_id,
                                        msg,
                                        help,
                                    ));
                                }
                            } else if !lang.tokens.iter().any(|t| t.name == param.type_name) {
                                let msg = format!("Undefined token type: {}", param.type_name);
                                let help = format!("Token type \"{}\" is undefined", param.name);
                                errors.push(Error::from_token(
                                    &ast.m_type.as_ref().as_ref().unwrap().m_id,
                                    msg,
                                    help,
                                ));
                            }
                        }
                    }
                }
            }
            // the others don't reference names, but written out explicitly so it gives an error if we add another top level statement
            pt::TopLevelDefine::DefineIncludeStatement(_) => {}
            pt::TopLevelDefine::DefineIgnoreTokenRuleStatement(_) => {}
            pt::TopLevelDefine::DefineContextStatement(_) => {}
            pt::TopLevelDefine::DefineTokenTypeStatement(_) => {}
            pt::TopLevelDefine::DefineSemanticStatement(_) => {}
        }
    }
    if errors.is_empty() {
        Ok(lang)
    } else {
        Err(errors)
    }
}
