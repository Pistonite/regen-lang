use std::collections::{HashMap, HashSet};

use crate::sdk::generated::{PTRuleDefineBody, PTTopLevelDefine, PTTopLevelStatement};
use crate::sdk::RegenError;
use crate::{err, tree_cast};

use super::rule::{Rule, RuleValue, RetType2};
use super::token::{TokenDef, TokenRule};

/// Definition of a language defined with Regen
///
/// A language consists of:
/// - a set of tokens, defined with the `token` keyword
/// - a set of tokenizer rules, defined with `TokenType "literal"` or `TokenType /regex/`.
///   The syntax of the regex depends on the target language
/// - a set of semantics, defined with the `semantics` keyword
/// - a set of derivations, defined with the `rule` keyword
pub struct RegenLangDef {
    tokens: Vec<TokenDef>,
    token_rules: Vec<TokenRule>,
    semantics: HashSet<String>,
    rules: HashMap<String, Rule>,
    emit_actions: Vec<EmitAction>,
}

impl TryFrom<Vec<PTTopLevelStatement<'_>>> for RegenLangDef {
    type Error = Vec<RegenError>;
    fn try_from(pt: Vec<PTTopLevelStatement>) -> Result<Self, Self::Error> {
        // Validate that identifier references everywhere are valid
        // If not, we can't continue
        let semantics = validate_references(&pt)?;

        // If good, take out objects from PT
        let (tokens, token_rules, mut rules, emit_actions) = extract_from_pt(pt);

        // Resolve return types
        //let mut ret_type_map = HashMap::new();
        let mut rule_map = HashMap::new();
        let mut ret_type_map = HashMap::new();
        for r in rules {
            rule_map.insert(r.name.clone(), r);
        }
        for r in rule_map.values() {
            r.resolve_ret_type(&mut ret_type_map, &rule_map);
        }

        let mut errors = Vec::new();

        for (name, r) in &rule_map {
            if let RetType2::Unresolved(msg) = ret_type_map.get(name).unwrap() {
                errors.push(RegenError {
                    pos: (0,1),
                    msg: format!("Rule {} has unresolved return type: {}", name, msg),
                });
            }
        }

        if !errors.is_empty() {
            return Err(errors);
        }


        Ok(Self {
            tokens,
            token_rules,
            semantics,
            rules: rule_map,
            emit_actions,
        })
    }
}


fn validate_references(
    pt: &[PTTopLevelStatement],
) -> Result<HashSet<String> /* semantics */, Vec<RegenError>> {
    let mut errors = Vec::new();

    let mut token_names = HashSet::new();
    let mut duplicate_token_names = HashSet::new();
    let mut semantics = HashSet::new();
    let mut duplicate_semantics = HashSet::new();
    let mut rule_names = HashSet::new();
    let mut duplicated_rule_names = HashSet::new();

    // bootstrap defined names
    for pt in pt {
        match pt {
            PTTopLevelStatement::PTTopLevelDefineStatement(stmt) => {
                match stmt.val.as_ref() {
                    PTTopLevelDefine::PTDefineTokenTypeStatement(token_def) => {
                        let name = &token_def.val.as_ref().unwrap().name;
                        if token_names.contains(name) {
                            duplicate_token_names.insert(name.clone());
                        } else {
                            token_names.insert(name.clone());
                        }
                    }
                    PTTopLevelDefine::PTDefineSemanticStatement(semantic) => {
                        let name = semantic.val.as_ref().unwrap();
                        if semantics.contains(name) {
                            duplicate_semantics.insert(name.clone());
                        } else {
                            semantics.insert(name.clone());
                        }
                    }
                    _ => {} // The other don't define names
                }
            }
            PTTopLevelStatement::PTDefineRuleStatement(rule) => {
                let name = &rule.val.as_ref().unwrap().name;
                if rule_names.contains(name) {
                    duplicated_rule_names.insert(name.clone());
                } else {
                    rule_names.insert(name.clone());
                }
            }
        }
    }

    // validate that referenced names exist
    for pt in pt {
        match pt {
            PTTopLevelStatement::PTTopLevelDefineStatement(stmt) => match stmt.val.as_ref() {
                PTTopLevelDefine::PTDefineTokenTypeStatement(token_def) => {
                    let name = &token_def.val.as_ref().unwrap().name;
                    if duplicate_token_names.contains(name) {
                        let msg = format!("Duplicate token definition: {}", name);
                        errors.push(err!(token_def.as_ref().pt.ast.m_token_type_2, msg));
                    }
                }
                PTTopLevelDefine::PTDefineSemanticStatement(semantic) => {
                    let name = semantic.val.as_ref().unwrap();
                    if duplicate_semantics.contains(name) {
                        let msg = format!("Duplicate semantic definition: {}", name);
                        errors.push(err!(semantic.as_ref().pt.ast.m_id_1, msg));
                    }
                }
                PTTopLevelDefine::PTDefineTokenRuleStatement(rule) => {
                    if let TokenRule::Literal(name, _) | TokenRule::RegExp(name, _) =
                        &rule.val.as_ref().unwrap()
                    {
                        if !token_names.contains(name) {
                            let msg = format!("Undefined token type: {}", name);
                            errors.push(err!(rule.as_ref().pt.ast.m_token_type_0, msg));
                        }
                    }
                }
                _ => {},
            },
            PTTopLevelStatement::PTDefineRuleStatement(rule_pt) => {
                // Make sure self is not duplicated
                let rule = rule_pt.val.as_ref().unwrap();
                let name = &rule.name;
                if duplicated_rule_names.contains(name) {
                    let msg = format!("Duplicate rule definition: {}.", name);
                    errors.push(err!(rule_pt.as_ref().pt.ast.m_rule_name_2, msg));
                }
                // Make sure all referenced rules exist

                match &rule.value {
                    RuleValue::Union(subrules) => {
                        // Check this so we don't get an unresolved PT
                        // We must use PT to get the tokens to put error messages if needed
                        assert!(!subrules.is_empty());
                        let pt = &rule_pt.as_ref().pt.m_body.as_ref().pt;
                        let pt = tree_cast!(PTRuleDefineBody::PTUnionRuleBody pt);
                        let pt = pt.val.as_ref().as_ref().unwrap();
                        for (next_rule, token) in pt.val.iter().zip(pt.ast_vec.iter()) {
                            if !rule_names.contains(next_rule) {
                                let msg = format!("Undefined rule: {}.", next_rule);
                                errors.push(err!(token, msg));
                                eprintln!("{:?}", &rule_names);
                            }
                        }
                    }
                    RuleValue::Function(params, _) => {
                        // param should be clean because the rule is resolved
                        let pt = &rule_pt.as_ref().pt.m_body.as_ref().pt;
                        let pt = tree_cast!(PTRuleDefineBody::PTFunctionalRuleBody pt);
                        let pt = &pt.m_params.as_ref().as_ref().unwrap().pt;

                        for (param, ast) in params.iter().zip(pt.ast_vec.iter()) {
                            if param.is_token {
                                if !token_names.contains(&param.type_name) {
                                    let msg = format!(
                                        "Parameter \"{}\" has an undefined token type {}.",
                                        param.name, param.type_name
                                    );
                                    errors.push(err!(
                                        ast.m_type_3.as_ref().as_ref().unwrap().m_id_2,
                                        msg
                                    ));
                                }
                            } else {
                                if !rule_names.contains(&param.type_name) {
                                    let msg = format!(
                                        "Parameter \"{}\" has an undefined rule type {}.",
                                        param.name, param.type_name
                                    );
                                    errors.push(err!(
                                        ast.m_type_3.as_ref().as_ref().unwrap().m_id_2,
                                        msg
                                    ));
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    if errors.is_empty() {
        Ok(semantics)
    } else {
        Err(errors)
    }
}
fn extract_from_pt(
    pt: Vec<PTTopLevelStatement>,
) -> (
    Vec<TokenDef>,
    Vec<TokenRule>,
    Vec<Rule>,
    Vec<EmitAction>,
) {
    let mut tokens = Vec::new();
    let mut token_rules = Vec::new();
    let mut rules = Vec::new();
    let mut emit_actions = Vec::new();

    for pt in pt {
        match pt {
            PTTopLevelStatement::PTTopLevelDefineStatement(stmt) => {
                match *stmt.val {
                    PTTopLevelDefine::PTDefineTokenTypeStatement(mut token_def) => {
                        tokens.push(token_def.take_unchecked());
                    }
                    PTTopLevelDefine::PTDefineSemanticStatement(_) => {
                        // Already added
                    }
                    PTTopLevelDefine::PTDefineTokenRuleStatement(mut token_rule) => {
                        token_rules.push(token_rule.take_unchecked());
                    }
                    PTTopLevelDefine::PTDefineIgnoreTokenRuleStatement(mut token_rule) => {
                        token_rules.push(token_rule.take_unchecked());
                    }
                    PTTopLevelDefine::PTTokenLiteral(comment) => {
                        emit_actions.push(EmitAction::Comment(comment.val));
                    }
                }
            }
            PTTopLevelStatement::PTDefineRuleStatement(mut rule) => {
                let rule = rule.take_unchecked();
                let name = rule.name.clone();
                rules.push(rule);
                emit_actions.push(EmitAction::Rule(name));
            }
        }
    }
    (tokens, token_rules, rules, emit_actions)
}

enum EmitAction {
    Comment(String),
    Rule(String),
}
