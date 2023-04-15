use std::collections::{HashMap, HashSet};

use crate::sdk::generated::{
    ASTDefineRuleStatement, PTRuleDefineBody, PTTopLevelDefine, PTTopLevelStatement,
};
use crate::sdk::Error;
use crate::tree_cast;

use super::rule::{RetType, Rule, RuleValue};
use super::token::{TokenDef, TokenRule};

/// Definition of a language defined with Regen
///
/// A language consists of:
/// - a set of tokens, defined with the `token` keyword
/// - a set of tokenizer rules, defined with `TokenType "literal"` or `TokenType /regex/`.
///   The syntax of the regex depends on the target language
/// - a set of semantics, defined with the `semantics` keyword
/// - a set of derivations, defined with the `rule` keyword
pub struct Language {
    target: String,
    tokens: Vec<TokenDef>,
    token_rules: Vec<TokenRule>,
    semantics: HashSet<String>,
    rules: HashMap<String, Rule>,
    emit_actions: Vec<EmitAction>,
    ret_types: HashMap<String, RetType>,
}

impl TryFrom<Vec<PTTopLevelStatement<'_>>> for Language {
    type Error = Vec<Error>;
    fn try_from(pt: Vec<PTTopLevelStatement>) -> Result<Self, Self::Error> {
        // Validate that identifier references everywhere are valid
        // If not, we can't continue
        let semantics = validate_references(&pt)?;

        // If good, take out objects from PT
        let (tokens, token_rules, rule_vec, emit_actions, rule_ast_vec) = extract_from_pt(pt);

        // We need at least 1 rule
        let mut errors = Vec::new();
        if rule_vec.is_empty() {
            errors.push(Error::global("No rule defined.".to_owned(), "Define a rule with the \"rule\" keyword.".to_owned()));
            return Err(errors);
        }
        let target = rule_vec.first().unwrap().name.clone();

        // Resolve return types
        let mut rules = HashMap::new();
        let mut rule_asts = HashMap::new();
        for (rule, ast) in rule_vec.into_iter().zip(rule_ast_vec) {
            rule_asts.insert(rule.name.clone(), ast);
            rules.insert(rule.name.clone(), rule);
        }
        let mut ret_types = HashMap::new();
        rules
            .get(&target)
            .unwrap()
            .resolve_ret_type(&mut ret_types, &rules);

        for (name, _) in &rules {
            match ret_types.get(name) {
                Some(RetType::Unresolved(msg)) => {
                    let ast = rule_asts.get(name).unwrap();
                    let msg = format!("Cannot resolve return type for rule \"{name}\": {msg}");
                    let help = "Check the parameters and body of the rule.".to_owned();
                    errors.push(Error::from_token(&ast.m_rule_name_2, msg, help));
                }
                None => {
                    let ast = rule_asts.get(name).unwrap();
                    let msg = format!("Rule \"{name}\" is not referenced by target \"{target}\"");
                    let help = "Remove or comment out the rule.".to_owned();
                    errors.push(Error::from_token(&ast.m_rule_name_2, msg, help));
                }
                _ => {}
            }
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        Ok(Self {
            target,
            tokens,
            token_rules,
            semantics,
            rules,
            emit_actions,
            ret_types,
        })
    }
}

fn validate_references(
    pt: &[PTTopLevelStatement],
) -> Result<HashSet<String> /* semantics */, Vec<Error>> {
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
                match stmt.m_body.as_ref() {
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
            PTTopLevelStatement::PTTopLevelDefineStatement(stmt) => match stmt.m_body.as_ref() {
                PTTopLevelDefine::PTDefineTokenTypeStatement(token_def) => {
                    let name = &token_def.val.as_ref().unwrap().name;
                    if duplicate_token_names.contains(name) {
                        let msg = format!("Duplicate token definition: {}", name);
                        let help = "Remove or rename the duplicate definition".to_owned();
                        errors.push(Error::from_token(
                            &token_def.as_ref().pt.ast.m_token_type_2,
                            msg,
                            help,
                        ));
                    }
                }
                PTTopLevelDefine::PTDefineSemanticStatement(semantic) => {
                    let name = semantic.val.as_ref().unwrap();
                    if duplicate_semantics.contains(name) {
                        let msg = format!("Duplicate semantic definition: {}", name);
                        let help = "Remove or rename the duplicate definition".to_owned();
                        errors.push(Error::from_token(
                            &semantic.as_ref().pt.ast.m_id_1,
                            msg,
                            help,
                        ));
                    }
                }
                PTTopLevelDefine::PTDefineTokenRuleStatement(rule) => {
                    if let TokenRule::Literal(name, _) | TokenRule::RegExp(name, _) =
                        &rule.val.as_ref().unwrap()
                    {
                        if !token_names.contains(name) {
                            let msg = format!("Undefined token type: {}", name);
                            errors.push(Error::from_token_without_help(
                                &rule.as_ref().pt.ast.m_token_type_0,
                                msg,
                            ));
                        }
                    }
                }
                _ => {}
            },
            PTTopLevelStatement::PTDefineRuleStatement(rule_pt) => {
                // Make sure self is not duplicated
                let rule = rule_pt.val.as_ref().unwrap();
                let name = &rule.name;
                if duplicated_rule_names.contains(name) {
                    let msg = format!("Duplicate rule definition: {}.", name);
                    let help = "Remove or rename the duplicate definition".to_owned();
                    errors.push(Error::from_token(
                        &rule_pt.as_ref().pt.ast.m_rule_name_2,
                        msg,
                        help,
                    ));
                }
                // Make sure all referenced rules exist

                match &rule.value {
                    RuleValue::Union(subrules) => {
                        // Check this so we don't get an unresolved PT
                        // We must use PT to get the tokens to put error messages if needed
                        assert!(!subrules.is_empty());
                        let pt = &rule_pt.as_ref().pt.m_body.as_ref().pt;
                        let pt = tree_cast!(PTRuleDefineBody::PTUnionRuleBody pt);
                        for (next_rule, token) in pt.vals.iter().zip(pt.asts.iter()) {
                            if !rule_names.contains(next_rule) {
                                let msg = format!("Undefined rule: {}.", next_rule);
                                errors.push(Error::from_token_without_help(token, msg));
                            }
                        }
                    }
                    RuleValue::Function(params, _) => {
                        // param should be clean because the rule is resolved
                        let pt = &rule_pt.as_ref().pt.m_body.as_ref().pt;
                        let pt = tree_cast!(PTRuleDefineBody::PTFunctionalRuleBody pt);
                        let pt = &pt.m_params.as_ref().as_ref().unwrap().pt;

                        for (param, ast) in params.iter().zip(pt.asts.iter()) {
                            if param.is_token {
                                if !token_names.contains(&param.type_name) {
                                    let msg = format!("Undefined token type: {}", param.type_name);
                                    let help =
                                        format!("Token type \"{}\" is undefined", param.name);
                                    errors.push(Error::from_token(
                                        &ast.m_type_3.as_ref().as_ref().unwrap().m_id_2,
                                        msg,
                                        help,
                                    ));
                                }
                            } else {
                                if !rule_names.contains(&param.type_name) {
                                    let msg = format!("Undefined rule: {}.", param.type_name);
                                    let help = format!("Rule \"{}\" is undefined.", param.name);
                                    errors.push(Error::from_token(
                                        &ast.m_type_3.as_ref().as_ref().unwrap().m_id_2,
                                        msg,
                                        help,
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
    Vec<&ASTDefineRuleStatement>,
) {
    let mut tokens = Vec::new();
    let mut token_rules = Vec::new();
    let mut rules = Vec::new();
    let mut emit_actions = Vec::new();
    let mut rule_asts = Vec::new();

    for pt in pt {
        match pt {
            PTTopLevelStatement::PTTopLevelDefineStatement(stmt) => {
                match *stmt.m_body {
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
                        emit_actions.push(EmitAction::Comment(comment.m_t));
                    }
                }
            }
            PTTopLevelStatement::PTDefineRuleStatement(mut rule) => {
                rule_asts.push(rule.as_ref().pt.ast);
                let rule = rule.take_unchecked();
                let name = rule.name.clone();
                rules.push(rule);
                emit_actions.push(EmitAction::Rule(name));
            }
        }
    }
    (tokens, token_rules, rules, emit_actions, rule_asts)
}

enum EmitAction {
    Comment(String),
    Rule(String),
}
