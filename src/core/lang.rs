use std::collections::{HashMap, HashSet};

use crate::emit::Emitter;
use crate::sdk::generated::{ast, pt};
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
  pub target: String,
  pub tokens: Vec<TokenDef>,
  pub token_rules: Vec<TokenRule>,
  pub rules: HashMap<String, Rule>,
  pub ret_types: HashMap<String, RetType>,
  stmts: Vec<Stmt>,
}

impl Language {
  pub fn emit_sdk<E>(&self, mut emitter: E) -> Result<String, Box<dyn std::error::Error>>
  where
    E: Emitter,
  {
    emitter.start(self)?;
    for stmt in &self.stmts {
      match stmt {
        Stmt::Include(path) => emitter.emit_include(self, path)?,
        Stmt::Token(token) => emitter.emit_token(self, &self.tokens[*token])?,
        Stmt::TokenRule(rule) => emitter.emit_token_rule(self, &self.token_rules[*rule])?,
        Stmt::Semantic(semantic) => emitter.emit_semantic(self, semantic)?,
        Stmt::Rule(rule) => emitter.emit_rule(self, self.rules.get(rule).unwrap())?,
      }
    }
    emitter.done(self)
  }
}

impl TryFrom<Vec<pt::TopLevelStatement<'_>>> for Language {
  type Error = Vec<Error>;
  fn try_from(pt: Vec<pt::TopLevelStatement>) -> Result<Self, Self::Error> {
    // Validate that identifier references everywhere are valid
    // If not, we can't continue
    validate_references(&pt)?;

    // If good, take out objects from PT
    let (target, stmts, tokens, token_rules, rules, rule_asts) = extract_from_pt(pt);

    // We need at least 1 rule
    let mut errors = Vec::new();
    let target = match target {
      None => {
        errors.push(Error::global(
          "No rule defined.".to_owned(),
          "Define a rule with the \"rule\" keyword.".to_owned(),
        ));
        return Err(errors);
      }
      Some(x) => x,
    };

    // Resolve return types
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
          errors.push(Error::from_token(&ast.m_rule_name, msg, help));
        }
        None => {
          let ast = rule_asts.get(name).unwrap();
          let msg = format!("Rule \"{name}\" is not referenced by target \"{target}\"");
          let help = "Remove or comment out the rule.".to_owned();
          errors.push(Error::from_token(&ast.m_rule_name, msg, help));
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
      rules,
      stmts,
      ret_types,
    })
  }
}

fn validate_references(pt: &[pt::TopLevelStatement]) -> Result<() /* semantics */, Vec<Error>> {
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
      pt::TopLevelStatement::TopLevelDefineStatement(stmt) => {
        match stmt.m_body.as_ref() {
          pt::TopLevelDefine::DefineTokenTypeStatement(token_def) => {
            let name = &token_def.val.as_ref().unwrap().name;
            if token_names.contains(name) {
              duplicate_token_names.insert(name.clone());
            } else {
              token_names.insert(name.clone());
            }
          }
          pt::TopLevelDefine::DefineSemanticStatement(semantic) => {
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
      pt::TopLevelStatement::DefineRuleStatement(rule) => {
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
      pt::TopLevelStatement::TopLevelDefineStatement(stmt) => match stmt.m_body.as_ref() {
        pt::TopLevelDefine::DefineTokenTypeStatement(token_def) => {
          let name = &token_def.val.as_ref().unwrap().name;
          if duplicate_token_names.contains(name) {
            let msg = format!("Duplicate token definition: {}", name);
            let help = "Remove or rename the duplicate definition".to_owned();
            errors.push(Error::from_token(
              &token_def.as_ref().pt.ast.m_token_type,
              msg,
              help,
            ));
          }
        }
        pt::TopLevelDefine::DefineSemanticStatement(semantic) => {
          let name = semantic.val.as_ref().unwrap();
          if duplicate_semantics.contains(name) {
            let msg = format!("Duplicate semantic definition: {}", name);
            let help = "Remove or rename the duplicate definition".to_owned();
            errors.push(Error::from_token(&semantic.as_ref().pt.ast.m_id, msg, help));
          }
        }
        pt::TopLevelDefine::DefineTokenRuleStatement(rule) => {
          if let TokenRule::Literal(name, _) | TokenRule::RegExp(name, _) =
            &rule.val.as_ref().unwrap()
          {
            if !token_names.contains(name) {
              let msg = format!("Undefined token type: {}", name);
              errors.push(Error::from_token_without_help(
                &rule.as_ref().pt.ast.m_token_type,
                msg,
              ));
            }
          }
        }
        _ => {}
      },
      pt::TopLevelStatement::DefineRuleStatement(rule_pt) => {
        // Make sure self is not duplicated
        let rule = rule_pt.val.as_ref().unwrap();
        let name = &rule.name;
        if duplicated_rule_names.contains(name) {
          let msg = format!("Duplicate rule definition: {}.", name);
          let help = "Remove or rename the duplicate definition".to_owned();
          errors.push(Error::from_token(
            &rule_pt.as_ref().pt.ast.m_rule_name,
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
            let pt = tree_cast!((pt::RuleDefineBody::UnionRuleBody) pt);
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
            let pt = tree_cast!((pt::RuleDefineBody::FunctionalRuleBody) pt);
            let pt = &pt.m_params.as_ref().as_ref().unwrap().pt;

            for (param, ast) in params.iter().zip(pt.asts.iter()) {
              if param.is_token {
                if !token_names.contains(&param.type_name) {
                  let msg = format!("Undefined token type: {}", param.type_name);
                  let help = format!("Token type \"{}\" is undefined", param.name);
                  errors.push(Error::from_token(
                    &ast.m_type.as_ref().as_ref().unwrap().m_id,
                    msg,
                    help,
                  ));
                }
              } else {
                if !rule_names.contains(&param.type_name) {
                  let msg = format!("Undefined rule: {}.", param.type_name);
                  let help = format!("Rule \"{}\" is undefined.", param.name);
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
      }
    }
  }
  if errors.is_empty() {
    Ok(())
  } else {
    Err(errors)
  }
}
fn extract_from_pt(
  pt: Vec<pt::TopLevelStatement>,
) -> (
  Option<String>,
  Vec<Stmt>,
  Vec<TokenDef>,
  Vec<TokenRule>,
  HashMap<String, Rule>,
  HashMap<String, &ast::DefineRuleStatement>,
) {
  let mut target = None;
  let mut stmts = Vec::new();
  let mut tokens = Vec::new();
  let mut token_rules = Vec::new();
  let mut rules = HashMap::new();
  let mut rule_asts = HashMap::new();

  for pt in pt {
    match pt {
      pt::TopLevelStatement::TopLevelDefineStatement(stmt) => match *stmt.m_body {
        pt::TopLevelDefine::DefineTokenTypeStatement(mut token_def) => {
          stmts.push(Stmt::Token(tokens.len()));
          tokens.push(token_def.take_unchecked());
        }
        pt::TopLevelDefine::DefineSemanticStatement(mut semantic) => {
          stmts.push(Stmt::Semantic(semantic.take_unchecked()));
        }
        pt::TopLevelDefine::DefineTokenRuleStatement(mut token_rule) => {
          stmts.push(Stmt::TokenRule(token_rules.len()));
          token_rules.push(token_rule.take_unchecked());
        }
        pt::TopLevelDefine::DefineIgnoreTokenRuleStatement(mut token_rule) => {
          stmts.push(Stmt::TokenRule(token_rules.len()));
          token_rules.push(token_rule.take_unchecked());
        }
        pt::TopLevelDefine::TokenLiteral(include) => {
          stmts.push(Stmt::Include(super::strip_quotes(&include.m_t)));
        }
      },
      pt::TopLevelStatement::DefineRuleStatement(mut rule) => {
        let ast = rule.as_ref().pt.ast;
        let rule = rule.take_unchecked();
        let name = rule.name.clone();
        if target.is_none() {
          target = Some(name.clone());
        }
        rules.insert(name.clone(), rule);
        rule_asts.insert(name.clone(), ast);
        stmts.push(Stmt::Rule(name));
      }
    }
  }
  (target, stmts, tokens, token_rules, rules, rule_asts)
}

enum Stmt {
  Include(String),
  Token(usize),
  TokenRule(usize),
  Semantic(String),
  Rule(String),
}
