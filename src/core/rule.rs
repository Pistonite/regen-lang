//! Core logic for language rules

use crate::grammar::{pt, Ctx, Token};
use crate::merge_list_tail_optional_first;
use crate::sdk::{Error, MergedListTail};
use heck::ToUpperCamelCase;

use super::hook::Hook;
use super::param::Param;

/// Definition for a rule (a.k.a. derivation)
///
/// A rule is a derivation step that can be used to generate the Abstract Syntax Tree (AST).
///
/// There are two types of derivations:
/// - Union: a union of derivations. The first derivation that succeeds is used.
///   For example: `rule Biz = Foo | Bar`, where Foo and Bar are also defined by the keyword `rule`
///   The AST generator will try to derive `Foo` first, and if it fails, it will try to derive `Bar`.
/// - Derivation: Derive into smaller rules. Each part is represented by a parameter analogous to a function parameter.
///   For example: `rule Foo(bat: Bar, baz: Baz);`,
///     where Bar and Baz are also defined by the keyword `rule`
///   The AST generator will derive Bar and Baz in order, and then combine them into a Foo AST node.
///
/// With Union rules, only other rules can be specified as part of the union.
/// With derivation rules, token types can also be specified.
#[derive(Debug, Clone)]
pub struct Rule {
  /// Name of the rule. The AST and PT generated types depend on this name.
  pub name: String,
  /// Optional parser hook
  pub hook: Option<Hook>,
  /// Value of the rule
  pub value: RuleValue,
}

impl Rule {
  /// Get the base name of the struct for this rule,
  /// which is the UpperCamelCase of the name
  #[inline]
  pub fn struct_name(&self) -> String {
    self.name.to_upper_camel_case()
  }
}

/// Parser hook for Rule
pub fn parse_rule(pt: &mut pt::DefineRuleStatement, ctx: &mut Ctx) -> Option<()> {
  // Take value which might be unresolved
  let value = match pt.m_body.as_ref().val {
    None => {
      // Rule body is not resolved
      // we will still pass the rule through to make sure rule names can be resolved
      // otherwise there might be an unhealthy amount of errors
      RuleValue::Union(vec![])
    }
    Some(_) => pt.m_body.take_unchecked(),
  };

  if let RuleValue::Union(union) = &value {
    // Validate that rule union cannot be recursive
    if union.contains(&pt.m_rule_name) {
      let msg = "Union rule cannot be recursive".to_owned();
      let help = format!(
        "consider removing \"{name}\" from the union",
        name = &pt.m_rule_name
      );
      ctx
        .err
        .push(Error::from_token(&pt.ast.m_rule_name, msg, help));
    }
  }
  // Take hook which might be None or unresolved
  let hook = pt.m_hook_attr.as_mut().as_mut().map(|x| x.take_unchecked());

  // Add the rule to the context
  if !ctx.val.add_rule(Rule {
    name: pt.m_rule_name.clone(),
    hook,
    value,
  }) {
    let name = &pt.m_rule_name;
    let msg = format!("Duplicate rule definition: {name}.");
    let help = "Remove or rename the duplicate definition".to_owned();
    ctx
      .err
      .push(Error::from_token(&pt.ast.m_rule_name, msg, help));
  }

  None
}

/// Value of a rule
#[derive(Debug, Clone)]
pub enum RuleValue {
  /// Union derivation
  Union(Vec<String>),
  /// Function-like derivation
  Function(Vec<Param>),
}
/// Parser hook for RuleValue
pub fn parse_rule_value(pt: &mut pt::RuleDefineBody, ctx: &mut Ctx) -> Option<RuleValue> {
  match pt {
    pt::RuleDefineBody::UnionRuleBody(pt) => parse_rule_value_union(pt, &mut ctx.err),
    pt::RuleDefineBody::FunctionalRuleBody(pt) => parse_rule_value_function(pt, &mut ctx.err),
  }
}

fn parse_rule_value_union(
  pt: &mut pt::UnionRuleBody,
  errors: &mut Vec<Error>,
) -> Option<RuleValue> {
  // make sure at least one rule exists in the union
  // There aren't going to be many subrules, so it's fine to use a vec instead of hashset for deduplication

  let merged_subrules: MergedListTail<&Token, &str> =
    merge_list_tail_optional_first!(pt, m_first, m_rest, m_r);
  if !merged_subrules.has_first {
    if merged_subrules.vals.is_empty() {
      let msg = "Empty union is not allowed.".to_owned();
      let help = "Add at least one rule after \"=\", or consider using the optional keyword from the referencing rule.".to_owned();
      errors.push(Error::from_token(&pt.ast.m_0, msg, help));
      return None;
    } else {
      let msg = "Expecting a rule".to_owned();
      let help = "Add a rule after \"=\".".to_owned();
      errors.push(Error::from_token(&pt.ast.m_0, msg, help));
      return None;
    }
  }

  let mut subrules = vec![];

  for (ast, subrule) in merged_subrules.into_iter() {
    if subrules.iter().any(|r| r == subrule) {
      // Don't add duplicate rules and give an error
      let msg = "Duplicate rule in a union".to_owned();
      let help = format!("Remove this duplicated \"{}\"", subrule);
      errors.push(Error::from_token(ast, msg, help));
    } else {
      subrules.push(subrule.to_string());
    }
  }

  Some(RuleValue::Union(subrules))
}

fn parse_rule_value_function(
  pt: &mut Box<pt::FunctionalRuleBody>,
  errors: &mut Vec<Error>,
) -> Option<RuleValue> {
  // Validate parameters
  let merged_params = merge_list_tail_optional_first!(mut pt, m_first_param, m_rest_params, m_p);

  if !merged_params.has_first {
    if merged_params.vals.is_empty() {
      let msg = "Functional rules must have at least one parameter".to_owned();
      let help = "Add a parameter after \"(\", or consider using the optional keyword from the referencing rule.".to_owned();
      errors.push(Error::from_token(&pt.ast.m_0 /*(*/, msg, help));
    } else {
      let msg = "Expecting a parameter".to_owned();
      let help = "Add a parameter after \"(\".".to_owned();
      errors.push(Error::from_token(&pt.ast.m_0 /*(*/, msg, help));
    }
    return None;
  }

  let mut params = vec![];

  // There aren't going to be many params, so it's fine to use a vec instead of hashset for deduplication
  for param in merged_params.vals.into_iter() {
    // Make sure param is valid before taking it out
    let param_val = match param.val.as_ref() {
      None => {
        continue;
      }
      Some(_) => param.take_unchecked(),
    };
    // If param won't be in PT, it's fine to have duplicate names
    if !param_val.is_in_pt() {
      // Still need to add it so we can give better error message when validation function bodies.
      params.push(param_val);
      continue;
    }

    let name = &param_val.name;
    if params.iter().any(|p| &p.name == name) {
      // Don't add duplicate params
      let msg = format!("Duplicate parameter name: \"{}\"", name);
      let help = "Rename the parameter or remove the duplicate.".to_string();
      errors.push(Error::from_token(&param.pt.ast.m_variable, msg, help));
    } else {
      params.push(param_val);
    }
  }

  Some(RuleValue::Function(params))
}
