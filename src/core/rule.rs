use crate::sdk::generated::{pt, SemInfo};
use crate::sdk::Error;
use crate::tree_cast;
use heck::ToUpperCamelCase;
use std::collections::HashMap;
use std::fmt::Display;

use super::expr::Expr;
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
///   For example: `rule Foo(bat: Bar, baz: Baz) { bat, baz };`,
///     where Bar and Baz are also defined by the keyword `rule`
///   The AST generator will derive Bar and Baz in order, and then combine them into a Foo AST node.
///
/// With Union rules, only other rules can be specified as part of the union.
/// With derivation rules, token types can also be specified.
#[derive(Debug)]
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
  // /// Internal AST type name
  // pub fn ast_node_name(&self) -> String {
  //     format!("ast::{}", super::to_camel_case(&self.name, true))
  // }
  // /// External Parse Tree node type.
  // /// If the rule has a hook, this will be the return type of the hook
  // pub fn pt_type(&self) -> String {
  //     match &self.hook {
  //         Some(hook) => format!(
  //             "ParseHook<{}, {}>",
  //             hook.return_type,
  //             &self.pt_internal_type(true)
  //         ),
  //         None => self.pt_internal_type(true),
  //     }
  // }
  // /// The internal Parse Tree node type.
  // /// If the rule has a hook, this will be the type passed to the hook
  // pub fn pt_internal_type(&self, include_lifetime: bool) -> String {
  //     let mut t = format!("PT{}", super::to_camel_case(&self.name, true));
  //     if include_lifetime {
  //         t.push_str("<'p>")
  //     }
  //     t
  // }
  fn set_opqaue_ret_type<'s>(&'s self, ret_types: &mut HashMap<String, RetType>) {
    let t = RetType::Unit(ParamType::Item(false, self.name.clone()));
    ret_types.insert(self.name.clone(), t);
  }

  /// Recursively resolve the return type of the rule
  /// It adds the return type as viewed by the rule itself to the hash map
  /// and returns the recursively resolved return type
  ///
  /// The returned type is either a Param, indicating that the rule returns a single value
  /// or a Vec, indicating that the rule returns a vector of values
  pub fn resolve_ret_type<'s>(
    &self,
    ret_types: &'s mut HashMap<String, RetType>,
    rules: &HashMap<String, Rule>,
  ) {
    if let Some(t) = ret_types.get(&self.name) {
      match t {
        RetType::Pending => {}
        _ => return,
      }
    }

    match &self.value {
      RuleValue::Union(subrules) => {
        // Union types are opaque
        self.set_opqaue_ret_type(ret_types);
        for r in subrules {
          rules.get(r).unwrap().resolve_ret_type(ret_types, rules);
        }
      }
      RuleValue::Function(params, body) => {
        self.resolve_func_ret_type(params, body, ret_types, rules)
      }
    }
  }

  /// Helper function to resolve the return type of a function rule
  fn resolve_func_ret_type<'s>(
    &self,
    params: &Vec<Param>,
    body: &Expr,
    ret_types: &'s mut HashMap<String, RetType>,
    rules: &HashMap<String, Rule>,
  ) {
    // First, resolve the type of all parameters
    let mut param_types = HashMap::new();
    for param in params {
      if let Some(t) = param.get_type() {
        param_types.insert(param.name.clone(), t);
      }
    }

    match body {
      Expr::Concat(vars) => self.resolve_func_concat_ret_type(vars, &param_types, ret_types, rules),
      Expr::Var(name) => {
        // If it's a var, check if we are returning a subrule or a token by checking the param type
        let param_type = param_types.get(name).unwrap();
        if let ParamType::Item(_, subrule) = param_type {
          ret_types.insert(self.name.clone(), RetType::Pending);
          // Recursively resolve referenced rule
          let rule = rules.get(subrule).unwrap();
          if let Some(t) = ret_types.get(&rule.name) {
            if let RetType::Pending = t {
              // We have a pending dependency on this rule, but it is also pending
              // This means that we have a circular dependency
              ret_types.insert(
                self.name.clone(),
                RetType::Unresolved(format!(
                  "Infinite recursion detected for rule \"{}\"",
                  self.name
                )),
              );
              return;
            }
          }
          let sub_ret_type = {
            rule.resolve_ret_type(ret_types, rules);
            ret_types.get(subrule).unwrap()
          };
          if let RetType::Unresolved(err) = sub_ret_type {
            // If the referenced rule has an unresolved return type, we also have an unresolved return type
            ret_types.insert(
              self.name.clone(),
              RetType::Unresolved(format!(
                "Cannot resolve return type of subrule {subrule}: {err}"
              )),
            );
            return;
          }
          ret_types.insert(self.name.clone(), sub_ret_type.clone());
        } else {
          self.set_opqaue_ret_type(ret_types);
        }
      }
      Expr::Dict(vars) => {
        self.set_opqaue_ret_type(ret_types);
        let mut error = String::new();
        // Recursively resolve referenced rules
        for var in vars {
          let param_type = param_types.get(var).unwrap();
          if let ParamType::Item(_, subrule) = param_type {
            let sub_ret_type = {
              rules
                .get(subrule)
                .unwrap()
                .resolve_ret_type(ret_types, rules);
              ret_types.get(subrule).unwrap()
            };
            if let RetType::Unresolved(err) = sub_ret_type {
              error.push_str(&format!(
                "Cannot resolve return type of variable {var}: {err}"
              ))
            }
          }
        }
        if !error.is_empty() {
          ret_types.insert(
            self.name.clone(),
            RetType::Unresolved(format!("Cannot resolve return type of dictionary: {error}")),
          );
        }
      }
    }
  }

  /// Helper function to resolve the return type of a function rule that returns a concatenation
  fn resolve_func_concat_ret_type<'s>(
    &self,
    vars: &Vec<String>,
    param_types: &HashMap<String, ParamType>,
    //apparent_ret_types: &mut HashMap<String, RetType>,
    ret_types: &'s mut HashMap<String, RetType>,
    rules: &HashMap<String, Rule>,
  ) {
    let name = &self.name;
    // First, resolve the type of the first var in the concatenation
    let first_var_type = param_types.get(vars.first().unwrap()).unwrap();
    let first_ret_type = RetType::Unit(first_var_type.clone());
    // Mark type of self as a vector of the first var type, so recursive calls can resolve
    ret_types.insert(name.clone(), RetType::Vec(Box::new(first_ret_type.clone())));
    //let mut self_ret_type = RetType::Vec(Box::new(first_ret_type.clone()));
    // If first var is a subrule, make sure it is not recursive (as Vec<A> != A)
    if let ParamType::Item(_, first_name) = first_var_type {
      let first_sub_ret_type = {
        rules
          .get(first_name)
          .unwrap()
          .resolve_ret_type(ret_types, rules);
        ret_types.get(first_name).unwrap()
      };

      if let RetType::Unresolved(err) = first_sub_ret_type {
        // If the referenced rule has an unresolved return type, we also have an unresolved return type
        ret_types.insert(
          name.clone(),
          RetType::Unresolved(format!(
            "Cannot resolve return type of subrule {first_name}: {err}"
          )),
        );
        return;
      }
      if !first_ret_type.is_compatible(first_sub_ret_type) {
        // If the referenced rule has a different return type, we also have an unresolved return type
        ret_types.insert(name.clone(), RetType::Unresolved(format!(
                    "Return type of {first_name} ({first_sub_ret_type}) is inconsistent with {first_ret_type} as the first item in a concatenation"
                )));
        return;
      }

      // update self ret type to be vector of first
      ret_types.insert(
        name.clone(),
        RetType::Vec(Box::new(first_sub_ret_type.clone())),
      );
    }
    // Now resolve the last var in the concatenation
    let self_ret_type = ret_types.get(name).unwrap().clone();
    let last_var_type = param_types.get(vars.last().unwrap()).unwrap();
    if let ParamType::Item(_, last_name) = last_var_type {
      let last_ret_type = {
        rules
          .get(last_name)
          .unwrap()
          .resolve_ret_type(ret_types, rules);
        ret_types.get(last_name).unwrap()
      };
      if !self_ret_type.is_compatible(last_ret_type) {
        ret_types.insert(name.clone(), RetType::Unresolved(format!(
                    "The return type of {last_name} ({last_ret_type}) is inconsistent with the rest of the items in the concatenation ({self_ret_type})"
                )));
        return;
      }
      // no need to update as compatible means having the same vec type
    } else {
      // If last var is not a subrule, it cannot be a vector
      ret_types.insert(
        name.clone(),
        RetType::Unresolved(
          "Return type of the last variable in a concatenation must be a vector".to_owned(),
        ),
      );
      return;
    }

    // Make sure the middle are the same as the first (not just compatible)
    let item_type = tree_cast!((RetType::Vec) & self_ret_type).as_ref();
    for var in vars.iter().skip(1).take(vars.len() - 2) {
      let var_type = param_types.get(var).unwrap();
      let var_unit_type = RetType::Unit(var_type.clone());
      // Recursively resolve the return type of var
      let var_ret_type = match var_type {
        ParamType::Item(_, subrule) => {
          rules
            .get(subrule)
            .unwrap()
            .resolve_ret_type(ret_types, rules);
          ret_types.get(subrule).unwrap()
        }
        _ => &var_unit_type,
      };

      if var_ret_type != item_type {
        ret_types.insert(name.clone(), RetType::Unresolved(format!(
                    "Return type of {var} in a concatenation is inconsistent with the rest of the items in the concatenation"
                )));
        return;
      }
    }
  }
}

/// Parser hook for Rule
pub fn parse_rule(
  pt: &mut pt::DefineRuleStatement,
  _si: &mut SemInfo,
  errors: &mut Vec<Error>,
) -> Option<Rule> {
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
      errors.push(Error::from_token(&pt.ast.m_rule_name, msg, help));
    }
  }
  // Take hook which might be None or unresolved
  let hook = pt.m_hook_attr.as_mut().as_mut().map(|x| x.take_unchecked());
  Some(Rule {
    name: pt.m_rule_name.clone(),
    hook,
    value,
  })
}

/// Value of a rule
#[derive(Debug)]
pub enum RuleValue {
  /// Union derivation
  Union(Vec<String>),
  /// Function-like derivation
  Function(Vec<Param>, Expr),
}
/// Parser hook for RuleValue
pub fn parse_rule_value(
  pt: &mut pt::RuleDefineBody,
  _si: &mut SemInfo,
  errors: &mut Vec<Error>,
) -> Option<RuleValue> {
  match pt {
    pt::RuleDefineBody::UnionRuleBody(pt) => parse_rule_value_union(pt, errors),
    pt::RuleDefineBody::FunctionalRuleBody(pt) => parse_rule_value_function(pt, errors),
  }
}

fn parse_rule_value_union(
  pt: &Box<pt::UnionRuleBody>,
  errors: &mut Vec<Error>,
) -> Option<RuleValue> {
  // make sure at least one rule exists in the union
  if pt.vals.is_empty() {
    let msg = "Empty union is not allowed.".to_owned();
    let help = "Add at least one rule after \"=\", or consider using the optional keyword from the referencing rule.".to_owned();
    errors.push(Error::from_token(&pt.ast.m_0, msg, help));
    return None;
  }
  // There aren't going to be many subrules, so it's fine to use not use a hash set for deduplication
  let mut subrules = Vec::new();
  for (next_rule, token) in pt.vals.iter().zip(pt.asts.iter()) {
    if subrules.iter().find(|r| r == &next_rule).is_some() {
      let msg = "Duplicate rule in a union".to_owned();
      let help = format!("Remove this duplicated \"{}\"", next_rule);
      errors.push(Error::from_token(token, msg, help));
      // Don't add duplicate rules
    } else {
      subrules.push(next_rule.clone());
    }
  }

  Some(RuleValue::Union(subrules))
}

fn parse_rule_value_function(
  pt: &mut Box<pt::FunctionalRuleBody>,
  errors: &mut Vec<Error>,
) -> Option<RuleValue> {
  // Validate parameters
  let params = match pt.m_params.as_mut().as_mut() {
    None => {
      let msg = "Functional rules must have at least one parameter".to_owned();
      let help = "Add a parameter after \"(\", or consider using the optional keyword from the referencing rule.".to_owned();
      errors.push(Error::from_token(&pt.ast.m_0 /*(*/, msg, help));
      None
    }
    Some(params) => {
      let params = params.take_unchecked();
      // Not possible to be empty because of language definition
      assert!(!params.is_empty());
      Some(params)
    }
  }?;
  // Validate body (all vars in body must be defined in params and in PT)
  let body = match pt.m_body.as_mut().as_mut() {
    None => {
      let msg = "Rule must have a body".to_owned();
      let help = "Add an expression before \";\"".to_owned();
      errors.push(Error::from_token(&pt.ast.m_4 /*;*/, msg, help));
      None
    }
    Some(body) => {
      let it = match &body.pt {
        pt::Expression::ConcatExpression(pt) => {
          let pt = pt.as_ref();
          pt.vals.iter().zip(pt.asts.iter())
        }

        pt::Expression::DictExpression(pt) => {
          let pt = pt.as_ref();
          pt.vals.iter().zip(pt.asts.iter())
        }
      };
      let mut has_error = false;
      for (var, ast) in it {
        let mut found = false;
        for param in &params {
          if &param.name == var {
            found = true;
            if !param.is_in_pt() {
              let msg = format!("Variable \"{var}\" is not in the parse tree.");
              let help = format!("This is a required token that matches an literal, which doesn't give you information. Consider removing it.");
              errors.push(Error::from_token(ast, msg, help));
              has_error = true;
            }
            break;
          }
        }
        if !found {
          let msg = format!("Variable \"{var}\" is undefined");
          errors.push(Error::from_token_without_help(ast, msg));
          has_error = true;
        }
      }
      if has_error {
        None
      } else {
        Some(body.take_unchecked())
      }
    }
  }?;

  Some(RuleValue::Function(params, body))
}

/// Resolved type of a parameter in a functional rule
#[derive(Debug, Clone, PartialEq)]
pub enum ParamType {
  /// Subrule value ( i.e. param: SubRule or param: optional SubRule )
  Item(bool /*optional*/, String /* name */),
  /// Token value (i.e. param: token Something or param: optional token Something)
  String(bool /*optional*/),
  /// Token value with literal spec (i.e. param: optional token Something "spec")
  Bool,
}

impl Display for ParamType {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      ParamType::Item(optional, name) => {
        if *optional {
          write!(f, "optional ")?;
        }
        write!(f, "{}", name)
      }
      ParamType::String(optional) => {
        if *optional {
          write!(f, "optional ")?;
        }
        write!(f, "string")
      }
      ParamType::Bool => write!(f, "bool"),
    }
  }
}

impl ParamType {
  pub fn is_optional(&self) -> bool {
    match self {
      ParamType::Item(optional, _) => *optional,
      ParamType::String(optional) => *optional,
      ParamType::Bool => false,
    }
  }
}

/// The vector characteristics of a rule.
/// This is used to determine if a rule returns a vector value.
/// A functional rule returns a vector value if it returns:
/// 1. A valid concatenation of two or more values
/// 2. A single value that is a rule that returns a vector value
///
/// This is used in resolving return types.
/// For example, the last variable in a concatenation must be a rule that returns a vector value.
#[derive(Debug, Clone, PartialEq)]
pub enum RetType {
  Unresolved(String),
  Pending,
  // PT will be either a union (enum), or struct ({ast, m_x, m_y, ...}), or single value {ast, val}
  Unit(ParamType /* rule name */),
  // PT will be vec ({ast, asts, vals})
  Vec(Box<RetType> /* rule name */),
}

impl Display for RetType {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      RetType::Unresolved(_) => f.pad("unresolved"),
      RetType::Pending => f.pad("pending"),
      RetType::Unit(s) => write!(f, "{}", s),
      RetType::Vec(s) => write!(f, "vec<{}>", s),
    }
  }
}

impl RetType {
  /// Return if sub is compatible with self as a return type.
  /// This is used when resolving return types, where a temporary return type is set to a rule before recursively resolving dependencies.
  pub fn is_compatible(&self, sub: &Self) -> bool {
    match (self, sub) {
      (RetType::Unit(_), RetType::Unit(_)) => true,
      (RetType::Vec(s), RetType::Vec(b)) => {
        // If both self and subrule are vectors, they must have the same item type
        s.as_ref() == b.as_ref()
      }
      (RetType::Unit(_), RetType::Vec(b)) => {
        // If self is a unit and subrule is a vec, subrule cannot be a vec of self
        // otherwise, it will be a vector with infinite depth
        self != b.as_ref()
      }
      (RetType::Vec(_), RetType::Unit(_)) => false,
      _ => false, // one is unresolved or pending
    }
  }
}

macro_rules! rule {
  ($identifier:literal, $value:expr) => {
    Rule {
      name: $identifier.to_owned(),
      hook: None,
      value: $value,
    }
  };
  ($identifier:literal, $value:expr, $hook:expr) => {
    Rule {
      name: $identifier.to_owned(),
      hook: Some($hook),
      value: $value,
    }
  };
}
pub(crate) use rule;
