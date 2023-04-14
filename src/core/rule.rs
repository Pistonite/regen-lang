use crate::err;
use crate::sdk::generated::{
    PTDefineRuleStatement, PTExpression, PTFunctionalRuleBody, PTRuleDefineBody, PTUnionRuleBody,
    SemInfo,
};
use crate::sdk::RegenError;
use std::collections::HashMap;
use std::ops::ControlFlow;

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
    hook: Option<Hook>,
    /// Value of the rule
    pub value: RuleValue,
}

impl Rule {
    /// Recursively resolve the return type of the rule
    /// It adds the return type as viewed by the rule itself to the hash map
    /// and returns the return type as viewed by the caller.
    /// 
    /// The return value is either 
    pub fn resolve_ret_type(
        &self,
        ret_types: &mut HashMap<String, RetType>,
        rules: &HashMap<String, &Rule>,
    ) {
        if let Some(t) = ret_types.get(&self.name) {
            // Already resolved or is resolving
            return;
        }

        match &self.value {
            RuleValue::Union(_) => {
                ret_types.insert(self.name.clone(), RetType::Struct);
            }
            RuleValue::Function(params, body) => {
                self.resolve_func_ret_type(params, body, ret_types, rules)
            }
        }
    }

    fn resolve_func_ret_type(
        &self,
        params: &Vec<Param>,
        body: &Expr,
        ret_types: &mut HashMap<String, RetType>,
        rules: &HashMap<String, &Rule>,
    ) {
        // First, resolve the type of all parameters
        let mut param_types = HashMap::new();
        for param in params {
            if let Some(t) = param.get_type() {
                param_types.insert(param.name.clone(), t);
            }
        }

        match body {
            Expr::Concat(vars) => {
                self.resolve_func_concat_ret_type(vars, &param_types, ret_types, rules)
            }
            Expr::Var(name) => {
                let param_type = param_types.get(name).unwrap();
                let self_type = RetType::Param(param_type.clone());
                ret_types.insert(self.name.clone(), self_type);
            }
            Expr::Dict(vars) => {
                ret_types.insert(self.name.clone(), RetType::Struct);
                // Recursively resolve referenced rules
                for var in vars {
                    let param_type = param_types.get(var).unwrap();
                    if let ParamType::Item(_, t) = param_type {
                        let rule = rules.get(t).unwrap();
                        rule.resolve_ret_type(ret_types, rules);
                    }
                }
            }
        }
    }

    fn resolve_func_concat_ret_type(
        &self,
        vars: &Vec<String>,
        param_types: &HashMap<String, ParamType>,
        ret_types: &mut HashMap<String, RetType>,
        rules: &HashMap<String, &Rule>,
    ) {
        let name = &self.name;
        // First, resolve the type of the first var in the concatenation
        let first_var_type = param_types.get(vars.first().unwrap()).unwrap();
        // Mark type of self as a vector of the first var type, so recursive calls can resolve
        let mut self_type = RetType::Vec(first_var_type.clone());
        ret_types.insert(name.clone(), self_type.clone());
        // If first var is a subrule, make sure it is not recursive (as Vec<A> != A)
        if let ParamType::Item(_, first_name) = first_var_type {
            if first_name == name {
                let t = RetType::Unresolved(format!("Cannot resolve return type of {name}: {name} is recursive as the first value in concatenation"));
                ret_types.insert(name.clone(), t);
                return;
            }
            // Recursively resolve the first type
                rules.get(first_name)
                .unwrap()
                .resolve_ret_type(ret_types, rules);
        }
        // Now resolve the last var in the concatenation
        let last_var_type = param_types.get(vars.last().unwrap()).unwrap();
        if let ParamType::Item(_, last_name) = last_var_type {
            // Last is a subrule, resolve it
            rules
                .get(last_name)
                .unwrap()
                .resolve_ret_type(ret_types, rules);
            match ret_types.get(last_name).unwrap() {
                // Subrule last var returns a vector, make sure it is the same type as the first var
                RetType::Vec(t) => {
                    if t != first_var_type {
                        ret_types.insert(
                            name.clone(),
                            RetType::Unresolved(format!("Cannot resolve return type of {id}: Last in concate expression must be a vector of the same type as the rest of the items", id = &self.name))
                        );
                        return;
                    }
                }
                RetType::Param(ParamType::Item(_, t)) => {
                    
                    let last_type = ret_types.get(t).unwrap();
                    if last_type != &self_type {
                        ret_types.insert(
                            self.name.clone(),
                            RetType::Unresolved(format!("Cannot resolve return type of {id}: {t} does not have a vector return type", id = &self.name))
                        );
                        return;
                    }
                }
                _ => {
                    ret_types.insert(
                        self.name.clone(),
                        RetType::Unresolved(format!("Cannot resolve return type of {id}: Last in concate expression must be a vector", id = &self.name))
                    );
                    return;
                }
            }
        } else {
            // Only subrules can be a vector, so last is not a vector, fail.
            ret_types.insert(
                self.name.clone(),
                RetType::Unresolved(format!("Cannot resolve return type of {id}: Last in concate expression must be a vector", id = &self.name))
            );
            return;
        }
        // First update the return type to be a vector of first variable
        // If the first variable is a subrule, resolve it

        // make sure the last is a vector of the same type

        // Make sure the middle are the same as the first
        for var in vars.iter().skip(1).take(vars.len() - 2) {
            let var_type = param_types.get(var).unwrap();
            if var_type != first_var_type {
                ret_types.insert(
                    self.name.clone(),
                    RetType::Unresolved(format!("Cannot resolve return type of {id}: All except last items in concate expression must be the same type", id = &self.name))
                );
                return;
            }
        }
    }
}

/// Parser hook for Rule
pub fn parse_rule(
    pt: &mut PTDefineRuleStatement,
    _si: &mut SemInfo,
    errors: &mut Vec<RegenError>,
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
            errors.push(err!(
                pt.ast.m_rule_name_2,
                "Union rule cannot be recursive".to_owned()
            ));
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
    pt: &mut PTRuleDefineBody,
    _si: &mut SemInfo,
    errors: &mut Vec<RegenError>,
) -> Option<RuleValue> {
    match pt {
        PTRuleDefineBody::PTUnionRuleBody(pt) => parse_rule_value_union(pt, errors),
        PTRuleDefineBody::PTFunctionalRuleBody(pt) => parse_rule_value_function(pt, errors),
    }
}

fn parse_rule_value_union(
    pt: &Box<PTUnionRuleBody>,
    errors: &mut Vec<RegenError>,
) -> Option<RuleValue> {
    // make sure at least one rule exists in the union
    match pt.val.as_ref().as_ref().filter(|x| !x.val.is_empty()) {
        None => {
            let msg = "Union rule must have at least one rule".to_owned();
            errors.push(err!(pt.ast.m___0, msg));
            None
        }
        Some(rules) => {
            // There aren't going to be many subrules, so it's fine to use not use a hash set for deduplication
            let mut subrules = Vec::new();
            for (next_rule, token) in rules.val.iter().zip(rules.ast_vec.iter()) {
                if subrules.iter().find(|r| r == &next_rule).is_some() {
                    let msg = format!("Duplicate rule in a union: {}", next_rule);
                    errors.push(err!(token, msg));
                    // Don't add duplicate params
                } else {
                    subrules.push(next_rule.clone());
                }
            }

            Some(RuleValue::Union(subrules))
        }
    }
}

fn parse_rule_value_function(
    pt: &mut Box<PTFunctionalRuleBody>,
    errors: &mut Vec<RegenError>,
) -> Option<RuleValue> {
    // Validate parameters
    let params = match pt.m_params.as_mut().as_mut() {
        None => {
            let msg = "Rule must have at least one parameter. Use an optional parameter from the outer rule instead.".to_owned();
            errors.push(err!(pt.ast.m___0 /*(*/, msg));
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
            errors.push(err!(pt.ast.m___4 /*;*/, msg.clone()));
            None
        }
        Some(body) => {
            let it = match &body.pt {
                PTExpression::PTConcatExpression(pt) => {
                    let pt = pt.as_ref();
                    pt.val.iter().zip(pt.ast_vec.iter())
                }

                PTExpression::PTDictExpression(pt) => {
                    // dict expression is more structurely complex because of the {}
                    // unwrapping here because empty case is checked in expr hook
                    let pt = pt.as_ref().val.as_ref().as_ref().unwrap();
                    pt.val.iter().zip(pt.ast_vec.iter())
                }
            };
            let mut has_error = false;
            for (var, ast) in it {
                let mut found = false;
                for param in &params {
                    if &param.name == var {
                        found = true;
                        if !param.is_in_pt() {
                            let msg = format!("Cannot use variable \"{var}\" because it is a required token that matches an literal, which will not be in the Parse Tree.");
                            errors.push(err!(ast, msg));
                            has_error = true;
                        }
                        break;
                    }
                }
                if !found {
                    let msg = format!("Variable \"{var}\" is undefined");
                    errors.push(err!(ast, msg));
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

/// Resolved type of a return type of a functional rule
#[derive(Debug, Clone, PartialEq)]
pub enum RetType {
    /// Indicate that the return type cannot be resolved
    /// For example, when a rule concatenates two different types
    Unresolved(String /* error */),
    /// Returning a parameter directly
    Param(ParamType),
    /// Returning a concatenation of parameters
    /// The last parameter must be a rule that returns a Vec type, and the rest must be the corresponding item type.
    /// The return type will be recursively resolved to make sure types are consistent
    Vec(ParamType /* item type */),
    /// Either a union rule, or returning a dictionary of parameters
    Struct,
}
