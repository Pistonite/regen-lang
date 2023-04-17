use std::collections::{HashMap, HashSet};
use std::fmt::Write;
pub mod expr;
use expr::expr;
pub use expr::Expr;
pub mod hook;
pub use hook::Hook;
pub mod lang;
pub use lang::Language;
pub mod param;
pub use param::Param;
pub mod rule;
pub use rule::{ParamType, RetType, Rule, RuleValue};
pub mod semantic;
pub mod token;
pub use token::{TokenDef, TokenRule};

use hook::hook;
use param::param;
use rule::rule;
use token::{token, tokenrule};

use crate::emit::{NameContext, RustNameContext};

pub fn strip_quotes(s: &str) -> String {
  if !s.starts_with('"') || !s.ends_with('"') {
    panic!("Expected string to start and end with quotes");
  }
  s[1..s.len() - 1].to_string()
}

pub fn strip_slashes(s: &str) -> String {
  if !s.starts_with('/') || !s.ends_with('/') {
    panic!("Expected string to start and end with slashes");
  }
  s[1..s.len() - 1].to_string()
}

/// The main language definition
pub struct LangDef {
  target: String,
  tokens: Vec<TokenDef>,
  token_rules: Vec<TokenRule>,
  semantics: Vec<String>,
  rules: HashMap<String, Rule>,
}

// convienient macros for testing



impl LangDef {
  pub fn make_test() -> Self {
    let rules = vec![
      // rule!(
      //     "Target",
      //     RuleValue::Function(
      //         vec![
      //             param!("first": "TopLevelStatement"),
      //             param!("rest": optional "Target"),
      //         ],
      //         expr!["first" | "rest"]
      //     )
      // ),
      rule!(
        "TopLevelStatement",
        RuleValue::Union(vec![
          "DefineRuleStatement".to_string(),
          "TopLevelDefineStatement".to_string(),
        ])
      ),
      rule!(
        "DefineRuleStatement",
        RuleValue::Function(
          vec![
            param!("_": token "Keyword" "rule"),
            param!("hookAttr": optional "HookAttribute"),
            param!(("Rule") "ruleName": token "Identifier"),
            param!("body": "RuleDefineBody"),
          ],
          expr! {hookAttr, ruleName, body}
        ),
        hook!("parse_rule" : "Rule")
      ),
      rule!(
        "HookAttribute",
        RuleValue::Function(
          vec![
            param!("_": token "Symbol" "("),
            param!(("HookName") "hookName": token "Literal"),
            param!("_": token "Symbol" ":"),
            param!(("HookType") "hookType": token "Literal"),
            param!("_": token "Symbol" ")"),
          ],
          expr! {hookName, hookType}
        ),
        hook!("parse_hook" : "Hook")
      ),
      rule!(
        "RuleDefineBody",
        RuleValue::Union(vec![
          "UnionRuleBody".to_string(),
          "FunctionalRuleBody".to_string(),
        ]),
        hook!("parse_rule_value" : "RuleValue")
      ),
      rule!(
        "UnionRuleBody",
        RuleValue::Function(
          vec![
            param!("_": token "Symbol" "="),
            param!("rules": optional "UnionRuleList"),
            param!("_": token "Symbol" ";"),
          ],
          Expr::Var("rules".to_string())
        )
      ),
      rule!(
        "UnionRuleList",
        RuleValue::Function(
          vec![
            param!(("Rule") "first": token "Identifier"),
            param!("rest": optional "UnionRuleListTail"),
          ],
          expr!(first | rest)
        )
      ),
      rule!(
        "UnionRuleListTail",
        RuleValue::Function(
          vec![
            param!("_": token "Symbol" "|"),
            param!(("Rule") "first": token "Identifier"),
            param!("rest": optional "UnionRuleListTail"),
          ],
          expr!(first | rest)
        )
      ),
      rule!(
        "FunctionalRuleBody",
        RuleValue::Function(
          vec![
            param!("_": token "Symbol" "("),
            param!("params": optional "ParamList"),
            param!("_": token "Symbol" ")"),
            param!("body": optional "Expression"),
            param!("_": token "Symbol" ";"),
          ],
          expr!(params, body)
        )
      ),
      rule!(
        "ParamList",
        RuleValue::Function(
          vec![
            param!("first": "Parameter"),
            param!("rest": optional "ParamListTail"),
          ],
          expr!(first | rest)
        ),
        hook!("parse_param_list" : "Vec<Param>")
      ),
      rule!(
        "ParamListTail",
        RuleValue::Function(
          vec![
            param!("_": token "Symbol" ","),
            param!("first": "Parameter"),
            param!("rest": optional "ParamListTail"),
          ],
          expr!(first | rest)
        )
      ),
      rule!(
        "Parameter",
        RuleValue::Function(
          vec![
            param!("semAttr": optional "ParamSemantic"),
            param!(("Variable") "variable": token "Identifier"),
            param!("_": token "Symbol" ":"),
            param!("type": optional "RuleType"),
          ],
          expr! {semAttr, variable, type}
        ),
        hook!("parse_param" : "Param")
      ),
      rule!(
        "ParamSemantic",
        RuleValue::Function(
          vec![
            param!("_": token "Symbol" "("),
            param!(("Semantic") "semanticName": optional token "Identifier"),
            param!("_": token "Symbol" ")"),
          ],
          Expr::Var("semanticName".to_string())
        )
      ),
      rule!(
        "RuleType",
        RuleValue::Function(
          vec![
            param!("kwOptional": optional token "Keyword" "optional"),
            param!("kwToken": optional token "Keyword" "token"),
            param!("id": token "Identifier"),
            param!("tokenContent": optional token "Literal"),
          ],
          expr! {kwOptional, kwToken, id, tokenContent}
        )
      ),
      rule!(
        "TopLevelDefineStatement",
        RuleValue::Function(
          vec![
            param!("body": "TopLevelDefine"),
            param!("_": token "Symbol" ";"),
          ],
          Expr::Var("body".to_string())
        )
      ),
      rule!(
        "TopLevelDefine",
        RuleValue::Union(vec![
          "TokenLiteral".to_string(),
          "DefineTokenTypeStatement".to_string(),
          "DefineIgnoreTokenRuleStatement".to_string(),
          "DefineTokenRuleStatement".to_string(),
          "DefineSemanticStatement".to_string(),
        ])
      ),
      rule!(
        "DefineTokenTypeStatement",
        RuleValue::Function(
          vec![
            param!("kwExtract": optional token "Keyword" "extract"),
            param!("_": token "Keyword" "token"),
            param!(("Token") "tokenType": token "Identifier"),
          ],
          expr! {kwExtract, tokenType}
        ),
        hook!("parse_token_def" : "TokenDef")
      ),
      rule!(
        "DefineIgnoreTokenRuleStatement",
        RuleValue::Function(
          vec![
            param!("_": token "Keyword" "ignore"),
            param!("value": "LiteralOrRegExp"),
          ],
          Expr::Var("value".to_string())
        ),
        hook!("parse_token_ignore_rule" : "TokenRule")
      ),
      rule!(
        "DefineTokenRuleStatement",
        RuleValue::Function(
          vec![
            param!(("Token") "tokenType": token "Identifier"),
            param!("value": "LiteralOrRegExp"),
          ],
          expr! {tokenType, value}
        ),
        hook!("parse_token_rule": "TokenRule")
      ),
      rule!(
        "LiteralOrRegExp",
        RuleValue::Union(vec!["TokenLiteral".to_string(), "TokenRegExp".to_string(),])
      ),
      rule!(
        "TokenLiteral",
        RuleValue::Function(
          vec![param!("t": token "Literal"),],
          Expr::Var("t".to_string())
        )
      ),
      rule!(
        "TokenRegExp",
        RuleValue::Function(
          vec![param!("t": token "RegExp"),],
          Expr::Var("t".to_string())
        )
      ),
      rule!(
        "DefineSemanticStatement",
        RuleValue::Function(
          vec![
            param!("_": token "Keyword" "semantic"),
            param!(("Semantic") "id": token "Identifier"),
          ],
          Expr::Var("id".to_string())
        ),
        hook!("parse_semantic": "String")
      ),
      rule!(
        "Expression",
        RuleValue::Union(vec![
          "ConcatExpression".to_string(),
          "DictExpression".to_string(),
        ]),
        hook!("parse_expr": "Expr")
      ),
      rule!(
        "ConcatExpression",
        RuleValue::Function(
          vec![
            param!(("Variable") "first": token "Identifier"),
            param!("rest": optional "ConcatExpressionTail"),
          ],
          expr!(first | rest)
        )
      ),
      rule!(
        "ConcatExpressionTail",
        RuleValue::Function(
          vec![
            param!("_": token "Symbol" "|"),
            param!("rest": "ConcatExpression"),
          ],
          Expr::Var("rest".to_string())
        )
      ),
      rule!(
        "DictExpression",
        RuleValue::Function(
          vec![
            param!("_": token "Symbol" "{"),
            param!("values": optional "VariableList"),
            param!("_": token "Symbol" "}"),
          ],
          Expr::Var("values".to_string())
        )
      ),
      rule!(
        "VariableList",
        RuleValue::Function(
          vec![
            param!(("Variable") "first": token "Identifier"),
            param!("rest": optional "VariableListTail"),
          ],
          expr!(first | rest)
        )
      ),
      rule!(
        "VariableListTail",
        RuleValue::Function(
          vec![
            param!("_": token "Symbol" ","),
            param!("rest": optional "VariableList"),
          ],
          Expr::Var("rest".to_string())
        )
      ),
    ];
    let target = rules.first().unwrap().name.clone();
    let rule_map = rules.into_iter().map(|r| (r.name.clone(), r)).collect();
    LangDef {
      target,
      rules: rule_map,
      semantics: vec![
        "Variable".to_string(),
        "Token".to_string(),
        "Semantic".to_string(),
        "Rule".to_string(),
        "HookName".to_string(),
        "HookType".to_string(),
      ],
      tokens: vec![
        token!(extract Comment),
        token!(Keyword),
        token!(Identifier),
        token!(RegExp),
        token!(Literal),
        token!(Symbol),
      ],
      token_rules: vec![
        tokenrule!(ignore regex r"\s+"),
        tokenrule!(Comment regex r"\/\/[^\n]*\n?"),
        tokenrule!(Comment regex r"\/\*([^\*]|(\*[^\/]))*\*\/"),
        tokenrule!(Literal regex r#""((\\.)|[^\\"])*""#),
        tokenrule!(RegExp regex r"\/((\\.)|[^\\\/])*\/"),
        tokenrule!(Keyword "ignore"),
        tokenrule!(Keyword "extract"),
        tokenrule!(Keyword "token"),
        tokenrule!(Keyword "semantic"),
        tokenrule!(Keyword "hook"),
        tokenrule!(Keyword "rule"),
        tokenrule!(Keyword "optional"),
        tokenrule!(Keyword "or"),
        tokenrule!(Keyword "null"),
        tokenrule!(Symbol regex r"[{};|()=,:\.\[\]]"),
        tokenrule!(Identifier regex r"[_a-zA-Z]\w*"),
      ],
    }
  }

  pub fn emit_rust(&mut self) {
    let mut should_extract_set = HashSet::new();
    for token in &self.tokens {
      if token.is_extract {
        should_extract_set.insert(&token.name);
      }
    }
    // base SDK
    println!("{}", include_str!("../sdk/generated.rs.pp"));
    println!("crate::sdk!(crate;");
    let first_target = self.rules.get(&self.target).unwrap();
    let mut name_ctx = RustNameContext::new(&self.rules);
    println!("    target: {};", name_ctx.ast(&first_target.name, false));

    println!("    tokens: [");
    for token in &self.tokens {
      println!("        T{},", &token.name);
    }
    println!("    ];");
    if self.token_rules.is_empty() {
      println!("    regex: [,];");
    } else {
      println!("    regex: [");
      for (i, rule) in self.token_rules.iter().enumerate() {
        if let Some(literal) = rule.get_regex() {
          let mut escape = String::new();
          while literal.contains(&format!("\"{}", escape)) {
            escape.push_str("#");
          }
          let literal = literal.replace(r"\/", "/");
          println!("        re{i} = r{escape}\"^{literal}\"{escape},");
        }
      }
      println!("    ];");
    }
    println!("    rules: [");

    for (i, rule) in self.token_rules.iter().enumerate() {
      match rule {
        TokenRule::IgnoreLiteral(literal) => {
          println!("        [\"{literal}\", {}],", literal.len());
        }
        TokenRule::IgnoreRegExp(_) => {
          println!("        [re{i}],");
        }
        TokenRule::Literal(name, literal) => {
          let should_extract = should_extract_set.contains(name);
          println!(
            "        [{should_extract}, T{name}, \"{literal}\", {}],",
            literal.len()
          );
        }
        TokenRule::RegExp(name, _ /* don't need this as regex is already compiled */) => {
          let should_extract = should_extract_set.contains(name);
          println!("        [{should_extract}, T{name}, re{i}],");
        }
      }
    }
    println!("    ];");

    println!("    semantics: [");
    for semantic in &self.semantics {
      println!("        S{semantic},");
    }
    println!("    ];");
    println!(");");
    
    let mut ret_type_map = HashMap::new();

    // for (_, r) in &self.rules {
    //     pt_type_map.insert(r.name.clone(), r.pt_type());
    //     //pt_internal_type_map.insert(r.name.clone(), r.pt_internal_type(false));
    // }

    for (name, r) in &self.rules {
      r.resolve_ret_type(&mut ret_type_map, &self.rules);
    }

    let mut mod_ast = String::new();
    let mut mod_pt = String::new();
    let mut mod_main = String::new();
    writeln!(mod_ast, "pub mod ast {{");
    writeln!(mod_ast, "use super::*;");
    writeln!(mod_pt, "pub mod pt {{");
    writeln!(mod_pt, "use super::*;");

    for (name, r) in &self.rules {
      //println!("// {id}", id = &r.name);
      self.emit_rust_ast_rule(r, &mut name_ctx, &mut mod_ast, &mut mod_main);
      self.emit_rust_pt_rule(r, &mut name_ctx, &ret_type_map, &mut mod_pt, &mut mod_main);
    }

    writeln!(mod_ast, "}}");
    writeln!(mod_pt, "}}");
    println!("{}", mod_ast);
    println!("{}", mod_pt);
    println!("{}", mod_main);
  }

  fn emit_rust_ast_rule(
    &self,
    rule: &Rule,
    name_ctx: &mut RustNameContext,
    mod_ast: &mut String,
    mod_main: &mut String,
  ) {
    let ast_t = name_ctx.ast(&rule.name, false);

    match &rule.value {
      RuleValue::Union(subrules) => {
        writeln!(mod_ast, "#[derive(Debug)] pub enum {ast_t} {{");
        for subrule in subrules {
          let ast_t = name_ctx.ast(subrule, false);
          writeln!(mod_ast, "    {ast_t}(Box<{ast_t}>),");
        }
        writeln!(mod_ast, "}}");
      }
      RuleValue::Function(params, _unused_body) => {
        let id = &rule.name;

        let ast_ns_t = name_ctx.ast(&rule.name, true);
        // Generating the AST does not need body
        let member_names = params
          .iter()
          .enumerate()
          .map(|(i, p)| {
            if p.is_in_pt() {
              name_ctx.member_name(&p.name)
            } else {
              name_ctx.member_name_with_idx(&p.name, i)
            }
          })
          .collect::<Vec<_>>();

        // Generate struct type
        writeln!(mod_ast, "#[derive(Debug)] pub struct {ast_t} {{");
        for (param, member) in params.iter().zip(member_names.iter()) {
          let param_t = name_ctx.ast_param_type(&param);
          writeln!(mod_ast, "    pub {member}: {param_t},");
        }
        writeln!(mod_ast, "}}");
        writeln!(mod_main, "impl {ast_ns_t} {{");
        // Generate parser func
        writeln!(
          mod_main,
          "    fn parse(ts: &mut TokenStream<Tok>) -> Option<Self> {{"
        );
        // Rust struct expressions have guaranteed eval order, so this is fine
        writeln!(mod_main, "        Some(Self {{");
        for (param, member) in params.iter().zip(member_names.iter()) {
          let type_name = &param.type_name;
          if !param.is_token {
            let t = name_ctx.ast(&type_name, false);
            if param.is_optional {
              writeln!(
                mod_main,
                "        {member}: Box::new(optional!(ts, ast::{t}::parse(ts))),"
              );
            } else {
              writeln!(
                mod_main,
                "        {member}: Box::new(ast::{t}::parse(ts)?),"
              );
            }
          } else {
            if param.is_optional {
              match &param.match_literal {
                None => {
                  writeln!(
                    mod_main,
                    "        {member}: optional!(ts, token!(T{type_name}::parse(ts))),"
                  );
                }
                Some(literal) => {
                  writeln!(
                    mod_main,
                    "        {member}: optional!(ts, token!(T{type_name}::\"{literal}\"(ts))),"
                  );
                }
              }
            } else {
              match &param.match_literal {
                None => {
                  writeln!(
                    mod_main,
                    "        {member}: token!(T{type_name}::parse(ts))?,"
                  );
                }
                Some(literal) => {
                  writeln!(
                    mod_main,
                    "        {member}: token!(T{type_name}::\"{literal}\"(ts))?,"
                  );
                }
              }
            }
          }
        }

        // for (i, param) in params.iter().enumerate() {
        //     let member = format!("m_{}_{i}", to_snake_case(&param.name));
        //     if !&param.is_token {
        //         println!("            {member}: Box::new({member}),");
        //     } else {
        //         println!("            {member},");
        //     }
        // }
        writeln!(mod_main, "        }})");
        writeln!(mod_main, "    }}");
        writeln!(
          mod_main,
          "    fn apply_semantic(&self, _si: &mut SemInfo) {{"
        );
        for (param, member) in params.iter().zip(member_names.iter()) {
          if !param.is_token {
            if param.is_optional {
              writeln!(
                mod_main,
                "        if let Some(m) = self.{member}.as_ref() {{ m.apply_semantic(_si); }}"
              );
            } else {
              writeln!(mod_main, "        self.{member}.apply_semantic(_si);");
            }
          } else {
            if let Some(s) = &param.semantic {
              if param.is_optional {
                writeln!(
                  mod_main,
                  "        if let Some(m) = &self.{member} {{ _si.set(m, Sem::S{s}); }}"
                );
              } else {
                writeln!(mod_main, "        _si.set(&self.{member}, Sem::S{s});");
              }
            }
          }
        }
        writeln!(mod_main, "    }}");
        writeln!(mod_main, "}}");
      }
    }
  }

  fn emit_rust_pt_rule(
    &self,
    rule: &Rule,
    name_ctx: &mut RustNameContext,
    // ast_type_map: &HashMap<String, String>,
    // pt_type_map: &HashMap<String, String>,
    // pt_internal_type_map: &HashMap<String, String>,
    ret_type_map: &HashMap<String, RetType>,
    mod_pt: &mut String,
    mod_main: &mut String,
  ) {
    //let id = &rule.name;
    // let ast_type = ast_type_map.get(id).unwrap();
    // let pt_type = pt_type_map.get(id).unwrap();
    // let pt_internal_type = pt_internal_type_map.get(id).unwrap();

    //let mut type_lifetime ;

    let macro_func_name = match &rule.hook {
      None => "from_ast",
      Some(_) => "from_ast_internal",
    };
    let pt_t = name_ctx.pt_inner_type(&rule.name, false);
    let pt_ns_t = name_ctx.pt_inner_type(&rule.name, true);
    let ast_ns_t = name_ctx.ast(&rule.name, true);

    match &rule.value {
      RuleValue::Union(type_defs) => {
        // The union type might not have all PT variants if multiple hooks
        // returns the same type.
        let mut pt_union_variants = HashSet::new();

        let ast_t = name_ctx.ast(&rule.name, false);

        writeln!(mod_main, "crate::impl_union!({macro_func_name}, {ast_t},");
        writeln!(mod_main, "{{");
        for type_def in type_defs {
          let t = name_ctx.ast(&type_def, false);
          writeln!(mod_main, "    {t},");
          pt_union_variants.insert(type_def);
          //println!("    {t},", t = ast_type_map.get(type_def).unwrap());
        }
        writeln!(mod_main, "}});");

        writeln!(mod_pt, "#[derive(Debug)] pub enum {pt_t}<'p> {{ ");
        for type_def in pt_union_variants {
          let pt_variant = name_ctx.pt_outer_type(type_def);
          let pt_internal_variant = name_ctx.pt_inner_type(type_def, false);
          writeln!(mod_pt, "    {pt_internal_variant}(Box<{pt_variant}>),");
        }
        writeln!(mod_pt, "}}");
      }
      RuleValue::Function(params, body) => {
        // PT declaration:
        // if it is union, it's special, we can skip that
        // function:
        // All PTs have ast

        // implementation:
        // first parse all relavant members
        // then how to construct the return value?

        // seemes like RetType can be directly computed from Expr

        // Bind parameters to their types and the AST member names
        // This is needed because body expression can refer to the parameters in any order
        //let mut bind_ast_vars = HashMap::new();
        let var_to_member = params
          .iter()
          .enumerate()
          .map(|(i, p)| {
            let member_name = if p.is_in_pt() {
              name_ctx.member_name(&p.name)
            } else {
              name_ctx.member_name_with_idx(&p.name, i)
            };
            (p.name.clone(), member_name)
          })
          .collect::<HashMap<_, _>>();
        // bind param types
        let mut param_types = HashMap::new();
        for (i, param) in params.iter().enumerate() {
          if let Some(t) = param.get_type() {
            param_types.insert(param.name.clone(), t);
          }
          // bind_ast_vars.insert(
          //     param.name.clone(),
          //     format!("m_{}_{i}", to_snake_case(&param.name)),
          // );
        }
        let rt_type = ret_type_map.get(&rule.name).unwrap();

        writeln!(mod_pt, "#[derive(Debug)] pub struct {pt_t}<'p> {{");
        // ast reference
        writeln!(mod_pt, "    pub ast: &'p {ast_ns_t},");
        let vars = body.get_vars();

        // if return type is a vector, also store the ast references of the vector items
        // we have to first check VecType. if it is a vec (path A), we need to declare asts and vals
        if let RetType::Vec(item) = rt_type {
          let (asts_type, vals_type) = name_ctx.pt_vec_type(item.as_ref());
          writeln!(mod_pt, "    pub asts: {asts_type},");
          writeln!(mod_pt, "    pub vals: {vals_type},");
        } else {
          // check body
          // Otherwise, we need to check RetType.
          // If it is a struct, we need to declare m_x, m_y, ... (path B)
          // otherwise only need to declare m_x (path C)
          for var in &vars {
            let var_type = param_types.get(var).unwrap();
            let member = var_to_member.get(var).unwrap();
            let var_type_str = name_ctx.pt_var_type(var_type);
            writeln!(mod_pt, "    pub {member}: {var_type_str},");
          }
        }

        writeln!(mod_pt, "}}");

        writeln!(mod_main, "impl<'p> {pt_ns_t}<'p> {{");
        writeln!(mod_main, "    fn {macro_func_name}(ast: &'p {ast_ns_t}, _si: &mut SemInfo, _errors: &mut Vec<Error>) -> Self {{");

        let vars = body.get_vars();
        for var in vars {
          let member = var_to_member.get(&var).unwrap();
          let var_type = param_types.get(&var).unwrap();
          match var_type {
            ParamType::Item(optional, name) => {
              let pt_ns_t = name_ctx.pt_inner_type(name, true);
              if *optional {
                writeln!(mod_main, "        let {member} = if let Some(v) = ast.{member}.as_ref() {{ Box::new(Some({pt_ns_t}::from_ast(v, _si, _errors))) }} else {{ Box::new(None) }};");
              } else {
                writeln!(mod_main, "        let {member} = Box::new({pt_ns_t}::from_ast(&ast.{member}.as_ref(), _si, _errors));");
              }
            }
            ParamType::String(optional) => {
              if *optional {
                writeln!(
                  mod_main,
                  "        let {member} = ast.{member}.as_ref().map(|t| t.value.clone());"
                );
              } else {
                writeln!(
                  mod_main,
                  "        let {member} = ast.{member}.value.clone();"
                );
              }
            }
            ParamType::Bool => {
              writeln!(mod_main, "        let {member} = !ast.{member}.is_none();");
            }
          }
        }
        // check body expr:
        // if concat (path A)
        // if dict (path B)
        // if var, can be either path A or path C, depending on VecType

        // scenarios:

        match &body {
          Expr::Concat(vars) => {
            // 4. return a concatenation VecType = Vec, RetType = Vec, Expr = Concat
            //    implementation: move the asts and vals over, push_front the rest
            let last = vars.last().unwrap();
            let last_type = param_types.get(last).unwrap();
            let last_member = var_to_member.get(last).unwrap();

            if last_type.is_optional() {
              writeln!(
                mod_main,
                "        let (mut asts, mut vals) = move_pt_vec_optional!({last_member});"
              );
            } else {
              writeln!(
                mod_main,
                "        let (mut asts, mut vals) = move_pt_vec!({last_member});"
              );
            }

            for var in vars.iter().rev().skip(1) {
              let var_type = param_types.get(var).unwrap();
              let member = var_to_member.get(var).unwrap();

              match var_type {
                ParamType::Item(optional, _) => {
                  writeln!(mod_main, "        vals.push_front(*{member});");
                  if *optional {
                    writeln!(mod_main, "        asts.push_front(ast.{member}.as_ref());");
                  } else {
                    writeln!(mod_main, "        asts.push_front(&ast.{member});");
                  }
                }
                ParamType::String(optional) => {
                  writeln!(mod_main, "        vals.push_front({member});");
                  if *optional {
                    writeln!(mod_main, "        asts.push_front(ast.{member}.as_ref());");
                  } else {
                    writeln!(mod_main, "        asts.push_front(&ast.{member});");
                  }
                }
                ParamType::Bool => {
                  writeln!(mod_main, "        vals.push_front({member});");
                  writeln!(mod_main, "        asts.push_front(ast.{member}.as_ref());");
                }
              }
            }
            writeln!(mod_main, "        Self {{ ast, vals, asts }}");
          }
          Expr::Var(var) => {
            // 1. return a single non-vec value VecType = Unit, RetType = Param, Expr = Var
            //    implementation: move the val over
            // 2. return a single vec value VecType = Vec, RetType = Param, Expr = Var
            //    implementation: move the asts and vals over
            let member = var_to_member.get(var).unwrap();
            match rt_type {
              RetType::Unit(_) => {
                writeln!(mod_main, "        Self {{ ast, {member} }}");
              }
              RetType::Vec(_) => {
                if param_types.get(var).unwrap().is_optional() {
                  writeln!(
                    mod_main,
                    "        let (asts, vals) = move_pt_vec_optional!({member});"
                  );
                } else {
                  writeln!(
                    mod_main,
                    "        let (asts, vals) = move_pt_vec!({member});"
                  );
                }
                writeln!(mod_main, "        Self {{ ast, vals, asts }}");
              }
              _ => unreachable!(),
            }
          }
          Expr::Dict(vars) => {
            // 3. return a struct  VecType = Unit, RetType = Struct, Expr = Dict
            //    implementation: create the struct and move the val over

            writeln!(mod_main, "        Self {{");
            writeln!(mod_main, "            ast,");
            for var in vars {
              let member = var_to_member.get(var).unwrap();
              writeln!(mod_main, "            {member},");
            }
            writeln!(mod_main, "        }}");
          }
        }
        writeln!(mod_main, "    }}");
        writeln!(mod_main, "}}");
        //type_lifetime = "<'p>";
      }
    }
    if let Some(hook) = &rule.hook {
      // import the hook
      //let module = &hook.module;
      let func = &hook.name;
      let pt_t = name_ctx.pt_outer_type(&rule.name);
      //println!("use {module}::{{{func}, {pt_type}}};");
      writeln!(mod_main, "impl<'p> {pt_ns_t}<'p> {{");
      //ype_lifetime = if type_lifetime.is_empty() { "" } else { "'p" };
      writeln!(mod_main, "    #[inline] fn from_ast(ast: &'p {ast_ns_t}, si: &mut SemInfo, err: &mut Vec<Error>) -> {pt_t} {{");
      writeln!(
        mod_main,
        "        let mut pt = Self::from_ast_internal(ast, si, err);"
      );
      writeln!(
        mod_main,
        "        ParseHook {{ val: {func}(&mut pt, si, err), pt }}"
      );
      writeln!(mod_main, "    }}");
      writeln!(mod_main, "}}");
    }
  }
}
