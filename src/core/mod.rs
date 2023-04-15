pub mod expr;
pub mod param;
pub mod rule;
pub mod hook;
pub mod token;
pub mod semantic;
pub mod lang;

use rule::{ParamType, RetType, RuleValue, Rule, rule};
use param::{param, Param};
use hook::{hook, Hook};
use expr::{expr, Expr};

use std::collections::{HashMap, HashSet};



/// The value of a token
/// Escapes are allowed in the string
/// However, what's available depends on the target language.
enum TokenValue {
    Literal(String),
    RegExp(String),
}


/// Definition for a token
/// Syntax: extract? token <identifier>;
struct TokenDef {
    identifier: String,
    is_extract: bool,
}



struct TokenRule {
    token: Option<String>, // ignore if none
    value: TokenValue,
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


fn to_snake_case(s: &str) -> String {
    let mut result = String::new();
    for c in s.chars() {
        if c.is_uppercase() {
            result.push('_');
            result.push(c.to_ascii_lowercase());
        } else {
            result.push(c);
        }
    }
    result
}

fn to_camel_case(s: &str, upper: bool) -> String {
    let mut result = String::new();
    let mut capitalize_next = upper;
    for c in s.chars() {
        if c == '_' {
            capitalize_next = true;
        } else if capitalize_next {
            result.push(c.to_ascii_uppercase());
            capitalize_next = false;
        } else {
            result.push(c);
        }
    }
    result
}




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
                    expr!{hookAttr, ruleName, body}
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
                    expr!{hookName, hookType}
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
                    expr!(params , body)
                )
            ),
            rule!(
                "ParamList",
                RuleValue::Function(
                    vec![
                        param!("first": "Param"),
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
                        param!("first": "Param"),
                        param!("rest": optional "ParamListTail"),
                    ],
                    expr!(first | rest)
                )
            ),
            rule!(
                "Param",
                RuleValue::Function(
                    vec![
                        param!("semAttr": optional "ParamSemantic"),
                        param!(("Variable") "variable": token "Identifier"),
                        param!("_": token "Symbol" ":"),
                        param!("type": optional "RuleType"),
                    ],
                    expr!{semAttr, variable, type}
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
                   expr!{kwOptional, kwToken, id, tokenContent}
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
                    expr!{kwExtract, tokenType}
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
                    expr!{tokenType, value}
                ),
                hook!("parse_token_rule": "TokenRule")
            ),
            rule!(
                "LiteralOrRegExp",
                RuleValue::Union(vec!["TokenLiteral".to_string(), "TokenRegExp".to_string(),])
            ),
            rule!(
                "TokenLiteral",
                RuleValue::Function(vec![param!("t": token "Literal"),], Expr::Var("t".to_string()))
            ),
            rule!(
                "TokenRegExp",
                RuleValue::Function(vec![param!("t": token "RegExp"),], Expr::Var("t".to_string()))
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
        let rule_map = rules
            .into_iter()
            .map(|r| (r.name.clone(), r))
            .collect();
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
                TokenDef {
                    identifier: "Comment".to_string(),
                    is_extract: true,
                },
                TokenDef {
                    identifier: "Keyword".to_string(),
                    is_extract: false,
                },
                TokenDef {
                    identifier: "Identifier".to_string(),
                    is_extract: false,
                },
                TokenDef {
                    identifier: "RegExp".to_string(),
                    is_extract: false,
                },
                TokenDef {
                    identifier: "Literal".to_string(),
                    is_extract: false,
                },
                TokenDef {
                    identifier: "Symbol".to_string(),
                    is_extract: false,
                },
            ],
            token_rules: vec![
                TokenRule {
                    token: None,
                    value: TokenValue::RegExp(r"\s+".to_string()),
                },
                TokenRule {
                    token: Some("Comment".to_string()),
                    value: TokenValue::RegExp(r"\/\/[^\n]*\n?".to_string()),
                },
                TokenRule {
                    token: Some("Comment".to_string()),
                    value: TokenValue::RegExp(r"\/\*([^\*]|(\*[^\/]))*\*\/".to_string()),
                },
                TokenRule {
                    token: Some("Literal".to_string()),
                    value: TokenValue::RegExp(r#""((\\.)|[^\\"])*""#.to_string()),
                },
                TokenRule {
                    token: Some("RegExp".to_string()),
                    value: TokenValue::RegExp(r"\/((\\.)|[^\\\/])*\/".to_string()),
                },
                TokenRule {
                    token: Some("Keyword".to_string()),
                    value: TokenValue::Literal("ignore".to_string()),
                },
                TokenRule {
                    token: Some("Keyword".to_string()),
                    value: TokenValue::Literal("extract".to_string()),
                },
                TokenRule {
                    token: Some("Keyword".to_string()),
                    value: TokenValue::Literal("token".to_string()),
                },
                TokenRule {
                    token: Some("Keyword".to_string()),
                    value: TokenValue::Literal("semantic".to_string()),
                },
                TokenRule {
                    token: Some("Keyword".to_string()),
                    value: TokenValue::Literal("hook".to_string()),
                },
                TokenRule {
                    token: Some("Keyword".to_string()),
                    value: TokenValue::Literal("rule".to_string()),
                },
                TokenRule {
                    token: Some("Keyword".to_string()),
                    value: TokenValue::Literal("optional".to_string()),
                },
                TokenRule {
                    token: Some("Keyword".to_string()),
                    value: TokenValue::Literal("or".to_string()),
                },
                TokenRule {
                    token: Some("Keyword".to_string()),
                    value: TokenValue::Literal("null".to_string()),
                },
                TokenRule {
                    token: Some("Keyword".to_string()),
                    value: TokenValue::Literal("vigorous".to_string()),
                },
                TokenRule {
                    token: Some("Symbol".to_string()),
                    value: TokenValue::RegExp(r"[{};|()=,:\.\[\]]".to_string()),
                },
                TokenRule {
                    token: Some("Identifier".to_string()),
                    value: TokenValue::RegExp(r"[_a-zA-Z]\w*".to_string()),
                },
            ],
        }
    }

    pub fn emit_rust(&mut self) {
        let mut should_extract = HashSet::new();
        for token in &self.tokens {
            if token.is_extract {
                should_extract.insert(&token.identifier);
            }
        }
        // base SDK
        println!("{}", crate::sdk_generated::SDK_RS);
        println!("regen::sdk!(");
        let first_target = self.rules.get(&self.target).unwrap();
        println!("    target: ({}, {});", first_target.pt_internal_type(false), first_target.ast_type());

        println!("    tokens: [");
        for token in &self.tokens {
            println!("        T{},", &token.identifier);
        }
        println!("    ];");
        if self.token_rules.is_empty() {
            println!("    regex: [,];");
        } else {
            println!("    regex: [");
            for (i, rule) in self.token_rules.iter().enumerate() {
                if let TokenValue::RegExp(literal) = &rule.value {
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
            match &rule.token {
                None => match &rule.value {
                    TokenValue::Literal(literal) => {
                        println!("        [\"{literal}\", {}],", literal.len());
                    }
                    TokenValue::RegExp(_) => {
                        println!(
                            "        [re{i}],"
                        );
                    }
                },
                Some(token) => {
                    let should_extract = should_extract.contains(&token);
                    match &rule.value {
                        TokenValue::Literal(literal) => {
                            println!("        [{should_extract}, T{token}, \"{literal}\", {}],", literal.len());
                        }
                        TokenValue::RegExp(
                            _, /* don't need this as regex is already compiled */
                        ) => {
                            println!("        [{should_extract}, T{token}, re{i}],");
                        }
                    }
                }
            }
        }
        println!("    ];");
        

        
        println!("    semantics: [");
        // println!("    Token(Tokens),");
        // println!("    Tag(String, Box<Semantics>),");
        for semantic in &self.semantics {
            println!("        S{semantic},");
        }
        println!("    ];");
        println!(");");
        // =============================
        // Entry Point
        
        // =============================
        // Tokenizer

       

        // =============================
        // AST
        println!("// =============================");
        println!("// AST");
        let mut ast_type_map = HashMap::new();
        for (name, r) in &self.rules {
            ast_type_map.insert(r.name.clone(), r.ast_type());
        }
        

        // =============================
        // Parse Tree
        
        // resolve types
        let mut pt_type_map = HashMap::new();
        let mut pt_internal_type_map = HashMap::new();
        let mut ret_type_map = HashMap::new();
        
        for (_, r) in &self.rules {
            pt_type_map.insert(r.name.clone(), r.pt_type());
            pt_internal_type_map.insert(r.name.clone(), r.pt_internal_type(false));
        }
 
        
        for (name, r) in &self.rules {
            eprintln!("resolving {}", r.name);
            let ret_type = r.resolve_ret_type(&mut ret_type_map,&self.rules);
        }


        

        for (name, r) in &self.rules {
            println!("// {id}", id = &r.name);
            self.emit_rust_ast_rule(r, &ast_type_map);
            self.emit_rust_pt_rule(
                r,
                //&has_lifetime_map,
                &ast_type_map,
                &pt_type_map,
                &pt_internal_type_map,
                &ret_type_map
            );
        }
    }

    fn emit_rust_ast_rule(&self, rule: &Rule, ast_type_map: &HashMap<String, String>) {
        let id = &rule.name;

        
        match &rule.value {
            RuleValue::Union(type_defs) => {
                
            }
            RuleValue::Function(params, _unused_body) => {
                // Generating the AST does not need hook or the body

                // Generate struct type
                println!("#[derive(Debug)]");
                println!("pub struct {t} {{", t=ast_type_map.get(id).unwrap());
                for (i, param) in params.iter().enumerate() {
                    let member = format!("m_{}_{i}", to_snake_case(&param.name));
                    let type_name = &param.type_name;
                    if !param.is_token {
                        if param.is_optional {
                            println!("    pub {member}: Box<Option<{t}>>,", t=ast_type_map.get(type_name).unwrap());
                        } else {
                            println!("    pub {member}: Box<{t}>,", t=ast_type_map.get(type_name).unwrap());
                        }
                    } else {
                        if param.is_optional {
                            println!("    pub {member}: Option<Token<Tokens>>,");
                        } else {
                            println!("    pub {member}: Token<Tokens>,");
                        }
                    }
                }
                println!("}}");
                println!("impl {t} {{", t=ast_type_map.get(id).unwrap());
                // Generate parser func
                println!("    fn parse(ts: &mut TokenStream<Tokens>) -> Option<Self> {{");
                //println!("        if !ts.push() {{ return None; }}");
                for (i, param) in params.iter().enumerate() {
                    let member = format!("m_{}_{i}", to_snake_case(&param.name));
                    let type_name = &param.type_name;
                    if !param.is_token {
                        if param.is_optional {
                            println!("        let {member} = optional!(ts, {t}::parse(ts));", t=ast_type_map.get(type_name).unwrap());
                        } else {
                            println!(
                                "        let {member} = {t}::parse(ts)?;", t=ast_type_map.get(type_name).unwrap()
                            );
                        }
                    } else {
                        if param.is_optional {
                            match &param.match_literal {
                                None => {
                                    println!("        let {member} = optional!(ts, token!(T{type_name}::parse(ts)));");
                                }
                                Some(literal) => {
                                    println!("        let {member} = optional!(ts, token!(T{type_name}::\"{literal}\"(ts)));");
                                }
                            }
                        } else {
                            match &param.match_literal {
                                None => {
                                    println!("        let {member} = token!(T{type_name}::parse(ts))?;");
                                }
                                Some(literal) => {
                                    println!("        let {member} = token!(T{type_name}::\"{literal}\"(ts))?;");
                                }
                            }
                        }
                    }
                }
                //println!("        ts.pop();");
                println!("        Some(Self {{");
                for (i, param) in params.iter().enumerate() {
                    let member = format!("m_{}_{i}", to_snake_case(&param.name));
                    if !&param.is_token {
                        println!("            {member}: Box::new({member}),");
                    } else {
                        println!("            {member},");
                    }
                }
                println!("        }})");
                println!("    }}");
                println!("    fn apply_semantic(&self, _si: &mut SemInfo) {{");
                for (i, param) in params.iter().enumerate() {
                    let member = format!("m_{}_{i}", to_snake_case(&param.name));
                    if !param.is_token {
                        if param.is_optional {
                            println!("        if let Some(m) = self.{member}.as_ref() {{ m.apply_semantic(_si); }}");
                        } else {
                            println!("        self.{member}.apply_semantic(_si);");
                        }
                    } else {
                        if let Some(s) = &param.semantic {
                            if param.is_optional {
                                println!("        if let Some(m) = &self.{member} {{ _si.set(m, Semantics::S{s}); }}");
                            } else {
                                println!(
                                    "        _si.set(&self.{member}, Semantics::S{s});"
                                );
                            }
                        }
                    }
                }
                println!("    }}");
                println!("}}");
            }
        }
    }

    // fn resolve_non_struct_ret(&self, pvt: &RetType, ast_type_map: &HashMap<String, String>, pt_type_map: &HashMap<String, String>, pt_internal_type_map: &HashMap<String, String>) -> String {
        

        
    // }

    fn emit_rust_pt_rule(
        &self,
        rule: &Rule,
        ast_type_map: &HashMap<String, String>,
        pt_type_map: &HashMap<String, String>,
        pt_internal_type_map: &HashMap<String, String>,
        ret_type_map: &HashMap<String, RetType>,
    ) {
        let id = &rule.name;
        let ast_type = ast_type_map.get(id).unwrap();
        let pt_type = pt_type_map.get(id).unwrap();
        let pt_internal_type = pt_internal_type_map.get(id).unwrap();

        //let mut type_lifetime ;

        let macro_func_name = match &rule.hook {
            None => "from_ast",
            Some(_) => "from_ast_internal",
        };

        match &rule.value {
            RuleValue::Union(type_defs) => {
                // println!("regen::generate_ast_union!({t},", t=ast_type_map.get(id).unwrap());
                // for type_def in type_defs {
                //     println!("    {t},", t=ast_type_map.get(type_def).unwrap());
                // }
                // println!(");");
                // The union type might not have all PT variants if multiple hooks
                // returns the same type.
                let mut pt_union_variants = HashMap::new();
                
                println!("regen::impl_union!({macro_func_name},");
                println!("[{ast_type}, ");
                for type_def in type_defs {
                    println!("    {t},", t=ast_type_map.get(type_def).unwrap());
                }
                println!("], [{pt_internal_type}<'p>, ");
                //println!("regen::generate_union_impl!({macro_func_name}, {ast_type}, {pt_internal_type},");
                for type_def in type_defs {
                    let hooked_type = pt_type_map.get(type_def).unwrap();
                    let unhooked_type = pt_internal_type_map.get(type_def).unwrap();
                    println!(
                        "    {}, {},",
                        hooked_type,
                        unhooked_type,
                    );
                    pt_union_variants.insert(type_def, hooked_type != unhooked_type);
                }
                
                //println!(");");
                
                println!("],);");
                
                //let mut seen = HashSet::new();
                println!("#[derive(Debug)] pub enum {pt_internal_type}<'p> {{ ");
                for (type_def, is_hooked) in pt_union_variants {
                    let pt_variant = pt_type_map.get(type_def).unwrap();
                    let pt_internal_variant = pt_internal_type_map.get(type_def).unwrap();
                    let ast_type = ast_type_map.get(type_def).unwrap();
                    // if seen.contains(pt_variant) {
                    //     continue;
                    // }
                    // seen.insert(pt_variant);
                    if is_hooked {
                        println!("    {pt_internal_variant}(Box<{pt_variant}>),");

                    } else {

                        println!("    {pt_internal_variant}(Box<{pt_variant}>),");

                    }
                }
                println!("}}");

                
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
                let mut bind_ast_vars = HashMap::new();
                let mut param_types = HashMap::new();
                for (i, param) in params.iter().enumerate() {
                    if let Some(t) = param.get_type() {
                        param_types.insert(param.name.clone(), t);
                    }
                    bind_ast_vars.insert(param.name.clone(), format!("m_{}_{i}", to_snake_case(&param.name)));
                }
                let rt_type = ret_type_map.get(id).unwrap();
                println!("#[derive(Debug)] pub struct {pt_internal_type}<'p> {{");
                // ast reference
                println!("    pub ast: &'p {ast_type},");
                let vars = body.get_vars();

                // if return type is a vector, also store the ast references of the vector items
                // we have to first check VecType. if it is a vec (path A), we need to declare asts and vals
                if let RetType::Vec(item) = rt_type {
                    let mut asts_type = String::from("VecDeque<");
                    let mut vals_type = String::from("VecDeque<");
                    let mut nest_level = 1;
                    // Declare asts and vals
                    let mut next_item = item.as_ref();
                    loop {
                        match next_item {
                            RetType::Vec(item) => {
                                asts_type.push_str("VecDeque<");
                                vals_type.push_str("VecDeque<");
                                nest_level += 1;
                                next_item = item.as_ref();
                            },
                            RetType::Unit(item) => {
                                match item {
                                    ParamType::Item(optional, name) => {
                                        let ast_type = ast_type_map.get(name).unwrap();
                                        let pt_type = pt_type_map.get(name).unwrap();
                
                                        if *optional {
                                            asts_type.push_str(&format!("Option<&'p {ast_type}>"));
                                            vals_type.push_str(&format!("Option<{pt_type}>"));
                                        } else {
                                            asts_type.push_str(&format!("&'p {ast_type}"));
                                            vals_type.push_str(pt_type);
                                        }
                                    },
                                    ParamType::String(optional) => {
                                        if *optional {
                                            asts_type.push_str("Option<&'p Token<Tokens>>");
                                            vals_type.push_str("Option<String>");
                                        } else {
                                            asts_type.push_str("&'p Token<Tokens>");
                                            vals_type.push_str("String");
                                        }
                                    },
                                    ParamType::Bool => {
                                        asts_type.push_str("&'p Token<Tokens>");
                                        vals_type.push_str("bool");
                                    },
                                }
                                
                                break;
                            },
                            _ => unreachable!()
                        }
                    }

                    for _ in 0..nest_level {
                        asts_type.push('>');
                        vals_type.push('>');
                    }
                    println!("    pub asts: {asts_type},");
                    println!("    pub vals: {vals_type},");
                }else{
                    // check body
                    // Otherwise, we need to check RetType.
                    // If it is a struct, we need to declare m_x, m_y, ... (path B)
                    // otherwise only need to declare m_x (path C)
                    for var in &vars {
                        let var_type = param_types.get(var).unwrap();
                        let member = to_snake_case(var);
                        match var_type {
                            ParamType::Item(optional, name) => {
                                let pt_type = pt_type_map.get(name).unwrap();
                                if *optional {
                                    println!("    pub m_{member}: Box<Option<{pt_type}>>,");
                                } else {
                                    println!("    pub m_{member}: Box<{pt_type}>,");
                                }
                            },
                            ParamType::String(optional) => {
                                if *optional {
                                    println!("    pub m_{member}: Option<String>,");
                                } else {
                                    println!("    pub m_{member}: String,");
                                }
                            },
                            ParamType::Bool => {
                                println!("    pub m_{member}: bool,");
                            },
                        }
                    }
                    
                   
                }
                
                println!("}}");
   
                
                println!("impl<'p> {pt_internal_type}<'p> {{");
                println!("    fn {macro_func_name}(ast: &'p {ast_type}, _si: &mut SemInfo, _errors: &mut Vec<Error>) -> Self {{");

                

                let vars = body.get_vars();
                for var in vars {
                    let member = to_snake_case(&var);
                    let var_type = param_types.get(&var).unwrap();
                    let ast_var = bind_ast_vars.get(&var).unwrap();
                    //let mut_str = if i == vars.len() - 1 && last_mutable { "mut " } else { "" };
                    match var_type {
                        ParamType::Item(optional, name) => {
                            let pt_internal_type = pt_internal_type_map.get(name).unwrap();
                            if *optional {
                                println!("        let m_{member} = if let Some(v) = ast.{ast_var}.as_ref() {{ Box::new(Some({pt_internal_type}::from_ast(v, _si, _errors))) }} else {{ Box::new(None) }};");
                            } else {
                                println!("        let m_{member} = Box::new({pt_internal_type}::from_ast(&ast.{ast_var}.as_ref(), _si, _errors));");
                            }
                        },
                        ParamType::String(optional) => {
                            if *optional {
                                println!("        let m_{member} = ast.{ast_var}.as_ref().map(|t| t.value.clone());");
                            } else {
                                println!("        let m_{member} = ast.{ast_var}.value.clone();");
                            }
                        },
                        ParamType::Bool => {
                            println!("        let m_{member} = !ast.{ast_var}.is_none();");
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
                        let last_member = to_snake_case(&last);
                        let optional = if let ParamType::Item(optional, _) | ParamType::String(optional) = last_type {
                            *optional
                        } else {
                            false
                        };
                        
                        
                        if optional {
                            println!("        let (mut asts, mut vals) = move_pt_vec_optional!(m_{last_member});");

                        } else {
                            println!("        let (mut asts, mut vals) = move_pt_vec!(m_{last_member});");

                        }
                        //println!("        let mut asts = m_{last_member}.asts;");
                        //println!("        let (mut val, mut ast_vec) =");
                       // match last_type {
                        //     ParamType::Item(optional, name) => {
                        //         let mut ret_type = ret_type_map.get(name).unwrap();
                        //         let mut optional = *optional;
                        //         let mut nest = 0;
                        //         loop {
                                    

                        //             match ret_type {
                        //                 RetType::Vec(_) => {
                        //                     if optional {
                        //                         println!("            match *val {{ None => (VecDeque::new(), VecDeque::new()), Some(v) => (v.val, v.ast_vec) }}"); 
                        //                     } else {
                        //                         println!("            (val.val, val.ast_vec)");
                        //                     }
                        //                     println!("        {};", "}".repeat(nest));
                        //                     break;
                        //                 },
                        //                 RetType::Param(ParamType::Item(next_optional, next_name)) => {
                        //                     if optional {
                        //                         println!("            match *val {{ None => (VecDeque::new(), VecDeque::new()), Some(v) => {{ let val = v.val;");
                        //                         nest += 2; 
                        //                     } else {
                        //                         println!("            {{ let val = val.val;");
                        //                         nest += 1;
                        //                     }
                        //                     optional = *next_optional;
                        //                     ret_type = ret_type_map.get(next_name).unwrap();
                        //                 },
                        //                 _ => unreachable!()

                        //             }
                        //         }
                                
                        //     },
                        //     _ => unreachable!()
                        // }
                        
                        for var in vars.iter().rev().skip(1) {
                            let var_type = param_types.get(var).unwrap();
                            let member = to_snake_case(&var);
                            let ast_var = bind_ast_vars.get(var).unwrap();
                            
                            match var_type {
                                ParamType::Item(optional, _) => {
                                    println!("        vals.push_front(*m_{member});");
                                    if *optional {
                                        println!("        asts.push_front(ast.{ast_var}.as_ref());");
                                    } else {
                                        println!("        asts.push_front(&ast.{ast_var});");
                                    }
                                },
                                ParamType::String(optional) => {
                                    println!("        vals.push_front(m_{member});");
                                    if *optional {
                                        println!("        asts.push_front(ast.{ast_var}.as_ref());");
                                    } else {
                                        println!("        asts.push_front(&ast.{ast_var});");
                                    }
                                },
                                ParamType::Bool => {
                                    println!("        vals.push_front(m_{member});");
                                    println!("        asts.push_front(ast.{ast_var}.as_ref());");
                                }
                                
                            }
                        }
                        println!("        Self {{ ast, vals, asts }}");
                    },
                    Expr::Var(var) => {
                        // 1. return a single non-vec value VecType = Unit, RetType = Param, Expr = Var
//    implementation: move the val over
// 2. return a single vec value VecType = Vec, RetType = Param, Expr = Var
//    implementation: move the asts and vals over
                        let member = to_snake_case(&var);
                        match rt_type {
                            RetType::Unit(_) => {
                                println!("        Self {{ ast, m_{member} }}");
                            },
                            RetType::Vec(_) => {
                                let var_type = param_types.get(var).unwrap();
                                let optional = if let ParamType::Item(optional, _) | ParamType::String(optional) = var_type {
                                    *optional
                                } else {
                                    false
                                };
                                
                                
                                if optional {
                                    println!("        let (asts, vals) = move_pt_vec_optional!(m_{member});");

                                } else {
                                    println!("        let (asts, vals) = move_pt_vec!(m_{member});");

                                }
                                println!("        Self {{ ast, vals, asts }}");
                            },
                            _ => unreachable!()
                        }
                        //println!("        Self {{ ast, val: m_{member} }}");
                    },
                    Expr::Dict(vars) => {
                        // 3. return a struct  VecType = Unit, RetType = Struct, Expr = Dict
//    implementation: create the struct and move the val over

                        println!("        Self {{");
                        println!("            ast,");
                        for var in vars {
                            let member = to_snake_case(&var);
                            println!("            m_{member},");
                        }
                        println!("        }}");

                    },
                }
                println!("    }}");
                println!("}}");
                //type_lifetime = "<'p>";
            }
            
        }
        if let Some(hook) = &rule.hook {
            // import the hook
            //let module = &hook.module;
            let func = &hook.name;
            //println!("use {module}::{{{func}, {pt_type}}};");
            println!("impl<'p> {pt_internal_type}<'p> {{");
            //ype_lifetime = if type_lifetime.is_empty() { "" } else { "'p" };
            println!("    #[inline] fn from_ast(ast: &'p {ast_type}, si: &mut SemInfo, err: &mut Vec<Error>) -> {pt_type} {{");
            println!("        let mut pt = Self::from_ast_internal(ast, si, err);");
            println!("        ParseHook {{ val: {func}(&mut pt, si, err), pt }}");
            println!("    }}");
            println!("}}");
        }
    }
}
