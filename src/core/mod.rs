pub mod expr;
pub mod param;
pub mod rule;
pub mod hook;
pub mod token;
pub mod semantic;
pub mod lang;
pub mod eval;

use rule::{ParamType, RetType};
use param::{param, Param};

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





enum Expr {
    Concat(Vec<String>),
    Var(String /* identifier */),
    Dict(Vec<String>),
}

impl Expr {
    fn get_vars(&self) -> Vec<String> {
        match self {
            Expr::Concat(vars) => vars.clone(),
            Expr::Var(identifier) => vec![identifier.clone()],
            Expr::Dict(vars) => vars.clone()
        }
    }
}

enum RuleValue {
    Union(Vec<String>),
    Function(Vec<Param>, Expr),
}

struct HookDef {
    name: String,
    return_type: String,
}

macro_rules! hookdef {
    ($name:ident $type:literal) => {
        HookDef {
            name: stringify!($name).to_string(),
            return_type: $type.to_string(),
        }
    };
}

struct RuleDef {
    identifier: String,
    hook: Option<HookDef>,
    value: RuleValue,
}

impl RuleDef {
    /// Internal AST type name
    fn ast_type(&self) -> String {
        format!("AST{}", to_camel_case(&self.identifier, true))
    }
    /// External Parse Tree node type.
    /// If the rule has a hook, this will be the return type of the hook
    fn pt_type(&self) -> String {
        match &self.hook {
            Some(hook) => format!("ParseHook<{}, {}>", hook.return_type, &self.pt_internal_type(true)),
            None =>self.pt_internal_type(true)
        }
    }
    /// The internal Parse Tree node type.
    /// If the rule has a hook, this will be the type passed to the hook
    fn pt_internal_type(&self, include_lifetime: bool) -> String {
        let mut t = format!("PT{}", to_camel_case(&self.identifier, true));
        if include_lifetime {
            t.push_str("<'p>")
        }
        t
    }
    /// Recursively resolve the return type of a rule
    fn resolve_ret_type(&self, ret_types: &mut HashMap<String, RetType>, rules: &HashMap<String, &RuleDef>) {
        if ret_types.contains_key(&self.identifier) {
            // Already resolved or is resolving
            return;
        }
        
        match &self.value {
            RuleValue::Union(_) => {
                ret_types.insert(self.identifier.clone(), RetType::Struct);
            }
            RuleValue::Function(params, body) => {
                let mut param_types = HashMap::new();
                for param in params {
                    if let Some(t) = param.get_type() {
                        param_types.insert(param.name.clone(), t);
                    }
                }
                match body {
                    Expr::Concat(vars) => {
                        // First update the return type to be a vector of first variable
                        let first_var = vars.first().unwrap();
                        let first_var_type = param_types.get(first_var).unwrap();
                        let self_type = RetType::Vec(first_var_type.clone());
                        ret_types.insert(self.identifier.clone(), self_type.clone());
                        // If the first variable is a subrule, resolve it
                        if let ParamType::Item(_, t) = first_var_type {
                            // It cannot be recursive, otherwise we would have vectors of infinite depth
                            if t == &self.identifier {
                                ret_types.insert(
                                    self.identifier.clone(),
                                    RetType::Unresolved(format!("Cannot resolve return type of {id}: {id} is recursive as the first value in concatenation", id = &self.identifier))
                                );
                                return;
                            }
                            let rule = rules.get(t).unwrap();
                            rule.resolve_ret_type(ret_types, rules);
                        }
                        

                        // make sure the last is a vector of the same type
                        let last_var = vars.last().unwrap();
                        let last_var_type = param_types.get(last_var).unwrap();
                        if let ParamType::Item(_, t) = last_var_type {
                            let rule = rules.get(t).unwrap();
                            rule.resolve_ret_type(ret_types, rules);
                            match ret_types.get(t).unwrap() {
                                RetType::Vec(t) => {
                                    if t != first_var_type {
                                        
                                        ret_types.insert(
                                            self.identifier.clone(),
                                            RetType::Unresolved(format!("Cannot resolve return type of {id}: Last in concate expression must be a vector of the same type as the rest of the items", id = &self.identifier))
                                        );
                                        return;
                                    }
                                }
                                RetType::Param(ParamType::Item(_, t)) => {
                                    let last_type = ret_types.get(t).unwrap();
                                    if last_type != &self_type {
                                        ret_types.insert(
                                            self.identifier.clone(),
                                            RetType::Unresolved(format!("Cannot resolve return type of {id}: {t} does not have a vector return type", id = &self.identifier))
                                        );
                                        return;
                                    }
                                    
                                }
                                _ => {
                                    
                                    ret_types.insert(
                                        self.identifier.clone(),
                                        RetType::Unresolved(format!("Cannot resolve return type of {id}: Last in concate expression must be a vector", id = &self.identifier))
                                    );
                                    return;
                                }
                            }
                        } else {
                            // Only subrules can be a vector, so last is not a vector, fail.
                            ret_types.insert(
                                self.identifier.clone(),
                                RetType::Unresolved(format!("Cannot resolve return type of {id}: Last in concate expression must be a vector", id = &self.identifier))
                            );
                            return;
                        }
                        
                        // Make sure the middle are the same as the first
                        for var in vars.iter().skip(1).take(vars.len() - 2) {
                            let var_type = param_types.get(var).unwrap();
                            if var_type != first_var_type {
                                ret_types.insert(
                                    self.identifier.clone(),
                                    RetType::Unresolved(format!("Cannot resolve return type of {id}: All except last items in concate expression must be the same type", id = &self.identifier))
                                );
                                return;
                            }
                        }
                    }
                    Expr::Var(name) => {
                        let param_type = param_types.get(name).unwrap();
                        let self_type = RetType::Param(param_type.clone());
                        ret_types.insert(self.identifier.clone(), self_type);
                    },
                    Expr::Dict(vars) => {
                        ret_types.insert(self.identifier.clone(), RetType::Struct);
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

        }
    }
}

/// The main language definition
pub struct LangDef {
    tokens: Vec<TokenDef>,
    token_rules: Vec<TokenRule>,
    semantics: Vec<String>,
    rules: Vec<RuleDef>,
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


macro_rules! ruledef {
    ($identifier:literal, $value:expr) => {
        RuleDef {
            identifier: $identifier.to_string(),
            hook: None,
            value: $value,
        }
    };
    ($identifier:literal, $value:expr, $hook:expr) => {
        RuleDef {
            identifier: $identifier.to_string(),
            hook: Some($hook),
            value: $value,
        }
    };
}

macro_rules! expr {
    ($( $t:literal)|* ) => {
        Expr::Concat(vec![ $( $t.to_string() ),*])
    };
    ($( $t:literal),* ) => {
        Expr::Dict(vec![ $( $t.to_string() ),*])
    };

}

impl LangDef {
    pub fn make_test() -> Self {
        LangDef {
            rules: vec![
                // ruledef!(
                //     "Target",
                //     RuleValue::Function(
                //         vec![
                //             param!("first": "TopLevelStatement"),
                //             param!("rest": optional "Target"),
                //         ],
                //         expr!["first" | "rest"]
                //     )
                // ),
                ruledef!(
                    "TopLevelStatement",
                    RuleValue::Union(vec![
                        "DefineRuleStatement".to_string(),
                        "TopLevelDefineStatement".to_string(),
                    ])
                ),
                ruledef!(
                    "DefineRuleStatement",
                    RuleValue::Function(
                        vec![
                            param!("_": token "Keyword" "rule"),
                            param!("hookAttr": optional "HookAttribute"),
                            param!(("Rule") "ruleName": token "Identifier"),
                            param!("body": "RuleDefineBody"),
                        ],
                        expr!("hookAttr", "ruleName", "body")
                    ),
                    hookdef!(parse_rule "Rule")
                ),
                ruledef!(
                    "HookAttribute",
                    RuleValue::Function(
                        vec![
                            param!("_": token "Symbol" "("),
                            param!(("HookName") "hookName": token "Literal"),
                            param!("_": token "Symbol" ":"),
                            param!(("HookType") "hookType": token "Literal"),
                            param!("_": token "Symbol" ")"),
                        ],
                        expr!("hookName", "hookType")
                    ),
                    hookdef!(parse_hook "Hook")

                ),
                ruledef!(
                    "RuleDefineBody",
                    RuleValue::Union(vec![
                        "UnionRuleBody".to_string(),
                        "FunctionalRuleBody".to_string(),
                    ]),
                    hookdef!(parse_rule_value "RuleValue")

                ),
                ruledef!(
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
                ruledef!(
                    "UnionRuleList",
                    RuleValue::Function(
                        vec![
                            param!(("Rule") "first": token "Identifier"),
                            param!("rest": optional "UnionRuleListTail"),
                        ],
                        expr!("first" | "rest")
                    )
                ),
                ruledef!(
                    "UnionRuleListTail",
                    RuleValue::Function(
                        vec![
                            param!("_": token "Symbol" "|"),
                            param!(("Rule") "first": token "Identifier"),
                            param!("rest": optional "UnionRuleListTail"),
                        ],
                        expr!("first" | "rest")
                    )
                ),
                ruledef!(
                    "FunctionalRuleBody",
                    RuleValue::Function(
                        vec![
                            param!("_": token "Symbol" "("),
                            param!("params": optional "ParamList"),
                            param!("_": token "Symbol" ")"),
                            param!("body": optional "Expression"),
                            param!("_": token "Symbol" ";"),
                        ],
                        expr!("params" , "body")
                    )
                ),
                ruledef!(
                    "ParamList",
                    RuleValue::Function(
                        vec![
                            param!("first": "Param"),
                            param!("rest": optional "ParamListTail"),
                        ],
                        expr!("first" | "rest")
                    ),
                    hookdef!(parse_param_list "Vec<Param>")
                ),
                ruledef!(
                    "ParamListTail",
                    RuleValue::Function(
                        vec![
                            param!("_": token "Symbol" ","),
                            param!("first": "Param"),
                            param!("rest": optional "ParamListTail"),
                        ],
                        expr!("first" | "rest")
                    )
                ),
                ruledef!(
                    "Param",
                    RuleValue::Function(
                        vec![
                            param!("semAttr": optional "ParamSemantic"),
                            param!(("Variable") "variable": token "Identifier"),
                            param!("_": token "Symbol" ":"),
                            param!("type": optional "RuleType"),
                        ],
                        expr!("semAttr", "variable", "type")
                    ),
                    hookdef!(parse_param "Param")
                ),
                ruledef!(
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
                ruledef!(
                    "RuleType",
                    RuleValue::Function(
                        vec![
                            param!("kwOptional": optional token "Keyword" "optional"),
                            param!("kwToken": optional token "Keyword" "token"),
                            param!("id": token "Identifier"),
                            param!("tokenContent": optional token "Literal"),
                        ],
                       expr!("kwOptional", "kwToken", "id", "tokenContent")
                    )
                ),
                ruledef!(
                    "TopLevelDefineStatement",
                    RuleValue::Function(
                        vec![
                            param!("body": "TopLevelDefine"),
                            param!("_": token "Symbol" ";"),
                        ],
                        Expr::Var("body".to_string())
                    )
                ),
                ruledef!(
                    "TopLevelDefine",
                    RuleValue::Union(vec![
                        "TokenLiteral".to_string(),
                        "DefineTokenTypeStatement".to_string(),
                        "DefineIgnoreTokenRuleStatement".to_string(),
                        "DefineTokenRuleStatement".to_string(),
                        "DefineSemanticStatement".to_string(),
                    ])
                ),
                ruledef!(
                    "DefineTokenTypeStatement",
                    RuleValue::Function(
                        vec![
                            param!("kwExtract": optional token "Keyword" "extract"),
                            param!("_": token "Keyword" "token"),
                            param!(("Token") "tokenType": token "Identifier"),
                        ],
                        expr!("kwExtract", "tokenType")
                    ),
                    hookdef!(parse_token_def "TokenDef")
                ),
                ruledef!(
                    "DefineIgnoreTokenRuleStatement",
                    RuleValue::Function(
                        vec![
                            param!("_": token "Keyword" "ignore"),
                            param!("value": "LiteralOrRegExp"),
                        ],
                        Expr::Var("value".to_string())
                    ),
                    hookdef!(parse_token_ignore_rule "TokenRule")
                ),
                ruledef!(
                    "DefineTokenRuleStatement",
                    RuleValue::Function(
                        vec![
                            param!(("Token") "tokenType": token "Identifier"),
                            param!("value": "LiteralOrRegExp"),
                        ],
                        expr!("tokenType", "value")
                    ),
                    hookdef!(parse_token_rule "TokenRule")
                ),
                ruledef!(
                    "LiteralOrRegExp",
                    RuleValue::Union(vec!["TokenLiteral".to_string(), "TokenRegExp".to_string(),])
                ),
                ruledef!(
                    "TokenLiteral",
                    RuleValue::Function(vec![param!("t": token "Literal"),], Expr::Var("t".to_string()))
                ),
                ruledef!(
                    "TokenRegExp",
                    RuleValue::Function(vec![param!("t": token "RegExp"),], Expr::Var("t".to_string()))
                ),
                ruledef!(
                    "DefineSemanticStatement",
                    RuleValue::Function(
                        vec![
                            param!("_": token "Keyword" "semantic"),
                            param!(("Semantic") "id": token "Identifier"),
                        ],
                        Expr::Var("id".to_string())
                    ),
                    hookdef!(parse_semantic "String")

                ),
                ruledef!(
                    "Expression",
                    RuleValue::Union(vec![
                        "ConcatExpression".to_string(),
                        "DictExpression".to_string(),
                    ]),
                    hookdef!(parse_expr "Expr")
                ),
                ruledef!(
                    "ConcatExpression",
                    RuleValue::Function(
                        vec![
                            param!(("Variable") "first": token "Identifier"),
                            param!("rest": optional "ConcatExpressionTail"),
                        ],
                        expr!("first" | "rest")
                    )
                ),
                ruledef!(
                    "ConcatExpressionTail",
                    RuleValue::Function(
                        vec![
                            param!("_": token "Symbol" "|"),
                            param!("rest": "ConcatExpression"),
                        ],
                        Expr::Var("rest".to_string())
                    )
                ),
                ruledef!(
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
                ruledef!(
                    "VariableList",
                    RuleValue::Function(
                        vec![
                            param!(("Variable") "first": token "Identifier"),
                            param!("rest": optional "VariableListTail"),
                        ],
                        expr!("first" | "rest")
                    )
                ),
                ruledef!(
                    "VariableListTail",
                    RuleValue::Function(
                        vec![
                            param!("_": token "Symbol" ","),
                            param!("rest": optional "VariableList"),
                        ],
                        Expr::Var("rest".to_string())
                    )
                ),
            ],
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

    pub fn emit_rust(&self) {
        let mut should_extract = HashSet::new();
        for token in &self.tokens {
            if token.is_extract {
                should_extract.insert(&token.identifier);
            }
        }
        // base SDK
        println!("{}", crate::sdk_generated::SDK_RS);
        println!("regen::generate_base!();");
        println!("regen::generate_token_sdk!(");
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
        

        println!(");");
        println!("regen::generate_semantic_sdk![");
        // println!("    Token(Tokens),");
        // println!("    Tag(String, Box<Semantics>),");
        for semantic in &self.semantics {
            println!("    S{semantic},");
        }
        println!("];");

        // =============================
        // Entry Point
        let first_target = &self.rules.first().unwrap();
        println!("regen::generate_api!({}, {}, tokenize_internal);", first_target.pt_internal_type(false), first_target.ast_type());

        // =============================
        // Tokenizer

       

        // =============================
        // AST
        println!("// =============================");
        println!("// AST");
        let mut ast_type_map = HashMap::new();
        for r in &self.rules {
            ast_type_map.insert(r.identifier.clone(), r.ast_type());
        }
        

        // =============================
        // Parse Tree
        
        // resolve types
        let mut pt_type_map = HashMap::new();
        let mut pt_internal_type_map = HashMap::new();
        let mut ret_type_map = HashMap::new();
        let mut rule_map = HashMap::new();
        
        for r in &self.rules {
            pt_type_map.insert(r.identifier.clone(), r.pt_type());
            pt_internal_type_map.insert(r.identifier.clone(), r.pt_internal_type(false));
            rule_map.insert(r.identifier.clone(), r);
        }
        // import hooks
        // let mut import_map: HashMap<&String, HashSet<&String>> = HashMap::new();
        // for r in &self.rules {
        //     if let Some(hook) = &r.hook {
        //         let module = &hook.module;
        //         let pt_type = &hook.return_type;
        //         let func = &hook.name;
        //         match import_map.get_mut(module) {
        //             Some(set) => {
        //                 set.insert(pt_type);
        //                 set.insert(func);
        //             }
        //             None => {
        //                 let mut set = HashSet::new();
        //                 set.insert(pt_type);
        //                 set.insert(func);
        //                 import_map.insert(module, set);
        //             }
        //         }
        //     }
        // }
        // for (module, symbols) in import_map {
        //     println!("use {module}::{{");
        //     for s in symbols {
        //         println!("    {s},");
        //     }
        //     println!("}};");
        // }
        
        for r in &self.rules {
            eprintln!("resolving {}", r.identifier);
            r.resolve_ret_type(&mut ret_type_map, &rule_map);
        }

        // let mut has_lifetime_map = HashMap::new();
        
        // for r in &self.rules {
        //     match &r.value {
        //         RuleValue::Union(type_defs) => {
        //             let mut has_non_hook = false;
        //             for type_def in type_defs {
        //                 let hooked_type = pt_type_map.get(type_def).unwrap();
        //                 let unhooked_type = pt_internal_type_map.get(type_def).unwrap();
        //                 // println!(
        //                 //     "    {}, {},",
        //                 //     hooked_type,
        //                 //     unhooked_type,
        //                 // );
        //                 //pt_union_variants.insert(hooked_type, hooked_type != unhooked_type);
        //                 if hooked_type == unhooked_type {
        //                     has_non_hook = true;
        //                     break;
        //                 }
        //             }
        //             has_lifetime_map.insert(r.identifier.clone(), true)
        //         },
        //         _ => has_lifetime_map.insert(r.identifier.clone(), true)
        //     };
            
        // }
        

        for r in &self.rules {
            println!("// {id}", id = &r.identifier);
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

    fn emit_rust_ast_rule(&self, rule: &RuleDef, ast_type_map: &HashMap<String, String>) {
        let id = &rule.identifier;

        
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
                            println!("    pub {member}: Option<Token>,");
                        } else {
                            println!("    pub {member}: Token,");
                        }
                    }
                }
                println!("}}");
                println!("impl {t} {{", t=ast_type_map.get(id).unwrap());
                // Generate parser func
                println!("    fn parse(ts: &mut TokenStream) -> Option<Self> {{");
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

    fn resolve_non_struct_ret(&self, pvt: &RetType, ast_type_map: &HashMap<String, String>, pt_type_map: &HashMap<String, String>, pt_internal_type_map: &HashMap<String, String>) -> String {
        match pvt {
            RetType::Unresolved(err) => {
                panic!("Unresolved return type: {err}");
            },
            RetType::Struct => {
                panic!("Don't call this function for struct return type")
            },
            RetType::Param(p) => {
                match p {
                    ParamType::Item(optional, name) => {
                        let pt_type = pt_type_map.get(name).unwrap();
                        //let lifetime = if *has_lifetime_map.get(name).unwrap() { "<'p>" } else {""};

                        if *optional {
                            format!("Box<Option<{pt_type}>>")
                        } else {
                            format!("Box<{pt_type}>")
                        }
                    },
                    ParamType::String(optional) => {
                        if *optional {
                            "Option<String>".to_string()
                        } else {
                            "String".to_string()
                        }
                    },
                    ParamType::Bool => {
                        "bool".to_string()
                    },
                }
            }
            RetType::Vec(p) => {
                match p {
                    ParamType::Item(optional, name) => {
                        let pt_type = pt_type_map.get(name).unwrap();
                        //let ast_type = ast_type_map.get(name).unwrap();
                       // let lifetime = if pt_type == pt_internal_type_map.get(name).unwrap() { "<'p>" } else {""};

                        if *optional {
                            format!("VecDeque<Option<{pt_type}>>")
                        } else {
                            format!("VecDeque<{pt_type}>")
                        }
                    },
                    ParamType::String(optional) => {
                        if *optional {
                            "VecDeque<Option<String>>".to_string()
                        } else {
                            "VecDeque<String>".to_string()
                        }
                    },
                    ParamType::Bool => {
                        "VecDeque<bool>".to_string()
                    },
                }
 
            },

        }
    }

    fn emit_rust_pt_rule(
        &self,
        rule: &RuleDef,
        ast_type_map: &HashMap<String, String>,
        pt_type_map: &HashMap<String, String>,
        pt_internal_type_map: &HashMap<String, String>,
        ret_type_map: &HashMap<String, RetType>,
    ) {
        let id = &rule.identifier;
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
                
                println!("regen::generate_union_impl!({macro_func_name},");
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
                // if return type is a vector, also store the ast references of the vector items
                if let RetType::Vec(item) = rt_type {
                    match item {
                        ParamType::Item(optional, name) => {
                            let ast_type = ast_type_map.get(name).unwrap();
    
                            if *optional {
                                println!("    pub ast_vec: VecDeque<Option<&'p {ast_type}>>,");
                            } else {
                                println!("    pub ast_vec: VecDeque<&'p {ast_type}>,");
                            }
                        },
                        ParamType::String(optional) => {
                            if *optional {
                                println!("    pub ast_vec: VecDeque<Option<&'p Token>>,");
                            } else {
                                println!("    pub ast_vec: VecDeque<&'p Token>,");
                            }
                        },
                        ParamType::Bool => {
                            println!("    pub ast_vec: VecDeque<&'p Token>,");
                        },
                    }
                    
                }
                match rt_type {
                    RetType::Struct => {
                        match body {
                            Expr::Dict(vars) => {
                                //println!("#[derive(Debug)] pub struct Dict{pt_internal_type_map} {{");
                                for var in vars {
                                    let var_type = param_types.get(var).unwrap();
                                    let member = to_snake_case(var);
                                    match var_type {
                                        ParamType::Item(optional, name) => {
                                            let pt_type = pt_type_map.get(name).unwrap();
                                            //let lifetime = if pt_type == pt_internal_type_map.get(name).unwrap() { "<'p>" } else {""};
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
                                //p//rintln!("}}");
                            },
                            _ => {
                                panic!("Struct return type must have dict body");
                            }
                        }
                    }
                    _ => println!("    pub val: {t}", t=self.resolve_non_struct_ret(rt_type, ast_type_map, pt_type_map, pt_internal_type_map)),
                }
                println!("}}");
                // if let RetType::Struct = rt_type {
                //     match body {
                //         Expr::Dict(vars) => {
                //             //println!("#[derive(Debug)] pub struct Dict{pt_internal_type_map} {{");
                //             for var in vars {
                //                 let var_type = param_types.get(var).unwrap();
                //                 let member = to_snake_case(var);
                //                 match var_type {
                //                     ParamType::Item(optional, name) => {
                //                         let pt_type = pt_internal_type_map.get(name).unwrap();
                //                         let lifetime = if pt_type == pt_internal_type_map.get(name).unwrap() { "<'p>" } else {""};
                //                         if *optional {
                //                             println!("    pub m_{member}: Box<Option<{pt_type}{lifetime}>>,");
                //                         } else {
                //                             println!("    pub m_{member}: Box<{pt_type}{lifetime}>,");
                //                         }
                //                     },
                //                     ParamType::String(optional) => {
                //                         if *optional {
                //                             println!("    pub m_{member}: Option<String>,");
                //                         } else {
                //                             println!("    pub m_{member}: String,");
                //                         }
                //                     },
                //                     ParamType::Bool => {
                //                         println!("    pub m_{member}: bool,");
                //                     },
                //                 }
                //             }
                //             //p//rintln!("}}");
                //         },
                //         _ => unreachable!()
                //     }
                // }   
                
                println!("impl<'p> {pt_internal_type}<'p> {{");
                println!("    fn {macro_func_name}(ast: &'p {ast_type}, _si: &mut SemInfo, _errors: &mut Vec<RegenError>) -> Self {{");

                //let last_mutable = if let Expr::Concat(_) = &body { true } else { false };

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
                match &body {
                    Expr::Concat(vars) => {
                        let last = vars.last().unwrap();
                        let last_type = param_types.get(last).unwrap();
                        let last_member = to_snake_case(&last);
                        println!("        let val = m_{last_member};");
                        println!("        let (mut val, mut ast_vec) =");
                        match last_type {
                            ParamType::Item(optional, name) => {
                                let mut ret_type = ret_type_map.get(name).unwrap();
                                let mut optional = *optional;
                                let mut nest = 0;
                                loop {
                                    

                                    match ret_type {
                                        RetType::Vec(_) => {
                                            if optional {
                                                println!("            match *val {{ None => (VecDeque::new(), VecDeque::new()), Some(v) => (v.val, v.ast_vec) }}"); 
                                            } else {
                                                println!("            (val.val, val.ast_vec)");
                                            }
                                            println!("        {};", "}".repeat(nest));
                                            break;
                                        },
                                        RetType::Param(ParamType::Item(next_optional, next_name)) => {
                                            if optional {
                                                println!("            match *val {{ None => (VecDeque::new(), VecDeque::new()), Some(v) => {{ let val = v.val;");
                                                nest += 2; 
                                            } else {
                                                println!("            {{ let val = val.val;");
                                                nest += 1;
                                            }
                                            optional = *next_optional;
                                            ret_type = ret_type_map.get(next_name).unwrap();
                                        },
                                        _ => unreachable!()

                                    }
                                }
                                
                            },
                            _ => unreachable!()
                        }
                        
                        for var in vars.iter().rev().skip(1) {
                            let var_type = param_types.get(var).unwrap();
                            let member = to_snake_case(&var);
                            let ast_var = bind_ast_vars.get(var).unwrap();
                            
                            match var_type {
                                ParamType::Item(optional, _) => {
                                    println!("        val.push_front(*m_{member});");
                                    if *optional {
                                        println!("        ast_vec.push_front(ast.{ast_var}.as_ref());");
                                    } else {
                                        println!("        ast_vec.push_front(&ast.{ast_var});");
                                    }
                                },
                                ParamType::String(optional) => {
                                    println!("        val.push_front(m_{member});");
                                    if *optional {
                                        println!("        ast_vec.push_front(ast.{ast_var}.as_ref());");
                                    } else {
                                        println!("        ast_vec.push_front(&ast.{ast_var});");
                                    }
                                },
                                ParamType::Bool => {
                                    println!("        val.push_front(m_{member});");
                                    println!("        ast_vec.push_front(ast.{ast_var}.as_ref());");
                                }
                                
                            }
                        }
                        println!("        Self {{ ast, val, ast_vec }}");
                    },
                    Expr::Var(var) => {
                        let member = to_snake_case(&var);
                        println!("        Self {{ ast, val: m_{member} }}");
                    },
                    Expr::Dict(vars) => {
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
            println!("    #[inline] fn from_ast(ast: &'p {ast_type}, si: &mut SemInfo, err: &mut Vec<RegenError>) -> {pt_type} {{");
            println!("        let mut pt = Self::from_ast_internal(ast, si, err);");
            println!("        ParseHook {{ val: {func}(&mut pt, si, err), pt }}");
            println!("    }}");
            println!("}}");
        }
    }
}
