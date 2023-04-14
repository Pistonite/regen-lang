
#![allow(non_snake_case)]
/*SDK_RS*/use crate as regen;
/*SDK_RS*/// =================================================================================================
/*SDK_RS*/
/*SDK_RS*/use crate::core::{
/*SDK_RS*/    rule::{parse_rule, Rule, parse_rule_value, RuleValue},
/*SDK_RS*/    expr::{parse_expr, Expr},
/*SDK_RS*/    param::{parse_param, Param, parse_param_list},
/*SDK_RS*/    hook::{parse_hook, Hook},
/*SDK_RS*/    semantic::parse_semantic,
/*SDK_RS*/    token::{parse_token_def, TokenDef, parse_token_ignore_rule, parse_token_rule, TokenRule}
/*SDK_RS*/};
regen::generate_base!();
regen::generate_token_sdk!(
    tokens: [
        TComment,
        TKeyword,
        TIdentifier,
        TRegExp,
        TLiteral,
        TSymbol,
    ];
    regex: [
        re0 = r"^\s+",
        re1 = r"^//[^\n]*\n?",
        re2 = r"^/\*([^\*]|(\*[^/]))*\*/",
        re3 = r#"^"((\\.)|[^\\"])*""#,
        re4 = r"^/((\\.)|[^\\/])*/",
        re15 = r"^[{};|()=,:\.\[\]]",
        re16 = r"^[_a-zA-Z]\w*",
    ];
    rules: [
        [re0],
        [true, TComment, re1],
        [true, TComment, re2],
        [false, TLiteral, re3],
        [false, TRegExp, re4],
        [false, TKeyword, "ignore", 6],
        [false, TKeyword, "extract", 7],
        [false, TKeyword, "token", 5],
        [false, TKeyword, "semantic", 8],
        [false, TKeyword, "hook", 4],
        [false, TKeyword, "rule", 4],
        [false, TKeyword, "optional", 8],
        [false, TKeyword, "or", 2],
        [false, TKeyword, "null", 4],
        [false, TKeyword, "vigorous", 8],
        [false, TSymbol, re15],
        [false, TIdentifier, re16],
    ];
);
regen::generate_semantic_sdk![
    SVariable,
    SToken,
    SSemantic,
    SRule,
    SHookName,
    SHookType,
];
regen::generate_api!(PTTopLevelStatement, ASTTopLevelStatement, tokenize_internal);
// =============================
// AST
// TopLevelStatement
regen::generate_union_impl!(from_ast,
[ASTTopLevelStatement, 
    ASTDefineRuleStatement,
    ASTTopLevelDefineStatement,
], [PTTopLevelStatement<'p>, 
    ParseHook<Rule, PTDefineRuleStatement<'p>>, PTDefineRuleStatement,
    PTTopLevelDefineStatement<'p>, PTTopLevelDefineStatement,
],);
#[derive(Debug)] pub enum PTTopLevelStatement<'p> { 
    PTTopLevelDefineStatement(Box<PTTopLevelDefineStatement<'p>>),
    PTDefineRuleStatement(Box<ParseHook<Rule, PTDefineRuleStatement<'p>>>),
}
// DefineRuleStatement
#[derive(Debug)]
pub struct ASTDefineRuleStatement {
    pub m___0: Token,
    pub m_hook_attr_1: Box<Option<ASTHookAttribute>>,
    pub m_rule_name_2: Token,
    pub m_body_3: Box<ASTRuleDefineBody>,
}
impl ASTDefineRuleStatement {
    fn parse(ts: &mut TokenStream) -> Option<Self> {
        let m___0 = token!(TKeyword::"rule"(ts))?;
        let m_hook_attr_1 = optional!(ts, ASTHookAttribute::parse(ts));
        let m_rule_name_2 = token!(TIdentifier::parse(ts))?;
        let m_body_3 = ASTRuleDefineBody::parse(ts)?;
        Some(Self {
            m___0,
            m_hook_attr_1: Box::new(m_hook_attr_1),
            m_rule_name_2,
            m_body_3: Box::new(m_body_3),
        })
    }
    fn apply_semantic(&self, _si: &mut SemInfo) {
        if let Some(m) = self.m_hook_attr_1.as_ref() { m.apply_semantic(_si); }
        _si.set(&self.m_rule_name_2, Semantics::SRule);
        self.m_body_3.apply_semantic(_si);
    }
}
#[derive(Debug)] pub struct PTDefineRuleStatement<'p> {
    pub ast: &'p ASTDefineRuleStatement,
    pub m_hook_attr: Box<Option<ParseHook<Hook, PTHookAttribute<'p>>>>,
    pub m_rule_name: String,
    pub m_body: Box<ParseHook<RuleValue, PTRuleDefineBody<'p>>>,
}
impl<'p> PTDefineRuleStatement<'p> {
    fn from_ast_internal(ast: &'p ASTDefineRuleStatement, _si: &mut SemInfo, _errors: &mut Vec<RegenError>) -> Self {
        let m_hook_attr = if let Some(v) = ast.m_hook_attr_1.as_ref() { Box::new(Some(PTHookAttribute::from_ast(v, _si, _errors))) } else { Box::new(None) };
        let m_rule_name = ast.m_rule_name_2.value.clone();
        let m_body = Box::new(PTRuleDefineBody::from_ast(&ast.m_body_3.as_ref(), _si, _errors));
        Self {
            ast,
            m_hook_attr,
            m_rule_name,
            m_body,
        }
    }
}
impl<'p> PTDefineRuleStatement<'p> {
    #[inline] fn from_ast(ast: &'p ASTDefineRuleStatement, si: &mut SemInfo, err: &mut Vec<RegenError>) -> ParseHook<Rule, PTDefineRuleStatement<'p>> {
        let mut pt = Self::from_ast_internal(ast, si, err);
        ParseHook { val: parse_rule(&mut pt, si, err), pt }
    }
}
// HookAttribute
#[derive(Debug)]
pub struct ASTHookAttribute {
    pub m___0: Token,
    pub m_hook_name_1: Token,
    pub m___2: Token,
    pub m_hook_type_3: Token,
    pub m___4: Token,
}
impl ASTHookAttribute {
    fn parse(ts: &mut TokenStream) -> Option<Self> {
        let m___0 = token!(TSymbol::"("(ts))?;
        let m_hook_name_1 = token!(TLiteral::parse(ts))?;
        let m___2 = token!(TSymbol::":"(ts))?;
        let m_hook_type_3 = token!(TLiteral::parse(ts))?;
        let m___4 = token!(TSymbol::")"(ts))?;
        Some(Self {
            m___0,
            m_hook_name_1,
            m___2,
            m_hook_type_3,
            m___4,
        })
    }
    fn apply_semantic(&self, _si: &mut SemInfo) {
        _si.set(&self.m_hook_name_1, Semantics::SHookName);
        _si.set(&self.m_hook_type_3, Semantics::SHookType);
    }
}
#[derive(Debug)] pub struct PTHookAttribute<'p> {
    pub ast: &'p ASTHookAttribute,
    pub m_hook_name: String,
    pub m_hook_type: String,
}
impl<'p> PTHookAttribute<'p> {
    fn from_ast_internal(ast: &'p ASTHookAttribute, _si: &mut SemInfo, _errors: &mut Vec<RegenError>) -> Self {
        let m_hook_name = ast.m_hook_name_1.value.clone();
        let m_hook_type = ast.m_hook_type_3.value.clone();
        Self {
            ast,
            m_hook_name,
            m_hook_type,
        }
    }
}
impl<'p> PTHookAttribute<'p> {
    #[inline] fn from_ast(ast: &'p ASTHookAttribute, si: &mut SemInfo, err: &mut Vec<RegenError>) -> ParseHook<Hook, PTHookAttribute<'p>> {
        let mut pt = Self::from_ast_internal(ast, si, err);
        ParseHook { val: parse_hook(&mut pt, si, err), pt }
    }
}
// RuleDefineBody
regen::generate_union_impl!(from_ast_internal,
[ASTRuleDefineBody, 
    ASTUnionRuleBody,
    ASTFunctionalRuleBody,
], [PTRuleDefineBody<'p>, 
    PTUnionRuleBody<'p>, PTUnionRuleBody,
    PTFunctionalRuleBody<'p>, PTFunctionalRuleBody,
],);
#[derive(Debug)] pub enum PTRuleDefineBody<'p> { 
    PTFunctionalRuleBody(Box<PTFunctionalRuleBody<'p>>),
    PTUnionRuleBody(Box<PTUnionRuleBody<'p>>),
}
impl<'p> PTRuleDefineBody<'p> {
    #[inline] fn from_ast(ast: &'p ASTRuleDefineBody, si: &mut SemInfo, err: &mut Vec<RegenError>) -> ParseHook<RuleValue, PTRuleDefineBody<'p>> {
        let mut pt = Self::from_ast_internal(ast, si, err);
        ParseHook { val: parse_rule_value(&mut pt, si, err), pt }
    }
}
// UnionRuleBody
#[derive(Debug)]
pub struct ASTUnionRuleBody {
    pub m___0: Token,
    pub m_rules_1: Box<Option<ASTUnionRuleList>>,
    pub m___2: Token,
}
impl ASTUnionRuleBody {
    fn parse(ts: &mut TokenStream) -> Option<Self> {
        let m___0 = token!(TSymbol::"="(ts))?;
        let m_rules_1 = optional!(ts, ASTUnionRuleList::parse(ts));
        let m___2 = token!(TSymbol::";"(ts))?;
        Some(Self {
            m___0,
            m_rules_1: Box::new(m_rules_1),
            m___2,
        })
    }
    fn apply_semantic(&self, _si: &mut SemInfo) {
        if let Some(m) = self.m_rules_1.as_ref() { m.apply_semantic(_si); }
    }
}
#[derive(Debug)] pub struct PTUnionRuleBody<'p> {
    pub ast: &'p ASTUnionRuleBody,
    pub val: Box<Option<PTUnionRuleList<'p>>>
}
impl<'p> PTUnionRuleBody<'p> {
    fn from_ast(ast: &'p ASTUnionRuleBody, _si: &mut SemInfo, _errors: &mut Vec<RegenError>) -> Self {
        let m_rules = if let Some(v) = ast.m_rules_1.as_ref() { Box::new(Some(PTUnionRuleList::from_ast(v, _si, _errors))) } else { Box::new(None) };
        Self { ast, val: m_rules }
    }
}
// UnionRuleList
#[derive(Debug)]
pub struct ASTUnionRuleList {
    pub m_first_0: Token,
    pub m_rest_1: Box<Option<ASTUnionRuleListTail>>,
}
impl ASTUnionRuleList {
    fn parse(ts: &mut TokenStream) -> Option<Self> {
        let m_first_0 = token!(TIdentifier::parse(ts))?;
        let m_rest_1 = optional!(ts, ASTUnionRuleListTail::parse(ts));
        Some(Self {
            m_first_0,
            m_rest_1: Box::new(m_rest_1),
        })
    }
    fn apply_semantic(&self, _si: &mut SemInfo) {
        _si.set(&self.m_first_0, Semantics::SRule);
        if let Some(m) = self.m_rest_1.as_ref() { m.apply_semantic(_si); }
    }
}
#[derive(Debug)] pub struct PTUnionRuleList<'p> {
    pub ast: &'p ASTUnionRuleList,
    pub ast_vec: VecDeque<&'p Token>,
    pub val: VecDeque<String>
}
impl<'p> PTUnionRuleList<'p> {
    fn from_ast(ast: &'p ASTUnionRuleList, _si: &mut SemInfo, _errors: &mut Vec<RegenError>) -> Self {
        let m_first = ast.m_first_0.value.clone();
        let m_rest = if let Some(v) = ast.m_rest_1.as_ref() { Box::new(Some(PTUnionRuleListTail::from_ast(v, _si, _errors))) } else { Box::new(None) };
        let val = m_rest;
        let (mut val, mut ast_vec) =
            match *val { None => (VecDeque::new(), VecDeque::new()), Some(v) => (v.val, v.ast_vec) }
        ;
        val.push_front(m_first);
        ast_vec.push_front(&ast.m_first_0);
        Self { ast, val, ast_vec }
    }
}
// UnionRuleListTail
#[derive(Debug)]
pub struct ASTUnionRuleListTail {
    pub m___0: Token,
    pub m_first_1: Token,
    pub m_rest_2: Box<Option<ASTUnionRuleListTail>>,
}
impl ASTUnionRuleListTail {
    fn parse(ts: &mut TokenStream) -> Option<Self> {
        let m___0 = token!(TSymbol::"|"(ts))?;
        let m_first_1 = token!(TIdentifier::parse(ts))?;
        let m_rest_2 = optional!(ts, ASTUnionRuleListTail::parse(ts));
        Some(Self {
            m___0,
            m_first_1,
            m_rest_2: Box::new(m_rest_2),
        })
    }
    fn apply_semantic(&self, _si: &mut SemInfo) {
        _si.set(&self.m_first_1, Semantics::SRule);
        if let Some(m) = self.m_rest_2.as_ref() { m.apply_semantic(_si); }
    }
}
#[derive(Debug)] pub struct PTUnionRuleListTail<'p> {
    pub ast: &'p ASTUnionRuleListTail,
    pub ast_vec: VecDeque<&'p Token>,
    pub val: VecDeque<String>
}
impl<'p> PTUnionRuleListTail<'p> {
    fn from_ast(ast: &'p ASTUnionRuleListTail, _si: &mut SemInfo, _errors: &mut Vec<RegenError>) -> Self {
        let m_first = ast.m_first_1.value.clone();
        let m_rest = if let Some(v) = ast.m_rest_2.as_ref() { Box::new(Some(PTUnionRuleListTail::from_ast(v, _si, _errors))) } else { Box::new(None) };
        let val = m_rest;
        let (mut val, mut ast_vec) =
            match *val { None => (VecDeque::new(), VecDeque::new()), Some(v) => (v.val, v.ast_vec) }
        ;
        val.push_front(m_first);
        ast_vec.push_front(&ast.m_first_1);
        Self { ast, val, ast_vec }
    }
}
// FunctionalRuleBody
#[derive(Debug)]
pub struct ASTFunctionalRuleBody {
    pub m___0: Token,
    pub m_params_1: Box<Option<ASTParamList>>,
    pub m___2: Token,
    pub m_body_3: Box<Option<ASTExpression>>,
    pub m___4: Token,
}
impl ASTFunctionalRuleBody {
    fn parse(ts: &mut TokenStream) -> Option<Self> {
        let m___0 = token!(TSymbol::"("(ts))?;
        let m_params_1 = optional!(ts, ASTParamList::parse(ts));
        let m___2 = token!(TSymbol::")"(ts))?;
        let m_body_3 = optional!(ts, ASTExpression::parse(ts));
        let m___4 = token!(TSymbol::";"(ts))?;
        Some(Self {
            m___0,
            m_params_1: Box::new(m_params_1),
            m___2,
            m_body_3: Box::new(m_body_3),
            m___4,
        })
    }
    fn apply_semantic(&self, _si: &mut SemInfo) {
        if let Some(m) = self.m_params_1.as_ref() { m.apply_semantic(_si); }
        if let Some(m) = self.m_body_3.as_ref() { m.apply_semantic(_si); }
    }
}
#[derive(Debug)] pub struct PTFunctionalRuleBody<'p> {
    pub ast: &'p ASTFunctionalRuleBody,
    pub m_params: Box<Option<ParseHook<Vec<Param>, PTParamList<'p>>>>,
    pub m_body: Box<Option<ParseHook<Expr, PTExpression<'p>>>>,
}
impl<'p> PTFunctionalRuleBody<'p> {
    fn from_ast(ast: &'p ASTFunctionalRuleBody, _si: &mut SemInfo, _errors: &mut Vec<RegenError>) -> Self {
        let m_params = if let Some(v) = ast.m_params_1.as_ref() { Box::new(Some(PTParamList::from_ast(v, _si, _errors))) } else { Box::new(None) };
        let m_body = if let Some(v) = ast.m_body_3.as_ref() { Box::new(Some(PTExpression::from_ast(v, _si, _errors))) } else { Box::new(None) };
        Self {
            ast,
            m_params,
            m_body,
        }
    }
}
// ParamList
#[derive(Debug)]
pub struct ASTParamList {
    pub m_first_0: Box<ASTParam>,
    pub m_rest_1: Box<Option<ASTParamListTail>>,
}
impl ASTParamList {
    fn parse(ts: &mut TokenStream) -> Option<Self> {
        let m_first_0 = ASTParam::parse(ts)?;
        let m_rest_1 = optional!(ts, ASTParamListTail::parse(ts));
        Some(Self {
            m_first_0: Box::new(m_first_0),
            m_rest_1: Box::new(m_rest_1),
        })
    }
    fn apply_semantic(&self, _si: &mut SemInfo) {
        self.m_first_0.apply_semantic(_si);
        if let Some(m) = self.m_rest_1.as_ref() { m.apply_semantic(_si); }
    }
}
#[derive(Debug)] pub struct PTParamList<'p> {
    pub ast: &'p ASTParamList,
    pub ast_vec: VecDeque<&'p ASTParam>,
    pub val: VecDeque<ParseHook<Param, PTParam<'p>>>
}
impl<'p> PTParamList<'p> {
    fn from_ast_internal(ast: &'p ASTParamList, _si: &mut SemInfo, _errors: &mut Vec<RegenError>) -> Self {
        let m_first = Box::new(PTParam::from_ast(&ast.m_first_0.as_ref(), _si, _errors));
        let m_rest = if let Some(v) = ast.m_rest_1.as_ref() { Box::new(Some(PTParamListTail::from_ast(v, _si, _errors))) } else { Box::new(None) };
        let val = m_rest;
        let (mut val, mut ast_vec) =
            match *val { None => (VecDeque::new(), VecDeque::new()), Some(v) => (v.val, v.ast_vec) }
        ;
        val.push_front(*m_first);
        ast_vec.push_front(&ast.m_first_0);
        Self { ast, val, ast_vec }
    }
}
impl<'p> PTParamList<'p> {
    #[inline] fn from_ast(ast: &'p ASTParamList, si: &mut SemInfo, err: &mut Vec<RegenError>) -> ParseHook<Vec<Param>, PTParamList<'p>> {
        let mut pt = Self::from_ast_internal(ast, si, err);
        ParseHook { val: parse_param_list(&mut pt, si, err), pt }
    }
}
// ParamListTail
#[derive(Debug)]
pub struct ASTParamListTail {
    pub m___0: Token,
    pub m_first_1: Box<ASTParam>,
    pub m_rest_2: Box<Option<ASTParamListTail>>,
}
impl ASTParamListTail {
    fn parse(ts: &mut TokenStream) -> Option<Self> {
        let m___0 = token!(TSymbol::","(ts))?;
        let m_first_1 = ASTParam::parse(ts)?;
        let m_rest_2 = optional!(ts, ASTParamListTail::parse(ts));
        Some(Self {
            m___0,
            m_first_1: Box::new(m_first_1),
            m_rest_2: Box::new(m_rest_2),
        })
    }
    fn apply_semantic(&self, _si: &mut SemInfo) {
        self.m_first_1.apply_semantic(_si);
        if let Some(m) = self.m_rest_2.as_ref() { m.apply_semantic(_si); }
    }
}
#[derive(Debug)] pub struct PTParamListTail<'p> {
    pub ast: &'p ASTParamListTail,
    pub ast_vec: VecDeque<&'p ASTParam>,
    pub val: VecDeque<ParseHook<Param, PTParam<'p>>>
}
impl<'p> PTParamListTail<'p> {
    fn from_ast(ast: &'p ASTParamListTail, _si: &mut SemInfo, _errors: &mut Vec<RegenError>) -> Self {
        let m_first = Box::new(PTParam::from_ast(&ast.m_first_1.as_ref(), _si, _errors));
        let m_rest = if let Some(v) = ast.m_rest_2.as_ref() { Box::new(Some(PTParamListTail::from_ast(v, _si, _errors))) } else { Box::new(None) };
        let val = m_rest;
        let (mut val, mut ast_vec) =
            match *val { None => (VecDeque::new(), VecDeque::new()), Some(v) => (v.val, v.ast_vec) }
        ;
        val.push_front(*m_first);
        ast_vec.push_front(&ast.m_first_1);
        Self { ast, val, ast_vec }
    }
}
// Param
#[derive(Debug)]
pub struct ASTParam {
    pub m_sem_attr_0: Box<Option<ASTParamSemantic>>,
    pub m_variable_1: Token,
    pub m___2: Token,
    pub m_type_3: Box<Option<ASTRuleType>>,
}
impl ASTParam {
    fn parse(ts: &mut TokenStream) -> Option<Self> {
        let m_sem_attr_0 = optional!(ts, ASTParamSemantic::parse(ts));
        let m_variable_1 = token!(TIdentifier::parse(ts))?;
        let m___2 = token!(TSymbol::":"(ts))?;
        let m_type_3 = optional!(ts, ASTRuleType::parse(ts));
        Some(Self {
            m_sem_attr_0: Box::new(m_sem_attr_0),
            m_variable_1,
            m___2,
            m_type_3: Box::new(m_type_3),
        })
    }
    fn apply_semantic(&self, _si: &mut SemInfo) {
        if let Some(m) = self.m_sem_attr_0.as_ref() { m.apply_semantic(_si); }
        _si.set(&self.m_variable_1, Semantics::SVariable);
        if let Some(m) = self.m_type_3.as_ref() { m.apply_semantic(_si); }
    }
}
#[derive(Debug)] pub struct PTParam<'p> {
    pub ast: &'p ASTParam,
    pub m_sem_attr: Box<Option<PTParamSemantic<'p>>>,
    pub m_variable: String,
    pub m_type: Box<Option<PTRuleType<'p>>>,
}
impl<'p> PTParam<'p> {
    fn from_ast_internal(ast: &'p ASTParam, _si: &mut SemInfo, _errors: &mut Vec<RegenError>) -> Self {
        let m_sem_attr = if let Some(v) = ast.m_sem_attr_0.as_ref() { Box::new(Some(PTParamSemantic::from_ast(v, _si, _errors))) } else { Box::new(None) };
        let m_variable = ast.m_variable_1.value.clone();
        let m_type = if let Some(v) = ast.m_type_3.as_ref() { Box::new(Some(PTRuleType::from_ast(v, _si, _errors))) } else { Box::new(None) };
        Self {
            ast,
            m_sem_attr,
            m_variable,
            m_type,
        }
    }
}
impl<'p> PTParam<'p> {
    #[inline] fn from_ast(ast: &'p ASTParam, si: &mut SemInfo, err: &mut Vec<RegenError>) -> ParseHook<Param, PTParam<'p>> {
        let mut pt = Self::from_ast_internal(ast, si, err);
        ParseHook { val: parse_param(&mut pt, si, err), pt }
    }
}
// ParamSemantic
#[derive(Debug)]
pub struct ASTParamSemantic {
    pub m___0: Token,
    pub m_semantic_name_1: Option<Token>,
    pub m___2: Token,
}
impl ASTParamSemantic {
    fn parse(ts: &mut TokenStream) -> Option<Self> {
        let m___0 = token!(TSymbol::"("(ts))?;
        let m_semantic_name_1 = optional!(ts, token!(TIdentifier::parse(ts)));
        let m___2 = token!(TSymbol::")"(ts))?;
        Some(Self {
            m___0,
            m_semantic_name_1,
            m___2,
        })
    }
    fn apply_semantic(&self, _si: &mut SemInfo) {
        if let Some(m) = &self.m_semantic_name_1 { _si.set(m, Semantics::SSemantic); }
    }
}
#[derive(Debug)] pub struct PTParamSemantic<'p> {
    pub ast: &'p ASTParamSemantic,
    pub val: Option<String>
}
impl<'p> PTParamSemantic<'p> {
    fn from_ast(ast: &'p ASTParamSemantic, _si: &mut SemInfo, _errors: &mut Vec<RegenError>) -> Self {
        let m_semantic_name = ast.m_semantic_name_1.as_ref().map(|t| t.value.clone());
        Self { ast, val: m_semantic_name }
    }
}
// RuleType
#[derive(Debug)]
pub struct ASTRuleType {
    pub m_kw_optional_0: Option<Token>,
    pub m_kw_token_1: Option<Token>,
    pub m_id_2: Token,
    pub m_token_content_3: Option<Token>,
}
impl ASTRuleType {
    fn parse(ts: &mut TokenStream) -> Option<Self> {
        let m_kw_optional_0 = optional!(ts, token!(TKeyword::"optional"(ts)));
        let m_kw_token_1 = optional!(ts, token!(TKeyword::"token"(ts)));
        let m_id_2 = token!(TIdentifier::parse(ts))?;
        let m_token_content_3 = optional!(ts, token!(TLiteral::parse(ts)));
        Some(Self {
            m_kw_optional_0,
            m_kw_token_1,
            m_id_2,
            m_token_content_3,
        })
    }
    fn apply_semantic(&self, _si: &mut SemInfo) {
    }
}
#[derive(Debug)] pub struct PTRuleType<'p> {
    pub ast: &'p ASTRuleType,
    pub m_kw_optional: bool,
    pub m_kw_token: bool,
    pub m_id: String,
    pub m_token_content: Option<String>,
}
impl<'p> PTRuleType<'p> {
    fn from_ast(ast: &'p ASTRuleType, _si: &mut SemInfo, _errors: &mut Vec<RegenError>) -> Self {
        let m_kw_optional = !ast.m_kw_optional_0.is_none();
        let m_kw_token = !ast.m_kw_token_1.is_none();
        let m_id = ast.m_id_2.value.clone();
        let m_token_content = ast.m_token_content_3.as_ref().map(|t| t.value.clone());
        Self {
            ast,
            m_kw_optional,
            m_kw_token,
            m_id,
            m_token_content,
        }
    }
}
// TopLevelDefineStatement
#[derive(Debug)]
pub struct ASTTopLevelDefineStatement {
    pub m_body_0: Box<ASTTopLevelDefine>,
    pub m___1: Token,
}
impl ASTTopLevelDefineStatement {
    fn parse(ts: &mut TokenStream) -> Option<Self> {
        let m_body_0 = ASTTopLevelDefine::parse(ts)?;
        let m___1 = token!(TSymbol::";"(ts))?;
        Some(Self {
            m_body_0: Box::new(m_body_0),
            m___1,
        })
    }
    fn apply_semantic(&self, _si: &mut SemInfo) {
        self.m_body_0.apply_semantic(_si);
    }
}
#[derive(Debug)] pub struct PTTopLevelDefineStatement<'p> {
    pub ast: &'p ASTTopLevelDefineStatement,
    pub val: Box<PTTopLevelDefine<'p>>
}
impl<'p> PTTopLevelDefineStatement<'p> {
    fn from_ast(ast: &'p ASTTopLevelDefineStatement, _si: &mut SemInfo, _errors: &mut Vec<RegenError>) -> Self {
        let m_body = Box::new(PTTopLevelDefine::from_ast(&ast.m_body_0.as_ref(), _si, _errors));
        Self { ast, val: m_body }
    }
}
// TopLevelDefine
regen::generate_union_impl!(from_ast,
[ASTTopLevelDefine, 
    ASTTokenLiteral,
    ASTDefineTokenTypeStatement,
    ASTDefineIgnoreTokenRuleStatement,
    ASTDefineTokenRuleStatement,
    ASTDefineSemanticStatement,
], [PTTopLevelDefine<'p>, 
    PTTokenLiteral<'p>, PTTokenLiteral,
    ParseHook<TokenDef, PTDefineTokenTypeStatement<'p>>, PTDefineTokenTypeStatement,
    ParseHook<TokenRule, PTDefineIgnoreTokenRuleStatement<'p>>, PTDefineIgnoreTokenRuleStatement,
    ParseHook<TokenRule, PTDefineTokenRuleStatement<'p>>, PTDefineTokenRuleStatement,
    ParseHook<String, PTDefineSemanticStatement<'p>>, PTDefineSemanticStatement,
],);
#[derive(Debug)] pub enum PTTopLevelDefine<'p> { 
    PTDefineSemanticStatement(Box<ParseHook<String, PTDefineSemanticStatement<'p>>>),
    PTDefineTokenRuleStatement(Box<ParseHook<TokenRule, PTDefineTokenRuleStatement<'p>>>),
    PTTokenLiteral(Box<PTTokenLiteral<'p>>),
    PTDefineIgnoreTokenRuleStatement(Box<ParseHook<TokenRule, PTDefineIgnoreTokenRuleStatement<'p>>>),
    PTDefineTokenTypeStatement(Box<ParseHook<TokenDef, PTDefineTokenTypeStatement<'p>>>),
}
// DefineTokenTypeStatement
#[derive(Debug)]
pub struct ASTDefineTokenTypeStatement {
    pub m_kw_extract_0: Option<Token>,
    pub m___1: Token,
    pub m_token_type_2: Token,
}
impl ASTDefineTokenTypeStatement {
    fn parse(ts: &mut TokenStream) -> Option<Self> {
        let m_kw_extract_0 = optional!(ts, token!(TKeyword::"extract"(ts)));
        let m___1 = token!(TKeyword::"token"(ts))?;
        let m_token_type_2 = token!(TIdentifier::parse(ts))?;
        Some(Self {
            m_kw_extract_0,
            m___1,
            m_token_type_2,
        })
    }
    fn apply_semantic(&self, _si: &mut SemInfo) {
        _si.set(&self.m_token_type_2, Semantics::SToken);
    }
}
#[derive(Debug)] pub struct PTDefineTokenTypeStatement<'p> {
    pub ast: &'p ASTDefineTokenTypeStatement,
    pub m_kw_extract: bool,
    pub m_token_type: String,
}
impl<'p> PTDefineTokenTypeStatement<'p> {
    fn from_ast_internal(ast: &'p ASTDefineTokenTypeStatement, _si: &mut SemInfo, _errors: &mut Vec<RegenError>) -> Self {
        let m_kw_extract = !ast.m_kw_extract_0.is_none();
        let m_token_type = ast.m_token_type_2.value.clone();
        Self {
            ast,
            m_kw_extract,
            m_token_type,
        }
    }
}
impl<'p> PTDefineTokenTypeStatement<'p> {
    #[inline] fn from_ast(ast: &'p ASTDefineTokenTypeStatement, si: &mut SemInfo, err: &mut Vec<RegenError>) -> ParseHook<TokenDef, PTDefineTokenTypeStatement<'p>> {
        let mut pt = Self::from_ast_internal(ast, si, err);
        ParseHook { val: parse_token_def(&mut pt, si, err), pt }
    }
}
// DefineIgnoreTokenRuleStatement
#[derive(Debug)]
pub struct ASTDefineIgnoreTokenRuleStatement {
    pub m___0: Token,
    pub m_value_1: Box<ASTLiteralOrRegExp>,
}
impl ASTDefineIgnoreTokenRuleStatement {
    fn parse(ts: &mut TokenStream) -> Option<Self> {
        let m___0 = token!(TKeyword::"ignore"(ts))?;
        let m_value_1 = ASTLiteralOrRegExp::parse(ts)?;
        Some(Self {
            m___0,
            m_value_1: Box::new(m_value_1),
        })
    }
    fn apply_semantic(&self, _si: &mut SemInfo) {
        self.m_value_1.apply_semantic(_si);
    }
}
#[derive(Debug)] pub struct PTDefineIgnoreTokenRuleStatement<'p> {
    pub ast: &'p ASTDefineIgnoreTokenRuleStatement,
    pub val: Box<PTLiteralOrRegExp<'p>>
}
impl<'p> PTDefineIgnoreTokenRuleStatement<'p> {
    fn from_ast_internal(ast: &'p ASTDefineIgnoreTokenRuleStatement, _si: &mut SemInfo, _errors: &mut Vec<RegenError>) -> Self {
        let m_value = Box::new(PTLiteralOrRegExp::from_ast(&ast.m_value_1.as_ref(), _si, _errors));
        Self { ast, val: m_value }
    }
}
impl<'p> PTDefineIgnoreTokenRuleStatement<'p> {
    #[inline] fn from_ast(ast: &'p ASTDefineIgnoreTokenRuleStatement, si: &mut SemInfo, err: &mut Vec<RegenError>) -> ParseHook<TokenRule, PTDefineIgnoreTokenRuleStatement<'p>> {
        let mut pt = Self::from_ast_internal(ast, si, err);
        ParseHook { val: parse_token_ignore_rule(&mut pt, si, err), pt }
    }
}
// DefineTokenRuleStatement
#[derive(Debug)]
pub struct ASTDefineTokenRuleStatement {
    pub m_token_type_0: Token,
    pub m_value_1: Box<ASTLiteralOrRegExp>,
}
impl ASTDefineTokenRuleStatement {
    fn parse(ts: &mut TokenStream) -> Option<Self> {
        let m_token_type_0 = token!(TIdentifier::parse(ts))?;
        let m_value_1 = ASTLiteralOrRegExp::parse(ts)?;
        Some(Self {
            m_token_type_0,
            m_value_1: Box::new(m_value_1),
        })
    }
    fn apply_semantic(&self, _si: &mut SemInfo) {
        _si.set(&self.m_token_type_0, Semantics::SToken);
        self.m_value_1.apply_semantic(_si);
    }
}
#[derive(Debug)] pub struct PTDefineTokenRuleStatement<'p> {
    pub ast: &'p ASTDefineTokenRuleStatement,
    pub m_token_type: String,
    pub m_value: Box<PTLiteralOrRegExp<'p>>,
}
impl<'p> PTDefineTokenRuleStatement<'p> {
    fn from_ast_internal(ast: &'p ASTDefineTokenRuleStatement, _si: &mut SemInfo, _errors: &mut Vec<RegenError>) -> Self {
        let m_token_type = ast.m_token_type_0.value.clone();
        let m_value = Box::new(PTLiteralOrRegExp::from_ast(&ast.m_value_1.as_ref(), _si, _errors));
        Self {
            ast,
            m_token_type,
            m_value,
        }
    }
}
impl<'p> PTDefineTokenRuleStatement<'p> {
    #[inline] fn from_ast(ast: &'p ASTDefineTokenRuleStatement, si: &mut SemInfo, err: &mut Vec<RegenError>) -> ParseHook<TokenRule, PTDefineTokenRuleStatement<'p>> {
        let mut pt = Self::from_ast_internal(ast, si, err);
        ParseHook { val: parse_token_rule(&mut pt, si, err), pt }
    }
}
// LiteralOrRegExp
regen::generate_union_impl!(from_ast,
[ASTLiteralOrRegExp, 
    ASTTokenLiteral,
    ASTTokenRegExp,
], [PTLiteralOrRegExp<'p>, 
    PTTokenLiteral<'p>, PTTokenLiteral,
    PTTokenRegExp<'p>, PTTokenRegExp,
],);
#[derive(Debug)] pub enum PTLiteralOrRegExp<'p> { 
    PTTokenLiteral(Box<PTTokenLiteral<'p>>),
    PTTokenRegExp(Box<PTTokenRegExp<'p>>),
}
// TokenLiteral
#[derive(Debug)]
pub struct ASTTokenLiteral {
    pub m_t_0: Token,
}
impl ASTTokenLiteral {
    fn parse(ts: &mut TokenStream) -> Option<Self> {
        let m_t_0 = token!(TLiteral::parse(ts))?;
        Some(Self {
            m_t_0,
        })
    }
    fn apply_semantic(&self, _si: &mut SemInfo) {
    }
}
#[derive(Debug)] pub struct PTTokenLiteral<'p> {
    pub ast: &'p ASTTokenLiteral,
    pub val: String
}
impl<'p> PTTokenLiteral<'p> {
    fn from_ast(ast: &'p ASTTokenLiteral, _si: &mut SemInfo, _errors: &mut Vec<RegenError>) -> Self {
        let m_t = ast.m_t_0.value.clone();
        Self { ast, val: m_t }
    }
}
// TokenRegExp
#[derive(Debug)]
pub struct ASTTokenRegExp {
    pub m_t_0: Token,
}
impl ASTTokenRegExp {
    fn parse(ts: &mut TokenStream) -> Option<Self> {
        let m_t_0 = token!(TRegExp::parse(ts))?;
        Some(Self {
            m_t_0,
        })
    }
    fn apply_semantic(&self, _si: &mut SemInfo) {
    }
}
#[derive(Debug)] pub struct PTTokenRegExp<'p> {
    pub ast: &'p ASTTokenRegExp,
    pub val: String
}
impl<'p> PTTokenRegExp<'p> {
    fn from_ast(ast: &'p ASTTokenRegExp, _si: &mut SemInfo, _errors: &mut Vec<RegenError>) -> Self {
        let m_t = ast.m_t_0.value.clone();
        Self { ast, val: m_t }
    }
}
// DefineSemanticStatement
#[derive(Debug)]
pub struct ASTDefineSemanticStatement {
    pub m___0: Token,
    pub m_id_1: Token,
}
impl ASTDefineSemanticStatement {
    fn parse(ts: &mut TokenStream) -> Option<Self> {
        let m___0 = token!(TKeyword::"semantic"(ts))?;
        let m_id_1 = token!(TIdentifier::parse(ts))?;
        Some(Self {
            m___0,
            m_id_1,
        })
    }
    fn apply_semantic(&self, _si: &mut SemInfo) {
        _si.set(&self.m_id_1, Semantics::SSemantic);
    }
}
#[derive(Debug)] pub struct PTDefineSemanticStatement<'p> {
    pub ast: &'p ASTDefineSemanticStatement,
    pub val: String
}
impl<'p> PTDefineSemanticStatement<'p> {
    fn from_ast_internal(ast: &'p ASTDefineSemanticStatement, _si: &mut SemInfo, _errors: &mut Vec<RegenError>) -> Self {
        let m_id = ast.m_id_1.value.clone();
        Self { ast, val: m_id }
    }
}
impl<'p> PTDefineSemanticStatement<'p> {
    #[inline] fn from_ast(ast: &'p ASTDefineSemanticStatement, si: &mut SemInfo, err: &mut Vec<RegenError>) -> ParseHook<String, PTDefineSemanticStatement<'p>> {
        let mut pt = Self::from_ast_internal(ast, si, err);
        ParseHook { val: parse_semantic(&mut pt, si, err), pt }
    }
}
// Expression
regen::generate_union_impl!(from_ast_internal,
[ASTExpression, 
    ASTConcatExpression,
    ASTDictExpression,
], [PTExpression<'p>, 
    PTConcatExpression<'p>, PTConcatExpression,
    PTDictExpression<'p>, PTDictExpression,
],);
#[derive(Debug)] pub enum PTExpression<'p> { 
    PTConcatExpression(Box<PTConcatExpression<'p>>),
    PTDictExpression(Box<PTDictExpression<'p>>),
}
impl<'p> PTExpression<'p> {
    #[inline] fn from_ast(ast: &'p ASTExpression, si: &mut SemInfo, err: &mut Vec<RegenError>) -> ParseHook<Expr, PTExpression<'p>> {
        let mut pt = Self::from_ast_internal(ast, si, err);
        ParseHook { val: parse_expr(&mut pt, si, err), pt }
    }
}
// ConcatExpression
#[derive(Debug)]
pub struct ASTConcatExpression {
    pub m_first_0: Token,
    pub m_rest_1: Box<Option<ASTConcatExpressionTail>>,
}
impl ASTConcatExpression {
    fn parse(ts: &mut TokenStream) -> Option<Self> {
        let m_first_0 = token!(TIdentifier::parse(ts))?;
        let m_rest_1 = optional!(ts, ASTConcatExpressionTail::parse(ts));
        Some(Self {
            m_first_0,
            m_rest_1: Box::new(m_rest_1),
        })
    }
    fn apply_semantic(&self, _si: &mut SemInfo) {
        _si.set(&self.m_first_0, Semantics::SVariable);
        if let Some(m) = self.m_rest_1.as_ref() { m.apply_semantic(_si); }
    }
}
#[derive(Debug)] pub struct PTConcatExpression<'p> {
    pub ast: &'p ASTConcatExpression,
    pub ast_vec: VecDeque<&'p Token>,
    pub val: VecDeque<String>
}
impl<'p> PTConcatExpression<'p> {
    fn from_ast(ast: &'p ASTConcatExpression, _si: &mut SemInfo, _errors: &mut Vec<RegenError>) -> Self {
        let m_first = ast.m_first_0.value.clone();
        let m_rest = if let Some(v) = ast.m_rest_1.as_ref() { Box::new(Some(PTConcatExpressionTail::from_ast(v, _si, _errors))) } else { Box::new(None) };
        let val = m_rest;
        let (mut val, mut ast_vec) =
            match *val { None => (VecDeque::new(), VecDeque::new()), Some(v) => { let val = v.val;
            (val.val, val.ast_vec)
        }};
        val.push_front(m_first);
        ast_vec.push_front(&ast.m_first_0);
        Self { ast, val, ast_vec }
    }
}
// ConcatExpressionTail
#[derive(Debug)]
pub struct ASTConcatExpressionTail {
    pub m___0: Token,
    pub m_rest_1: Box<ASTConcatExpression>,
}
impl ASTConcatExpressionTail {
    fn parse(ts: &mut TokenStream) -> Option<Self> {
        let m___0 = token!(TSymbol::"|"(ts))?;
        let m_rest_1 = ASTConcatExpression::parse(ts)?;
        Some(Self {
            m___0,
            m_rest_1: Box::new(m_rest_1),
        })
    }
    fn apply_semantic(&self, _si: &mut SemInfo) {
        self.m_rest_1.apply_semantic(_si);
    }
}
#[derive(Debug)] pub struct PTConcatExpressionTail<'p> {
    pub ast: &'p ASTConcatExpressionTail,
    pub val: Box<PTConcatExpression<'p>>
}
impl<'p> PTConcatExpressionTail<'p> {
    fn from_ast(ast: &'p ASTConcatExpressionTail, _si: &mut SemInfo, _errors: &mut Vec<RegenError>) -> Self {
        let m_rest = Box::new(PTConcatExpression::from_ast(&ast.m_rest_1.as_ref(), _si, _errors));
        Self { ast, val: m_rest }
    }
}
// DictExpression
#[derive(Debug)]
pub struct ASTDictExpression {
    pub m___0: Token,
    pub m_values_1: Box<Option<ASTVariableList>>,
    pub m___2: Token,
}
impl ASTDictExpression {
    fn parse(ts: &mut TokenStream) -> Option<Self> {
        let m___0 = token!(TSymbol::"{"(ts))?;
        let m_values_1 = optional!(ts, ASTVariableList::parse(ts));
        let m___2 = token!(TSymbol::"}"(ts))?;
        Some(Self {
            m___0,
            m_values_1: Box::new(m_values_1),
            m___2,
        })
    }
    fn apply_semantic(&self, _si: &mut SemInfo) {
        if let Some(m) = self.m_values_1.as_ref() { m.apply_semantic(_si); }
    }
}
#[derive(Debug)] pub struct PTDictExpression<'p> {
    pub ast: &'p ASTDictExpression,
    pub val: Box<Option<PTVariableList<'p>>>
}
impl<'p> PTDictExpression<'p> {
    fn from_ast(ast: &'p ASTDictExpression, _si: &mut SemInfo, _errors: &mut Vec<RegenError>) -> Self {
        let m_values = if let Some(v) = ast.m_values_1.as_ref() { Box::new(Some(PTVariableList::from_ast(v, _si, _errors))) } else { Box::new(None) };
        Self { ast, val: m_values }
    }
}
// VariableList
#[derive(Debug)]
pub struct ASTVariableList {
    pub m_first_0: Token,
    pub m_rest_1: Box<Option<ASTVariableListTail>>,
}
impl ASTVariableList {
    fn parse(ts: &mut TokenStream) -> Option<Self> {
        let m_first_0 = token!(TIdentifier::parse(ts))?;
        let m_rest_1 = optional!(ts, ASTVariableListTail::parse(ts));
        Some(Self {
            m_first_0,
            m_rest_1: Box::new(m_rest_1),
        })
    }
    fn apply_semantic(&self, _si: &mut SemInfo) {
        _si.set(&self.m_first_0, Semantics::SVariable);
        if let Some(m) = self.m_rest_1.as_ref() { m.apply_semantic(_si); }
    }
}
#[derive(Debug)] pub struct PTVariableList<'p> {
    pub ast: &'p ASTVariableList,
    pub ast_vec: VecDeque<&'p Token>,
    pub val: VecDeque<String>
}
impl<'p> PTVariableList<'p> {
    fn from_ast(ast: &'p ASTVariableList, _si: &mut SemInfo, _errors: &mut Vec<RegenError>) -> Self {
        let m_first = ast.m_first_0.value.clone();
        let m_rest = if let Some(v) = ast.m_rest_1.as_ref() { Box::new(Some(PTVariableListTail::from_ast(v, _si, _errors))) } else { Box::new(None) };
        let val = m_rest;
        let (mut val, mut ast_vec) =
            match *val { None => (VecDeque::new(), VecDeque::new()), Some(v) => { let val = v.val;
            match *val { None => (VecDeque::new(), VecDeque::new()), Some(v) => (v.val, v.ast_vec) }
        }};
        val.push_front(m_first);
        ast_vec.push_front(&ast.m_first_0);
        Self { ast, val, ast_vec }
    }
}
// VariableListTail
#[derive(Debug)]
pub struct ASTVariableListTail {
    pub m___0: Token,
    pub m_rest_1: Box<Option<ASTVariableList>>,
}
impl ASTVariableListTail {
    fn parse(ts: &mut TokenStream) -> Option<Self> {
        let m___0 = token!(TSymbol::","(ts))?;
        let m_rest_1 = optional!(ts, ASTVariableList::parse(ts));
        Some(Self {
            m___0,
            m_rest_1: Box::new(m_rest_1),
        })
    }
    fn apply_semantic(&self, _si: &mut SemInfo) {
        if let Some(m) = self.m_rest_1.as_ref() { m.apply_semantic(_si); }
    }
}
#[derive(Debug)] pub struct PTVariableListTail<'p> {
    pub ast: &'p ASTVariableListTail,
    pub val: Box<Option<PTVariableList<'p>>>
}
impl<'p> PTVariableListTail<'p> {
    fn from_ast(ast: &'p ASTVariableListTail, _si: &mut SemInfo, _errors: &mut Vec<RegenError>) -> Self {
        let m_rest = if let Some(v) = ast.m_rest_1.as_ref() { Box::new(Some(PTVariableList::from_ast(v, _si, _errors))) } else { Box::new(None) };
        Self { ast, val: m_rest }
    }
}
