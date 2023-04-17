/*
  Generated with regen-lang v0.0.2
*/
//// /* src/sdk/grammar.rs.pp */
/* 
 This is the preprocessor file for generated.rs.

 In regen grammar files, you can specify a literal string appended with ";" as a statement to include another file
 THe path is relative to the grammar file. This is useful for including import for hook and others

 Included files at the beginning or end of the grammar file will be included in the beginning or end of the generated file, in the order they are specified. Includes in the middle of the grammar file will be included in the middle if the implementation of the target language allows it.

 (Honestly there is no reason to include something in the middle.. right?)
*/
#![cfg_attr(rustfmt, rustfmt_skip)]
// Hooks
// Since hooks are defined in the grammar file as literal strings, we need to include the implementation here.
use crate::core::{Rule, RuleValue, Expr, Param, Hook, TokenDef, TokenRule, Context};
use crate::core::rule::{parse_rule, parse_rule_value};
use crate::core::expr::parse_expr;
use crate::core::param::{parse_param, parse_param_list};
use crate::core::hook::parse_hook;
use crate::core::token::{parse_token_def, parse_token_ignore_rule, parse_token_rule};
use crate::core::semantic::parse_semantic;
use crate::core::ctx::parse_context;
//// /* src/sdk/grammar.rs.pp */
crate::sdk!(
  crate;
  context: Context;
  target: TopLevelStatement;
  tokens: [
    TKeyword,
    TIdentifier,
    TRegExp,
    TLiteral,
    TSymbol,
    TComment,
  ];
  regex: [
    re0 = r"^\s+",
    re1 = r"^//[^\n]*\n?",
    re2 = r"^/\*([^\*]|(\*[^/]))*\*/",
    re3 = r#"^"((\\.)|[^\\"])*""#,
    re4 = r"^/((\\.)|[^\\/])*/",
    re5 = r"^[{};|()=,:\.\[\]]",
    re6 = r"^[_a-zA-Z]\w*",
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
    [false, TKeyword, "rule", 4],
    [false, TKeyword, "optional", 8],
    [false, TKeyword, "context", 7],
    [false, TSymbol, re5],
    [false, TIdentifier, re6],
  ];
  semantics: [
    SVariable,
    SToken,
    SSemantic,
    SRule,
    SHookName,
    SHookType,
    SContextType,
  ];
);
pub mod ast {
  use super::*;
  #[derive(Debug)] pub struct TopLevelStatement { pub m_body: Box<TopLevelDefine>, pub m_1: Token, }
  #[derive(Debug)] pub enum TopLevelDefine {
    TokenLiteral(Box<TokenLiteral>),
    DefineContextStatement(Box<DefineContextStatement>),
    DefineRuleStatement(Box<DefineRuleStatement>),
    DefineTokenTypeStatement(Box<DefineTokenTypeStatement>),
    DefineIgnoreTokenRuleStatement(Box<DefineIgnoreTokenRuleStatement>),
    DefineTokenRuleStatement(Box<DefineTokenRuleStatement>),
    DefineSemanticStatement(Box<DefineSemanticStatement>),
  }
  #[derive(Debug)] pub struct DefineContextStatement { pub m_0: Token, pub m_context_type: Token, }
  #[derive(Debug)] pub struct DefineRuleStatement {
    pub m_0: Token,
    pub m_hook_attr: Box<Option<HookAttribute>>,
    pub m_rule_name: Token,
    pub m_body: Box<RuleDefineBody>,
  }
  #[derive(Debug)] pub struct HookAttribute {
    pub m_0: Token,
    pub m_hook_name: Token,
    pub m_2: Token,
    pub m_hook_type: Token,
    pub m_4: Token,
  }
  #[derive(Debug)] pub enum RuleDefineBody {
    UnionRuleBody(Box<UnionRuleBody>),
    FunctionalRuleBody(Box<FunctionalRuleBody>),
  }
  #[derive(Debug)] pub struct UnionRuleBody { pub m_0: Token, pub m_rules: Box<Option<UnionRuleList>>, }
  #[derive(Debug)] pub struct UnionRuleList { pub m_first: Token, pub m_rest: Box<Option<UnionRuleListTail>>, }
  #[derive(Debug)] pub struct UnionRuleListTail {
    pub m_0: Token,
    pub m_first: Token,
    pub m_rest: Box<Option<UnionRuleListTail>>,
  }
  #[derive(Debug)] pub struct FunctionalRuleBody {
    pub m_0: Token,
    pub m_params: Box<Option<ParamList>>,
    pub m_2: Token,
    pub m_body: Box<Option<Expression>>,
  }
  #[derive(Debug)] pub struct ParamList { pub m_first: Box<Parameter>, pub m_rest: Box<Option<ParamListTail>>, }
  #[derive(Debug)] pub struct ParamListTail {
    pub m_0: Token,
    pub m_first: Box<Parameter>,
    pub m_rest: Box<Option<ParamListTail>>,
  }
  #[derive(Debug)] pub struct Parameter {
    pub m_sem_attr: Box<Option<ParamSemantic>>,
    pub m_variable: Token,
    pub m_2: Token,
    pub m_type: Box<Option<RuleType>>,
  }
  #[derive(Debug)] pub struct ParamSemantic {
    pub m_0: Token,
    pub m_semantic_name: Option<Token>,
    pub m_2: Token,
  }
  #[derive(Debug)] pub struct RuleType {
    pub m_kw_optional: Option<Token>,
    pub m_kw_token: Option<Token>,
    pub m_id: Token,
    pub m_token_content: Option<Token>,
  }
  #[derive(Debug)] pub struct DefineTokenTypeStatement {
    pub m_kw_extract: Option<Token>,
    pub m_1: Token,
    pub m_token_type: Token,
  }
  #[derive(Debug)] pub struct DefineIgnoreTokenRuleStatement { pub m_0: Token, pub m_value: Box<LiteralOrRegExp>, }
  #[derive(Debug)] pub struct DefineTokenRuleStatement { pub m_token_type: Token, pub m_value: Box<LiteralOrRegExp>, }
  #[derive(Debug)] pub enum LiteralOrRegExp {
    TokenLiteral(Box<TokenLiteral>),
    TokenRegExp(Box<TokenRegExp>),
  }
  #[derive(Debug)] pub struct TokenLiteral { pub m_t: Token, }
  #[derive(Debug)] pub struct TokenRegExp { pub m_t: Token, }
  #[derive(Debug)] pub struct DefineSemanticStatement { pub m_0: Token, pub m_id: Token, }
  #[derive(Debug)] pub enum Expression {
    ConcatExpression(Box<ConcatExpression>),
    DictExpression(Box<DictExpression>),
  }
  #[derive(Debug)] pub struct ConcatExpression { pub m_first: Token, pub m_rest: Box<Option<ConcatExpressionTail>>, }
  #[derive(Debug)] pub struct ConcatExpressionTail { pub m_0: Token, pub m_rest: Box<ConcatExpression>, }
  #[derive(Debug)] pub struct DictExpression {
    pub m_0: Token,
    pub m_values: Box<Option<VariableList>>,
    pub m_2: Token,
  }
  #[derive(Debug)] pub struct VariableList { pub m_first: Token, pub m_rest: Box<Option<VariableListTail>>, }
  #[derive(Debug)] pub struct VariableListTail { pub m_0: Token, pub m_rest: Box<Option<VariableList>>, }
}
pub mod pt {
  use super::*;
  #[derive(Debug)] pub struct TopLevelStatement<'p> { pub ast: &'p ast::TopLevelStatement, pub m_body: Box<pt::TopLevelDefine<'p>>, }
  #[derive(Debug)] pub enum TopLevelDefine<'p> { 
    TokenLiteral(Box<pt::TokenLiteral<'p>>),
    DefineContextStatement(Box<ParseHook<(), pt::DefineContextStatement<'p>>>),
    DefineRuleStatement(Box<ParseHook<Rule, pt::DefineRuleStatement<'p>>>),
    DefineTokenTypeStatement(Box<ParseHook<TokenDef, pt::DefineTokenTypeStatement<'p>>>),
    DefineIgnoreTokenRuleStatement(Box<ParseHook<TokenRule, pt::DefineIgnoreTokenRuleStatement<'p>>>),
    DefineTokenRuleStatement(Box<ParseHook<TokenRule, pt::DefineTokenRuleStatement<'p>>>),
    DefineSemanticStatement(Box<ParseHook<String, pt::DefineSemanticStatement<'p>>>),
  }
  #[derive(Debug)] pub struct DefineContextStatement<'p> { pub ast: &'p ast::DefineContextStatement, pub m_context_type: String, }
  #[derive(Debug)] pub struct DefineRuleStatement<'p> {
    pub ast: &'p ast::DefineRuleStatement,
    pub m_hook_attr: Box<Option<ParseHook<Hook, pt::HookAttribute<'p>>>>,
    pub m_rule_name: String,
    pub m_body: Box<ParseHook<RuleValue, pt::RuleDefineBody<'p>>>,
  }
  #[derive(Debug)] pub struct HookAttribute<'p> {
    pub ast: &'p ast::HookAttribute,
    pub m_hook_name: String,
    pub m_hook_type: String,
  }
  #[derive(Debug)] pub enum RuleDefineBody<'p> {  UnionRuleBody(Box<pt::UnionRuleBody<'p>>), FunctionalRuleBody(Box<pt::FunctionalRuleBody<'p>>), }
  #[derive(Debug)] pub struct UnionRuleBody<'p> {
    pub ast: &'p ast::UnionRuleBody,
    pub asts: VecDeque<&'p Token>,
    pub vals: VecDeque<String>,
  }
  #[derive(Debug)] pub struct UnionRuleList<'p> {
    pub ast: &'p ast::UnionRuleList,
    pub asts: VecDeque<&'p Token>,
    pub vals: VecDeque<String>,
  }
  #[derive(Debug)] pub struct UnionRuleListTail<'p> {
    pub ast: &'p ast::UnionRuleListTail,
    pub asts: VecDeque<&'p Token>,
    pub vals: VecDeque<String>,
  }
  #[derive(Debug)] pub struct FunctionalRuleBody<'p> {
    pub ast: &'p ast::FunctionalRuleBody,
    pub m_params: Box<Option<ParseHook<Vec<Param>, pt::ParamList<'p>>>>,
    pub m_body: Box<Option<ParseHook<Expr, pt::Expression<'p>>>>,
  }
  #[derive(Debug)] pub struct ParamList<'p> {
    pub ast: &'p ast::ParamList,
    pub asts: VecDeque<&'p ast::Parameter>,
    pub vals: VecDeque<ParseHook<Param, pt::Parameter<'p>>>,
  }
  #[derive(Debug)] pub struct ParamListTail<'p> {
    pub ast: &'p ast::ParamListTail,
    pub asts: VecDeque<&'p ast::Parameter>,
    pub vals: VecDeque<ParseHook<Param, pt::Parameter<'p>>>,
  }
  #[derive(Debug)] pub struct Parameter<'p> {
    pub ast: &'p ast::Parameter,
    pub m_sem_attr: Box<Option<pt::ParamSemantic<'p>>>,
    pub m_variable: String,
    pub m_type: Box<Option<pt::RuleType<'p>>>,
  }
  #[derive(Debug)] pub struct ParamSemantic<'p> { pub ast: &'p ast::ParamSemantic, pub m_semantic_name: Option<String>, }
  #[derive(Debug)] pub struct RuleType<'p> {
    pub ast: &'p ast::RuleType,
    pub m_kw_optional: bool,
    pub m_kw_token: bool,
    pub m_id: String,
    pub m_token_content: Option<String>,
  }
  #[derive(Debug)] pub struct DefineTokenTypeStatement<'p> {
    pub ast: &'p ast::DefineTokenTypeStatement,
    pub m_kw_extract: bool,
    pub m_token_type: String,
  }
  #[derive(Debug)] pub struct DefineIgnoreTokenRuleStatement<'p> { pub ast: &'p ast::DefineIgnoreTokenRuleStatement, pub m_value: Box<pt::LiteralOrRegExp<'p>>, }
  #[derive(Debug)] pub struct DefineTokenRuleStatement<'p> {
    pub ast: &'p ast::DefineTokenRuleStatement,
    pub m_token_type: String,
    pub m_value: Box<pt::LiteralOrRegExp<'p>>,
  }
  #[derive(Debug)] pub enum LiteralOrRegExp<'p> {  TokenLiteral(Box<pt::TokenLiteral<'p>>), TokenRegExp(Box<pt::TokenRegExp<'p>>), }
  #[derive(Debug)] pub struct TokenLiteral<'p> { pub ast: &'p ast::TokenLiteral, pub m_t: String, }
  #[derive(Debug)] pub struct TokenRegExp<'p> { pub ast: &'p ast::TokenRegExp, pub m_t: String, }
  #[derive(Debug)] pub struct DefineSemanticStatement<'p> { pub ast: &'p ast::DefineSemanticStatement, pub m_id: String, }
  #[derive(Debug)] pub enum Expression<'p> {  ConcatExpression(Box<pt::ConcatExpression<'p>>), DictExpression(Box<pt::DictExpression<'p>>), }
  #[derive(Debug)] pub struct ConcatExpression<'p> {
    pub ast: &'p ast::ConcatExpression,
    pub asts: VecDeque<&'p Token>,
    pub vals: VecDeque<String>,
  }
  #[derive(Debug)] pub struct ConcatExpressionTail<'p> {
    pub ast: &'p ast::ConcatExpressionTail,
    pub asts: VecDeque<&'p Token>,
    pub vals: VecDeque<String>,
  }
  #[derive(Debug)] pub struct DictExpression<'p> {
    pub ast: &'p ast::DictExpression,
    pub asts: VecDeque<&'p Token>,
    pub vals: VecDeque<String>,
  }
  #[derive(Debug)] pub struct VariableList<'p> {
    pub ast: &'p ast::VariableList,
    pub asts: VecDeque<&'p Token>,
    pub vals: VecDeque<String>,
  }
  #[derive(Debug)] pub struct VariableListTail<'p> {
    pub ast: &'p ast::VariableListTail,
    pub asts: VecDeque<&'p Token>,
    pub vals: VecDeque<String>,
  }
}
impl ast::TopLevelStatement {
  fn parse(ts: &mut TokenStream<Tok>) -> Option<Self> {
    Some(Self { m_body: Box::new(ast::TopLevelDefine::parse(ts)?), m_1: token!(TSymbol::";"(ts))?, })
  }
  fn apply_semantic(&self, si: &mut SemInfo, _ovr: &Option<Sem>) {
    self.m_body.apply_semantic(si, _ovr);
    if let Some(o) = _ovr { si.set(&self.m_1, o.clone()); }
  }
}
impl<'p> pt::TopLevelStatement<'p> {
  fn from_ast(ast: &'p ast::TopLevelStatement, _ctx: &mut Ctx) -> Self {
    let m_body = Box::new(pt::TopLevelDefine::from_ast(ast.m_body.as_ref(), _ctx));
    Self { ast, m_body }
  }
}
crate::impl_union!(from_ast, TopLevelDefine, {
  TokenLiteral,
  DefineContextStatement,
  DefineRuleStatement,
  DefineTokenTypeStatement,
  DefineIgnoreTokenRuleStatement,
  DefineTokenRuleStatement,
  DefineSemanticStatement,
});
impl ast::DefineContextStatement {
  fn parse(ts: &mut TokenStream<Tok>) -> Option<Self> {
    Some(Self { m_0: token!(TKeyword::"context"(ts))?, m_context_type: token!(TLiteral::parse(ts))?, })
  }
  fn apply_semantic(&self, si: &mut SemInfo, _ovr: &Option<Sem>) {
    if let Some(o) = _ovr { si.set(&self.m_0, o.clone()); }
    si.set(&self.m_context_type, _ovr.as_ref().cloned().unwrap_or(Sem::SContextType));
  }
}
impl<'p> pt::DefineContextStatement<'p> {
  fn from_ast_internal(ast: &'p ast::DefineContextStatement, _ctx: &mut Ctx) -> Self {
    let m_context_type = ast.m_context_type.value.clone();
    Self { ast, m_context_type }
  }
  #[inline] #[allow(clippy::unnecessary_mut_passed)] fn from_ast(ast: &'p ast::DefineContextStatement, ctx: &mut Ctx) -> ParseHook<(), pt::DefineContextStatement<'p>> {
    let mut pt = Self::from_ast_internal(ast, ctx);
    ParseHook { val: parse_context(&mut pt, ctx), pt }
  }
}
impl ast::DefineRuleStatement {
  fn parse(ts: &mut TokenStream<Tok>) -> Option<Self> {
    Some(Self {
      m_0: token!(TKeyword::"rule"(ts))?,
      m_hook_attr: Box::new(optional!(ts, ast::HookAttribute::parse(ts))),
      m_rule_name: token!(TIdentifier::parse(ts))?,
      m_body: Box::new(ast::RuleDefineBody::parse(ts)?),
    })
  }
  fn apply_semantic(&self, si: &mut SemInfo, _ovr: &Option<Sem>) {
    if let Some(o) = _ovr { si.set(&self.m_0, o.clone()); }
    if let Some(m) = self.m_hook_attr.as_ref() { m.apply_semantic(si, _ovr); }
    si.set(&self.m_rule_name, _ovr.as_ref().cloned().unwrap_or(Sem::SRule));
    self.m_body.apply_semantic(si, _ovr);
  }
}
impl<'p> pt::DefineRuleStatement<'p> {
  fn from_ast_internal(ast: &'p ast::DefineRuleStatement, _ctx: &mut Ctx) -> Self {
    let m_hook_attr = if let Some(v) = ast.m_hook_attr.as_ref() { Box::new(Some(pt::HookAttribute::from_ast(v, _ctx))) } else { Box::new(None) };
    let m_rule_name = ast.m_rule_name.value.clone();
    let m_body = Box::new(pt::RuleDefineBody::from_ast(ast.m_body.as_ref(), _ctx));
    Self {
      ast,
      m_hook_attr,
      m_rule_name,
      m_body,
    }
  }
  #[inline] #[allow(clippy::unnecessary_mut_passed)] fn from_ast(ast: &'p ast::DefineRuleStatement, ctx: &mut Ctx) -> ParseHook<Rule, pt::DefineRuleStatement<'p>> {
    let mut pt = Self::from_ast_internal(ast, ctx);
    ParseHook { val: parse_rule(&mut pt, ctx), pt }
  }
}
impl ast::HookAttribute {
  fn parse(ts: &mut TokenStream<Tok>) -> Option<Self> {
    Some(Self {
      m_0: token!(TSymbol::"("(ts))?,
      m_hook_name: token!(TLiteral::parse(ts))?,
      m_2: token!(TSymbol::":"(ts))?,
      m_hook_type: token!(TLiteral::parse(ts))?,
      m_4: token!(TSymbol::")"(ts))?,
    })
  }
  fn apply_semantic(&self, si: &mut SemInfo, _ovr: &Option<Sem>) {
    if let Some(o) = _ovr { si.set(&self.m_0, o.clone()); }
    si.set(&self.m_hook_name, _ovr.as_ref().cloned().unwrap_or(Sem::SHookName));
    if let Some(o) = _ovr { si.set(&self.m_2, o.clone()); }
    si.set(&self.m_hook_type, _ovr.as_ref().cloned().unwrap_or(Sem::SHookType));
    if let Some(o) = _ovr { si.set(&self.m_4, o.clone()); }
  }
}
impl<'p> pt::HookAttribute<'p> {
  fn from_ast_internal(ast: &'p ast::HookAttribute, _ctx: &mut Ctx) -> Self {
    let m_hook_name = ast.m_hook_name.value.clone();
    let m_hook_type = ast.m_hook_type.value.clone();
    Self { ast, m_hook_name, m_hook_type, }
  }
  #[inline] #[allow(clippy::unnecessary_mut_passed)] fn from_ast(ast: &'p ast::HookAttribute, ctx: &mut Ctx) -> ParseHook<Hook, pt::HookAttribute<'p>> {
    let mut pt = Self::from_ast_internal(ast, ctx);
    ParseHook { val: parse_hook(&mut pt, ctx), pt }
  }
}
crate::impl_union!(from_ast_internal, RuleDefineBody, { UnionRuleBody, FunctionalRuleBody, });
impl<'p> pt::RuleDefineBody<'p> {
  #[inline] #[allow(clippy::unnecessary_mut_passed)] fn from_ast(ast: &'p ast::RuleDefineBody, ctx: &mut Ctx) -> ParseHook<RuleValue, pt::RuleDefineBody<'p>> {
    let mut pt = Self::from_ast_internal(ast, ctx);
    ParseHook { val: parse_rule_value(&mut pt, ctx), pt }
  }
}
impl ast::UnionRuleBody {
  fn parse(ts: &mut TokenStream<Tok>) -> Option<Self> {
    Some(Self { m_0: token!(TSymbol::"="(ts))?, m_rules: Box::new(optional!(ts, ast::UnionRuleList::parse(ts))), })
  }
  fn apply_semantic(&self, si: &mut SemInfo, _ovr: &Option<Sem>) {
    if let Some(o) = _ovr { si.set(&self.m_0, o.clone()); }
    if let Some(m) = self.m_rules.as_ref() { m.apply_semantic(si, _ovr); }
  }
}
impl<'p> pt::UnionRuleBody<'p> {
  fn from_ast(ast: &'p ast::UnionRuleBody, _ctx: &mut Ctx) -> Self {
    let m_rules = if let Some(v) = ast.m_rules.as_ref() { Box::new(Some(pt::UnionRuleList::from_ast(v, _ctx))) } else { Box::new(None) };
    let (asts, vals) = move_pt_vec_optional!(m_rules);
    Self { ast, asts, vals }
  }
}
impl ast::UnionRuleList {
  fn parse(ts: &mut TokenStream<Tok>) -> Option<Self> {
    Some(Self { m_first: token!(TIdentifier::parse(ts))?, m_rest: Box::new(optional!(ts, ast::UnionRuleListTail::parse(ts))), })
  }
  fn apply_semantic(&self, si: &mut SemInfo, _ovr: &Option<Sem>) {
    si.set(&self.m_first, _ovr.as_ref().cloned().unwrap_or(Sem::SRule));
    if let Some(m) = self.m_rest.as_ref() { m.apply_semantic(si, _ovr); }
  }
}
impl<'p> pt::UnionRuleList<'p> {
  fn from_ast(ast: &'p ast::UnionRuleList, _ctx: &mut Ctx) -> Self {
    let m_first = ast.m_first.value.clone();
    let m_rest = if let Some(v) = ast.m_rest.as_ref() { Box::new(Some(pt::UnionRuleListTail::from_ast(v, _ctx))) } else { Box::new(None) };
    let (mut asts, mut vals) = move_pt_vec_optional!(m_rest);
    vals.push_front(m_first);
    asts.push_front(&ast.m_first);
    Self { ast, asts, vals }
  }
}
impl ast::UnionRuleListTail {
  fn parse(ts: &mut TokenStream<Tok>) -> Option<Self> {
    Some(Self {
      m_0: token!(TSymbol::"|"(ts))?,
      m_first: token!(TIdentifier::parse(ts))?,
      m_rest: Box::new(optional!(ts, ast::UnionRuleListTail::parse(ts))),
    })
  }
  fn apply_semantic(&self, si: &mut SemInfo, _ovr: &Option<Sem>) {
    if let Some(o) = _ovr { si.set(&self.m_0, o.clone()); }
    si.set(&self.m_first, _ovr.as_ref().cloned().unwrap_or(Sem::SRule));
    if let Some(m) = self.m_rest.as_ref() { m.apply_semantic(si, _ovr); }
  }
}
impl<'p> pt::UnionRuleListTail<'p> {
  fn from_ast(ast: &'p ast::UnionRuleListTail, _ctx: &mut Ctx) -> Self {
    let m_first = ast.m_first.value.clone();
    let m_rest = if let Some(v) = ast.m_rest.as_ref() { Box::new(Some(pt::UnionRuleListTail::from_ast(v, _ctx))) } else { Box::new(None) };
    let (mut asts, mut vals) = move_pt_vec_optional!(m_rest);
    vals.push_front(m_first);
    asts.push_front(&ast.m_first);
    Self { ast, asts, vals }
  }
}
impl ast::FunctionalRuleBody {
  fn parse(ts: &mut TokenStream<Tok>) -> Option<Self> {
    Some(Self {
      m_0: token!(TSymbol::"("(ts))?,
      m_params: Box::new(optional!(ts, ast::ParamList::parse(ts))),
      m_2: token!(TSymbol::")"(ts))?,
      m_body: Box::new(optional!(ts, ast::Expression::parse(ts))),
    })
  }
  fn apply_semantic(&self, si: &mut SemInfo, _ovr: &Option<Sem>) {
    if let Some(o) = _ovr { si.set(&self.m_0, o.clone()); }
    if let Some(m) = self.m_params.as_ref() { m.apply_semantic(si, _ovr); }
    if let Some(o) = _ovr { si.set(&self.m_2, o.clone()); }
    if let Some(m) = self.m_body.as_ref() { m.apply_semantic(si, _ovr); }
  }
}
impl<'p> pt::FunctionalRuleBody<'p> {
  fn from_ast(ast: &'p ast::FunctionalRuleBody, _ctx: &mut Ctx) -> Self {
    let m_params = if let Some(v) = ast.m_params.as_ref() { Box::new(Some(pt::ParamList::from_ast(v, _ctx))) } else { Box::new(None) };
    let m_body = if let Some(v) = ast.m_body.as_ref() { Box::new(Some(pt::Expression::from_ast(v, _ctx))) } else { Box::new(None) };
    Self { ast, m_params, m_body, }
  }
}
impl ast::ParamList {
  fn parse(ts: &mut TokenStream<Tok>) -> Option<Self> {
    Some(Self { m_first: Box::new(ast::Parameter::parse(ts)?), m_rest: Box::new(optional!(ts, ast::ParamListTail::parse(ts))), })
  }
  fn apply_semantic(&self, si: &mut SemInfo, _ovr: &Option<Sem>) {
    self.m_first.apply_semantic(si, _ovr);
    if let Some(m) = self.m_rest.as_ref() { m.apply_semantic(si, _ovr); }
  }
}
impl<'p> pt::ParamList<'p> {
  fn from_ast_internal(ast: &'p ast::ParamList, _ctx: &mut Ctx) -> Self {
    let m_first = Box::new(pt::Parameter::from_ast(ast.m_first.as_ref(), _ctx));
    let m_rest = if let Some(v) = ast.m_rest.as_ref() { Box::new(Some(pt::ParamListTail::from_ast(v, _ctx))) } else { Box::new(None) };
    let (mut asts, mut vals) = move_pt_vec_optional!(m_rest);
    vals.push_front(*m_first);
    asts.push_front(&ast.m_first);
    Self { ast, asts, vals }
  }
  #[inline] #[allow(clippy::unnecessary_mut_passed)] fn from_ast(ast: &'p ast::ParamList, ctx: &mut Ctx) -> ParseHook<Vec<Param>, pt::ParamList<'p>> {
    let mut pt = Self::from_ast_internal(ast, ctx);
    ParseHook { val: parse_param_list(&mut pt, ctx), pt }
  }
}
impl ast::ParamListTail {
  fn parse(ts: &mut TokenStream<Tok>) -> Option<Self> {
    Some(Self {
      m_0: token!(TSymbol::","(ts))?,
      m_first: Box::new(ast::Parameter::parse(ts)?),
      m_rest: Box::new(optional!(ts, ast::ParamListTail::parse(ts))),
    })
  }
  fn apply_semantic(&self, si: &mut SemInfo, _ovr: &Option<Sem>) {
    if let Some(o) = _ovr { si.set(&self.m_0, o.clone()); }
    self.m_first.apply_semantic(si, _ovr);
    if let Some(m) = self.m_rest.as_ref() { m.apply_semantic(si, _ovr); }
  }
}
impl<'p> pt::ParamListTail<'p> {
  fn from_ast(ast: &'p ast::ParamListTail, _ctx: &mut Ctx) -> Self {
    let m_first = Box::new(pt::Parameter::from_ast(ast.m_first.as_ref(), _ctx));
    let m_rest = if let Some(v) = ast.m_rest.as_ref() { Box::new(Some(pt::ParamListTail::from_ast(v, _ctx))) } else { Box::new(None) };
    let (mut asts, mut vals) = move_pt_vec_optional!(m_rest);
    vals.push_front(*m_first);
    asts.push_front(&ast.m_first);
    Self { ast, asts, vals }
  }
}
impl ast::Parameter {
  fn parse(ts: &mut TokenStream<Tok>) -> Option<Self> {
    Some(Self {
      m_sem_attr: Box::new(optional!(ts, ast::ParamSemantic::parse(ts))),
      m_variable: token!(TIdentifier::parse(ts))?,
      m_2: token!(TSymbol::":"(ts))?,
      m_type: Box::new(optional!(ts, ast::RuleType::parse(ts))),
    })
  }
  fn apply_semantic(&self, si: &mut SemInfo, _ovr: &Option<Sem>) {
    if let Some(m) = self.m_sem_attr.as_ref() { m.apply_semantic(si, _ovr); }
    si.set(&self.m_variable, _ovr.as_ref().cloned().unwrap_or(Sem::SVariable));
    if let Some(o) = _ovr { si.set(&self.m_2, o.clone()); }
    if let Some(m) = self.m_type.as_ref() { m.apply_semantic(si, _ovr); }
  }
}
impl<'p> pt::Parameter<'p> {
  fn from_ast_internal(ast: &'p ast::Parameter, _ctx: &mut Ctx) -> Self {
    let m_sem_attr = if let Some(v) = ast.m_sem_attr.as_ref() { Box::new(Some(pt::ParamSemantic::from_ast(v, _ctx))) } else { Box::new(None) };
    let m_variable = ast.m_variable.value.clone();
    let m_type = if let Some(v) = ast.m_type.as_ref() { Box::new(Some(pt::RuleType::from_ast(v, _ctx))) } else { Box::new(None) };
    Self {
      ast,
      m_sem_attr,
      m_variable,
      m_type,
    }
  }
  #[inline] #[allow(clippy::unnecessary_mut_passed)] fn from_ast(ast: &'p ast::Parameter, ctx: &mut Ctx) -> ParseHook<Param, pt::Parameter<'p>> {
    let mut pt = Self::from_ast_internal(ast, ctx);
    ParseHook { val: parse_param(&mut pt, ctx), pt }
  }
}
impl ast::ParamSemantic {
  fn parse(ts: &mut TokenStream<Tok>) -> Option<Self> {
    Some(Self {
      m_0: token!(TSymbol::"("(ts))?,
      m_semantic_name: optional!(ts, token!(TIdentifier::parse(ts))),
      m_2: token!(TSymbol::")"(ts))?,
    })
  }
  fn apply_semantic(&self, si: &mut SemInfo, _ovr: &Option<Sem>) {
    if let Some(o) = _ovr { si.set(&self.m_0, o.clone()); }
    if let Some(m) = &self.m_semantic_name { si.set(m, _ovr.as_ref().cloned().unwrap_or(Sem::SSemantic)); }
    if let Some(o) = _ovr { si.set(&self.m_2, o.clone()); }
  }
}
impl<'p> pt::ParamSemantic<'p> {
  fn from_ast(ast: &'p ast::ParamSemantic, _ctx: &mut Ctx) -> Self {
    let m_semantic_name = ast.m_semantic_name.as_ref().map(|t| t.value.clone());
    Self { ast, m_semantic_name }
  }
}
impl ast::RuleType {
  fn parse(ts: &mut TokenStream<Tok>) -> Option<Self> {
    Some(Self {
      m_kw_optional: optional!(ts, token!(TKeyword::"optional"(ts))),
      m_kw_token: optional!(ts, token!(TKeyword::"token"(ts))),
      m_id: token!(TIdentifier::parse(ts))?,
      m_token_content: optional!(ts, token!(TLiteral::parse(ts))),
    })
  }
  fn apply_semantic(&self, si: &mut SemInfo, _ovr: &Option<Sem>) {
    if let Some(m) = &self.m_kw_optional { if let Some(o) = _ovr { si.set(m, o.clone()); } }
    if let Some(m) = &self.m_kw_token { if let Some(o) = _ovr { si.set(m, o.clone()); } }
    if let Some(o) = _ovr { si.set(&self.m_id, o.clone()); }
    if let Some(m) = &self.m_token_content { if let Some(o) = _ovr { si.set(m, o.clone()); } }
  }
}
impl<'p> pt::RuleType<'p> {
  fn from_ast(ast: &'p ast::RuleType, _ctx: &mut Ctx) -> Self {
    let m_kw_optional = ast.m_kw_optional.is_some();
    let m_kw_token = ast.m_kw_token.is_some();
    let m_id = ast.m_id.value.clone();
    let m_token_content = ast.m_token_content.as_ref().map(|t| t.value.clone());
    Self {
      ast,
      m_kw_optional,
      m_kw_token,
      m_id,
      m_token_content,
    }
  }
}
impl ast::DefineTokenTypeStatement {
  fn parse(ts: &mut TokenStream<Tok>) -> Option<Self> {
    Some(Self {
      m_kw_extract: optional!(ts, token!(TKeyword::"extract"(ts))),
      m_1: token!(TKeyword::"token"(ts))?,
      m_token_type: token!(TIdentifier::parse(ts))?,
    })
  }
  fn apply_semantic(&self, si: &mut SemInfo, _ovr: &Option<Sem>) {
    if let Some(m) = &self.m_kw_extract { if let Some(o) = _ovr { si.set(m, o.clone()); } }
    if let Some(o) = _ovr { si.set(&self.m_1, o.clone()); }
    si.set(&self.m_token_type, _ovr.as_ref().cloned().unwrap_or(Sem::SToken));
  }
}
impl<'p> pt::DefineTokenTypeStatement<'p> {
  fn from_ast_internal(ast: &'p ast::DefineTokenTypeStatement, _ctx: &mut Ctx) -> Self {
    let m_kw_extract = ast.m_kw_extract.is_some();
    let m_token_type = ast.m_token_type.value.clone();
    Self { ast, m_kw_extract, m_token_type, }
  }
  #[inline] #[allow(clippy::unnecessary_mut_passed)] fn from_ast(ast: &'p ast::DefineTokenTypeStatement, ctx: &mut Ctx) -> ParseHook<TokenDef, pt::DefineTokenTypeStatement<'p>> {
    let mut pt = Self::from_ast_internal(ast, ctx);
    ParseHook { val: parse_token_def(&mut pt, ctx), pt }
  }
}
impl ast::DefineIgnoreTokenRuleStatement {
  fn parse(ts: &mut TokenStream<Tok>) -> Option<Self> {
    Some(Self { m_0: token!(TKeyword::"ignore"(ts))?, m_value: Box::new(ast::LiteralOrRegExp::parse(ts)?), })
  }
  fn apply_semantic(&self, si: &mut SemInfo, _ovr: &Option<Sem>) {
    if let Some(o) = _ovr { si.set(&self.m_0, o.clone()); }
    self.m_value.apply_semantic(si, _ovr);
  }
}
impl<'p> pt::DefineIgnoreTokenRuleStatement<'p> {
  fn from_ast_internal(ast: &'p ast::DefineIgnoreTokenRuleStatement, _ctx: &mut Ctx) -> Self {
    let m_value = Box::new(pt::LiteralOrRegExp::from_ast(ast.m_value.as_ref(), _ctx));
    Self { ast, m_value }
  }
  #[inline] #[allow(clippy::unnecessary_mut_passed)] fn from_ast(ast: &'p ast::DefineIgnoreTokenRuleStatement, ctx: &mut Ctx) -> ParseHook<TokenRule, pt::DefineIgnoreTokenRuleStatement<'p>> {
    let mut pt = Self::from_ast_internal(ast, ctx);
    ParseHook { val: parse_token_ignore_rule(&mut pt, ctx), pt }
  }
}
impl ast::DefineTokenRuleStatement {
  fn parse(ts: &mut TokenStream<Tok>) -> Option<Self> {
    Some(Self { m_token_type: token!(TIdentifier::parse(ts))?, m_value: Box::new(ast::LiteralOrRegExp::parse(ts)?), })
  }
  fn apply_semantic(&self, si: &mut SemInfo, _ovr: &Option<Sem>) {
    si.set(&self.m_token_type, _ovr.as_ref().cloned().unwrap_or(Sem::SToken));
    self.m_value.apply_semantic(si, _ovr);
  }
}
impl<'p> pt::DefineTokenRuleStatement<'p> {
  fn from_ast_internal(ast: &'p ast::DefineTokenRuleStatement, _ctx: &mut Ctx) -> Self {
    let m_token_type = ast.m_token_type.value.clone();
    let m_value = Box::new(pt::LiteralOrRegExp::from_ast(ast.m_value.as_ref(), _ctx));
    Self { ast, m_token_type, m_value, }
  }
  #[inline] #[allow(clippy::unnecessary_mut_passed)] fn from_ast(ast: &'p ast::DefineTokenRuleStatement, ctx: &mut Ctx) -> ParseHook<TokenRule, pt::DefineTokenRuleStatement<'p>> {
    let mut pt = Self::from_ast_internal(ast, ctx);
    ParseHook { val: parse_token_rule(&mut pt, ctx), pt }
  }
}
crate::impl_union!(from_ast, LiteralOrRegExp, { TokenLiteral, TokenRegExp, });
impl ast::TokenLiteral {
  fn parse(ts: &mut TokenStream<Tok>) -> Option<Self> {
    Some(Self { m_t: token!(TLiteral::parse(ts))?, })
  }
  fn apply_semantic(&self, si: &mut SemInfo, _ovr: &Option<Sem>) {
    if let Some(o) = _ovr { si.set(&self.m_t, o.clone()); }
  }
}
impl<'p> pt::TokenLiteral<'p> {
  fn from_ast(ast: &'p ast::TokenLiteral, _ctx: &mut Ctx) -> Self {
    let m_t = ast.m_t.value.clone();
    Self { ast, m_t }
  }
}
impl ast::TokenRegExp {
  fn parse(ts: &mut TokenStream<Tok>) -> Option<Self> {
    Some(Self { m_t: token!(TRegExp::parse(ts))?, })
  }
  fn apply_semantic(&self, si: &mut SemInfo, _ovr: &Option<Sem>) {
    if let Some(o) = _ovr { si.set(&self.m_t, o.clone()); }
  }
}
impl<'p> pt::TokenRegExp<'p> {
  fn from_ast(ast: &'p ast::TokenRegExp, _ctx: &mut Ctx) -> Self {
    let m_t = ast.m_t.value.clone();
    Self { ast, m_t }
  }
}
impl ast::DefineSemanticStatement {
  fn parse(ts: &mut TokenStream<Tok>) -> Option<Self> {
    Some(Self { m_0: token!(TKeyword::"semantic"(ts))?, m_id: token!(TIdentifier::parse(ts))?, })
  }
  fn apply_semantic(&self, si: &mut SemInfo, _ovr: &Option<Sem>) {
    if let Some(o) = _ovr { si.set(&self.m_0, o.clone()); }
    si.set(&self.m_id, _ovr.as_ref().cloned().unwrap_or(Sem::SSemantic));
  }
}
impl<'p> pt::DefineSemanticStatement<'p> {
  fn from_ast_internal(ast: &'p ast::DefineSemanticStatement, _ctx: &mut Ctx) -> Self {
    let m_id = ast.m_id.value.clone();
    Self { ast, m_id }
  }
  #[inline] #[allow(clippy::unnecessary_mut_passed)] fn from_ast(ast: &'p ast::DefineSemanticStatement, ctx: &mut Ctx) -> ParseHook<String, pt::DefineSemanticStatement<'p>> {
    let mut pt = Self::from_ast_internal(ast, ctx);
    ParseHook { val: parse_semantic(&mut pt, ctx), pt }
  }
}
crate::impl_union!(from_ast_internal, Expression, { ConcatExpression, DictExpression, });
impl<'p> pt::Expression<'p> {
  #[inline] #[allow(clippy::unnecessary_mut_passed)] fn from_ast(ast: &'p ast::Expression, ctx: &mut Ctx) -> ParseHook<Expr, pt::Expression<'p>> {
    let mut pt = Self::from_ast_internal(ast, ctx);
    ParseHook { val: parse_expr(&mut pt, ctx), pt }
  }
}
impl ast::ConcatExpression {
  fn parse(ts: &mut TokenStream<Tok>) -> Option<Self> {
    Some(Self { m_first: token!(TIdentifier::parse(ts))?, m_rest: Box::new(optional!(ts, ast::ConcatExpressionTail::parse(ts))), })
  }
  fn apply_semantic(&self, si: &mut SemInfo, _ovr: &Option<Sem>) {
    si.set(&self.m_first, _ovr.as_ref().cloned().unwrap_or(Sem::SVariable));
    if let Some(m) = self.m_rest.as_ref() { m.apply_semantic(si, _ovr); }
  }
}
impl<'p> pt::ConcatExpression<'p> {
  fn from_ast(ast: &'p ast::ConcatExpression, _ctx: &mut Ctx) -> Self {
    let m_first = ast.m_first.value.clone();
    let m_rest = if let Some(v) = ast.m_rest.as_ref() { Box::new(Some(pt::ConcatExpressionTail::from_ast(v, _ctx))) } else { Box::new(None) };
    let (mut asts, mut vals) = move_pt_vec_optional!(m_rest);
    vals.push_front(m_first);
    asts.push_front(&ast.m_first);
    Self { ast, asts, vals }
  }
}
impl ast::ConcatExpressionTail {
  fn parse(ts: &mut TokenStream<Tok>) -> Option<Self> {
    Some(Self { m_0: token!(TSymbol::"|"(ts))?, m_rest: Box::new(ast::ConcatExpression::parse(ts)?), })
  }
  fn apply_semantic(&self, si: &mut SemInfo, _ovr: &Option<Sem>) {
    if let Some(o) = _ovr { si.set(&self.m_0, o.clone()); }
    self.m_rest.apply_semantic(si, _ovr);
  }
}
impl<'p> pt::ConcatExpressionTail<'p> {
  fn from_ast(ast: &'p ast::ConcatExpressionTail, _ctx: &mut Ctx) -> Self {
    let m_rest = Box::new(pt::ConcatExpression::from_ast(ast.m_rest.as_ref(), _ctx));
    let (asts, vals) = move_pt_vec!(m_rest);
    Self { ast, asts, vals }
  }
}
impl ast::DictExpression {
  fn parse(ts: &mut TokenStream<Tok>) -> Option<Self> {
    Some(Self {
      m_0: token!(TSymbol::"{"(ts))?,
      m_values: Box::new(optional!(ts, ast::VariableList::parse(ts))),
      m_2: token!(TSymbol::"}"(ts))?,
    })
  }
  fn apply_semantic(&self, si: &mut SemInfo, _ovr: &Option<Sem>) {
    if let Some(o) = _ovr { si.set(&self.m_0, o.clone()); }
    if let Some(m) = self.m_values.as_ref() { m.apply_semantic(si, _ovr); }
    if let Some(o) = _ovr { si.set(&self.m_2, o.clone()); }
  }
}
impl<'p> pt::DictExpression<'p> {
  fn from_ast(ast: &'p ast::DictExpression, _ctx: &mut Ctx) -> Self {
    let m_values = if let Some(v) = ast.m_values.as_ref() { Box::new(Some(pt::VariableList::from_ast(v, _ctx))) } else { Box::new(None) };
    let (asts, vals) = move_pt_vec_optional!(m_values);
    Self { ast, asts, vals }
  }
}
impl ast::VariableList {
  fn parse(ts: &mut TokenStream<Tok>) -> Option<Self> {
    Some(Self { m_first: token!(TIdentifier::parse(ts))?, m_rest: Box::new(optional!(ts, ast::VariableListTail::parse(ts))), })
  }
  fn apply_semantic(&self, si: &mut SemInfo, _ovr: &Option<Sem>) {
    si.set(&self.m_first, _ovr.as_ref().cloned().unwrap_or(Sem::SVariable));
    if let Some(m) = self.m_rest.as_ref() { m.apply_semantic(si, _ovr); }
  }
}
impl<'p> pt::VariableList<'p> {
  fn from_ast(ast: &'p ast::VariableList, _ctx: &mut Ctx) -> Self {
    let m_first = ast.m_first.value.clone();
    let m_rest = if let Some(v) = ast.m_rest.as_ref() { Box::new(Some(pt::VariableListTail::from_ast(v, _ctx))) } else { Box::new(None) };
    let (mut asts, mut vals) = move_pt_vec_optional!(m_rest);
    vals.push_front(m_first);
    asts.push_front(&ast.m_first);
    Self { ast, asts, vals }
  }
}
impl ast::VariableListTail {
  fn parse(ts: &mut TokenStream<Tok>) -> Option<Self> {
    Some(Self { m_0: token!(TSymbol::","(ts))?, m_rest: Box::new(optional!(ts, ast::VariableList::parse(ts))), })
  }
  fn apply_semantic(&self, si: &mut SemInfo, _ovr: &Option<Sem>) {
    if let Some(o) = _ovr { si.set(&self.m_0, o.clone()); }
    if let Some(m) = self.m_rest.as_ref() { m.apply_semantic(si, _ovr); }
  }
}
impl<'p> pt::VariableListTail<'p> {
  fn from_ast(ast: &'p ast::VariableListTail, _ctx: &mut Ctx) -> Self {
    let m_rest = if let Some(v) = ast.m_rest.as_ref() { Box::new(Some(pt::VariableList::from_ast(v, _ctx))) } else { Box::new(None) };
    let (asts, vals) = move_pt_vec_optional!(m_rest);
    Self { ast, asts, vals }
  }
}
