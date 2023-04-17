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
use crate::core::{Rule, RuleValue, Expr, Param, Hook, TokenDef, TokenRule};
use crate::core::rule::{parse_rule, parse_rule_value};
use crate::core::expr::parse_expr;
use crate::core::param::{parse_param, parse_param_list};
use crate::core::hook::parse_hook;
use crate::core::token::{parse_token_def, parse_token_ignore_rule, parse_token_rule};
use crate::core::semantic::parse_semantic;