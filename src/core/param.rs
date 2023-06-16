//! Core logic for parameters
use std::fmt::Display;

use crate::core::ExtractFromLiteral;
use crate::grammar::{pt, Ctx, Tok};
use crate::sdk::Error;

/// Parameter of a rule derivation.
///
/// The parameters are used as derivation steps when generating the Abstract Syntax Tree (AST).
/// When generating the AST, they are processed in the order they are defined.
/// They are also used to generate the Parse Tree (PT). The rule's custom body defines what the Parse Tree looks like.
///
/// A parameter consistens of:
/// - Optional semantic annotation
/// - Name of the parameter
/// - Type of the parameter
///
/// The type can be:
/// - Another rule Foo (e.g. param: Foo)
/// - A token type Bar (e.g. param: token Bar)
/// - A token type that also requires the content to match (e.g. param token Keyword "if")
///
/// A parameter can also be optional (e.g. param: optional Foo) or a list (e.g. param: Foo+), or both.
/// An optional list can be empty while a required list needs to have at least 1 element.
///
/// The generated Parse Tree (PT) will have different object types depending on the parameter type.
#[derive(Debug, Clone)]
pub struct Param {
  /// Semantic type of the parameter.
  pub semantic: Option<String>,
  /// Name of the parameter.
  pub name: String,
  /// Type name of the parameter (either rule type of token type)
  pub type_name: String,
  /// Internal type representation of the parameter
  pub param_type: ParamType,
}

impl Param {
  /// Get if the parameter is in the Parse Tree (PT)
  /// A parameter is excluded from the PT if it is a single, required token with a literal matching
  #[inline]
  pub fn is_in_pt(&self) -> bool {
    !matches!(self.param_type.data, ParamDataType::Flag(_))
      || !matches!(self.param_type.decor, ParamDecorType::None)
  }
}

/// Parser hook for param
///
/// Checks if type is defined and if semantic is valid
pub fn parse_param(pt: &pt::Parameter, ctx: &mut Ctx) -> Option<Param> {
  // Validate parameter type is defined
  let param_type = pt.m_type.as_ref().or_else(|| {
    let msg = "Missing parameter type".to_owned();
    let help = "Add a type after \":\"".to_owned();
    ctx.err.push(Error::from_token(&pt.ast.m_2, msg, help));
    None
  })?;

  // get semantic annotation
  // valid only if the syntax exists and contains an identifier
  // It's ok if semantic is invalid. We add an error and treat it as no semantic
  let semantic = pt
    .m_sem_attr
    .as_ref()
    .as_ref()
    .and_then(|sem| match &sem.m_semantic_name {
      None => {
        let msg = "Missing semantic annotation".to_owned();
        let help = "Add semantic annotation inside \"()\"".to_owned();
        ctx.err.push(Error::from_token(&sem.ast.m_0, msg, help));

        None
      }
      x => x.as_ref(),
    });

  // At this point we can know if the type identifier is a rule or token
  // Add semantic info
  let is_token = param_type.m_kw_token;
  if let Some(ast) = pt.ast.m_type.as_ref() {
    if is_token {
      // Token type
      ctx.tbs.set(&ast.m_id, Tok::SToken);
    } else {
      // Rule type
      ctx.tbs.set(&ast.m_id, Tok::SRule);
    }
  }

  let param_t = ParamType {
    decor: if param_type.m_is_list {
      ParamDecorType::Vec(param_type.m_kw_optional)
    } else if param_type.m_kw_optional {
      ParamDecorType::Optional
    } else {
      ParamDecorType::None
    },
    data: if is_token {
      match &param_type.m_token_content {
        None => ParamDataType::String,
        Some(content) => ParamDataType::Flag(content.strip_quotes()),
      }
    } else {
      ParamDataType::Rule
    },
  };

  let param = Param {
    semantic: semantic.cloned(),
    name: pt.m_variable.clone(),
    type_name: param_type.m_id.clone(),
    param_type: param_t,
    // is_token,
    // is_optional: param_type.m_kw_optional,
    // match_literal: param_type
    //   .m_token_content
    //   .as_ref()
    //   .map(|x| x.strip_quotes()),
    // is_list: param_type.m_is_list,
  };

  if !param.is_in_pt() {
    ctx.tbs.set(
      &pt.ast.m_variable,
      Tok::Decor {
        tag: "unused".to_owned(),
        base: Box::new(Tok::SVariable),
      },
    );
  }

  Some(param)
}

/// The parameter type.
///
/// This is parsed from the type definition in the language as follows. `TYPE` refer to any identifier:
///
/// |Type|Data|Decoration|Parse Tree Type (Rust)|
/// |-|-|-|-|
/// |`TYPE`|`Rule`|`None`|`TYPE`|
/// |`optional TYPE`|`Rule`|`Optional`|`Option<TYPE>`|
/// |`TYPE+`|`Rule`|`Vec(false)`|`Vec<TYPE>`|
/// |`optional TYPE+`|`Rule`|`Vec(true)`|`Vec<TYPE>`|
/// |`token TYPE`|`String`|`None`|`String`
/// |`optional token TYPE`|`String`|`Optional`|`Option<String>`|
/// |`token TYPE+`|`String`|`Vec(false)`|`Vec<String>`|
/// |`optional token TYPE+`|`String`|`Vec(true)`|`Vec<String>`|
/// |`token TYPE"literal"`|`Flag`|`None`|N/A|
/// |`optional token TYPE"literal"`|`Flag`|`Optional`|`bool`|
/// |`token TYPE"literal"+`|`Flag`|`Vec(false)`|`usize`|
/// |`optional token TYPE"literal"+`|`Flag`|`Vec(true)`|`usize`|
#[derive(Debug, Clone, PartialEq)]
pub struct ParamType {
  pub decor: ParamDecorType,
  pub data: ParamDataType,
}

/// The decoration type of a parameter
#[derive(Debug, Clone, PartialEq)]
pub enum ParamDecorType {
  None,
  /// The parameter is optional
  Optional,
  /// The parameter is a list (defined with `+`)
  ///
  /// Optional and required lists both use this type.
  Vec(bool /* optional */),
}

/// The data type of a parameter
#[derive(Debug, Clone, PartialEq)]
pub enum ParamDataType {
  /// The parameter type is another rule
  Rule,
  /// The parameter type is a token without literal match
  String,
  /// The parameter type is a token with literal match
  Flag(String /* match_literal */),
}

impl Display for ParamDataType {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      ParamDataType::Rule => write!(f, "rule"),
      ParamDataType::String => write!(f, "string"),
      ParamDataType::Flag(literal) => write!(f, "flag<{literal}>"),
    }
  }
}
