use super::rule::ParamType;
use crate::sdk::generated::{PTParam, PTParamList, SemInfo, Semantics};
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
/// A parameter can also be optional (e.g. param: optional Foo).
/// In total, this gives 6 different types of parameters.
/// The generated Parse Tree (PT) will have different object types depending on the parameter type.
#[derive(Debug)]
pub struct Param {
    /// Semantic type of the parameter.
    pub semantic: Option<String>,
    /// Name of the parameter.
    pub name: String,
    /// Type of the parameter (either rule type of token type)
    pub type_name: String,
    /// Is this parameter a token?
    pub is_token: bool,
    /// Is this parameter optional?
    pub is_optional: bool,
    /// If the parameter is a token, if it requires exact literal match (for example, Keyword"if")
    pub match_literal: Option<String>,
}

impl Param {
    /// Get if the parameter is in the Parse Tree (PT)
    /// A parameter is excluded from the PT if it is a required token with a literal matching
    #[inline]
    pub fn is_in_pt(&self) -> bool {
        !(self.is_token && !self.is_optional && self.match_literal.is_some())
    }

    pub fn get_type(&self) -> Option<ParamType> {
        if self.is_token {
            if let Some(_) = &self.match_literal {
                if self.is_optional {
                    // optional token Type "spec"
                    Some(ParamType::Bool)
                } else {
                    // ignore (not in pt)
                    None
                }
            } else {
                // optional? token Type
                Some(ParamType::String(self.is_optional))
            }
        } else {
            // optional? Type
            Some(ParamType::Item(self.is_optional, self.type_name.clone()))
        }
    }
}

/// Parser hook for param list
/// Removes invalid param and checks if there is duplicate param names
pub fn parse_param_list(
    pt: &mut PTParamList,
    _si: &mut SemInfo,
    errors: &mut Vec<Error>,
) -> Option<Vec<Param>> {
    // There aren't going to be many params, so it's fine to use not use a hash set for deduplication
    let mut params = Vec::new();
    for param in pt.vals.iter_mut() {
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
        if params.iter().find(|p| &p.name == name).is_some() {
            let msg = format!("Duplicate parameter name: \"{}\"", name);
            let help = "Rename the parameter or remove the duplicate.".to_string();
            errors.push(Error::from_token(&param.pt.ast.m_variable_1, msg, help));
            // Don't add duplicate params
        } else {
            params.push(param_val);
        }
    }
    Some(params)
}

/// Parser hook for param
/// Checks if type is defined and if semantic is valid
pub fn parse_param(pt: &PTParam, si: &mut SemInfo, errors: &mut Vec<Error>) -> Option<Param> {
    // Validate parameter type is defined
    let param_type = pt.m_type.as_ref().as_ref().or_else(|| {
        let msg = "Missing parameter type".to_owned();
        let help = "Add a type after \":\"".to_owned();
        errors.push(Error::from_token(&pt.ast.m___2, msg, help));
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
                errors.push(Error::from_token(&sem.ast.m___0, msg, help));

                None
            }
            x => x.as_ref(),
        });

    // At this point we can know if the type identifier is a rule or token
    // Add semantic info
    let is_token = param_type.m_kw_token;
    if let Some(ast) = pt.ast.m_type_3.as_ref() {
        if is_token {
            // Token type
            si.set(&ast.m_id_2, Semantics::SToken);
        } else {
            // Rule type
            si.set(&ast.m_id_2, Semantics::SRule);
        }
    }

    let param = Param {
        semantic: semantic.cloned(),
        name: pt.m_variable.clone(),
        type_name: param_type.m_id.clone(),
        is_token,
        is_optional: param_type.m_kw_optional,
        match_literal: param_type.m_token_content.clone(),
    };

    if !param.is_in_pt() {
        si.set(
            &pt.ast.m_variable_1,
            Semantics::Tag("unused".to_owned(), Box::new(Semantics::SVariable)),
        );
    }

    Some(param)
}

/// Convinience macro for creating a Param
macro_rules! param {
    ($identifier:literal : $type:literal) => {
        Param {
            semantic: None,
            name: $identifier.to_string(),
            type_name: $type.to_string(),
            is_token: false,
            is_optional: false,
            match_literal: None,
        }
    };
    ($identifier:literal : optional $type:literal) => {
        Param {
            semantic: None,
            name: $identifier.to_string(),
            type_name: $type.to_string(),
            is_token: false,
            is_optional: true,
            match_literal: None,
        }
    };
    ($identifier:literal : token $type:literal) => {
        Param {
            semantic: None,
            name: $identifier.to_string(),
            type_name: $type.to_string(),
            is_token: true,
            is_optional: false,
            match_literal: None,
        }
    };
    ($identifier:literal : optional token $type:literal) => {
        Param {
            semantic: None,
            name: $identifier.to_string(),
            type_name: $type.to_string(),
            is_token: true,
            is_optional: true,
            match_literal: None,
        }
    };
    ($identifier:literal : token $type:literal $spec:literal) => {
        Param {
            semantic: None,
            name: $identifier.to_string(),
            type_name: $type.to_string(),
            is_token: true,
            is_optional: false,
            match_literal: Some($spec.to_string()),
        }
    };
    ($identifier:literal : optional token $type:literal $spec:literal) => {
        Param {
            semantic: None,
            name: $identifier.to_string(),
            type_name: $type.to_string(),
            is_token: true,
            is_optional: true,
            match_literal: Some($spec.to_string()),
        }
    };
    (($semantic:literal) $identifier:literal : $type:literal) => {
        Param {
            semantic: Some($semantic.to_string()),
            name: $identifier.to_string(),
            type_name: $type.to_string(),
            is_token: false,
            is_optional: false,
            match_literal: None,
        }
    };
    (($semantic:literal) $identifier:literal : optional $type:literal) => {
        Param {
            semantic: Some($semantic.to_string()),
            name: $identifier.to_string(),
            type_name: $type.to_string(),
            is_token: false,
            is_optional: true,
            match_literal: None,
        }
    };
    (($semantic:literal) $identifier:literal : token $type:literal) => {
        Param {
            semantic: Some($semantic.to_string()),
            name: $identifier.to_string(),
            type_name: $type.to_string(),
            is_token: true,
            is_optional: false,
            match_literal: None,
        }
    };
    (($semantic:literal) $identifier:literal : optional token $type:literal) => {
        Param {
            semantic: Some($semantic.to_string()),
            name: $identifier.to_string(),
            type_name: $type.to_string(),
            is_token: true,
            is_optional: true,
            match_literal: None,
        }
    };
    (($semantic:literal) $identifier:literal : token $type:literal $spec:literal) => {
        Param {
            semantic: Some($semantic.to_string()),
            name: $identifier.to_string(),
            type_name: $type.to_string(),
            is_token: true,
            is_optional: false,
            match_literal: Some($spec.to_string()),
        }
    };
    (($semantic:literal) $identifier:literal : optional token $type:literal $spec:literal) => {
        Param {
            semantic: Some($semantic.to_string()),
            name: $identifier.to_string(),
            type_name: $type.to_string(),
            is_token: true,
            is_optional: true,
            match_literal: Some($spec.to_string()),
        }
    };
}
pub(crate) use param;
