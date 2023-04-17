use crate::sdk::generated::{Sem, Tok};
use crate::sdk::{Environment, RootParser, Semantic};

const TAG_TOKENIZE: &str = "<!-- INCLUDE_REGEN_TOKENIZE -->";
const TAG_AST_SEMANTIC: &str = "<!-- INCLUDE_REGEN_AST_SEMANTIC -->";
const TAG_FULL_SEMANTIC: &str = "<!-- INCLUDE_REGEN_FULL_SEMANTIC -->";

pub fn emit_html<E, F, S>(env: &mut E, template_html: &str, semantic_mapping: F) -> String
where
  S: Semantic + Clone,
  E: Environment<S = S> + RootParser,
  F: Fn(&S) -> String,
{
  // This is an example of processing the input step-by-step
  // If you don't care about the intermediate steps, you can directly call Env::parse_pts_then()

  // Tokenize the source code
  env.tokenize();
  // Generate the highlighted source code with tokenized info
  let tokenized_html = env.as_ctx().si.get_html(&semantic_mapping);

  // Create Abstract Syntax Tree
  // Here, it won't retokenize the source code because it's already tokenized
  // Note that we can't access AST directly
  env.parse_asts();
  // Generate the highlighted source code with AST info
  let ast_html = env.as_ctx().si.get_html(&semantic_mapping);

  // Create Parse Tree
  // Here, it won't regenerate the AST. It will use the AST we created above
  // After PT is created, it is transfered to the consumer function where you can transform it
  // into the application object. Here we are just ignoring it, as we only need the semantic info.
  let _ = env.parse_pts_then(|_| Ok(()));

  let full_html = env.as_ctx().si.get_html(&semantic_mapping);

  // Inject the html into the template
  template_html
    .replace(TAG_TOKENIZE, &tokenized_html)
    .replace(TAG_AST_SEMANTIC, &ast_html)
    .replace(TAG_FULL_SEMANTIC, &full_html)
}

pub fn to_prismjs(s: &Sem) -> String {
  // Convert the semantic info into Prism JS class names so we can use their theme CSS
  match s {
    Sem::Token(Tok::TComment) => "token comment".to_owned(),
    Sem::Token(Tok::TKeyword) => "token keyword".to_owned(),
    Sem::Token(Tok::TIdentifier) => "".to_owned(),
    Sem::Token(Tok::TRegExp) => "token regex".to_owned(),
    Sem::Token(Tok::TLiteral) => "token string".to_owned(),
    Sem::Token(Tok::TSymbol) => "token punctuation".to_owned(),
    Sem::Tag(tag, delegate) => format!("{} {}", tag, to_prismjs(delegate)),
    Sem::SToken => "token tag".to_owned(),
    Sem::SVariable => "token attr-name".to_owned(),
    Sem::SRule => "token class-name".to_owned(),
    Sem::SSemantic => "token tag".to_owned(),
    Sem::SHookName => "token function".to_owned(),
    Sem::SHookType => "token regex".to_owned(),
    _ => s.to_html_class(),
  }
}
