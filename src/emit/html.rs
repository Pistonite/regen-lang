use crate::sdk::generated::{Semantics, Tokens, PTTopLevelStatement};
use crate::sdk::{tokenize, Error, ISemantics};

#[macro_export]
macro_rules! emit_html {
    (
        $PTTarget:ty, $tokenize:ident, $template_html:expr, $source:expr, $stack_size:expr, $semantic_mapping:expr
    ) => {{
        const TAG_TOKENIZE: &str = "<!-- INCLUDE_REGEN_TOKENIZE -->";
        const TAG_AST_SEMANTIC: &str = "<!-- INCLUDE_REGEN_AST_SEMANTIC -->";
        const TAG_FULL_SEMANTIC: &str = "<!-- INCLUDE_REGEN_FULL_SEMANTIC -->";
        // Tokenize the source code
        let ctx = $tokenize($source);
        // Generate the highlighted source code with tokenized info
        let tokenized_html = ctx.si.get_html($source, $semantic_mapping);

        // Create Abstract Syntax Tree
        // Here we use ast_all_unchecked because we are just highlighting the source code.
        // We will print all the errors at the end.
        let mut ast = ctx.ast_all_unchecked($stack_size);
        // Generate the highlighted source code with AST info
        let ast_html = ast.si.get_html($source, $semantic_mapping);

        // Create Parse Tree
        ast.parse_unchecked::<$PTTarget>();
        // Generate the highlighted source code with AST and PT info (most detailed)
        let full_html = ast.si.get_html($source, $semantic_mapping);

        // Inject the html into the template
        let output = $template_html
            .replace(TAG_TOKENIZE, &tokenized_html)
            .replace(TAG_AST_SEMANTIC, &ast_html)
            .replace(TAG_FULL_SEMANTIC, &full_html);

        (output, ast.err)
    }};
}
pub fn emit_html(template_html: &str, source: &str, stack_size: usize) -> (String, Vec<Error>) {
    
    emit_html!(
        PTTopLevelStatement,
        tokenize,
        template_html,
        source,
        stack_size,
        semantic_mapping
    )
}

fn semantic_mapping(s: &Semantics) -> String {
    // Convert the semantic info into Prism JS class names so we can use their theme CSS
    match s {
        Semantics::Token(Tokens::TComment) => "token comment".to_owned(),
        Semantics::Token(Tokens::TKeyword) => "token keyword".to_owned(),
        Semantics::Token(Tokens::TIdentifier) => "".to_owned(),
        Semantics::Token(Tokens::TRegExp) => "token regex".to_owned(),
        Semantics::Token(Tokens::TLiteral) => "token string".to_owned(),
        Semantics::Token(Tokens::TSymbol) => "token punctuation".to_owned(),
        Semantics::Tag(tag, delegate) => format!("{} {}", tag, semantic_mapping(delegate)),
        Semantics::SToken => "token tag".to_owned(),
        Semantics::SVariable => "token attr-name".to_owned(),
        Semantics::SRule => "token class-name".to_owned(),
        Semantics::SSemantic => "token tag".to_owned(),
        Semantics::SHookName => "token function".to_owned(),
        Semantics::SHookType => "token class-name".to_owned(),
        _ => s.to_html_class()
    }
}