use regen::core::LangBuilder;
use regen::grammar::{self, Tok};
use regen::sdk::{
    ASTParser, CreateParseTree, ParseTreeResultSemantic, TokenBlocks, TokenStream, TokenType,
};
use std::fs;

fn main() {
    // This example parses regen.grammar, and prints html code with prismjs classes

    // This example is ran from the `docs` directory
    let grammar_source = fs::read_to_string("../regen.grammar").unwrap();

    // These is a wrapper regen::parse_language_from_grammar that creates
    // a Language object from a grammar string. However, it does not store
    // semantic info, so we can't use that here.

    // Run tokenizer
    let lex_output = grammar::tokenize(&grammar_source);

    // Create token stream
    // 200 is the stack size, meaning the AST can have depth <= 200
    // which is plenty. The default stack size for CLI is 2048
    let mut ts = TokenStream::new(&lex_output.tokens, 200);

    // Generate AST (need the ASTParser trait)
    let parser = grammar::Parser;
    let asts = parser.parse_ast_all(&mut ts).unwrap(); // error if syntax error

    // collect semantic info so far
    let mut outer_tbs = TokenBlocks::new(&grammar_source);
    lex_output.apply_semantic(&mut outer_tbs);
    asts.iter()
        .for_each(|ast| ast.apply_semantic(&mut outer_tbs, &None));

    // Generate PT, because it fills in additional semantic info
    // This requires a lang builder as a context, but we won't need it
    let mut lang_builder: Box<LangBuilder> = Box::default();
    for ast in &asts {
        // if you don't need semantic you can use the parse_pt method
        match ast.parse_pt_with_semantic(outer_tbs, lang_builder) {
            ParseTreeResultSemantic::Ok { /*pt*/ ctx, tbs, .. } => {
                outer_tbs = tbs;
                lang_builder = ctx;
            },
            ParseTreeResultSemantic::Err { .. /*pt, ctx, tbs, err*/ } => {
                // should not happen, but you also get the context and semantic info back here
                unreachable!();
            }
        }
    }

    // now we have the semantic info in outer_tbs, we can convert it to HTML

    let code = outer_tbs.get_html(to_prismjs);

    println!("{}", code);
}

fn to_prismjs(t: Tok) -> String {
    match t {
        Tok::TComment => "token comment".to_owned(),
        Tok::TKeyword => "token keyword".to_owned(),
        Tok::TIdentifier => "".to_owned(),
        Tok::TRegExp => "token regex".to_owned(),
        Tok::TLiteral => "token string".to_owned(),
        Tok::TSymbol => "token punctuation".to_owned(),
        Tok::SToken => "token tag".to_owned(),
        Tok::SVariable => "token attr-name".to_owned(),
        Tok::SRule => "token class-name".to_owned(),
        Tok::SSemantic => "token tag".to_owned(),
        Tok::SHookName => "token function".to_owned(),
        Tok::SHookType => "token regex".to_owned(),
        Tok::SContextType => "token tag".to_owned(),
        Tok::Decor { tag, base } => format!("{} {}", tag, to_prismjs(*base)),
        _ => t.html_class().unwrap_or_default(),
    }
}
