use regen::grammar::{Env, Tok};
use regen::sdk::{Environment, Mode, TokenType};
use std::fs;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // This example parses regen.grammar, and prints html code with prismjs classes

    // This example is ran from the `docs` directory
    let grammar_source = fs::read_to_string("../regen.grammar")?;
    // Create environment to parse the file
    let mut env = Env::new_default(&grammar_source, Mode::All, 2048);
    // Ignoring the generated parse trees. We only need the semantic info.
    let _ = env.parse_pts_then(|_, _| Ok(()));
    // Generate the highlighted source code, converting the tokens to prismjs classes
    let code = env.ctx.tbs.get_html(to_prismjs);

    if env.ctx.err.is_empty() {
        println!("{}", code);
    } else {
        for e in env.ctx.err {
            eprintln!("{}", &e.pretty(&grammar_source, 1)?);
        }
        return Err("Errors found when parsing the input file.".into());
    }

    Ok(())
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
