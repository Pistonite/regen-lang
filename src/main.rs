use clap::{Parser, ValueEnum};
use regen::emit::{self, Emitter};
use regen::sdk::{ASTParser, TokenBlocks, TokenStream};
use std::collections::HashMap;
use std::fs;
use std::io;
use std::path::PathBuf;

/// Token stream stack size.
///
/// This means the AST can have at most 2048 nested levels.
/// This is a constant for the `regen` binary, but you can change this at the Env level
/// if using the library.
const STACK_SIZE: usize = 2048;

/// Command line interface
#[derive(Debug, Parser)]
#[command(
    bin_name = "regen",
    about,
    version,
    author,
    subcommand_required = true,
    arg_required_else_help = true
)]
enum Cli {
    /// Highlight code based on the input grammar
    ///
    /// This command takes in an input file and a grammar file to highlight the
    /// code with the grammar. The highlighted code is in HTML format and printed to the console.
    Html {
        /// The input grammar
        #[arg(required = true)]
        grammar: String,
        /// The input source code
        #[arg(required = true)]
        input: String,
        /// The output file name.
        ///
        /// If not provided, the output is printed to the console.
        #[arg(short, long)]
        output: Option<String>,
        /// HTML class mapping
        ///
        /// For example, --class "TComment=token comment" will convert `TComment` tokens to `<span class="token comment">`.
        /// The token type names are prefixed with `T` and the semantic names are prefixed with `S`.
        /// For token types that are not specified, the default name is used.
        #[arg(short, long, value_parser = parse_key_val::<String, String>)]
        class: Vec<(String, String)>,
    },
    /// Generate a SDK file for parsing the language specified by the Regen grammar file.
    Emit {
        /// The input grammar
        #[arg(required = true)]
        grammar: String,
        /// The output file name.
        ///
        /// If not provided, the output is printed to the console.
        #[arg(short, long)]
        output: Option<String>,
        /// The target language to generate the SDK for.
        #[arg(short = 't', long = "target", default_value = "rust")]
        target: SdkTarget,
    },
}
#[derive(Debug, ValueEnum, Clone)]
enum SdkTarget {
    Rust,
    RustSelf,
}

fn main() {
    match main_internal() {
        Ok(_) => {}
        Err(MainError::Language(errors, source)) => {
            for e in errors {
                eprintln!("{}", &e.pretty(&source, 1).unwrap());
            }
            eprintln!("error: errors found when parsing the input file.");
            std::process::exit(1);
        }
        Err(e) => {
            eprintln!("error: {}", e);
            std::process::exit(1);
        }
    }
}

fn main_internal() -> Result<(), MainError> {
    match Cli::parse() {
        Cli::Html {
            grammar,
            input,
            output,
            class,
        } => {
            // Read source code
            let source = fs::read_to_string(input)?;
            // Read source grammar
            let grammar_source = fs::read_to_string(grammar)?;
            let lang = match regen::parse_language_from_grammar(&grammar_source, STACK_SIZE) {
                Err(errors) => {
                    // if errors occur when parsing grammar, ignore the ASTs generated and return
                    // the errors
                    return Err(MainError::Language(errors, source));
                }
                Ok(lang) => lang,
            };

            // Create HTML class mapping from input
            let mapping = class.into_iter().collect::<HashMap<_, _>>();
            // Using the language as a dynamic parser, parse the source code
            let dyn_lex_output = lang.dyn_tokenize(&source);
            let mut ts = TokenStream::new(&dyn_lex_output.tokens, STACK_SIZE);
            let asts = match lang.parse_ast_all(&mut ts) {
                Err((_, errors)) => {
                    // if errors occur when parsing AST, ignore the ASTs generated and return
                    // the errors
                    return Err(MainError::Language(errors, source));
                }
                Ok(asts) => asts,
            };
            // collect semantic info
            let mut tbs = TokenBlocks::new(&source);
            // add from lexer
            dyn_lex_output.apply_semantic(&mut tbs);
            // add from AST
            for ast in &asts {
                ast.apply_semantic(&lang, &mut tbs, &None);
            }

            // Generate the highlighted source code
            let code = tbs.get_html(|token| mapping.get(&token).cloned().unwrap_or(token));

            write_output(&code, output.as_ref())?;
            Ok(())
        }

        Cli::Emit {
            grammar,
            target,
            output,
        } => {
            let path = PathBuf::from(&grammar);
            let parent_path = path.parent().unwrap();
            let source = fs::read_to_string(grammar)?;

            let lang = match regen::parse_language_from_grammar(&source, STACK_SIZE) {
                Err(errors) => {
                    // if errors occur when parsing grammar, ignore the ASTs generated and return
                    // the errors
                    return Err(MainError::Language(errors, source));
                }
                Ok(lang) => lang,
            };

            let emitter = match target {
                SdkTarget::Rust => emit::RustEmitter::new(false, parent_path.to_path_buf()),
                SdkTarget::RustSelf => emit::RustEmitter::new(true, parent_path.to_path_buf()),
            };

            let code = emitter.emit(&lang)?;
            write_output(&code, output.as_ref())?;
            Ok(())
        }
    }
}

/// Parse a single key-value pair
///
/// https://github.com/clap-rs/clap/blob/master/examples/typed-derive.rs#L48
fn parse_key_val<T, U>(
    s: &str,
) -> Result<(T, U), Box<dyn std::error::Error + Send + Sync + 'static>>
where
    T: std::str::FromStr,
    T::Err: std::error::Error + Send + Sync + 'static,
    U: std::str::FromStr,
    U::Err: std::error::Error + Send + Sync + 'static,
{
    let pos = s
        .find('=')
        .ok_or_else(|| format!("invalid KEY=value: no `=` found in `{s}`"))?;
    Ok((s[..pos].parse()?, s[pos + 1..].parse()?))
}

fn write_output(contents: &str, location: Option<&String>) -> io::Result<()> {
    if let Some(location) = location {
        fs::write(location, contents)
    } else {
        println!("{}", contents);
        Ok(())
    }
}

#[derive(Debug, thiserror::Error)]
enum MainError {
    #[error("IO error")]
    Io(#[from] io::Error),
    #[error("Emitter error")]
    Emitter(#[from] emit::EmitterError),
    #[error("Language error")]
    Language(Vec<regen::sdk::Error>, String),
}
