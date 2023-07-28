use clap::{Parser, ValueEnum};
use regen::core::build_language;
// use regen::dynamic::DynEnv;
use regen::emit::{self, Emitter};
use regen::grammar;
use regen::sdk::{Error, ASTParser, PTParser};
use std::collections::HashMap;
use std::fs;
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

fn write_output(contents: &str, location: Option<&String>) -> Result<(), std::io::Error> {
    if let Some(location) = location {
        fs::write(location, contents)
    } else {
        println!("{}", contents);
        Ok(())
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let (errors, source) = execute()?;

    if !errors.is_empty() {
        for e in errors {
            eprintln!("{}", &e.pretty(&source, 1)?);
        }
        return Err("Errors found when parsing the input file.".into());
    }

    Ok(())
}

fn execute() -> Result<(Vec<Error>, String), Box<dyn std::error::Error>> {
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
            // Parse the language from the grammar
            let mut ctx = grammar::Ctx::default();    
            let asts = grammar::Parser::parse_ast_all(&grammar_source, &mut ctx, STACK_SIZE);
            let pts = asts.iter().map(|ast| ast.parse(&mut ctx)).collect::<Vec<_>>();

            // check for errors in the context


            let result = match env.parse_pts_then(build_language) {
                Err(_) => (env.ctx.err, grammar_source),
                Ok(lang) => {
                    // Create HTML class mapping from input
                    let mapping = class.into_iter().collect::<HashMap<_, _>>();
                    // Using the language as context, parse the source code with dynamic AST
                    let mut env = DynEnv::new_ctx(&source, Mode::All, STACK_SIZE, lang);
                    // Ignoring the generated parse trees. We only need the semantic info.
                    let _ = env.parse_pts_then(|_, _| Ok(()));
                    // Generate the highlighted source code
                    let code = env
                        .ctx
                        .tbs
                        .get_html(|token| mapping.get(&token).cloned().unwrap_or(token));

                    if env.ctx.err.is_empty() {
                        write_output(&code, output.as_ref())?;
                    }

                    (env.ctx.err, source)
                }
            };
            Ok(result)
        }
        Cli::Emit {
            grammar,
            target,
            output,
        } => {
            let path = PathBuf::from(&grammar);
            let parent_path = path.parent().unwrap();
            let source = fs::read_to_string(grammar)?;
            let mut env = Env::new_ctx(&source, Mode::All, STACK_SIZE, Default::default());

            let emitter = match target {
                SdkTarget::Rust => emit::RustEmitter::new(false, parent_path.to_path_buf()),
                SdkTarget::RustSelf => emit::RustEmitter::new(true, parent_path.to_path_buf()),
            };

            let result = match env.parse_pts_then(build_language) {
                Err(_) => (env.ctx.err, source),
                Ok(lang) => {
                    let code = emitter.emit(&lang)?;
                    write_output(&code, output.as_ref())?;
                    (vec![], source)
                }
            };
            Ok(result)
        }
    }
}
