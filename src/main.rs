use clap::{Parser, ValueEnum};
use regen::core::Context;
use regen::emit;
use regen::sdk::{
  grammar::{Ctx, Env},
  EnvMode,
};
use std::fs;
use std::path::PathBuf;

/// Command line interface
#[derive(Parser)]
#[command(
    bin_name = "regen",
    about,
    long_about = None,
    version,
    author,
    subcommand_required = true,
    arg_required_else_help = true
)]
enum Cli {
  /// Highlight the input grammar file and generate HTML for the highlighted code
  ///
  /// This command will highlight the input Regen grammar file and
  /// emit the code in HTML format. A template HTML file can be provided with
  /// comment tags `<!-- INCLUDE_REGEN_TOKENIZE -->`, `<!-- INCLUDE_REGEN_AST_SEMANTIC -->` and `<!-- INCLUDE_REGEN_FULL_SEMANTIC -->`
  /// which will be replaced with the code highlighted with different levels of semantic information.
  Html {
    #[arg(required = true)]
    /// The input grammar file name.
    input: String,
    #[arg(short = 'o', long = "output", default_value = "")]
    /// The output file name. Prints to console if empty.
    output: String,
    #[arg(short = 's', long = "stack", default_value = "2048")]
    /// The token stream stack size.
    stack_size: usize,
    #[arg(short = 't', long = "template", default_value = "")]
    /// File name for the template html file to inject the output.
    template: String,
  },
  /// Generate a SDK file for parsing the language specified by the Regen grammar file.
  Sdk {
    #[arg(required = true)]
    /// The input grammar file name.
    input: String,
    #[arg(short = 'o', long = "output", default_value = "")]
    /// The output file name. Prints to console if empty.
    output: String,
    #[arg(short = 's', long = "stack", default_value = "2048")]
    /// The token stream stack size.
    stack_size: usize,
    #[arg(short = 't', long = "target", default_value = "rust")]
    /// The target language to generate the SDK for.
    target: SdkTarget,
  },
}
#[derive(ValueEnum, Clone)]
enum SdkTarget {
  Rust,
  RustSelf,
}

fn write_output(contents: &str, location: &str) -> Result<(), std::io::Error> {
  if location.is_empty() {
    println!("{}", contents);
    Ok(())
  } else {
    fs::write(location, contents)
  }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
  let (errors, source) = match Cli::parse() {
    Cli::Html {
      input,
      output,
      stack_size,
      template,
    } => {
      let source = fs::read_to_string(input)?;
      let template_html = if template.is_empty() {
        String::from(include_str!("./emit/default.html"))
      } else {
        fs::read_to_string(template).unwrap()
      };
      let mut env = Env::new_ctx(&source, EnvMode::All, stack_size, Context::default());
      let generated_code = emit::emit_html(&mut env, &template_html, emit::to_prismjs);
      write_output(&generated_code, &output)?;
      let ctx: Ctx = env.into();
      (ctx.err, source)
    }
    Cli::Sdk {
      target,
      input,
      output,
      stack_size,
    } => {
      let path = PathBuf::from(&input);
      let parent_path = path.parent().unwrap();
      let source = fs::read_to_string(input)?;
      let mut env = Env::new_ctx(&source, EnvMode::All, stack_size, Context::default());

      let emitter = match target {
        SdkTarget::Rust => emit::RustEmitter::new(false, 2, parent_path.to_path_buf()),
        SdkTarget::RustSelf => emit::RustEmitter::new(true, 2, parent_path.to_path_buf()),
      };

      match emit::emit_sdk(&mut env, emitter)? {
        Some(code) => {
          write_output(&code, &output)?;
          (vec![], source)
        }
        None => {
          let ctx: Ctx = env.into();
          (ctx.err, source)
        }
      }
    }
  };

  if !errors.is_empty() {
    for e in errors {
      eprintln!("{}", &e.pretty(&source, 1)?);
    }
    return Err("Errors found when parsing the grammar file.".into());
  }

  Ok(())
}
