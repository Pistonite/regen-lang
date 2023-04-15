// #[macro_use]
// extern crate macro_pub;

use clap::ArgMatches;
use clap::{Command, Arg};

use regen::core;


use std::fs;

fn arg_output() -> Arg {
    Arg::new("OUTPUT")
        .short('o')
        .long("output")
        .help("The output file name. Prints to console by default.")
        .action(clap::ArgAction::Set)
        .num_args(1)
        .default_value("")
}

fn arg_stack() -> Arg {
    Arg::new("STACK")
        .short('s')
        .long("stack")
        .help("The token stream stack size.")
        .value_parser(clap::value_parser!(usize))
        .action(clap::ArgAction::Set)
        .num_args(1)
        .default_value("2048")
}

fn arg_input() -> Arg {
    Arg::new("INPUT")
        .help("The input grammar file name.")
        .action(clap::ArgAction::Set)
        .num_args(1)
        .required(true)
}

fn arg_header() -> Arg {
    Arg::new("HEADER")
        .long("header")
        .help("Include the content of a header file in the output before the code")
        .action(clap::ArgAction::Set)
        .num_args(1)
        .default_value("")
}

fn arg_footer() -> Arg {
    Arg::new("FOOTER")
        .long("footer")
        .help("Include the content of a footer file in the output after the code")
        .action(clap::ArgAction::Set)
        .num_args(1)
        .default_value("")
}

fn read_input(matches: &ArgMatches) -> String {
    let input = matches.get_one::<String>("INPUT").unwrap();
    return fs::read_to_string(input).unwrap();
}

fn write_output(content: &str, matches: &ArgMatches) {
    let output = matches.get_one::<String>("OUTPUT").unwrap();
    let header = matches.get_one::<String>("HEADER").unwrap();
    let footer = matches.get_one::<String>("FOOTER").unwrap();
    let mut buf = String::new();
    if !header.is_empty() {
        buf.push_str(&fs::read_to_string(header).unwrap());
    }
    buf.push_str(content);
    if !footer.is_empty() {
        buf.push_str(&fs::read_to_string(footer).unwrap());
    }
    if output.is_empty() {
        println!("{}", buf);
    } else {
        fs::write(output, buf).unwrap()
    }
}

fn main() -> Result<(), &'static str> {
   

    let matches = Command::new("regen-lang")
        .bin_name("regen")
        .about("hello world regen")
        .version("0.0.1")
        .author("iTNTPiston")
        .subcommand_required(true)
        .arg_required_else_help(true)
        .subcommand(
            Command::new("sdk")
                .about("Generate a language SDK based on the input grammar file")
                .arg(arg_input())
                .arg(arg_output())
                .arg(arg_stack())
                .arg(arg_header())
                .arg(arg_footer())
                .arg(
                    Arg::new("TARGET")
                        .short('t')
                        .long("target")
                        .help("The target language.")
                        .action(clap::ArgAction::Set)
                        .value_parser(["rs", "self", "ts", "py"])
                        .num_args(1)
                        .default_value("rs")
                )
        )
        .subcommand(
            Command::new("tokenize")
                .about("Tokenize the input grammar file (does not include semantic analysis)")
                .arg(arg_input())
                .arg(arg_output())
                .arg(arg_header())
                .arg(arg_footer())
        )
        .subcommand(
            Command::new("highlight")
                .about("Highlight the input grammar file (includes semantic analysis)")
                .arg(arg_input())
                .arg(arg_output())
                .arg(arg_header())
                .arg(arg_footer())
                .arg(arg_stack())
        );

    match matches.get_matches().subcommand() {
        Some(("sdk", matches)) => {
            emit_self()
        },
        Some(("tokenize", matches)) => {
            let input = read_input(matches);
            let ctx = regen::sdk::tokenize(&input);
            let mut output = ctx.si.get_html();
            output.push('\n');
            write_output(&output, matches);
        },
        Some(("highlight", matches)) => {
            let input = read_input(matches);
            let stack = matches.get_one::<usize>("STACK").unwrap();
            let mut ctx = regen::sdk::tokenize(&input).ast_all_unchecked(*stack);
            let pt = ctx.parse_unchecked();
            let lang: Result<core::lang::RegenLangDef, Vec<regen::sdk::RegenError>> = pt.try_into();
            if let Err(mut e) = lang {
                ctx.err.append(&mut e);
            }
            let mut output = ctx.si.get_html();
            output.push('\n');
            write_output(&output, matches);
            if !ctx.err.is_empty() {
                for e in ctx.err {
                    e.pretty_print(&input, 2);
                }
                return Err("Errors found when parsing the input file.");
            }
        },
        _ => {}
    }
        
    Ok(())

    // let args: Vec<String> = env::args().collect();

    // if args.len() == 1 {
    //     emit_self()
    // } else {
    //     test()
    // }
}

fn emit_self() {
    let mut lang = regen::core::LangDef::make_test();
    lang.emit_rust();
}
