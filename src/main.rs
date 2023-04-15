// #[macro_use]
// extern crate macro_pub;

use clap::ArgMatches;
use clap::{Command, Arg};

use regen::emit;


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

fn arg_template() -> Arg {
    Arg::new("TEMPLATE")
        .short('t')
        .long("template")
        .help("File name for the context file, which is interleaved with the output.")
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
    // let header = matches.get_one::<String>("HEADER").unwrap();
    // let footer = matches.get_one::<String>("FOOTER").unwrap();
    // let mut buf = String::new();
    // if !header.is_empty() {
    //     buf.push_str(&fs::read_to_string(header).unwrap());
    // }
    // buf.push_str(content);
    // if !footer.is_empty() {
    //     buf.push_str(&fs::read_to_string(footer).unwrap());
    // }
    if output.is_empty() {
        println!("{}", content);
    } else {
        fs::write(output, content).unwrap()
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
                .arg(
                    Arg::new("TARGET")
                        .short('T')
                        .long("target")
                        .help("The target language.")
                        .action(clap::ArgAction::Set)
                        .value_parser(["rs", "ts", "py"])
                        .num_args(1)
                        .default_value("rs")
                )
        )
        .subcommand(
            Command::new("html")
                .about("Highlight the input grammar file and generate HTML for the highlighted code")
                .arg(arg_input())
                .arg(arg_output())
                .arg(arg_stack())
                .arg(arg_template())
        );
        

    match matches.get_matches().subcommand() {
        Some(("sdk", matches)) => {
            emit_self()
        },
        Some(("html", matches)) => {
            let source = read_input(matches);
            let stack_size = matches.get_one::<usize>("STACK").unwrap();
            let template = matches.get_one::<String>("TEMPLATE").unwrap();
            let template_html = if template.is_empty() {
                String::from(include_str!("./emit/default.html"))
            } else {
                fs::read_to_string(template).unwrap()
            };
            let (output, errors) = emit::emit_html(&template_html, &source, *stack_size);
            write_output(&output, matches);
            if !errors.is_empty() {
                for e in errors {
                    eprintln!("{}", &e.pretty(&source, 1).unwrap());
                }
                return Err("Errors found when parsing the  file.");
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
