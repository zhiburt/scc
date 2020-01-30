use std::io::Write;
use std::path::PathBuf;

use clap::Clap;

use simple_c_compiler::{checks, generator, il::tac, lexer::Lexer, parser};

mod pretty_output;

#[derive(Clap)]
#[clap(name = "scc", version = "0.0.1", about = "simple c compiler")]
struct Opt {
    #[clap(short = "v", parse(from_occurrences))]
    verbose: i32,

    #[clap(parse(from_os_str))]
    input_file: PathBuf,

    #[clap(short = "o", parse(from_os_str))]
    out_file: Option<PathBuf>,
}

fn main() {
    let opt = Opt::parse();
    let input_file = opt.input_file;
    let output_file = opt.out_file.map_or(PathBuf::from("asm.s"), |name| name);

    let program = std::fs::File::open(input_file).unwrap();
    let lexer = Lexer::new();
    let tokens = lexer.lex(program);
    let program = parser::parse(tokens).expect("Cannot parse program");

    {
        if opt.verbose > 0 {
            println!("\n{}\n", pretty_output::pretty_prog(&program));
        }
        if opt.verbose > 1 {
            let tac = tac::il(&program);
            for f in &tac {
                pretty_output::pretty_tac(std::io::stdout(), f);
            }

            println!();

            // for f in tac {
            //     println!("{}", generator::from_tac::gen(f));
            // }
        }
    }

    if !checks::function_checks::func_check(&program) {
        eprintln!("invalid function declaration or definition");
        std::process::exit(120);
    }

    let mut asm_file = std::fs::File::create(output_file).expect("Cannot create assembler code");
    asm_file
        .write_all(generator::gen(program, "main").unwrap().as_ref())
        .unwrap();
}
