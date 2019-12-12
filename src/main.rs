use std::io::Write;
use std::path::PathBuf;

use structopt::StructOpt;

use simple_c_compiler::{Lexer, parser, gen, checks};

mod pretty_output;

#[derive(Debug, StructOpt)]
#[structopt(name = "scc", about = "simple c compiler")]
struct Opt {
    #[structopt(short, long)]
    verbose: bool,

    #[structopt(parse(from_os_str))]
    input_file: PathBuf,

    #[structopt(short, parse(from_os_str))]
    out_file: Option<PathBuf>,
}

fn main() {
    let opt = Opt::from_args();
    let input_file = opt.input_file;
    let output_file = opt.out_file.map_or(PathBuf::from("asm.s"), |name| name);
    
    let program = std::fs::File::open(input_file).unwrap();
    let lexer = Lexer::new();
    let tokens = lexer.lex(program);
    let program = parser::parse(tokens).expect("Cannot parse program");
    
    if opt.verbose {
        println!("\n{}\n", pretty_output::pretty_prog(&program));
    }

    if !checks::func_check(&program) {
        eprintln!("invalid function declaration or definition");
        std::process::exit(120);
    }
    
    let mut asm_file = std::fs::File::create(output_file).expect("Cannot create assembler code");
    asm_file.write_all(gen(program, "main").unwrap().as_ref()).unwrap();
}
