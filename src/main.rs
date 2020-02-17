use std::io::Write;
use std::path::PathBuf;

use clap::Clap;

use simple_c_compiler::{checks, generator, il::tac, lexer::Lexer, parser};

mod pretty_output;

#[derive(Clap)]
#[clap(
    name = "scc",
    version = "0.0.2",
    about = "
        A handcrafted compiler of programs written in C to assembler language.

        To start use it pass the file name as an argument and then
        the result of compilation appears in asm.s file by default.

        It uses GASM syntax for Assembly.
    ",
    author = "Maxim Z."
)]
struct Opt {
    #[clap(short = "ast", long = "pretty-ast")]
    pretty_ast: bool,
    #[clap(short = "tac", long = "pretty-tac")]
    pretty_tac: bool,
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
    let ast = parser::parse(tokens).expect("Cannot parse program");

    if opt.pretty_ast {
        println!("\n{}", pretty_output::pretty_prog(&ast));
    }

    let tac = tac::il(&ast);
    if opt.pretty_tac {
        for f in &tac {
            println!();
            pretty_output::pretty_tac(std::io::stdout(), f);
            println!();
        }
    }

    if !checks::function_checks::func_check(&ast) {
        eprintln!("invalid function declaration or definition");
        std::process::exit(120);
    }

    let mut asm_file = std::fs::File::create(output_file).expect("Cannot create output file");
    let mut generator =
        generator::from_tac::Transit::new(generator::x64_translator::X64Backend::new());
    for f in tac {
        writeln!(asm_file, "{}", generator.gen(f)).unwrap();
    }
}
