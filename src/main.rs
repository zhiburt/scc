use std::io::Write;
use std::path::PathBuf;

use clap::Clap;

use simple_c_compiler::{
    checks, generator,
    il::{self, tac},
    lexer::Lexer,
    parser,
};

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
    #[clap(short = "lex", long = "pretty-lex")]
    pretty_lex: bool,
    #[clap(short = "ast", long = "pretty-ast")]
    pretty_ast: bool,
    #[clap(short = "tac", long = "pretty-tac")]
    pretty_tac: bool,
    #[clap(short = "O")]
    optimization: bool,
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

    if opt.pretty_lex {
        println!("\n{}", pretty_output::pretty_tokens(&tokens));
    }

    let ast = parser::parse(tokens).expect("Cannot parse program");

    if opt.pretty_ast {
        println!("\n{}", pretty_output::pretty_prog(&ast));
    }


    if !checks::function_checks::func_check(&ast) {
        eprintln!("invalid function declaration or definition");
        std::process::exit(120);
    }

    if !checks::global_vars::name_check(&ast) {
        eprintln!("global variable can not have the same name as function");
        std::process::exit(-121);
    }

    if !checks::global_vars::multi_definition(&ast) {
        eprintln!("global variable defined several times");
        std::process::exit(-122);
    }

    if !checks::global_vars::use_before_definition(&ast) {
        eprintln!("usage before declaration");
        std::process::exit(-123);
    }


    let mut tac = tac::il(&ast);
    if opt.optimization {
        tac.code = tac
            .code
            .into_iter()
            .map(|mut f| {
                il::constant_fold::fold(&mut f.instructions);
                f = il::unused_code::remove_unused(f);
                f
            })
            .collect();
    }

    if opt.pretty_tac {
        for f in &tac.code {
            println!();
            pretty_output::pretty_tac(std::io::stdout(), f);
            println!();
            let intervals =
                simple_c_compiler::il::lifeinterval::LiveIntervals::new(&f.instructions);
            writeln!(std::io::stdout(), "intervals {}\n{:?}", f.name, intervals.0).unwrap();
            println!();
        }
    }

    let mut asm_file = std::fs::File::create(output_file).expect("Cannot create output file");
    writeln!(asm_file, "{}", generator::gen(tac)).unwrap();
}
