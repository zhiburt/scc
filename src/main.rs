use std::io::Write;

use simple_c_compiler::{Lexer, parser, gen, checks};

mod pretty_output;

fn main() {
    let args = std::env::args().collect::<Vec<String>>();
    let file = args[1].clone();
    let output_file = args.get(2).map_or("asm.s", |name| name.as_ref());
    let program = std::fs::File::open(file).unwrap();
    let lexer = Lexer::new();
    let tokens = lexer.lex(program);
    let program = parser::parse(tokens).expect("Cannot parse program");
    println!("\n{}\n", pretty_output::pretty_prog(&program));
    if !checks::func_check(&program) {
        println!("invalid function declaration or definition");
        std::process::exit(120);
    }
    let mut asm_file = std::fs::File::create(output_file).expect("Cannot create assembler code");
    asm_file.write_all(gen(program, "main").unwrap().as_ref()).unwrap();
}
