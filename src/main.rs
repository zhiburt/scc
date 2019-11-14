use std::io::Write;

use simple_c_compiler::{Lexer, parser, gen};

mod pretty_output;

fn main() {
    let args = std::env::args().collect::<Vec<String>>();
    let file = args[1].clone();
    let output_file = args.get(2).map_or("asm.s", |name| name.as_ref());
    let program = std::fs::File::open(file).unwrap();
    let lexer = Lexer::new();
    let tokens = lexer.lex(program);
    let program = parser::parse(tokens).expect("Cannot parse program");
    println!("{}", pretty_output::pretty_decl(&program.0));
    let mut asm_file = std::fs::File::create(output_file).expect("Cannot create assembler code");
    asm_file.write_all(gen(program, "main").as_ref()).unwrap();
}
