use std::io::Write;

use simple_c_compiler::{gen, Lexer, parser};

mod pretty_output;

fn main() {
    let file = std::env::args().collect::<Vec<String>>()[1].clone();
    let program = std::fs::File::open(file).unwrap();
    let lexer = Lexer::new();
    let mut tokens = lexer.lex(program);
    let program = parser::Program::parse(&mut tokens).expect("Cannot parse program");
    println!("{}", pretty_output::pretty_decl(&program.0));
    let mut asm_file = std::fs::File::create("assembly.s").expect("Cannot create assembler code");
    asm_file.write_all(gen(program, "main").as_ref()).unwrap();
}
