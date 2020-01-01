mod generator;
mod lexer;
pub mod parser;
pub mod ast;
pub mod function_checks;
pub mod tac;

pub use generator::gen;
pub use lexer::{Lexer, Token, TokenType};
pub use function_checks as checks;
