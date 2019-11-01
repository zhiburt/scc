mod generator;
mod lexer;
mod parser;

pub use generator::gen;
pub use lexer::{Lexer, Token, TokenType};
pub use parser::{
    BinOp, Declaration, Expression, FactOp, Factor, Program, Result, Statement, Term, UnaryOp,
};
