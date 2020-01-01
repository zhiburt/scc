mod decl;
mod tokens;
mod tac;

pub use decl::{pretty_func, pretty_prog};
pub use tokens::pretty_tokens;
pub use tac::{pretty as pretty_tac};