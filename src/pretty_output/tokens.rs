use scc::lexer::{Token, TokenType};

pub fn pretty_tokens(tokens: &Vec<Token>) -> String {
    format!(
        "{:?}",
        tokens
            .iter()
            .map(|t| t.token_type)
            .collect::<Vec<TokenType>>()
    )
}
