use regex::Regex;
use std::io::{Read, Write};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TokenType {
    OpenBrace,
    CloseBrace,
    OpenParenthesis,
    CloseParenthesis,
    Semicolon,
    Return,
    Int,
    Identifier,
    IntegerLiteral,
}

#[derive(Debug, PartialEq, Eq)]
struct Token {
    token_type: TokenType,
    pos: Pos,
    val: Option<String>,
}

#[derive(Debug, PartialEq, Eq)]
struct Pos {
    start: usize,
    end: usize,
}

struct TokenDefinition {
    token: TokenType,
    regex: Regex,
}

impl TokenDefinition {
    fn new(token: TokenType, regex: &str) -> Self {
        TokenDefinition {
            regex: Regex::new(regex).unwrap(),
            token,
        }
    }

    fn check<'a>(&self, text: &'a str) -> Option<TokenMatch<'a>> {
        match self.regex.find(text) {
            Some(m) => Some(TokenMatch {
                token: self.token,
                value: &text[m.start()..m.end()],
                pos: Pos {
                    start: m.start(),
                    end: m.end(),
                },
                remainingText: &text[m.end()..],
            }),
            _ => None,
        }
    }
}

struct TokenMatch<'a> {
    token: TokenType,
    value: &'a str,
    pos: Pos,
    remainingText: &'a str,
}

struct Lexer {
    definition: Vec<TokenDefinition>,
}

impl Lexer {
    fn new() -> Self {
        Lexer {
            definition: vec![
                TokenDefinition::new(TokenType::Int, r"^int"),
                TokenDefinition::new(TokenType::Return, r"^return\s+"),
                TokenDefinition::new(TokenType::Identifier, r"^[a-zA-Z]\w*"),
                TokenDefinition::new(TokenType::IntegerLiteral, r"^\d+"),
                TokenDefinition::new(TokenType::OpenParenthesis, r"^\("),
                TokenDefinition::new(TokenType::CloseParenthesis, r"^\)"),
                TokenDefinition::new(TokenType::OpenBrace, r"^\{"),
                TokenDefinition::new(TokenType::CloseBrace, r"^}"),
                TokenDefinition::new(TokenType::Semicolon, r"^;"),
            ],
        }
    }

    fn lex<R: Read>(&self, mut reader: R) -> Result<Vec<Token>> {
        let mut file = String::new();
        reader.read_to_string(&mut file).unwrap();

        let mut lexemes = Vec::new();
        let mut remain_text = file.as_str();
        let mut offset = 0;
        while !remain_text.is_empty() {
            match self.find_match(&remain_text) {
                Some(m) => {
                    remain_text = m.remainingText;

                    let mut token = Lexer::create_token_from_match(m);
                    token.pos.start += offset;
                    token.pos.end += offset;
                    offset = token.pos.end;

                    lexemes.push(token);
                }
                None => {
                    remain_text = &remain_text[1..];
                    offset += 1;
                }
            }
        }

        Ok(lexemes)
    }

    fn find_match<'a>(&self, text: &'a str) -> Option<TokenMatch<'a>> {
        for def in &self.definition {
            if let Some(m) = def.check(text) {
                return Some(m);
            }
        }

        None
    }

    fn create_token_from_match(m: TokenMatch) -> Token {
        let mut token = Token {
            pos: m.pos,
            token_type: m.token,
            val: None,
        };
        match m.token {
            TokenType::Identifier | TokenType::IntegerLiteral => {
                token.val = Some(m.value.to_owned())
            }
            _ => (),
        }

        token
    }
}

use std::error;
use std::fmt;

type Result<T> = std::result::Result<T, CompilerError>;

#[derive(Debug)]
enum CompilerError {
    ParsingError,
}

impl fmt::Display for CompilerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "invalid first item to double")
    }
}

impl error::Error for CompilerError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

enum Expression {
    Const(isize),
}

impl Expression {
    fn parse(tokens: &mut Vec<Token>) -> Result<Self> {
        let int_constant = compare_token(tokens.remove(0), TokenType::IntegerLiteral)?;
        Ok(Expression::Const(int_constant.val.as_ref().unwrap().parse().unwrap()))
    }
}

enum Statement {
    Return(Expression),
}

impl Statement {
    fn parse(tokens: &mut Vec<Token>) -> Result<Self> {
        compare_token(tokens.remove(0), TokenType::Return)?;
        let expr = Expression::parse(tokens)?;
        compare_token(tokens.remove(0), TokenType::Semicolon)?;
        Ok(Statement::Return(expr))
    }
}

enum Declaration {
    Func(String, Statement),
}

impl Declaration {
    fn parse_func_decl(mut tokens: &mut Vec<Token>) -> Result<Self> {        
        compare_token(tokens.remove(0), TokenType::Int)?;
        let func_name = compare_token(tokens.remove(0), TokenType::Identifier)?;
        compare_token(tokens.remove(0), TokenType::OpenParenthesis)?;
        compare_token(tokens.remove(0), TokenType::CloseParenthesis)?;
        compare_token(tokens.remove(0), TokenType::OpenBrace)?;
        let body = Statement::parse(&mut tokens)?;
        compare_token(tokens.remove(0), TokenType::CloseBrace)?;


        Ok(Declaration::Func(func_name.val.unwrap().clone(), body))
    }
}

struct Program(Declaration);

impl Program {
    fn parse(mut tokens: &mut Vec<Token>) -> Result<Self> {
        Ok(Program(
            Declaration::parse_func_decl(&mut tokens)?
        ))
    }
}

fn check_token(tok: Option<&Token>) -> Result<&Token> {
    tok.map_or(Err(CompilerError::ParsingError), |tok| Ok(tok))
}

fn compare_token(tok: Token, tok_type: TokenType) -> Result<Token> {
    if tok.token_type == tok_type {
        Ok(tok)
    } else {
        Err(CompilerError::ParsingError)
    }
}

fn main() {
    let c_file = std::fs::File::open("main.c1").unwrap();
    let lexer = Lexer::new();
    println!(
        "{:?}",
        lexer
            .lex(c_file)
            .unwrap()
            .iter()
            .map(|t| t.token_type)
            .collect::<Vec<TokenType>>()
    );
}

mod tests {
    use super::*;
    use std::io::Cursor;
    #[test]
    fn default_test() {
        let program = r#"
        int main() {
            return 100;
        }"#;
        let buff = Cursor::new(program.as_bytes());
        let lexer = Lexer::new();

        let tokens = lexer.lex(buff);

        assert!(tokens.is_ok());
        assert_eq!(
            tokens.unwrap(),
            vec![
                Token {
                    token_type: TokenType::Int,
                    pos: Pos { start: 9, end: 12 },
                    val: None
                },
                Token {
                    token_type: TokenType::Identifier,
                    pos: Pos { start: 13, end: 17 },
                    val: Some("main".to_owned())
                },
                Token {
                    token_type: TokenType::OpenParenthesis,
                    pos: Pos { start: 17, end: 18 },
                    val: None
                },
                Token {
                    token_type: TokenType::CloseParenthesis,
                    pos: Pos { start: 18, end: 19 },
                    val: None
                },
                Token {
                    token_type: TokenType::OpenBrace,
                    pos: Pos { start: 20, end: 21 },
                    val: None
                },
                Token {
                    token_type: TokenType::Return,
                    pos: Pos { start: 34, end: 40 },
                    val: None
                },
                Token {
                    token_type: TokenType::IntegerLiteral,
                    pos: Pos { start: 41, end: 44 },
                    val: Some("100".to_owned())
                },
                Token {
                    token_type: TokenType::Semicolon,
                    pos: Pos { start: 44, end: 45 },
                    val: None
                },
                Token {
                    token_type: TokenType::CloseBrace,
                    pos: Pos { start: 54, end: 55 },
                    val: None
                }
            ]
        );
    }
}
