use regex::Regex;
use std::io::Read;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
    OpenBrace,
    CloseBrace,
    OpenParenthesis,
    CloseParenthesis,
    Semicolon,
    Return,
    Int,
    Identifier,
    IntegerLiteral,
    Negation,
    BitwiseComplement,
    LogicalNegation,
    BitwiseXor,
    BitwiseOr,
    BitwiseAnd,
    Addition,
    Multiplication,
    Division,
    Modulo,
    And,
    Or,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    BitwiseLeftShift,
    BitwiseRightShift,
    Assignment,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Token {
    pub token_type: TokenType,
    pub pos: Pos,
    pub val: Option<String>,
}

impl Token {
    pub fn is_type(&self, t: TokenType) -> bool {
        self.token_type == t
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Pos {
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

pub struct Lexer {
    definition: Vec<TokenDefinition>,
}

impl Lexer {
    pub fn new() -> Self {
        Lexer {
            definition: vec![
                TokenDefinition::new(TokenType::Int, r"^int"),
                TokenDefinition::new(TokenType::Return, r"^\breturn\b"),
                TokenDefinition::new(TokenType::Identifier, r"^[a-zA-Z]\w*"),
                TokenDefinition::new(TokenType::IntegerLiteral, r"^\d+"),
                TokenDefinition::new(TokenType::OpenParenthesis, r"^\("),
                TokenDefinition::new(TokenType::CloseParenthesis, r"^\)"),
                TokenDefinition::new(TokenType::OpenBrace, r"^\{"),
                TokenDefinition::new(TokenType::CloseBrace, r"^}"),
                TokenDefinition::new(TokenType::Semicolon, r"^;"),
                TokenDefinition::new(TokenType::Negation, r"^-"),
                TokenDefinition::new(TokenType::BitwiseComplement, r"^~"),
                TokenDefinition::new(TokenType::NotEqual, r"^!="),
                TokenDefinition::new(TokenType::LogicalNegation, r"^!"),
                TokenDefinition::new(TokenType::Addition, r"^\+"),
                TokenDefinition::new(TokenType::Multiplication, r"^\*"),
                TokenDefinition::new(TokenType::Modulo, r"^%"),
                TokenDefinition::new(TokenType::Division, r"^/"),
                TokenDefinition::new(TokenType::BitwiseLeftShift, r"^<<"),
                TokenDefinition::new(TokenType::BitwiseRightShift, r"^>>"),
                TokenDefinition::new(TokenType::And, r"^&&"),
                TokenDefinition::new(TokenType::Or, r"^\|\|"),
                TokenDefinition::new(TokenType::BitwiseAnd, r"^&"),
                TokenDefinition::new(TokenType::BitwiseOr, r"^\|"),
                TokenDefinition::new(TokenType::BitwiseXor, r"^\^"),
                TokenDefinition::new(TokenType::Equal, r"^=="),
                TokenDefinition::new(TokenType::LessThanOrEqual, r"^<="),
                TokenDefinition::new(TokenType::LessThan, r"^<"),
                TokenDefinition::new(TokenType::GreaterThanOrEqual, r"^>="),
                TokenDefinition::new(TokenType::GreaterThan, r"^>"),
                TokenDefinition::new(TokenType::Assignment, r"^="),
            ],
        }
    }

    pub fn lex<R: Read>(&self, mut reader: R) -> Vec<Token> {
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

        lexemes
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

mod tests {
    use super::*;
    use std::io::Cursor;

    #[test]
    fn bin_operators_test() {
        test_bin_op("&&", TokenType::And);
        test_bin_op("||", TokenType::Or);
        test_bin_op("==", TokenType::Equal);
        test_bin_op("!=", TokenType::NotEqual);
        test_bin_op("<", TokenType::LessThan);
        test_bin_op("<=", TokenType::LessThanOrEqual);
        test_bin_op(">", TokenType::GreaterThan);
        test_bin_op(">=", TokenType::GreaterThanOrEqual);
    }


    fn test_bin_op(op: &str, tt: TokenType) {
        let program = format!("1 {} 2", op);
        let buff = Cursor::new(program.as_bytes());
        let lexer = Lexer::new();

        let tokens = lexer.lex(buff);


        let first_int = Token {
            token_type: TokenType::IntegerLiteral,
            pos: Pos { start: 0, end: 1 },
            val: Some(String::from("1")),
        };
        let bin_op = Token {
            token_type: tt,
            pos: Pos { start: 2, end: 2 + op.len() },
            val: None,
        };
        let second_int = Token {
            token_type: TokenType::IntegerLiteral,
            pos: Pos { start: bin_op.pos.end + 1, end: bin_op.pos.end + 2 },
            val: Some(String::from("2")),
        };

        assert_eq!(
            tokens,
            vec![first_int, bin_op, second_int],
        );
    }

    #[test]
    fn default_test() {
        let program = r#"
        int main() {
            return 100;
        }"#;
        let buff = Cursor::new(program.as_bytes());
        let lexer = Lexer::new();

        let tokens = lexer.lex(buff);

        assert_eq!(
            tokens,
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
