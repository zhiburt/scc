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
    Negation,
    BitwiseComplement,
    LogicalNegation,
    Addition,
    Multiplication,
    Division,
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
                TokenDefinition::new(TokenType::LogicalNegation, r"^!"),
                TokenDefinition::new(TokenType::Addition, r"^\+"),
                TokenDefinition::new(TokenType::Multiplication, r"^\*"),
                TokenDefinition::new(TokenType::Division, r"^/"),
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

#[derive(Debug)]
enum UnaryOp {
    Negation,
    BitwiseComplement,
    LogicalNegation,
}

impl UnaryOp {
    fn from_token_type(t: TokenType) -> Option<UnaryOp> {
        match t {
            TokenType::Negation => Some(UnaryOp::Negation),
            TokenType::LogicalNegation => Some(UnaryOp::LogicalNegation),
            TokenType::BitwiseComplement => Some(UnaryOp::BitwiseComplement),
            _ => None
        }
    }
}

enum Expression {
    Term(Term),
    BinOp(Box<Term>, BinOp, Box<Term>),
}

enum BinOp {
    Plus,
    Minus,
}

enum FactOp {
    Multiplication,
    Division,
}

enum Term {
    Fact(Factor),
    FactorOp(Box<Factor>, FactOp, Box<Factor>),
}

impl Term {
    fn parse(mut tokens: &mut Vec<Token>) -> Result<Self> {
        let fact = Factor::parse(&mut tokens)?;
        let mut term = Term::Fact(fact);
        while let Some(token) = tokens.iter().peekable().peek() {
            match token.token_type {
                TokenType::Multiplication => {
                    tokens.remove(0);
                    let next_fact = Factor::parse(&mut tokens)?;
                    let f = match term {
                        Term::Fact(fact) => fact,
                        _ => unreachable!(),
                    };
                    term = Term::FactorOp(Box::new(f) , FactOp::Multiplication, Box::new(next_fact));
                },
                TokenType::Division => {
                    tokens.remove(0);
                    let next_fact = Factor::parse(&mut tokens)?;
                    let f = match term {
                        Term::Fact(fact) => fact,
                        _ => unreachable!(),
                    };
                    term = Term::FactorOp(Box::new(f) , FactOp::Division, Box::new(next_fact));
                },
                _ => { break; },
            }
        }

        Ok(term)
    }
}

enum Factor {
    Expr(Box<Expression>),
    UnOp(UnaryOp, Box<Factor>),
    Const(isize),
}


impl Factor {
    fn parse(mut tokens: &mut Vec<Token>) -> Result<Self> {
        let mut token = tokens.remove(0);
        match token.token_type {
            TokenType::OpenParenthesis => {
                let expr = Expression::parse(tokens)?;
                token = tokens.remove(0);
                if token.token_type != TokenType::CloseParenthesis {
                    return Err(CompilerError::ParsingError);
                }
            
                Ok(Factor::Expr(Box::new(expr)))
            },
            TokenType::IntegerLiteral => {
                Ok(Factor::Const(token.val.as_ref().unwrap().parse().unwrap()))
            },
            TokenType::Negation | TokenType::LogicalNegation | TokenType::BitwiseComplement => {
                let factor = Factor::parse(&mut tokens)?;
                Ok(Factor::UnOp(UnaryOp::from_token_type(token.token_type).unwrap(), Box::new(factor)))
            }
            _ => {
                Err(CompilerError::ParsingError)
            }
        }
    }
}

impl Expression {
    fn parse(mut tokens: &mut Vec<Token>) -> Result<Self> {
        let term = Term::parse(&mut tokens)?;
        let mut expr = Expression::Term(term);
        while let Some(token) = tokens.iter().peekable().peek() {
            match token.token_type {
                TokenType::Addition => {
                    tokens.remove(0);
                    let next_term = Term::parse(&mut tokens)?;
                    let e = match expr {
                        Expression::Term(expr) => expr,
                        _ => unreachable!(),
                    };
                    expr = Expression::BinOp(Box::new(e) , BinOp::Plus, Box::new(next_term));
                },
                TokenType::Negation => {
                    tokens.remove(0);
                    let next_term = Term::parse(&mut tokens)?;
                    let e = match expr {
                        Expression::Term(expr) => expr,
                        _ => unreachable!(),
                    };
                    expr = Expression::BinOp(Box::new(e) , BinOp::Minus, Box::new(next_term));
                },
                _ => { break; },
            }
        }

        Ok(expr)
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

fn gen(p: Program) -> String {
    gen_decl(&p.0)
}

fn gen_decl(st: &Declaration) -> String {
    match st {
        Declaration::Func(name, statement) => {
            format!(r"
  .globl {}
{0}:
{}", name, gen_statement(&statement))
        },
    }
}

fn gen_statement(st: &Statement) -> String {
    match st {
        Statement::Return(expr) => {
            let mut expr_code = gen_expr(&expr);
            expr_code.push_str("\n  ret");
            expr_code
        },
    }
}

fn gen_expr(expr: &Expression) -> String {
    match expr {
        Expression::Term(term) => {
            gen_term(term)
        },
        Expression::BinOp(left_term, op, right_term) => {
            let mut fact = gen_term(left_term);
            fact.push_str("\n  push %rax");
            fact.push_str(&gen_term(right_term));
            fact.push_str("\n  pop %rcx");

            match op {
                BinOp::Plus => {
                    fact.push_str("\n  add %rcx, %rax");
                },
                BinOp::Minus => fact.push_str("\n  sub %rax, %rcx"),
            }

            fact
        },
    }
}

fn gen_term(term: &Term) -> String {
    match term {
        Term::Fact(fact) => {
            gen_fact(fact)
        },
        Term::FactorOp(left_fact, op, right_fact) => {
            let mut fact = gen_fact(left_fact);
            fact.push_str("\n  push %rax");
            fact.push_str(&gen_fact(right_fact));
            fact.push_str("\n  pop %rcx");

            match op {
                FactOp::Division => {
                    fact.push_str("\n  imul %rcx, %rax");
                },
                FactOp::Multiplication => fact.push_str("\n  imul %rcx, %rax"),
            }

            fact
        },
    }
}

fn gen_fact(fact: &Factor) -> String {
    match fact {
        Factor::Const(val) => {
            format!("\n  movl    ${}, %eax", val)
        },
        Factor::Expr(expr) => {
            gen_expr(expr)
        },
        Factor::UnOp(op, factor) => {
            let mut fact_code = gen_fact(factor);
            match op {
                UnaryOp::BitwiseComplement => {
                    fact_code.push_str("\n  not    %eax");
                },
                UnaryOp::Negation => {
                    fact_code.push_str("\n  neg    %eax");
                },
                UnaryOp::LogicalNegation => {
                    fact_code.push_str("\n  cmpl    $0, %eax");
                    fact_code.push_str("\n  movl    $0, %eax");
                    fact_code.push_str("\n  sete    %al");
                },
            }
            
            fact_code
        },
    }
}

            // match op {
            //     UnaryOp::BitwiseComplement => {
            //         expr_code.push_str("\n  not    %eax");
            //     },
            //     UnaryOp::Negation => {
            //         expr_code.push_str("\n  neg    %eax");
            //     },
            //     UnaryOp::LogicalNegation => {
            //         expr_code.push_str("\n  cmpl    $0, %eax");
            //         expr_code.push_str("\n  movl    $0, %eax");
            //         expr_code.push_str("\n  sete    %al");
            //     },
            // }

fn pretty_tokens(tokens: &Vec<Token>) -> String {
    format!(
        "{:?}",
        tokens
            .iter()
            .map(|t| t.token_type)
            .collect::<Vec<TokenType>>()
    )
}

fn pretty_program(program: &Program) -> String {
    pretty_decl(&program.0)
}

fn pretty_decl(d: &Declaration) -> String {
    match d {
        Declaration::Func(name, statement) => {
            format!("FUN {}:\n   body:\n      {}", name, pretty_statement(&statement))
        },
    }
}

fn pretty_statement(s: &Statement) -> String {
    match s {
        Statement::Return(expr) => format!("RETURN {}", pretty_expr(expr))
    }
}

fn pretty_expr(s: &Expression) -> String {
    // match s {
        // Expression::Const(val) => format!("Int<{}>", val),
        // Expression::UnOp(op, val) => format!("UnOp<{:?}> {}", op, pretty_expr(val)),
    // }
    String::from("")
}

fn main() {
    let file = std::env::args().collect::<Vec<String>>()[1].clone();
    let program = std::fs::File::open(file).unwrap();
    let lexer = Lexer::new();
    let mut tokens = lexer.lex(program).unwrap();
    
    let program = Program::parse(&mut tokens).expect("Cannot parse program");
    
    // println!("{}", pretty_program(&program));
    
    let mut asm_file = std::fs::File::create("assembly.s").expect("Cannot create assembler code");
    asm_file.write_all(gen(program).as_ref()).unwrap();
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
