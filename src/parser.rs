use crate::{Token, TokenType};

use std::error;
use std::fmt;

pub type Result<T> = std::result::Result<T, CompilerError>;

#[derive(Debug)]
pub enum CompilerError {
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
pub enum UnaryOp {
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

pub enum Expression {
    Term(Term),
    BinOp(Box<Term>, BinOp, Box<Term>),
}

#[derive(Debug)]
pub enum BinOp {
    Plus,
    Minus,
}

#[derive(Debug)]
pub enum FactOp {
    Multiplication,
    Division,
}

pub enum Term {
    Fact(Factor),
    FactorOp(Box<Factor>, FactOp, Box<Factor>),
}

impl Term {
    pub fn parse(mut tokens: &mut Vec<Token>) -> Result<Self> {
        let fact = Factor::parse(&mut tokens)?;
        let mut term = Term::Fact(fact);
        while let Some(token) = tokens.iter().peekable().peek() {
            match token.token_type {
                TokenType::Multiplication => {
                    tokens.remove(0);
                    let next_fact = Factor::parse(&mut tokens)?;
                    let f = match term {
                        Term::Fact(fact) => fact,
                        _ => Factor::Expr(Box::new(Expression::Term(term))),
                    };
                    term = Term::FactorOp(Box::new(f) , FactOp::Multiplication, Box::new(next_fact));
                },
                TokenType::Division => {
                    tokens.remove(0);
                    let next_fact = Factor::parse(&mut tokens)?;
                    let f = match term {
                        Term::Fact(fact) => fact,
                        _ => Factor::Expr(Box::new(Expression::Term(term))),
                    };
                    term = Term::FactorOp(Box::new(f) , FactOp::Division, Box::new(next_fact));
                },
                _ => { break; },
            }
        }

        Ok(term)
    }
}

pub enum Factor {
    Expr(Box<Expression>),
    UnOp(UnaryOp, Box<Factor>),
    Const(isize),
}


impl Factor {
    pub fn parse(mut tokens: &mut Vec<Token>) -> Result<Self> {
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
    pub fn parse(mut tokens: &mut Vec<Token>) -> Result<Self> {
        let term = Term::parse(&mut tokens)?;
        let mut expr = Expression::Term(term);
        while let Some(token) = tokens.iter().peekable().peek() {
            match token.token_type {
                TokenType::Addition => {
                    tokens.remove(0);
                    let next_term = Term::parse(&mut tokens)?;
                    let term = match expr {
                        Expression::Term(term) => term,
                        _ => Term::Fact(Factor::Expr(Box::new(expr))),
                    };
                    expr = Expression::BinOp(Box::new(term) , BinOp::Plus, Box::new(next_term));
                },
                TokenType::Negation => {
                    tokens.remove(0);
                    let next_term = Term::parse(&mut tokens)?;
                    let term = match expr {
                        Expression::Term(term) => term,
                        _ => Term::Fact(Factor::Expr(Box::new(expr))),
                    };
                    expr = Expression::BinOp(Box::new(term) , BinOp::Minus, Box::new(next_term));
                },
                _ => { break; },
            }
        }

        Ok(expr)
    }
}

pub enum Statement {
    Return(Expression),
}

impl Statement {
    pub fn parse(tokens: &mut Vec<Token>) -> Result<Self> {
        compare_token(tokens.remove(0), TokenType::Return)?;
        let expr = Expression::parse(tokens)?;
        compare_token(tokens.remove(0), TokenType::Semicolon)?;
        Ok(Statement::Return(expr))
    }
}

pub enum Declaration {
    Func(String, Statement),
}

impl Declaration {
    pub fn parse_func_decl(mut tokens: &mut Vec<Token>) -> Result<Self> {        
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

pub struct Program(pub Declaration);

impl Program {
    pub fn parse(mut tokens: &mut Vec<Token>) -> Result<Self> {
        Ok(Program(
            Declaration::parse_func_decl(&mut tokens)?
        ))
    }
}

fn compare_token(tok: Token, tok_type: TokenType) -> Result<Token> {
    if tok.token_type == tok_type {
        Ok(tok)
    } else {
        Err(CompilerError::ParsingError)
    }
}