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
            _ => None,
        }
    }
}

pub struct Expression(pub LogicalAndExpr, pub Option<LogicalAndExpr>);

pub struct LogicalAndExpr(pub EqualityExpr, pub Option<EqualityExpr>);

impl LogicalAndExpr {
    pub fn parse(mut tokens: &mut Vec<Token>) -> Result<Self> {
        let eq_expr = EqualityExpr::parse(&mut tokens)?;
        let mut expr = LogicalAndExpr(eq_expr, None);
        while let Some(token) = tokens.iter().peekable().peek() {
            if token.token_type != TokenType::And {
                break;
            }

            tokens.remove(0);
            let next = EqualityExpr::parse(&mut tokens)?;
            match expr.1 {
                Some(_) => {
                    expr = LogicalAndExpr(
                        EqualityExpr(
                            RelationalExpr(
                                AdditiveExpr(
                                    BitwiseExpr(
                                        Term::Fact(Factor::Expr(Box::new(Expression(expr, None)))),
                                        None,
                                    ),
                                    None,
                                ),
                                None,
                            ),
                            None,
                        ),
                        Some(next),
                    );
                }
                None => {
                    expr.1 = Some(next);
                }
            }
        }

        Ok(expr)
    }
}

pub struct EqualityExpr(pub RelationalExpr, pub Option<(EqualityOp, RelationalExpr)>);

#[derive(Debug)]
pub enum EqualityOp {
    Equal,
    NotEqual,
}

impl EqualityExpr {
    pub fn parse(mut tokens: &mut Vec<Token>) -> Result<Self> {
        let eq_expr = RelationalExpr::parse(&mut tokens)?;
        let mut expr = EqualityExpr(eq_expr, None);
        while let Some(token) = tokens.iter().peekable().peek() {
            let op = match token.token_type {
                TokenType::Equal => EqualityOp::Equal,
                TokenType::NotEqual => EqualityOp::NotEqual,
                _ => break,
            };

            tokens.remove(0);
            let next = RelationalExpr::parse(&mut tokens)?;
            match expr.1 {
                Some(_) => {
                    expr = EqualityExpr(
                        RelationalExpr(
                            AdditiveExpr(
                                BitwiseExpr(
                                    Term::Fact(Factor::Expr(Box::new(Expression(
                                        LogicalAndExpr(expr, None),
                                        None,
                                    )))),
                                    None,
                                ),
                                None,
                            ),
                            None,
                        ),
                        Some((op, next)),
                    );
                }
                None => {
                    expr.1 = Some((op, next));
                }
            }
        }

        Ok(expr)
    }
}

pub struct RelationalExpr(pub AdditiveExpr, pub Option<(RelationalOp, AdditiveExpr)>);

#[derive(Debug)]
pub enum RelationalOp {
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,
}

impl RelationalExpr {
    pub fn parse(mut tokens: &mut Vec<Token>) -> Result<Self> {
        let eq_expr = AdditiveExpr::parse(&mut tokens)?;
        let mut expr = RelationalExpr(eq_expr, None);
        while let Some(token) = tokens.iter().peekable().peek() {
            let op = match token.token_type {
                TokenType::LessThan => RelationalOp::Less,
                TokenType::LessThanOrEqual => RelationalOp::LessOrEqual,
                TokenType::GreaterThan => RelationalOp::Greater,
                TokenType::GreaterThanOrEqual => RelationalOp::GreaterOrEqual,
                _ => break,
            };

            tokens.remove(0);
            let next = AdditiveExpr::parse(&mut tokens)?;
            match expr.1 {
                Some(_) => {
                    expr = RelationalExpr(
                        AdditiveExpr(BitwiseExpr(
                            Term::Fact(Factor::Expr(Box::new(Expression(
                                LogicalAndExpr(EqualityExpr(expr, None), None),
                                None,
                            )))), None),
                            None,
                        ),
                        Some((op, next)),
                    )
                }
                None => expr.1 = Some((op, next)),
            }
        }

        Ok(expr)
    }
}

pub struct AdditiveExpr(pub BitwiseExpr, pub Option<(BinOp, BitwiseExpr)>);

impl AdditiveExpr {
    pub fn parse(mut tokens: &mut Vec<Token>) -> Result<Self> {
        let term = BitwiseExpr::parse(&mut tokens)?;
        let mut expr = AdditiveExpr(term, None);
        while let Some(token) = tokens.iter().peekable().peek() {
            let op = match token.token_type {
                TokenType::Negation => BinOp::Minus,
                TokenType::Addition => BinOp::Plus,
                _ => break,
            };

            tokens.remove(0);
            let next = BitwiseExpr::parse(&mut tokens)?;
            match expr.1 {
                Some(_) => {
                    expr = AdditiveExpr(
                        BitwiseExpr(Term::Fact(Factor::Expr(Box::new(Expression(
                            LogicalAndExpr(EqualityExpr(RelationalExpr(expr, None), None), None),
                            None,
                        )))), None),
                        Some((op, next)),
                    )
                }
                None => expr.1 = Some((op, next)),
            }
        }

        Ok(expr)
    }
}

pub struct BitwiseExpr(pub Term, pub Option<(BitwiseOp, Term)>);

impl BitwiseExpr {
    pub fn parse(mut tokens: &mut Vec<Token>) -> Result<Self> {
        let term = Term::parse(&mut tokens)?;
        let mut expr = BitwiseExpr(term, None);
        while let Some(token) = tokens.iter().peekable().peek() {
            let op = match token.token_type {
                TokenType::BitwiseLeftShift => BitwiseOp::LeftShift,
                TokenType::BitwiseRightShift => BitwiseOp::RightShift,
                _ => break,
            };

            tokens.remove(0);
            let next = Term::parse(&mut tokens)?;
            match expr.1 {
                Some(_) => {
                    expr = BitwiseExpr(
                        Term::Fact(Factor::Expr(Box::new(Expression(
                            LogicalAndExpr(EqualityExpr(RelationalExpr(AdditiveExpr(expr, None), None), None), None),
                            None,
                        )))),
                        Some((op, next)),
                    )
                }
                None => expr.1 = Some((op, next)),
            }
        }

        Ok(expr)
    }
}

#[derive(Debug)]
pub enum BitwiseOp {
    LeftShift,
    RightShift,
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
    Modulo,
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
                        _ => Factor::Expr(Box::new(Expression(
                            LogicalAndExpr(
                                EqualityExpr(RelationalExpr(AdditiveExpr(BitwiseExpr(term, None), None), None), None),
                                None,
                            ),
                            None,
                        ))),
                    };
                    term = Term::FactorOp(Box::new(f), FactOp::Multiplication, Box::new(next_fact));
                }
                TokenType::Division => {
                    tokens.remove(0);
                    let next_fact = Factor::parse(&mut tokens)?;
                    let f = match term {
                        Term::Fact(fact) => fact,
                        _ => Factor::Expr(Box::new(Expression(
                            LogicalAndExpr(
                                EqualityExpr(RelationalExpr(AdditiveExpr(BitwiseExpr(term, None), None), None), None),
                                None,
                            ),
                            None,
                        ))),
                    };
                    term = Term::FactorOp(Box::new(f), FactOp::Division, Box::new(next_fact));
                }
                TokenType::Modulo => {
                    tokens.remove(0);
                    let next_fact = Factor::parse(&mut tokens)?;
                    let f = match term {
                        Term::Fact(fact) => fact,
                        _ => Factor::Expr(Box::new(Expression(
                            LogicalAndExpr(
                                EqualityExpr(RelationalExpr(AdditiveExpr(BitwiseExpr(term, None), None), None), None),
                                None,
                            ),
                            None,
                        ))),
                    };
                    term = Term::FactorOp(Box::new(f), FactOp::Modulo, Box::new(next_fact));
                }
                _ => {
                    break;
                }
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
            }
            TokenType::IntegerLiteral => {
                Ok(Factor::Const(token.val.as_ref().unwrap().parse().unwrap()))
            }
            TokenType::Negation | TokenType::LogicalNegation | TokenType::BitwiseComplement => {
                let factor = Factor::parse(&mut tokens)?;
                Ok(Factor::UnOp(
                    UnaryOp::from_token_type(token.token_type).unwrap(),
                    Box::new(factor),
                ))
            }
            _ => Err(CompilerError::ParsingError),
        }
    }
}

impl Expression {
    pub fn parse(mut tokens: &mut Vec<Token>) -> Result<Self> {
        let logical_and_exp = LogicalAndExpr::parse(&mut tokens)?;
        let mut expr = Expression(logical_and_exp, None);
        while let Some(token) = tokens.iter().peekable().peek() {
            if token.token_type != TokenType::Or {
                break;
            }
            tokens.remove(0);
            let next = LogicalAndExpr::parse(&mut tokens)?;
            match expr.1 {
                Some(_) => expr = Expression::from(expr, Some(next)),
                None => expr.1 = Some(next),
            }
        }

        Ok(expr)
    }

    fn from(expr: Expression, next: Option<LogicalAndExpr>) -> Expression {
        Expression(
            LogicalAndExpr(
                EqualityExpr(
                    RelationalExpr(
                        AdditiveExpr(BitwiseExpr(Term::Fact(Factor::Expr(Box::new(expr))), None), None),
                        None,
                    ),
                    None,
                ),
                None,
            ),
            next,
        )
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
        Ok(Program(Declaration::parse_func_decl(&mut tokens)?))
    }
}

fn compare_token(tok: Token, tok_type: TokenType) -> Result<Token> {
    if tok.token_type == tok_type {
        Ok(tok)
    } else {
        Err(CompilerError::ParsingError)
    }
}
