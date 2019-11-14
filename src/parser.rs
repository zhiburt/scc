use crate::{Token, TokenType, ast};

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

fn tokens_to_types(tokens: &Vec<Token>) -> Vec<TokenType> {
    tokens.iter().map(|t| t.token_type).collect()
}

fn parse_expr<ParsExpFunc>(parse_exp: ParsExpFunc, opt_tokens: &[TokenType], tokens: Vec<Token>)
    -> Result<(ast::Exp, Vec<Token>)>
    where ParsExpFunc : Fn(Vec<Token>) -> Result<(ast::Exp, Vec<Token>)>
{
    let (left, mut tokens) = parse_exp(tokens).unwrap();
    match tokens.get(0) {
        Some(tok) if opt_tokens.contains(&tok.token_type) => {
            let tok_type = tokens.remove(0).token_type;
            let (right, tokens) = parse_expr(parse_exp, opt_tokens, tokens)?;
            let op = map_token_to_ast(tok_type).unwrap();
            Ok((ast::Exp::BinOp(op, Box::new(left), Box::new(right)), tokens))
        }
        _ => Ok((left, tokens))
    }
}

fn map_token_to_ast(t: TokenType) -> Option<ast::BinOp> {
    match t {
            TokenType::BitwiseXor => Some(ast::BinOp::BitwiseXor),
            TokenType::BitwiseOr => Some(ast::BinOp::BitwiseOr),
            TokenType::BitwiseAnd => Some(ast::BinOp::BitwiseAnd),
            TokenType::Addition => Some(ast::BinOp::Addition),
            TokenType::Negation => Some(ast::BinOp::Sub),
            TokenType::Multiplication => Some(ast::BinOp::Multiplication),
            TokenType::Division => Some(ast::BinOp::Division),
            TokenType::Modulo => Some(ast::BinOp::Modulo),
            TokenType::And => Some(ast::BinOp::And),
            TokenType::Or => Some(ast::BinOp::Or),
            TokenType::Equal => Some(ast::BinOp::Equal),
            TokenType::NotEqual => Some(ast::BinOp::NotEqual),
            TokenType::LessThan => Some(ast::BinOp::LessThan),
            TokenType::LessThanOrEqual => Some(ast::BinOp::LessThanOrEqual),
            TokenType::GreaterThan => Some(ast::BinOp::GreaterThan),
            TokenType::GreaterThanOrEqual => Some(ast::BinOp::GreaterThanOrEqual),
            TokenType::BitwiseLeftShift => Some(ast::BinOp::BitwiseLeftShift),
            TokenType::BitwiseRightShift => Some(ast::BinOp::BitwiseRightShift),
            _ => None,
    }
}

pub fn parse_or_expr(tokens: Vec<Token>) -> Result<(ast::Exp, Vec<Token>)> {
    parse_expr(parse_and_expr, &[TokenType::Or], tokens)
}

pub fn parse_and_expr(tokens: Vec<Token>) -> Result<(ast::Exp, Vec<Token>)> {
    parse_expr(parse_equality_expr, &[TokenType::And], tokens)
}

pub fn parse_equality_expr(tokens: Vec<Token>) -> Result<(ast::Exp, Vec<Token>)> {
    parse_expr(parse_relational_expr, &[TokenType::Equal, TokenType::NotEqual], tokens)
}

pub fn parse_relational_expr(tokens: Vec<Token>) -> Result<(ast::Exp, Vec<Token>)> {
    parse_expr(parse_addictive_expr, &[TokenType::GreaterThan, TokenType::GreaterThanOrEqual, TokenType::LessThan, TokenType::LessThanOrEqual], tokens)
}

pub fn parse_addictive_expr(tokens: Vec<Token>) -> Result<(ast::Exp, Vec<Token>)> {
    parse_expr(parse_bitwise_expr, &[TokenType::Addition, TokenType::Negation], tokens)
}

pub fn parse_bitwise_expr(tokens: Vec<Token>) -> Result<(ast::Exp, Vec<Token>)> {
    parse_expr(parse_un_op_term, &[TokenType::BitwiseLeftShift, TokenType::BitwiseRightShift], tokens)
}

pub fn parse_un_op_term(tokens: Vec<Token>) -> Result<(ast::Exp, Vec<Token>)> {
    parse_expr(parse_term, &[TokenType::BitwiseAnd, TokenType::BitwiseOr, TokenType::BitwiseXor], tokens)
}

pub fn parse_term(tokens: Vec<Token>) -> Result<(ast::Exp, Vec<Token>)> {
    parse_expr(parse_factor, &[TokenType::Multiplication, TokenType::Modulo, TokenType::Division], tokens)
}

pub fn parse_factor(mut tokens: Vec<Token>) -> Result<(ast::Exp, Vec<Token>)> {
    let mut token = tokens.remove(0);
    match token.token_type {
        TokenType::OpenParenthesis => {
            let (expr, mut tokens) = parse_expr(parse_or_expr, &[TokenType::Or], tokens).unwrap();
            token = tokens.remove(0);
            if token.token_type != TokenType::CloseParenthesis {
                return Err(CompilerError::ParsingError);
            }
            Ok((expr, tokens))
        }
        TokenType::IntegerLiteral => {
            Ok((ast::Exp::Const(ast::Const::Int(token.val.as_ref().unwrap().parse().unwrap())), tokens))
        }
        TokenType::Negation | TokenType::LogicalNegation | TokenType::BitwiseComplement => {
            let (expr, tokens) = parse_expr(parse_or_expr, &[TokenType::Or], tokens).unwrap();
            Ok((ast::Exp::UnOp(ast::UnOp::LogicalNegation, Box::new(expr)), tokens))
        }
        _ => Err(CompilerError::ParsingError),
    }
}

pub fn parse_statement(mut tokens: Vec<Token>) -> Result<(ast::Statement, Vec<Token>)> {
    compare_token(tokens.remove(0), TokenType::Return).unwrap();
    let (exp, mut tokens) = parse_or_expr(tokens).unwrap();
    compare_token(tokens.remove(0), TokenType::Semicolon).unwrap();
    Ok((ast::Statement::Return{exp}, tokens))
}

pub fn parse_decl(mut tokens: Vec<Token>) -> Result<(ast::Declaration, Vec<Token>)> {
    compare_token(tokens.remove(0), TokenType::Int).unwrap();
    let func_name = compare_token(tokens.remove(0), TokenType::Identifier).unwrap();
    compare_token(tokens.remove(0), TokenType::OpenParenthesis).unwrap();
    compare_token(tokens.remove(0), TokenType::CloseParenthesis).unwrap();
    compare_token(tokens.remove(0), TokenType::OpenBrace).unwrap();
    let (body, mut tokens) = parse_statement(tokens).unwrap();
    compare_token(tokens.remove(0), TokenType::CloseBrace).unwrap();

    Ok((ast::Declaration::Func{name: func_name.val.unwrap().clone(), state: body}, tokens))
}

pub fn parse(tokens: Vec<Token>) -> Result<ast::Program> {
    let (decl, _) = parse_decl(tokens)?;
    Ok(ast::Program(decl))
}

fn compare_token(tok: Token, tok_type: TokenType) -> Result<Token> {
    if tok.token_type == tok_type {
        Ok(tok)
    } else {
        Err(CompilerError::ParsingError)
    }
}

// #[derive(Debug)]
// pub enum UnaryOp {
//     Negation,
//     BitwiseComplement,
//     LogicalNegation,
// }

// impl UnaryOp {
//     fn from_token_type(t: TokenType) -> Option<UnaryOp> {
//         match t {
//             TokenType::Negation => Some(UnaryOp::Negation),
//             TokenType::LogicalNegation => Some(UnaryOp::LogicalNegation),
//             TokenType::BitwiseComplement => Some(UnaryOp::BitwiseComplement),
//             _ => None,
//         }
//     }
// }

// pub struct Expression(pub LogicalAndExpr, pub Option<LogicalAndExpr>);

// pub struct LogicalAndExpr(pub EqualityExpr, pub Option<EqualityExpr>);

// impl LogicalAndExpr {
//     pub fn parse(mut tokens: &mut Vec<Token>) -> Result<Self> {
//         let eq_expr = EqualityExpr::parse(&mut tokens)?;
//         let mut expr = LogicalAndExpr(eq_expr, None);
//         while let Some(token) = tokens.iter().peekable().peek() {
//             if token.token_type != TokenType::And {
//                 break;
//             }

//             tokens.remove(0);
//             let next = EqualityExpr::parse(&mut tokens)?;
//             match expr.1 {
//                 Some(_) => {
//                     expr = LogicalAndExpr(
//                         EqualityExpr(
//                             RelationalExpr(
//                                 AdditiveExpr(
//                                     BitwiseExpr(
//                                         BitwiseLogicExpr(
//                                             Term::Fact(Factor::Expr(Box::new(Expression(expr, None)))),
//                                             None),
//                                         None,
//                                     ),
//                                     None,
//                                 ),
//                                 None,
//                             ),
//                             None,
//                         ),
//                         Some(next),
//                     );
//                 }
//                 None => {
//                     expr.1 = Some(next);
//                 }
//             }
//         }

//         Ok(expr)
//     }
// }

// pub struct EqualityExpr(pub RelationalExpr, pub Option<(EqualityOp, RelationalExpr)>);

// #[derive(Debug)]
// pub enum EqualityOp {
//     Equal,
//     NotEqual,
// }

// impl EqualityExpr {
//     pub fn parse(mut tokens: &mut Vec<Token>) -> Result<Self> {
//         let eq_expr = RelationalExpr::parse(&mut tokens)?;
//         let mut expr = EqualityExpr(eq_expr, None);
//         while let Some(token) = tokens.iter().peekable().peek() {
//             let op = match token.token_type {
//                 TokenType::Equal => EqualityOp::Equal,
//                 TokenType::NotEqual => EqualityOp::NotEqual,
//                 _ => break,
//             };

//             tokens.remove(0);
//             let next = RelationalExpr::parse(&mut tokens)?;
//             match expr.1 {
//                 Some(_) => {
//                     expr = EqualityExpr(
//                         RelationalExpr(
//                             AdditiveExpr(
//                                 BitwiseExpr(
//                                     BitwiseLogicExpr(
//                                         Term::Fact(Factor::Expr(Box::new(Expression(
//                                             LogicalAndExpr(expr, None),
//                                             None,
//                                         )))),
//                                         None),
//                                     None
//                                 ),
//                                 None,
//                             ),
//                             None,
//                         ),
//                         Some((op, next)),
//                     );
//                 }
//                 None => {
//                     expr.1 = Some((op, next));
//                 }
//             }
//         }

//         Ok(expr)
//     }
// }

// pub struct RelationalExpr(pub AdditiveExpr, pub Option<(RelationalOp, AdditiveExpr)>);

// #[derive(Debug)]
// pub enum RelationalOp {
//     Less,
//     LessOrEqual,
//     Greater,
//     GreaterOrEqual,
// }

// impl RelationalExpr {
//     pub fn parse(mut tokens: &mut Vec<Token>) -> Result<Self> {
//         let eq_expr = AdditiveExpr::parse(&mut tokens)?;
//         let mut expr = RelationalExpr(eq_expr, None);
//         while let Some(token) = tokens.iter().peekable().peek() {
//             let op = match token.token_type {
//                 TokenType::LessThan => RelationalOp::Less,
//                 TokenType::LessThanOrEqual => RelationalOp::LessOrEqual,
//                 TokenType::GreaterThan => RelationalOp::Greater,
//                 TokenType::GreaterThanOrEqual => RelationalOp::GreaterOrEqual,
//                 _ => break,
//             };

//             tokens.remove(0);
//             let next = AdditiveExpr::parse(&mut tokens)?;
//             match expr.1 {
//                 Some(_) => {
//                     expr = RelationalExpr(
//                         AdditiveExpr(BitwiseExpr(BitwiseLogicExpr(
//                             Term::Fact(Factor::Expr(Box::new(Expression(
//                                 LogicalAndExpr(EqualityExpr(expr, None), None),
//                                 None,
//                             )))), None), None),
//                             None,
//                         ),
//                         Some((op, next)),
//                     )
//                 }
//                 None => expr.1 = Some((op, next)),
//             }
//         }

//         Ok(expr)
//     }
// }

// pub struct AdditiveExpr(pub BitwiseExpr, pub Option<(BinOp, BitwiseExpr)>);

// impl AdditiveExpr {
//     pub fn parse(mut tokens: &mut Vec<Token>) -> Result<Self> {
//         let term = BitwiseExpr::parse(&mut tokens)?;
//         let mut expr = AdditiveExpr(term, None);
//         while let Some(token) = tokens.iter().peekable().peek() {
//             let op = match token.token_type {
//                 TokenType::Negation => BinOp::Minus,
//                 TokenType::Addition => BinOp::Plus,
//                 _ => break,
//             };

//             tokens.remove(0);
//             let next = BitwiseExpr::parse(&mut tokens)?;
//             match expr.1 {
//                 Some(_) => {
//                     expr = AdditiveExpr(
//                         BitwiseExpr(BitwiseLogicExpr(Term::Fact(Factor::Expr(Box::new(Expression(
//                             LogicalAndExpr(EqualityExpr(RelationalExpr(expr, None), None), None),
//                             None,
//                         )))), None), None),
//                         Some((op, next)),
//                     )
//                 }
//                 None => expr.1 = Some((op, next)),
//             }
//         }

//         Ok(expr)
//     }
// }

// pub struct BitwiseExpr(pub BitwiseLogicExpr, pub Option<(BitwiseOp, BitwiseLogicExpr)>);

// impl BitwiseExpr {
//     pub fn parse(mut tokens: &mut Vec<Token>) -> Result<Self> {
//         let bit_expr = BitwiseLogicExpr::parse(&mut tokens)?;
//         let mut expr = BitwiseExpr(bit_expr, None);
//         while let Some(token) = tokens.iter().peekable().peek() {
//             let op = match token.token_type {
//                 TokenType::BitwiseLeftShift => BitwiseOp::LeftShift,
//                 TokenType::BitwiseRightShift => BitwiseOp::RightShift,
//                 _ => break,
//             };

//             tokens.remove(0);
//             let next = BitwiseLogicExpr::parse(&mut tokens)?;
//             match expr.1 {
//                 Some(_) => {
//                     expr = BitwiseExpr(BitwiseLogicExpr(
//                         Term::Fact(Factor::Expr(Box::new(Expression(
//                             LogicalAndExpr(EqualityExpr(RelationalExpr(AdditiveExpr(expr, None), None), None), None),
//                             None
//                         )))), None),
//                         Some((op, next)),
//                     )
//                 }
//                 None => expr.1 = Some((op, next)),
//             }
//         }

//         Ok(expr)
//     }
// }

// // Is it correct precedence which corresponds with C11?
// pub struct BitwiseLogicExpr(pub Term, pub Option<(BitLogicOp, Term)>);

// impl BitwiseLogicExpr {
//     pub fn parse(mut tokens: &mut Vec<Token>) -> Result<Self> {
//         let term = Term::parse(&mut tokens)?;
//         let mut expr = BitwiseLogicExpr(term, None);
//         while let Some(token) = tokens.iter().peekable().peek() {
//             let op = match token.token_type {
//                 TokenType::BitwiseAnd => BitLogicOp::And,
//                 TokenType::BitwiseXor => BitLogicOp::Xor,
//                 TokenType::BitwiseOr => BitLogicOp::Or,
//                 _ => break,
//             };

//             tokens.remove(0);
//             let next = Term::parse(&mut tokens)?;
//             match expr.1 {
//                 Some(_) => {
//                     expr = BitwiseLogicExpr(
//                         Term::Fact(Factor::Expr(Box::new(Expression(
//                             LogicalAndExpr(EqualityExpr(RelationalExpr(AdditiveExpr(BitwiseExpr(expr, None), None), None), None), None),
//                             None,
//                         )))),
//                         Some((op, next)),
//                     )
//                 }
//                 None => expr.1 = Some((op, next)),
//             }
//         }

//         Ok(expr)
//     }
// }

// #[derive(Debug)]
// pub enum BitLogicOp {
//     Xor,
//     And,
//     Or,
// }

// #[derive(Debug)]
// pub enum BitwiseOp {
//     LeftShift,
//     RightShift,
// }

// #[derive(Debug)]
// pub enum BinOp {
//     Plus,
//     Minus,
// }

// #[derive(Debug)]
// pub enum FactOp {
//     Multiplication,
//     Division,
//     Modulo,
// }

// pub enum Term {
//     Fact(Factor),
//     FactorOp(Box<Factor>, FactOp, Box<Factor>),
// }

// impl Term {
//     pub fn parse(mut tokens: &mut Vec<Token>) -> Result<Self> {
//         let fact = Factor::parse(&mut tokens)?;
//         let mut term = Term::Fact(fact);
//         while let Some(token) = tokens.iter().peekable().peek() {
//             match token.token_type {
//                 TokenType::Multiplication => {
//                     tokens.remove(0);
//                     let next_fact = Factor::parse(&mut tokens)?;
//                     let f = match term {
//                         Term::Fact(fact) => fact,
//                         _ => Factor::Expr(Box::new(Expression(
//                             LogicalAndExpr(
//                                 EqualityExpr(RelationalExpr(AdditiveExpr(BitwiseExpr(BitwiseLogicExpr(term, None), None), None), None), None),
//                                 None,
//                             ),
//                             None,
//                         ))),
//                     };
//                     term = Term::FactorOp(Box::new(f), FactOp::Multiplication, Box::new(next_fact));
//                 }
//                 TokenType::Division => {
//                     tokens.remove(0);
//                     let next_fact = Factor::parse(&mut tokens)?;
//                     let f = match term {
//                         Term::Fact(fact) => fact,
//                         _ => Factor::Expr(Box::new(Expression(
//                             LogicalAndExpr(
//                                 EqualityExpr(RelationalExpr(AdditiveExpr(BitwiseExpr(BitwiseLogicExpr(term, None), None), None), None), None),
//                                 None,
//                             ),
//                             None,
//                         ))),
//                     };
//                     term = Term::FactorOp(Box::new(f), FactOp::Division, Box::new(next_fact));
//                 }
//                 TokenType::Modulo => {
//                     tokens.remove(0);
//                     let next_fact = Factor::parse(&mut tokens)?;
//                     let f = match term {
//                         Term::Fact(fact) => fact,
//                         _ => Factor::Expr(Box::new(Expression(
//                             LogicalAndExpr(
//                                 EqualityExpr(RelationalExpr(AdditiveExpr(BitwiseExpr(BitwiseLogicExpr(term, None), None), None), None), None),
//                                 None,
//                             ),
//                             None,
//                         ))),
//                     };
//                     term = Term::FactorOp(Box::new(f), FactOp::Modulo, Box::new(next_fact));
//                 }
//                 _ => {
//                     break;
//                 }
//             }
//         }

//         Ok(term)
//     }
// }

// pub enum Factor {
//     Expr(Box<Expression>),
//     UnOp(UnaryOp, Box<Factor>),
//     Const(isize),
// }

// impl Factor {
//     pub fn parse(mut tokens: &mut Vec<Token>) -> Result<Self> {
//         let mut token = tokens.remove(0);
//         match token.token_type {
//             TokenType::OpenParenthesis => {
//                 let expr = Expression::parse(tokens)?;
//                 token = tokens.remove(0);
//                 if token.token_type != TokenType::CloseParenthesis {
//                     return Err(CompilerError::ParsingError);
//                 }
//                 Ok(Factor::Expr(Box::new(expr)))
//             }
//             TokenType::IntegerLiteral => {
//                 Ok(Factor::Const(token.val.as_ref().unwrap().parse().unwrap()))
//             }
//             TokenType::Negation | TokenType::LogicalNegation | TokenType::BitwiseComplement => {
//                 let factor = Factor::parse(&mut tokens)?;
//                 Ok(Factor::UnOp(
//                     UnaryOp::from_token_type(token.token_type).unwrap(),
//                     Box::new(factor),
//                 ))
//             }
//             _ => Err(CompilerError::ParsingError),
//         }
//     }
// }

// impl Expression {
//     pub fn parse(mut tokens: &mut Vec<Token>) -> Result<Self> {
//         let logical_and_exp = LogicalAndExpr::parse(&mut tokens)?;
//         let mut expr = Expression(logical_and_exp, None);
//         while let Some(token) = tokens.iter().peekable().peek() {
//             if token.token_type != TokenType::Or {
//                 break;
//             }
//             tokens.remove(0);
//             let next = LogicalAndExpr::parse(&mut tokens)?;
//             match expr.1 {
//                 Some(_) => expr = Expression::from(expr, Some(next)),
//                 None => expr.1 = Some(next),
//             }
//         }

//         Ok(expr)
//     }

//     fn from(expr: Expression, next: Option<LogicalAndExpr>) -> Expression {
//         Expression(
//             LogicalAndExpr(
//                 EqualityExpr(
//                     RelationalExpr(
//                         AdditiveExpr(BitwiseExpr(BitwiseLogicExpr(Term::Fact(Factor::Expr(Box::new(expr))), None), None), None),
//                         None,
//                     ),
//                     None,
//                 ),
//                 None,
//             ),
//             next,
//         )
//     }
// }

// pub enum Statement {
//     Return(Expression),
// }

// impl Statement {
//     pub fn parse(tokens: &mut Vec<Token>) -> Result<Self> {
//         compare_token(tokens.remove(0), TokenType::Return)?;
//         let expr = Expression::parse(tokens)?;
//         compare_token(tokens.remove(0), TokenType::Semicolon)?;
//         Ok(Statement::Return(expr))
//     }
// }

// pub enum Declaration {
//     Func(String, Statement),
// }

// impl Declaration {
//     pub fn parse_func_decl(mut tokens: &mut Vec<Token>) -> Result<Self> {
//         compare_token(tokens.remove(0), TokenType::Int)?;
//         let func_name = compare_token(tokens.remove(0), TokenType::Identifier)?;
//         compare_token(tokens.remove(0), TokenType::OpenParenthesis)?;
//         compare_token(tokens.remove(0), TokenType::CloseParenthesis)?;
//         compare_token(tokens.remove(0), TokenType::OpenBrace)?;
//         let body = Statement::parse(&mut tokens)?;
//         compare_token(tokens.remove(0), TokenType::CloseBrace)?;

//         Ok(Declaration::Func(func_name.val.unwrap().clone(), body))
//     }
// }

// pub struct Program(pub Declaration);

// impl Program {
//     pub fn parse(mut tokens: &mut Vec<Token>) -> Result<Self> {
//         Ok(Program(Declaration::parse_func_decl(&mut tokens)?))
//     }
// }

// fn compare_token(tok: Token, tok_type: TokenType) -> Result<Token> {
//     if tok.token_type == tok_type {
//         Ok(tok)
//     } else {
//         Err(CompilerError::ParsingError)
//     }
// }
