/// Parse package does all stuff to create AST
/// 
/// TODO: should we have rejected logic when we remove(0) from tokens
/// might be better to check it and if something wrong fail?
/// but not effect original vector, but it's not very crucial now, until we return tokens even in error,
/// or take &tokens not move them

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
        write!(f, "syntax_err")
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

fn parse_expr<ParsExpFunc>(parse: ParsExpFunc, opt_tokens: &[TokenType], tokens: Vec<Token>)
    -> Result<(ast::Exp, Vec<Token>)>
    where ParsExpFunc : Fn(Vec<Token>) -> Result<(ast::Exp, Vec<Token>)>
{
    let (mut exp, mut tokens) = parse(tokens)?;
    while let Some(tok) = tokens.get(0)  {
            if !opt_tokens.contains(&tok.token_type) {
                break;
            }
            
            let tok_type = tokens.remove(0).token_type;
            let (right, stashed_tokens) = parse(tokens)?;
            let op = map_token_to_ast(tok_type).unwrap();
            exp = ast::Exp::BinOp(op, Box::new(exp), Box::new(right));
            tokens = stashed_tokens;
    }

    Ok((exp, tokens))
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

fn map_token_to_unop(t: TokenType) -> Option<ast::UnOp> {
    match t {
        TokenType::BitwiseComplement => Some(ast::UnOp::BitwiseComplement),
        TokenType::LogicalNegation => Some(ast::UnOp::LogicalNegation),
        TokenType::Negation => Some(ast::UnOp::Negation),
        _ => None,
    }
}

fn map_inc_dec_token_to_unop(t: TokenType, postfix: bool) -> Option<ast::UnOp> {
    if postfix {
        match t {
            TokenType::Increment => Some(ast::UnOp::IncrementPostfix),
            TokenType::Decrement => Some(ast::UnOp::DecrementPostfix),
            _ => None,
        }
    } else {
        match t {
            TokenType::Increment => Some(ast::UnOp::IncrementPrefix),
            TokenType::Decrement => Some(ast::UnOp::DecrementPrefix),
            _ => None,
        }
    }
}

pub fn is_operators(t: &[Token], operators: &[TokenType]) -> bool {
    for (i, op) in operators.iter().enumerate() {
        match t.get(i) {
            Some(tok) if tok.token_type != *op => return false,
            None => return false,
            _ => (),
        };
    }

    true
}

fn map_assign_op(t: &Token) -> Option<ast::AssignmentOp> {
    match t.token_type {
        TokenType::AssignmentPlus => Some(ast::AssignmentOp::Plus),
        TokenType::AssignmentMul => Some(ast::AssignmentOp::Mul),
        TokenType::AssignmentSub => Some(ast::AssignmentOp::Sub),
        TokenType::AssignmentDiv => Some(ast::AssignmentOp::Div),
        TokenType::AssignmentMod => Some(ast::AssignmentOp::Mod),
        TokenType::AssignmentBitAnd => Some(ast::AssignmentOp::BitAnd),
        TokenType::AssignmentBitOr => Some(ast::AssignmentOp::BitOr),
        TokenType::AssignmentBitXor => Some(ast::AssignmentOp::BitXor),
        TokenType::AssignmentBitLeftShift => Some(ast::AssignmentOp::BitLeftShift),
        TokenType::AssignmentBitRightShift => Some(ast::AssignmentOp::BitRightShift),
        _ => None,
    }
}

pub fn parse_exp(mut tokens: Vec<Token>) -> Result<(ast::Exp, Vec<Token>)> {
    if tokens[0].is_type(TokenType::Identifier)
        && tokens[1].is_type(TokenType::Assignment) {
        let var = tokens.remove(0);
        tokens.remove(0);
        let (exp, tokens) = parse_exp(tokens)?;

        Ok((ast::Exp::Assign(var.val.unwrap().to_owned(), Box::new(exp)), tokens))
    } else if tokens[0].is_type(TokenType::Identifier)
        && map_assign_op(&tokens[1]).is_some() {
        let var = tokens.remove(0);
        let op = map_assign_op(&tokens[0]).unwrap();
        tokens.remove(0);
        let (exp, tokens) = parse_exp(tokens)?;

        Ok((ast::Exp::AssignOp(var.val.unwrap().to_owned(), op, Box::new(exp)), tokens))
    } else {
        parse_conditional_expr(tokens)
    }
}

pub fn parse_conditional_expr(tokens: Vec<Token>) -> Result<(ast::Exp, Vec<Token>)> {
    let (mut exp, mut tokens) = parse_or_expr(tokens)?;
    match tokens.get(0) {
        Some(tok) if tok.token_type == TokenType::QuestionSign => {
            tokens.remove(0);
            
            let (left_exp, mut toks) = parse_exp(tokens)?;
            compare_token(toks.remove(0), TokenType::Colon)?;
            let (right_exp, toks) = parse_conditional_expr(toks)?;
            
            tokens = toks;
            exp = ast::Exp::CondExp(Box::new(exp), Box::new(left_exp), Box::new(right_exp))
        }
        _ => (),
    };

    Ok((exp, tokens))
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
    let picked_token = tokens.get(0).unwrap();
    match picked_token.token_type {
        TokenType::OpenParenthesis => {
            let mut token = tokens.remove(0);
            let (expr, mut tokens) = parse_exp(tokens).unwrap();
            token = tokens.remove(0);
            if token.token_type != TokenType::CloseParenthesis {
                return Err(CompilerError::ParsingError);
            }
            Ok((expr, tokens))
        }
        TokenType::Identifier => {
            let token = tokens.remove(0);
            let var = ast::Exp::Var(token.val.unwrap().to_owned());
            match tokens.get(0) {
                Some(tok) if tok.is_type(TokenType::Decrement) || tok.is_type(TokenType::Increment) => {
                    let tok_type = tok.token_type;
                    tokens.remove(0);
                    Ok((ast::Exp::UnOp(map_inc_dec_token_to_unop(tok_type, true).unwrap(), Box::new(var)), tokens))
                }
                _ => Ok((var, tokens)),
            }
        }
        TokenType::IntegerLiteral => {
            let token = tokens.remove(0);
            Ok((ast::Exp::Const(ast::Const::Int(token.val.as_ref().unwrap().parse().unwrap())), tokens))
        }
        TokenType::Negation | TokenType::LogicalNegation | TokenType::BitwiseComplement => {
            let token = tokens.remove(0);
            let (expr, tokens) = parse_expr(parse_factor, &[TokenType::Or], tokens).unwrap();
            Ok((ast::Exp::UnOp(map_token_to_unop(token.token_type).unwrap(), Box::new(expr)), tokens))
        }
        _ => parse_inc_dec_expr(tokens),
    }
}


pub fn parse_inc_dec_expr(mut tokens: Vec<Token>) -> Result<(ast::Exp, Vec<Token>)> {
    let mut token = tokens.remove(0);
    match token.token_type {
        TokenType::Increment => {
            let (expr, tokens) = parse_expr(parse_factor, &[TokenType::Or], tokens).unwrap();
            Ok((ast::Exp::UnOp(map_inc_dec_token_to_unop(token.token_type, false).unwrap(), Box::new(expr)), tokens))
        }
        TokenType::Decrement => {
            let (expr, tokens) = parse_expr(parse_factor, &[TokenType::Or], tokens).unwrap();
            Ok((ast::Exp::UnOp(map_inc_dec_token_to_unop(token.token_type, false).unwrap(), Box::new(expr)), tokens))
        }
        _ => Err(CompilerError::ParsingError),
    }
}

pub fn parse_opt_exp(tokens: Vec<Token>) -> Result<(Option<ast::Exp>, Vec<Token>)> {
    match tokens[0].token_type {
        TokenType::Semicolon | TokenType::CloseParenthesis => Ok((None, tokens)),
        _ => {
            let (exp, tokens) = parse_exp(tokens)?;
            Ok((Some(exp), tokens))
        }
    }
}

pub fn parse_statement(mut tokens: Vec<Token>) -> Result<(ast::Statement, Vec<Token>)> {
    let (stat, tokens) = match tokens.get(0).unwrap().token_type {
        TokenType::Return => {
            tokens.remove(0);

            let (exp, mut tokens) = parse_exp(tokens).unwrap();
            compare_token(tokens.remove(0), TokenType::Semicolon).unwrap();
            
            (ast::Statement::Return{exp: exp}, tokens)
        },
        TokenType::For => {
            tokens.remove(0);

            compare_token(tokens.remove(0), TokenType::OpenParenthesis).unwrap();
            let (decl, toks) = parse_decl(tokens)?;
            let (controll_exp, mut toks) = parse_opt_exp(toks)?;
            let controll_exp = controll_exp.map_or(ast::Exp::Const(ast::Const::Int(1)), |ce| ce);
            compare_token(toks.remove(0), TokenType::Semicolon).unwrap();
            let (exp, mut toks) = parse_opt_exp(toks)?;
            compare_token(toks.remove(0), TokenType::CloseParenthesis).unwrap();
            let (statement, toks) = parse_statement(toks)?;

            (ast::Statement::ForDecl{decl: decl, exp2: controll_exp, exp3: exp, statement: Box::new(statement)}, toks)
        }
        TokenType::While => {
            tokens.remove(0);

            compare_token(tokens.remove(0), TokenType::OpenParenthesis).unwrap();
            let (exp, mut toks) = parse_exp(tokens)?;
            compare_token(toks.remove(0), TokenType::CloseParenthesis).unwrap();
            let (statement, toks) = parse_statement(toks)?;

            (ast::Statement::While{exp: exp, statement: Box::new(statement)}, toks)
        }
        TokenType::Do => {
            tokens.remove(0);

            compare_token(tokens.remove(0), TokenType::OpenBrace).unwrap();
            let (statement, mut toks) = parse_statement(tokens)?;
            compare_token(toks.remove(0), TokenType::CloseBrace).unwrap();
            compare_token(toks.remove(0), TokenType::While).unwrap();
            compare_token(toks.remove(0), TokenType::OpenParenthesis).unwrap();
            let (exp, mut toks) = parse_exp(toks)?;
            compare_token(toks.remove(0), TokenType::CloseParenthesis).unwrap();
            compare_token(toks.remove(0), TokenType::Semicolon).unwrap();

            (ast::Statement::Do{statement: Box::new(statement), exp: exp}, toks)
        }
        TokenType::Break => {
            tokens.remove(0);
            compare_token(tokens.remove(0), TokenType::Semicolon)?;

            (ast::Statement::Break, tokens)
        }
        TokenType::Continue => {
            tokens.remove(0);
            compare_token(tokens.remove(0), TokenType::Semicolon)?;

            (ast::Statement::Continue, tokens)
        }
        TokenType::If => {
            tokens.remove(0);
            compare_token(tokens.remove(0), TokenType::OpenParenthesis).unwrap();
            let (exp, mut tokens) = parse_exp(tokens)?;
            compare_token(tokens.remove(0), TokenType::CloseParenthesis).unwrap();
            
            let (if_block, mut tokens) = parse_statement(tokens)?;            
            
            let else_block= match tokens.get(0) {
                Some(tok) if tok.token_type == TokenType::Else => {
                    tokens.remove(0);
                    
                    let (else_block, toks) = parse_statement(tokens)?;            
                    tokens = toks;
                    Some(Box::new(else_block))
                },
                _ => None,
            };
            
            (ast::Statement::Conditional{cond_expr: exp, if_block: Box::new(if_block), else_block}, tokens)
        }
        TokenType::OpenBrace => {
            tokens.remove(0);

            let mut list = Vec::new();
            while tokens[0].token_type != TokenType::CloseBrace {
                let (exp, toks) = parse_block_item(tokens)?;
                tokens = toks;
                list.push(exp);
            }
            tokens.remove(0);

            let list = if !list.is_empty() {
                Some(list)
            } else {
                None
            };

            (ast::Statement::Compound{list: list}, tokens)
        }
        _ => {
            let (exp, mut tokens) = parse_opt_exp(tokens)?;
            compare_token(tokens.remove(0), TokenType::Semicolon).unwrap();

            (ast::Statement::Exp{exp: exp}, tokens)
        },
    };

    Ok((stat, tokens))
}

pub fn parse_decl(mut tokens: Vec<Token>) -> Result<(ast::Declaration, Vec<Token>)> {
    match tokens.get(0) {
        Some(tok) if tok.token_type == TokenType::Int => {
            tokens.remove(0);
            let var = compare_token(tokens.remove(0), TokenType::Identifier)?;
            let exp = match tokens.get(0) {
                Some(tok) if tok.is_type(TokenType::Assignment) => {
                    tokens.remove(0);
                    let (exp, toks) = parse_exp(tokens)?;
                    tokens = toks;
                    Some(exp)
                } ,
                _ => None,
            };
            compare_token(tokens.remove(0), TokenType::Semicolon).unwrap();

            Ok((ast::Declaration::Declare{name: var.val.unwrap().to_owned(), exp: exp}, tokens))
        },
        _ =>  {
            Err(CompilerError::ParsingError)
        },
    }
}

pub fn is_seem_decl(tokens: &[Token]) -> bool {
    match tokens.get(0) {
        Some(tok) if tok.token_type == TokenType::Int => {
            true
        },
        _ =>  {
            false
        },
    }
}

/// TODO: should we take off the parte with parse_decl?
/// currently we check is it decl if it's we parse it.
/// New function is not created since it dublication of code some kinda
pub fn parse_block_item(mut tokens: Vec<Token>) -> Result<(ast::BlockItem, Vec<Token>)> {
    match tokens.get(0) {
        Some(tok) if is_seem_decl(&tokens) => {
            let (decl, tokens) = parse_decl(tokens)?;
            Ok((ast::BlockItem::Declaration(decl), tokens))
        },
        _ =>  {
            let (state, tokens) = parse_statement(tokens)?;
            Ok((ast::BlockItem::Statement(state), tokens))
        },
    }
}

pub fn parse_func(mut tokens: Vec<Token>) -> Result<(ast::FuncDecl, Vec<Token>)> {
    compare_token(tokens.remove(0), TokenType::Int).unwrap();
    let func_name = compare_token(tokens.remove(0), TokenType::Identifier).unwrap();
    compare_token(tokens.remove(0), TokenType::OpenParenthesis).unwrap();
    compare_token(tokens.remove(0), TokenType::CloseParenthesis).unwrap();
    compare_token(tokens.remove(0), TokenType::OpenBrace).unwrap();

    let mut blocks = Vec::new();
    while tokens.get(0).unwrap().token_type != TokenType::CloseBrace {
        let (block, toks) = parse_block_item(tokens).unwrap();
        blocks.push(block);
        tokens = toks;
    } 
    tokens.remove(0);

    Ok((ast::FuncDecl{name: func_name.val.unwrap().clone(), blocks: blocks}, tokens))
}

pub fn parse(tokens: Vec<Token>) -> Result<ast::Program> {
    let (decl, tokens) = parse_func(tokens)?;
    if !tokens.is_empty() {
        return Err(CompilerError::ParsingError);
    }
    
    Ok(ast::Program(decl))
}

fn compare_token(tok: Token, tok_type: TokenType) -> Result<Token> {
    if tok.token_type == tok_type {
        Ok(tok)
    } else {
        Err(CompilerError::ParsingError)
    }
}
