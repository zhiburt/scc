use simple_c_compiler::{parser};
use simple_c_compiler::{ast};

pub fn pretty_func(ast::FuncDecl{name, parameters, blocks}: &ast::FuncDecl) -> String {
    format!(
        "FUN {} with {}:\n   body:\n{}",
        name,
        parameters.iter().map(|p| format!("int {}", p)).collect::<Vec<String>>().join(","),
        blocks.as_ref().map_or("empty body".to_owned(), |blocks| blocks.iter().map(|block| format!("      {}", pretty_block(block))).collect::<Vec<_>>().join("\n"))
    )
}

fn pretty_block(block: &ast::BlockItem) -> String {
    match block {
        ast::BlockItem::Declaration(decl) => pretty_decl(decl),
        ast::BlockItem::Statement(statement) => pretty_stat(statement),
    }
}

fn pretty_stat(st: &ast::Statement) -> String {
    match st {
        ast::Statement::Return{exp} => format!("Return<{}>", pretty_expr(exp)),
        ast::Statement::Exp{exp} => format!("Exp<{}>", pretty_opt_expr(exp)),
        ast::Statement::Conditional{cond_expr, if_block, else_block} => {
            let else_block = if let Some(else_block) = else_block {
                pretty_stat(else_block)
            } else {
                "".to_owned()
            };

            format!("If<{}, {}, {}>", pretty_expr(cond_expr), pretty_stat(if_block), else_block )
        }
        ast::Statement::Compound{list} => {
            match list {
                Some(list) => {
                    format!("Compound<{}>", list.iter().map(|state_or_def| match state_or_def {
                        ast::BlockItem::Declaration(decl) => pretty_decl(decl),
                        ast::BlockItem::Statement(stat) => pretty_stat(stat),
                    }).collect::<Vec<_>>().join(" "))
                }
                None => format!("Compound<>"),
            }
        }
        ast::Statement::While{exp, statement} => {
            format!("While<Exp<{}>, Statement<{}>>", pretty_expr(exp), pretty_stat(statement))
        }
        ast::Statement::Do{statement, exp} => {
            format!("DoWhile<Statement<{}>, Exp<{}>>", pretty_stat(statement), pretty_expr(exp))
        }
        ast::Statement::ForDecl{decl, exp2, exp3, statement} => {
            format!("For<Decl<{}>, Exp<{}>, Exp<{}>, Statement<{}>>", pretty_decl(decl), pretty_expr(exp2), pretty_opt_expr(exp3), pretty_stat(statement))
        }
        ast::Statement::For{exp1, exp2, exp3, statement} => {
            format!("For<Exp<{}>, Exp<{}>, Exp<{}>, Statement<{}>>", pretty_opt_expr(exp1), pretty_expr(exp2), pretty_opt_expr(exp3), pretty_stat(statement))
        }
        ast::Statement::Break => {
            "Break".to_owned()
        }
        ast::Statement::Continue => {
            "Continue".to_owned()
        }
        _ => unimplemented!(),
    }
}

fn pretty_decl(st: &ast::Declaration) -> String {
    match st {
        ast::Declaration::Declare{name, exp} => {
            match exp {
                Some(exp) => format!("Declare<{}, {}>", name, pretty_expr(exp)),
                None => format!("Declare<{}>", name),
            }
        }
    }
}

fn pretty_opt_expr(exp: &Option<ast::Exp>) -> String {
    exp.as_ref().map_or("None".to_owned(), |exp| pretty_expr(exp))
}

fn pretty_expr(exp: &ast::Exp) -> String {
    match exp {
        ast::Exp::BinOp(op, exp1, exp2) => format!("{} BinOp<{:?}> {}", pretty_expr(exp1), op, pretty_expr(exp2)),
        ast::Exp::Const(c) => format!("Const<{:?}>", c),
        ast::Exp::UnOp(op, exp) => format!("UnOp<{:?}> {}", op, pretty_expr(exp)),
        ast::Exp::Assign(name, exp) => format!("Var<{}> assign Exp<{}>", name, pretty_expr(exp)),
        ast::Exp::Var(name) => format!("Var<{}>", name),
        ast::Exp::AssignOp(name, op, exp) => format!("VarOp<{}, {:?}> {}", name, op, pretty_expr(exp)),
        ast::Exp::CondExp(cond, exp1, exp2) => format!("TernarOp<{}, {}, {}>", pretty_expr(cond), pretty_expr(exp1), pretty_expr(exp2)),
    }
}


// fn pretty_statement(s: &parser::Statement) -> String {
//     match s {
//         parser::Statement::Return(expr) => format!("RETURN {}", pretty_expr(expr)),
//     }
// }

// fn pretty_expr(e: &parser::Expression) -> String {    
//     let bitwise_logic_expr = |e: &parser::BitwiseLogicExpr| -> String {
//         let s = pretty_term(&e.0);
//         if let Some((op, term)) = &e.1 {
//             format!("{} Op<{:?}> {}", s, op, pretty_term(term))
//         } else {
//             s
//         }
//     };

//     let bitwise_expr = |e: &parser::BitwiseExpr| -> String {
//         let s = bitwise_logic_expr(&e.0);
//         if let Some((op, term)) = &e.1 {
//             format!("{} Op<{:?}> {}", s, op, bitwise_logic_expr(term))
//         } else {
//             s
//         }
//     };

//     let additive_expr = |e: &parser::AdditiveExpr| -> String {
//         let s = bitwise_expr(&e.0);
//         if let Some((op, term)) = &e.1 {
//             format!("{} Op<{:?}> {}", s, op, bitwise_expr(term))
//         } else {
//             s
//         }
//     };

//     let relational_expr = |e: &parser::RelationalExpr| -> String {
//         let s = additive_expr(&e.0);
//         if let Some((op, e)) = &e.1 {
//             format!("{} Op<{:?} {}", s, op, additive_expr(e))
//         } else {
//             s
//         }
//     };

//     let pretty_equality_expr = |e: &parser::EqualityExpr| -> String {
//         let s = relational_expr(&e.0);
//         if let Some((op, e)) = &e.1 {
//             format!("{} Op<{:?} {}", s, op, relational_expr(e))
//         } else {
//             s
//         }
//     };

//     let pretty_logical_expr = |e: &parser::LogicalAndExpr| -> String {
//         let s = pretty_equality_expr(&e.0);
//         if let Some(e) = &e.1 {
//             format!("{} Op<Or> {}", s, pretty_equality_expr(e))
//         } else {
//             s
//         }
//     };

//     let s = pretty_logical_expr(&e.0);
//     if let Some(e) = &e.1 {
//         format!("{} Op<And> {}", s, pretty_logical_expr(e))
//     } else {
//         s
//     }
// }

// fn pretty_term(t: &parser::Term) -> String {
//     match t {
//         parser::Term::Fact(factor) => format!("Fact<{}>", pretty_fact(factor)),
//         parser::Term::FactorOp(lf, op, rf) => {
//             format!("{} Op<{:?}> {}", pretty_fact(lf), op, pretty_fact(rf))
//         }
//     }
// }

// fn pretty_fact(f: &parser::Factor) -> String {
//     match f {
//         parser::Factor::Const(e) => format!("Const<{}>", e),
//         parser::Factor::Expr(expr) => pretty_expr(expr),
//         parser::Factor::UnOp(op, f) => format!("UnOp<{:?}> Operation<{}>", op, pretty_fact(f)),
//     }
// }
