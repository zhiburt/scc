use simple_c_compiler::{parser};
use simple_c_compiler::{ast};

pub fn pretty_prog(prog: &ast::Program) -> String {
    let mut out = Vec::new();
    for func in &prog.0 {
        out.push(pretty_func(func));
    }

    out.join("\n\n")
}

pub fn pretty_func(ast::FuncDecl{name, parameters, blocks}: &ast::FuncDecl) -> String {
    let params = parameters.iter()
        .map(|p| format!("INT {}", p))
        .collect::<Vec<String>>()
        .join(", ");

    let body = blocks.as_ref().map_or("".to_owned(), |blocks| { 
        blocks
            .iter()
            .map(|block| {
                pretty_block(block)
                .iter()
                .map(|(line, deep)| format!("\t{1}{}", &line, str::repeat("\t", *deep)))
                .collect::<Vec<_>>()
            })
            .flatten()
            .collect::<Vec<String>>()
            .join("\n")
    });

    format!("FUNCTION {}:\n\tparameters: {}\n\tbody:\n{}", name, params, body)
}

fn pretty_block(block: &ast::BlockItem) -> Vec<(String, usize)> {
    match block {
        ast::BlockItem::Declaration(decl) => vec![(pretty_decl(decl), 0)],
        ast::BlockItem::Statement(statement) => pretty_stat(statement, 0),
    }
}

fn pretty_stat(st: &ast::Statement, deep: usize) -> Vec<(String, usize)> {

    match st {
        ast::Statement::Return{exp} => vec![(format!("RETURN {}", pretty_expr(exp)), deep)],
        ast::Statement::Exp{exp} => vec![(format!("{}", pretty_opt_expr(exp)), deep)],
        ast::Statement::Conditional{cond_expr, if_block, else_block} => {
            let mut out = Vec::new();
            out.push((format!("IF {}:", pretty_expr(cond_expr)), deep));
            out.extend(pretty_stat(if_block, deep+1));

            if let Some(else_block) = else_block {
                out.push(("ELSE:".to_string(), deep));
                out.extend(pretty_stat(else_block, deep+1));
            }

            out.push(("END".to_string(), deep));

            out
        }
        ast::Statement::Compound{list} => {
            match list {
                Some(list) => {
                    let mut out = Vec::new();
                    // out.push(("COMPOUND:".to_string(), deep));
                    for state_or_def in list {
                        match state_or_def {
                            ast::BlockItem::Declaration(decl) => out.push((pretty_decl(decl), deep)),
                            ast::BlockItem::Statement(stat) => out.extend(pretty_stat(stat, deep)),
                        }
                    }
                    out
                }
                None => vec![("COMPOUND:".to_string(), deep)],
            }
        }
        ast::Statement::While{exp, statement} => {
            let mut out = Vec::new();
            out.push((format!("WHILE {}:", pretty_expr(exp)), deep));
            out.push(("DO".to_string(), deep));
            out.extend(pretty_stat(statement, deep+1));
            out.push(("END".to_string(), deep));
            out
        }
        ast::Statement::Do{statement, exp} => {
            let mut out = Vec::new();
            out.push((format!("DOWHILE {}:", pretty_expr(exp)), deep));
            out.push(("DO".to_string(), deep));
            out.extend(pretty_stat(statement, deep+1));
            out.push(("END".to_string(), deep));
            out
        }
        ast::Statement::ForDecl{decl, exp2, exp3, statement} => {
            let mut out = Vec::new();
            out.push(("FOR:".to_string(), deep));
            out.push((format!("decl: {}", pretty_decl(decl)), deep));
            out.push((format!("cond: {}", pretty_expr(exp2)), deep));
            out.push((format!("exp: {}", exp3.as_ref().map_or("".to_string(), |e| pretty_expr(&e))), deep));
            out.push(("DO".to_string(), deep));
            out.extend(pretty_stat(statement, deep+1));
            out.push(("END".to_string(), deep));
            out
        }
        ast::Statement::For{exp1, exp2, exp3, statement} => {
            let mut out = Vec::new();
            out.push(("FOR:".to_string(), deep));
            out.push((format!("decl: {}", exp3.as_ref().map_or("".to_string(), |e| pretty_expr(&e))), deep));
            out.push((format!("cond: {}", pretty_expr(exp2)), deep));
            out.push((format!("exp: {}", exp3.as_ref().map_or("".to_string(), |e| pretty_expr(&e))), deep));
            out.push(("DO".to_string(), deep));
            out.extend(pretty_stat(statement, deep+1));
            out.push(("END".to_string(), deep));
            out
        }
        ast::Statement::Break => {
            vec![("BREAK".to_string(), deep)]
        }
        ast::Statement::Continue => {
            vec![("CONTINUE".to_string(), deep)]
        }
        _ => unimplemented!(),
    }
}

fn pretty_decl(st: &ast::Declaration) -> String {
    match st {
        ast::Declaration::Declare{name, exp} => {
            match exp {
                Some(exp) => format!("INT {} = {}", name, pretty_expr(exp)),
                None => format!("INT {}", name),
            }
        }
    }
}

fn pretty_opt_expr(exp: &Option<ast::Exp>) -> String {
    exp.as_ref().map_or("None".to_owned(), |exp| pretty_expr(exp))
}

fn pretty_expr(exp: &ast::Exp) -> String {
    match exp {
        ast::Exp::BinOp(op, exp1, exp2) => format!("{} BIN_OP<{:?}> {}", pretty_expr(exp1), op, pretty_expr(exp2)),
        ast::Exp::Const(c) => format!("{:?}", c),
        ast::Exp::UnOp(op, exp) => format!("UN_OP<{:?}> {}", op, pretty_expr(exp)),
        ast::Exp::IncOrDec(name, op) => format!("VAR[{}] {:?}", name, op),
        ast::Exp::Assign(name, exp) => format!("VAR[{}] = {}", name, pretty_expr(exp)),
        ast::Exp::Var(name) => format!("VAR[{}]", name),
        ast::Exp::AssignOp(name, op, exp) => format!("VAR[{}] ASSIGN_OP<{:?}> {}", name, op, pretty_expr(exp)),
        ast::Exp::CondExp(cond, exp1, exp2) => format!("TERNAR_OP IF {} DO {} ELSE DO {}", pretty_expr(cond), pretty_expr(exp1), pretty_expr(exp2)),
        ast::Exp::FuncCall(name, params) => format!("CALL {} WITH {}", name,
                                                        params.iter().map(|e| pretty_expr(e)).collect::<Vec<String>>().join(", ")),
    }
}
