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
        .join(",");

    let body = blocks.as_ref().map_or("".to_owned(), |blocks| { 
        blocks
            .iter()
            .map(|block| {
                pretty_block(block)
                .iter()
                .map(|line| format!("\t{}", &line))
                .collect::<Vec<_>>()
            })
            .flatten()
            .collect::<Vec<String>>()
            .join("\n")
    });

    format!("Function {}:\nparameters: {}\nbody:\n{}", name, params, body)
}

fn pretty_block(block: &ast::BlockItem) -> Vec<String> {
    match block {
        ast::BlockItem::Declaration(decl) => vec![pretty_decl(decl)],
        ast::BlockItem::Statement(statement) => pretty_stat(statement),
    }
}

fn pretty_stat(st: &ast::Statement) -> Vec<String> {
    match st {
        ast::Statement::Return{exp} => vec![format!("RETURN {}", pretty_expr(exp))],
        ast::Statement::Exp{exp} => vec![format!("{}", pretty_opt_expr(exp))],
        ast::Statement::Conditional{cond_expr, if_block, else_block} => {
            let mut out = Vec::new();
            out.push(format!("IF {}:", pretty_expr(cond_expr)));
            out.extend(pretty_stat(if_block));

            if let Some(else_block) = else_block {
                out.push("ELSE:".to_string());
                out.extend(pretty_stat(else_block));
            }

            out.push("END".to_string());

            out
        }
        ast::Statement::Compound{list} => {
            match list {
                Some(list) => {
                    let mut out = Vec::new();
                    out.push("COMPOUND:".to_string());
                    for state_or_def in list {
                        match state_or_def {
                            ast::BlockItem::Declaration(decl) => out.push(pretty_decl(decl)),
                            ast::BlockItem::Statement(stat) => out.extend(pretty_stat(stat)),
                        }
                    }
                    out
                }
                None => vec!["COMPOUND:".to_string()],
            }
        }
        ast::Statement::While{exp, statement} => {
            let mut out = Vec::new();
            out.push(format!("WHILE {}", pretty_expr(exp)));
            out.extend(pretty_stat(statement));
            out.push("END".to_string());
            out
        }
        ast::Statement::Do{statement, exp} => {
            let mut out = Vec::new();
            out.push(format!("DOWHILE {}", pretty_expr(exp)));
            out.extend(pretty_stat(statement));
            out.push("END".to_string());
            out
        }
        ast::Statement::ForDecl{decl, exp2, exp3, statement} => {
            let mut out = Vec::new();
            out.push("FOR:".to_string());
            out.push(format!("decl: {}", pretty_decl(decl)));
            out.push(format!("cond: {}", pretty_expr(exp2)));
            out.push(format!("exp: {}", exp3.as_ref().map_or("".to_string(), |e| pretty_expr(&e))));
            out.extend(pretty_stat(statement));
            out.push("END".to_string());
            out
        }
        ast::Statement::For{exp1, exp2, exp3, statement} => {
            let mut out = Vec::new();
            out.push("FOR:".to_string());
            out.push(format!("decl: {}", exp3.as_ref().map_or("".to_string(), |e| pretty_expr(&e))));
            out.push(format!("cond: {}", pretty_expr(exp2)));
            out.push(format!("exp: {}", exp3.as_ref().map_or("".to_string(), |e| pretty_expr(&e))));
            out.extend(pretty_stat(statement));
            out.push("END".to_string());
            out
        }
        ast::Statement::Break => {
            vec!["BREAK".to_string()]
        }
        ast::Statement::Continue => {
            vec!["CONTINUE".to_string()]
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
        ast::Exp::Const(c) => format!("CONST<{:?}>", c),
        ast::Exp::UnOp(op, exp) => format!("UN_OP<{:?}> {}", op, pretty_expr(exp)),
        ast::Exp::Assign(name, exp) => format!("VAR<{}> = EXP<{}>", name, pretty_expr(exp)),
        ast::Exp::Var(name) => format!("VAR<{}>", name),
        ast::Exp::AssignOp(name, op, exp) => format!("VAR<{}> ASSIGN_OP<{:?}> {}", name, op, pretty_expr(exp)),
        ast::Exp::CondExp(cond, exp1, exp2) => format!("TERNAR_OP if {} do {} else do {}", pretty_expr(cond), pretty_expr(exp1), pretty_expr(exp2)),
        ast::Exp::FuncCall(name, params) => format!("CALL {} with {}", name,
                                                        params.iter().map(|e| pretty_expr(e)).collect::<Vec<String>>().join(",")),
    }
}
