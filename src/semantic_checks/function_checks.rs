use crate::ast;
use std::collections::{HashMap, HashSet};

pub fn func_check(prog: &ast::Program) -> bool {
    if !global_check(prog) {
        return false;
    }

    calls_precidence_check(prog)
}

fn global_check(prog: &ast::Program) -> bool {
    let mut functions: HashMap<String, &ast::FuncDecl> = HashMap::new();
    for func in &prog.0 {
        if let Some(f) = functions.get(&func.name) {
            if f.parameters.len() == func.parameters.len() {
                break;
            }

            return false;
        }

        functions.insert(func.name.clone(), func);
    }

    return true;
}

fn calls_precidence_check(prog: &ast::Program) -> bool {
    let mut used_funcs = Vec::new();
    let mut declared_funcs = HashSet::new();
    for func in &prog.0 {
        declared_funcs.insert((&func.name, func.parameters.len()));
        match &func.blocks {
            Some(blocks) => {
                for block in blocks {
                    let mut check = |exp: &ast::Exp| match exp {
                        ast::Exp::FuncCall(name, params) => {
                            used_funcs.push((name.clone(), params.len()))
                        }
                        _ => (),
                    };

                    _block_check(block, &mut check);
                }

                for (f_name, param_size) in used_funcs.iter() {
                    if !declared_funcs.contains(&(&f_name, *param_size)) {
                        return false;
                    }
                }
            }
            None => (),
        }
    }

    true
}

fn _block_check<F: FnMut(&ast::Exp)>(block: &ast::BlockItem, mut exp_call: &mut F) {
    match block {
        ast::BlockItem::Statement(s) => {
            _statement_check(s, exp_call);
        }
        ast::BlockItem::Declaration(ast::Declaration::Declare { name, exp }) => {
            if let Some(exp) = exp {
                exp_call(exp);
            }
        }
    }
}

fn _statement_check<F: FnMut(&ast::Exp)>(s: &ast::Statement, mut exp_call: &mut F) {
    match s {
        ast::Statement::Compound { list } => {
            if let Some(list) = list {
                for b in list {
                    _block_check(b, exp_call);
                }
            }
        }
        ast::Statement::Return { exp } => {
            exp_call(exp);
        }
        ast::Statement::Exp { exp } => {
            if let Some(exp) = exp {
                exp_call(exp);
            }
        }
        ast::Statement::For {
            exp1,
            exp2,
            exp3,
            statement,
        } => {
            if let Some(exp) = exp1 {
                exp_call(exp);
            }
            if let Some(exp) = exp3 {
                exp_call(exp);
            }
            exp_call(exp2);
            _statement_check(statement, exp_call);
        }
        ast::Statement::ForDecl {
            decl,
            exp2,
            exp3,
            statement,
        } => {
            if let ast::Declaration::Declare { name, exp } = decl {
                if let Some(exp) = exp {
                    exp_call(exp);
                }
            }
            if let Some(exp) = exp3 {
                exp_call(exp);
            }
            exp_call(exp2);
            _statement_check(statement, exp_call);
        }
        ast::Statement::While { exp, statement } => {
            exp_call(exp);
            _statement_check(statement, exp_call);
        }
        ast::Statement::Do { statement, exp } => {
            exp_call(exp);
            _statement_check(statement, exp_call);
        }
        ast::Statement::Conditional {
            cond_expr,
            else_block,
            if_block,
        } => {
            exp_call(cond_expr);
            if let Some(s) = else_block {
                _statement_check(s, exp_call);
            }
            _statement_check(if_block, exp_call);
        }
        _ => {}
    }
}
