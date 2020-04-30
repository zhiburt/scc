use super::{FuncDecl, Statement, Declaration, Exp, BlockItem};

pub trait Visitor<'ast> {
    fn visit_function(&mut self, func: &'ast FuncDecl) {
        visit_function(self, func);
    }

    fn visit_statement(&mut self, st: &'ast Statement) {
        visit_statement(self, st);
    }

    fn visit_decl(&mut self, decl: &'ast Declaration) {
        visit_decl(self, decl);
    }

    fn visit_expr(&mut self, exp: &'ast Exp) {
        visit_expr(self, exp);
    }

    fn visit_block(&mut self, block: &'ast BlockItem) {
        visit_block(self, block);
    }
}

pub fn visit_statement<'ast, V: Visitor<'ast> + ?Sized>(v: &mut V, st: &'ast Statement) {
    match st {
        Statement::Return { exp } => v.visit_expr(exp),
        Statement::Exp { exp } => {
            if let Some(exp) = exp {
                v.visit_expr(exp)
            }
        }
        Statement::Conditional {
            cond_expr,
            if_block,
            else_block,
        } => {
            v.visit_expr(cond_expr);
            v.visit_statement(if_block);
            if let Some(else_block) = else_block {
                v.visit_statement(else_block);
            }
        }
        Statement::Compound { list } => match list {
            Some(list) => {
                for block in list {
                    v.visit_block(block);
                }
            }
            None => (),
        },
        Statement::While { exp, statement } => {
            v.visit_expr(exp);
            v.visit_statement(statement);
        }
        Statement::Do { statement, exp } => {
            v.visit_statement(statement);
            v.visit_expr(exp);
        }
        Statement::ForDecl {
            decl,
            exp2,
            exp3,
            statement,
        } => {
            v.visit_decl(decl);
            v.visit_expr(exp2);
            if let Some(exp3) = exp3 {
                v.visit_expr(exp3);
            }
            v.visit_statement(statement);
        }
        Statement::For {
            exp1,
            exp2,
            exp3,
            statement,
        } => {
            if let Some(exp1) = exp1 {
                v.visit_expr(exp1);
            }
            v.visit_expr(exp2);
            if let Some(exp3) = exp3 {
                v.visit_expr(exp3);
            }
            v.visit_statement(statement);
        }
        Statement::Break => (),
        Statement::Continue => (),
    }
}

pub fn visit_expr<'ast, V: Visitor<'ast> + ?Sized>(v: &mut V, exp: &'ast Exp) {
    match exp {
        Exp::BinOp(_, exp1, exp2) => {
            v.visit_expr(exp1);
            v.visit_expr(exp2);
        }
        Exp::UnOp(_, exp) => v.visit_expr(exp),
        Exp::Assign(_, exp) => v.visit_expr(exp),
        Exp::AssignOp(_, _, exp) => v.visit_expr(exp),
        Exp::CondExp(cond, exp1, exp2) => {
            v.visit_expr(cond);
            v.visit_expr(exp1);
            v.visit_expr(exp2);
        }
        Exp::FuncCall(_, params) => {
            for exp in params {
                v.visit_expr(exp);
            }
        }
        Exp::IncOrDec(..) => (),
        Exp::Var(..) => (),
        Exp::Const(..) => (),
    }
}

pub fn visit_block<'ast, V: Visitor<'ast> + ?Sized>(v: &mut V, block: &'ast BlockItem) {
    match block {
        BlockItem::Declaration(decl) => v.visit_decl(decl),
        BlockItem::Statement(st) => v.visit_statement(st),
    }
}

pub fn visit_decl<'ast, V: Visitor<'ast> + ?Sized>(v: &mut V, decl: &'ast Declaration) {
    match decl {
        Declaration::Declare { exp, .. } => {
            if let Some(exp) = exp {
                v.visit_expr(exp)
            }
        }
    }
}

pub fn visit_function<'ast, V: Visitor<'ast> + ?Sized>(v: &mut V, func: &'ast FuncDecl) {
    if let Some(body) = func.blocks.as_ref() {
        for b in body {
            v.visit_block(b);
        }
    }
}
