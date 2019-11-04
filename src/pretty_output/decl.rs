use simple_c_compiler::{Declaration, Expression, Factor, Program, Statement, Term};

pub fn pretty_decl(d: &Declaration) -> String {
    match d {
        Declaration::Func(name, statement) => format!(
            "FUN {}:\n   body:\n      {}",
            name,
            pretty_statement(&statement)
        ),
    }
}

fn pretty_statement(s: &Statement) -> String {
    match s {
        Statement::Return(expr) => format!("RETURN {}", pretty_expr(expr)),
    }
}

fn pretty_expr(s: &Expression) -> String {
    match s {
        Expression::Term(term) => format!("Term<{}>", pretty_term(term)),
        Expression::BinOp(lt, op, rt) => {
            format!("{} BinOp<{:?}> {}", pretty_term(lt), op, pretty_term(rt))
        }
    }
}

fn pretty_term(t: &Term) -> String {
    match t {
        Term::Fact(factor) => format!("Fact<{}>", pretty_fact(factor)),
        Term::FactorOp(lf, op, rf) => {
            format!("{} Op<{:?}> {}", pretty_fact(lf), op, pretty_fact(rf))
        }
    }
}

fn pretty_fact(f: &Factor) -> String {
    match f {
        Factor::Const(e) => format!("Const<{}>", e),
        Factor::Expr(expr) => pretty_expr(expr),
        Factor::UnOp(op, f) => format!("UnOp<{:?}> Operation<{}>", op, pretty_fact(f)),
    }
}
