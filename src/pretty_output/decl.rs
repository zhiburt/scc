use simple_c_compiler::{parser};

pub fn pretty_decl(d: &parser::Declaration) -> String {
    match d {
        parser::Declaration::Func(name, statement) => format!(
            "FUN {}:\n   body:\n      {}",
            name,
            pretty_statement(&statement)
        ),
    }
}

fn pretty_statement(s: &parser::Statement) -> String {
    match s {
        parser::Statement::Return(expr) => format!("RETURN {}", pretty_expr(expr)),
    }
}

fn pretty_expr(e: &parser::Expression) -> String {    
    let bitwise_logic_expr = |e: &parser::BitwiseLogicExpr| -> String {
        let s = pretty_term(&e.0);
        if let Some((op, term)) = &e.1 {
            format!("{} Op<{:?}> {}", s, op, pretty_term(term))
        } else {
            s
        }
    };

    let bitwise_expr = |e: &parser::BitwiseExpr| -> String {
        let s = bitwise_logic_expr(&e.0);
        if let Some((op, term)) = &e.1 {
            format!("{} Op<{:?}> {}", s, op, bitwise_logic_expr(term))
        } else {
            s
        }
    };

    let additive_expr = |e: &parser::AdditiveExpr| -> String {
        let s = bitwise_expr(&e.0);
        if let Some((op, term)) = &e.1 {
            format!("{} Op<{:?}> {}", s, op, bitwise_expr(term))
        } else {
            s
        }
    };

    let relational_expr = |e: &parser::RelationalExpr| -> String {
        let s = additive_expr(&e.0);
        if let Some((op, e)) = &e.1 {
            format!("{} Op<{:?} {}", s, op, additive_expr(e))
        } else {
            s
        }
    };

    let pretty_equality_expr = |e: &parser::EqualityExpr| -> String {
        let s = relational_expr(&e.0);
        if let Some((op, e)) = &e.1 {
            format!("{} Op<{:?} {}", s, op, relational_expr(e))
        } else {
            s
        }
    };

    let pretty_logical_expr = |e: &parser::LogicalAndExpr| -> String {
        let s = pretty_equality_expr(&e.0);
        if let Some(e) = &e.1 {
            format!("{} Op<Or> {}", s, pretty_equality_expr(e))
        } else {
            s
        }
    };

    let s = pretty_logical_expr(&e.0);
    if let Some(e) = &e.1 {
        format!("{} Op<And> {}", s, pretty_logical_expr(e))
    } else {
        s
    }
}

fn pretty_term(t: &parser::Term) -> String {
    match t {
        parser::Term::Fact(factor) => format!("Fact<{}>", pretty_fact(factor)),
        parser::Term::FactorOp(lf, op, rf) => {
            format!("{} Op<{:?}> {}", pretty_fact(lf), op, pretty_fact(rf))
        }
    }
}

fn pretty_fact(f: &parser::Factor) -> String {
    match f {
        parser::Factor::Const(e) => format!("Const<{}>", e),
        parser::Factor::Expr(expr) => pretty_expr(expr),
        parser::Factor::UnOp(op, f) => format!("UnOp<{:?}> Operation<{}>", op, pretty_fact(f)),
    }
}
