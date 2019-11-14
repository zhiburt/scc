use crate::{ast};

pub fn gen(p: ast::Program, start_point: &str) -> String {
    let header = format!("\t.globl {}", start_point);
    format!("{}\n{}", header, gen_decl(&p.0))
}

fn gen_decl(st: &ast::Declaration) -> String {
    match st {
        ast::Declaration::Func{name, state} => {
            let statements_code = gen_statement(state);
            let mut pretty_code = statements_code
                .iter()
                .map(|c| format!("\t{}", c))
                .collect::<Vec<String>>();
            let func_name = format!("{}:", name);
            pretty_code.insert(0, func_name);
            pretty_code.join("\n")
        }
    }
}

fn gen_statement(st: &ast::Statement) -> Vec<String> {
    match st {
        ast::Statement::Return{exp} => {
            let mut expr_code = gen_expr(&exp);
            expr_code.push("ret".to_owned());
            expr_code
        }
    }
}

fn gen_expr(expr: &ast::Exp) -> Vec<String> {
    match expr {
        ast::Exp::Const(c) => gen_const(c),
        ast::Exp::UnOp(op, exp) => gen_unop(op, exp),
        ast::Exp::BinOp(op, exp1, exp2) => gen_binop(op, exp1, exp2),
    }
}

fn gen_const(c: &ast::Const) -> Vec<String> {
    match c {
        ast::Const::Int(val) => vec![format!("mov    ${}, %rax", val)]
    }
}

fn gen_unop(op: &ast::UnOp, exp: &ast::Exp) -> Vec<String> {
    let mut code = gen_expr(exp);
    code.extend(
        match op {
            ast::UnOp::Negation => {
                vec!["neg    %rax".to_owned()]
            }
            ast::UnOp::LogicalNegation => {
                vec![
                    "cmpl    $0, %eax".to_owned(),
                    "movl    $0, %eax".to_owned(),
                    "sete    %al".to_owned()
                ]
            }
            ast::UnOp::BitwiseComplement => {
                vec!["not    %eax".to_owned()]
            }
        }
    );
    code
}

fn gen_binop(op: &ast::BinOp, exp1: &ast::Exp, exp2: &ast::Exp) -> Vec<String> {
    let mut code = gen_expr(exp1);
    let exp2_code = gen_expr(exp2);
    code.push("push %rax".to_owned());
    code.extend(exp2_code);
    code.push("pop %rcx".to_owned());
    match op {
        ast::BinOp::BitwiseXor => {
            vec!["xor %rcx, %rax".to_owned()]
        },
        ast::BinOp::BitwiseOr => {
            vec!["or %rcx, %rax".to_owned()]
        },
        ast::BinOp::BitwiseAnd => {
            vec!["and %rcx, %rax".to_owned()]
        },
        ast::BinOp::Addition => {
            vec!["add %rcx, %rax".to_owned()]
        },
        ast::BinOp::Sub => {
            vec![
                "sub %rax, %rcx".to_owned(),
                "mov %rcx, %rax".to_owned()
            ]
        },
        ast::BinOp::Multiplication => {
            vec!["imul %rcx, %rax".to_owned()]
        },
        ast::BinOp::Division => {
            vec![
                "mov %rax, %rbx".to_owned(),
                "mov %rcx, %rax".to_owned(),
                "mov %rbx, %rcx".to_owned(),
                "cqo".to_owned(),
                "idiv %rcx".to_owned()
            ]
        },
        ast::BinOp::Modulo => {
            vec![
                "mov %rax, %rbx".to_owned(),
                "mov %rcx, %rax".to_owned(),
                "mov %rbx, %rcx".to_owned(),
                "cqo".to_owned(),
                "idiv %rcx".to_owned(),
                "mov %rdx, %rax".to_owned()
            ]
        },
        ast::BinOp::And => {
            let label = unique_label("");
            let end_label = unique_label("end");
            vec![
                "cmp    $0, %rax".to_owned(),
                format!("jne    {}", label),
                format!("jmp    {}", end_label),
                format!("{}:", label),
            // code.extend(gen_eq_expr(rhs_expr));
                "cmp    $0, %rax".to_owned(),
                "mov    $0, %rax".to_owned(),
                "setne    %al".to_owned(),
                format!("{}:", end_label)
            ]
        },
        ast::BinOp::Or => {
            let label = unique_label("");
            let end_label = unique_label("end");
            vec![
                "cmp    $0, %rax".to_owned(),
                format!("je    {}", label),
                "mov    $1, %rax".to_owned(),
                format!("jmp    {}", end_label),
                format!("{}:", label),
                // code.extend(gen_logical_expr(rhs_expr));
                "cmp    $0, %rax".to_owned(),
                "mov    $0, %rax".to_owned(),
                "setne    %al".to_owned(),
                format!("{}:", end_label)
            ]
        },
        ast::BinOp::Equal => {
            vec!["sete    %al".to_owned()]
        },
        ast::BinOp::NotEqual => {
            vec!["setne    %al".to_owned()]
        },
        ast::BinOp::LessThan => {
            vec!["setl    %al".to_owned()]
        },
        ast::BinOp::LessThanOrEqual => {
            vec!["setle    %al".to_owned()]
        },
        ast::BinOp::GreaterThan => {
            vec!["setg    %al".to_owned()]
        },
        ast::BinOp::GreaterThanOrEqual => {
            vec!["setge    %al".to_owned()]
        },
        ast::BinOp::BitwiseLeftShift => {
            vec!["sal %rcx, %rax".to_owned()]
        },
        ast::BinOp::BitwiseRightShift => {
            vec!["sar %rcx, %rax".to_owned()]
        },
    }
}

fn unique_label(prefix: &str) -> String {
    static mut LABEL_COUNTER: usize = 0;
    unsafe {
        LABEL_COUNTER += 1;
        if prefix.is_empty() {
            format!("_clause{}", LABEL_COUNTER)
        } else {
            format!("_{}{}", prefix, LABEL_COUNTER)
        }
    }
}