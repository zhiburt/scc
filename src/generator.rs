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
    let exp1 = gen_expr(exp1);
    let exp2 = gen_expr(exp2);

    let code_with = |exp1: Vec<String>, exp2: Vec<String>, exp: &[String]| {
        let mut code = Vec::with_capacity(exp1.len() + exp2.len());
        code.extend(exp1);
        code.push("push %rax".to_owned());
        code.extend(exp2);
        code.push("pop %rcx".to_owned());
        code.extend_from_slice(exp);
        code
    };

    match op {
        ast::BinOp::BitwiseXor => {
            code_with(exp1, exp2, &["xor %rcx, %rax".to_owned()])
        },
        ast::BinOp::BitwiseOr => {
            code_with(exp1, exp2, &["or %rcx, %rax".to_owned()])
        },
        ast::BinOp::BitwiseAnd => {
            code_with(exp1, exp2, &["and %rcx, %rax".to_owned()])
        },
        ast::BinOp::Addition => {
            code_with(exp1, exp2, &["add %rcx, %rax".to_owned()])
        },
        ast::BinOp::Sub => {
            code_with(exp1, exp2, &[
                "sub %rax, %rcx".to_owned(),
                "mov %rcx, %rax".to_owned()
            ])
        },
        ast::BinOp::Multiplication => {
            code_with(exp1, exp2, &["imul %rcx, %rax".to_owned()])
        },
        ast::BinOp::Division => {
            code_with(exp1, exp2, &[
                "mov %rax, %rbx".to_owned(),
                "mov %rcx, %rax".to_owned(),
                "mov %rbx, %rcx".to_owned(),
                "cqo".to_owned(),
                "idiv %rcx".to_owned()
            ])
        },
        ast::BinOp::Modulo => {
            code_with(exp1, exp2, &[
                "mov %rax, %rbx".to_owned(),
                "mov %rcx, %rax".to_owned(),
                "mov %rbx, %rcx".to_owned(),
                "cqo".to_owned(),
                "idiv %rcx".to_owned(),
                "mov %rdx, %rax".to_owned()
            ])
        },
        ast::BinOp::And => {
            let label = unique_label("");
            let end_label = unique_label("end");
            let mut code = Vec::new();
            code.extend(exp1);
            code.push("cmp    $0, %rax".to_owned());
            code.push(format!("jne    {}", label));
            code.push(format!("jmp    {}", end_label));
            code.push(format!("{}:", label));
            code.extend(exp2);
            code.push("cmp    $0, %rax".to_owned());
            code.push("mov    $0, %rax".to_owned());
            code.push("setne    %al".to_owned());
            code.push(format!("{}:", end_label));
            code
        },
        ast::BinOp::Or => {
            let label = unique_label("");
            let end_label = unique_label("end");
            let mut code = Vec::new();
            code.extend(exp1);
            code.push("cmp    $0, %rax".to_owned());
            code.push(format!("je    {}", label));
            code.push("mov    $1, %rax".to_owned());
            code.push(format!("jmp    {}", end_label));
            code.push(format!("{}:", label));
            code.extend(exp2);
            code.push("cmp    $0, %rax".to_owned());
            code.push("mov    $0, %rax".to_owned());
            code.push("setne    %al".to_owned());
            code.push(format!("{}:", end_label));
            code
        },
        ast::BinOp::Equal => {
            code_with(exp1, exp2, &["sete    %al".to_owned()])
        },
        ast::BinOp::NotEqual => {
            code_with(exp1, exp2, &["setne    %al".to_owned()])
        },
        ast::BinOp::LessThan => {
            code_with(exp1, exp2, &["setl    %al".to_owned()])
        },
        ast::BinOp::LessThanOrEqual => {
            code_with(exp1, exp2, &["setle    %al".to_owned()])
        },
        ast::BinOp::GreaterThan => {
            code_with(exp1, exp2, &["setg    %al".to_owned()])
        },
        ast::BinOp::GreaterThanOrEqual => {
            code_with(exp1, exp2, &["setge    %al".to_owned()])
        },
        ast::BinOp::BitwiseLeftShift => {
            code_with(exp1, exp2, &["sal %rcx, %rax".to_owned()])
        },
        ast::BinOp::BitwiseRightShift => {
            code_with(exp1, exp2, &["sar %rcx, %rax".to_owned()])
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