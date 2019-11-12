use crate::{parser};

pub fn gen(p: parser::Program, start_point: &str) -> String {
    let header = format!("\t.globl {}", start_point);
    format!("{}\n{}", header, gen_decl(&p.0))
}

fn gen_decl(st: &parser::Declaration) -> String {
    match st {
        parser::Declaration::Func(name, statement) => {
            let statements_code = gen_statement(&statement);
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

fn gen_statement(st: &parser::Statement) -> Vec<String> {
    match st {
        parser::Statement::Return(expr) => {
            let mut expr_code = gen_expr(&expr);
            expr_code.push("ret".to_owned());
            expr_code
        }
    }
}

fn gen_expr(expr: &parser::Expression) -> Vec<String> {
    let mut code = gen_logical_expr(&expr.0);
    if let Some(rhs_expr) = &expr.1 {
        let label = unique_label("");
        let end_label = unique_label("end");
        code.push("cmp    $0, %rax".to_owned());
        code.push(format!("je    {}", label));
        code.push("mov    $1, %rax".to_owned());
        code.push(format!("jmp    {}", end_label));
        code.push(format!("{}:", label));
        code.extend(gen_logical_expr(rhs_expr));
        code.push("cmp    $0, %rax".to_owned());
        code.push("mov    $0, %rax".to_owned());
        code.push("setne    %al".to_owned());
        code.push(format!("{}:", end_label));
    
    }

    
    code
}

fn gen_logical_expr(expr: &parser::LogicalAndExpr) -> Vec<String> {
    let mut code = gen_eq_expr(&expr.0);
    if let Some(rhs_expr) = &expr.1 {
        let label = unique_label("");
        let end_label = unique_label("end");
        code.push("cmp    $0, %rax".to_owned());
        code.push(format!("jne    {}", label));
        code.push(format!("jmp    {}", end_label));
        code.push(format!("{}:", label));
        code.extend(gen_eq_expr(rhs_expr));
        code.push("cmp    $0, %rax".to_owned());
        code.push("mov    $0, %rax".to_owned());
        code.push("setne    %al".to_owned());
        code.push(format!("{}:", end_label));
    }
    
    code
}

fn gen_eq_expr(expr: &parser::EqualityExpr) -> Vec<String> {
    let mut code = gen_relational_expr(&expr.0);
    if let Some((op, rhs_expr)) = &expr.1 {
        code.push("push %rax".to_owned());
        code.extend(gen_relational_expr(rhs_expr));
        code.push("pop %rcx".to_owned());
        code.push("cmp    %rax, %rcx".to_owned());
        code.push("mov    $0, %eax".to_owned());
        match op {
            parser::EqualityOp::Equal => {
                code.push("sete    %al".to_owned());
            }
            parser::EqualityOp::NotEqual => {
                code.push("setne    %al".to_owned());
            }
        }
    }
    
    code
}

fn gen_relational_expr(expr: &parser::RelationalExpr) -> Vec<String> {
    let mut code = gen_addictive_expr(&expr.0);
    if let Some((op, rhs_expr)) = &expr.1 {
        code.push("push %rax".to_owned());
        code.extend(gen_addictive_expr(rhs_expr));
        code.push("pop %rcx".to_owned());
        code.push("cmp    %rax, %rcx".to_owned());
        code.push("mov    $0, %eax".to_owned());
        match op {
            parser::RelationalOp::Greater => {
                code.push("setg    %al".to_owned());
            }
            parser::RelationalOp::GreaterOrEqual => {
                code.push("setge    %al".to_owned());
            }
            parser::RelationalOp::Less => {
                code.push("setl    %al".to_owned());
            }
            parser::RelationalOp::LessOrEqual => {
                code.push("setle    %al".to_owned());
            }
        }
    }

    code
}

fn gen_bitwise_expr(expr: &parser::BitwiseExpr) -> Vec<String> {
    let mut code = gen_bitwise_logic_expr(&expr.0);
    if let Some((op, rhs_expr)) = &expr.1 {
        code.push("push %rax".to_owned());
        code.extend(gen_bitwise_logic_expr(rhs_expr));
        code.push("pop %rcx".to_owned());
        match op {
            parser::BitwiseOp::LeftShift => {
                code.push("sal %rcx, %rax".to_owned());
            }
            parser::BitwiseOp::RightShift => {
                code.push("sar %rcx, %rax".to_owned());
            }
        }
    }

    code
}

fn gen_bitwise_logic_expr(expr: &parser::BitwiseLogicExpr) -> Vec<String> {
    let mut code = gen_term(&expr.0);
    if let Some((op, rhs_expr)) = &expr.1 {
        code.push("push %rax".to_owned());
        code.extend(gen_term(rhs_expr));
        code.push("pop %rcx".to_owned());
        match op {
            parser::BitLogicOp::And => {
                code.push("and %rcx, %rax".to_owned());
            }
            parser::BitLogicOp::Xor => {
                code.push("xor %rcx, %rax".to_owned());
            }
            parser::BitLogicOp::Or => {
                code.push("or %rcx, %rax".to_owned());
            }
        }
    }

    code
}

fn gen_addictive_expr(expr: &parser::AdditiveExpr) -> Vec<String> {
    let mut code = gen_bitwise_expr(&expr.0);
    if let Some((op, right_term)) = &expr.1 {
        code.push("push %rax".to_owned());
        code.extend(gen_bitwise_expr(right_term));
        code.push("pop %rcx".to_owned());
        match op {
            parser::BinOp::Plus => {
                code.push("add %rcx, %rax".to_owned());
            }
            parser::BinOp::Minus => {
                code.push("sub %rax, %rcx".to_owned());
                code.push("mov %rcx, %rax".to_owned());
            }
        }
    };

    code
}

fn gen_term(term: &parser::Term) -> Vec<String> {
    match term {
        parser::Term::Fact(fact) => gen_fact(fact),
        parser::Term::FactorOp(left_fact, op, right_fact) => {
            let mut fact = gen_fact(left_fact);
            fact.push("push %rax".to_owned());
            fact.extend(gen_fact(right_fact));
            fact.push("pop %rcx".to_owned());

            match op {
                parser::FactOp::Division => {
                    fact.push("mov %rax, %rbx".to_owned());
                    fact.push("mov %rcx, %rax".to_owned());
                    fact.push("mov %rbx, %rcx".to_owned());
                    fact.push("cqo".to_owned());
                    fact.push("idiv %rcx".to_owned());
                }
                parser::FactOp::Modulo => {
                    fact.push("mov %rax, %rbx".to_owned());
                    fact.push("mov %rcx, %rax".to_owned());
                    fact.push("mov %rbx, %rcx".to_owned());
                    fact.push("cqo".to_owned());
                    fact.push("idiv %rcx".to_owned());
                    fact.push("mov %rdx, %rax".to_owned());
                }
                parser::FactOp::Multiplication => fact.push("imul %rcx, %rax".to_owned()),
            }

            fact
        }
    }
}

fn gen_fact(fact: &parser::Factor) -> Vec<String> {
    match fact {
        parser::Factor::Const(val) => vec![format!("mov    ${}, %rax", val)],
        parser::Factor::Expr(expr) => gen_expr(expr),
        parser::Factor::UnOp(op, factor) => {
            let mut fact_code = gen_fact(factor);
            match op {
                parser::UnaryOp::BitwiseComplement => {
                    fact_code.push("not    %eax".to_owned());
                }
                parser::UnaryOp::Negation => {
                    fact_code.push("neg    %rax".to_owned());
                }
                parser::UnaryOp::LogicalNegation => {
                    fact_code.push("cmpl    $0, %eax".to_owned());
                    fact_code.push("movl    $0, %eax".to_owned());
                    fact_code.push("sete    %al".to_owned());
                }
            }
            fact_code
        }
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