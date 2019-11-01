use std::io::{Read, Write};

use simple_c_compiler::{
    TokenType, Token, Lexer,
    Expression, Declaration, Program, Statement, BinOp, UnaryOp,
    Term, FactOp, Factor,
};

fn gen(p: Program, start_point: &str) -> String {
    let header = format!("\t.globl {}", start_point);
    format!("{}\n{}", header, gen_decl(&p.0))
}

fn gen_decl(st: &Declaration) -> String {
    match st {
        Declaration::Func(name, statement) => {
            let statements_code = gen_statement(&statement);
            let mut pretty_code = statements_code.iter().map(|c| format!("\t{}", c)).collect::<Vec<String>>();
            let func_name = format!("{}:", name);
            pretty_code.insert(0, func_name);
            pretty_code.join("\n")
        },
    }
}

fn gen_statement(st: &Statement) -> Vec<String> {
    match st {
        Statement::Return(expr) => {
            let mut expr_code = gen_expr(&expr);
            expr_code.push("ret".to_owned());
            expr_code
        },
    }
}

fn gen_expr(expr: &Expression) -> Vec<String> {
    match expr {
        Expression::Term(term) => {
            gen_term(term)
        },
        Expression::BinOp(left_term, op, right_term) => {
            let mut fact = gen_term(left_term);
            fact.push("push %rax".to_owned());
            fact.extend(gen_term(right_term));
            fact.push("pop %rcx".to_owned());

            match op {
                BinOp::Plus => {
                    fact.push("add %rcx, %rax".to_owned());
                },
                BinOp::Minus => {
                    fact.push("sub %rax, %rcx".to_owned());
                    fact.push("mov %rcx, %rax".to_owned());
                },
            }

            fact
        },
    }
}

fn gen_term(term: &Term) -> Vec<String> {
    match term {
        Term::Fact(fact) => {
            gen_fact(fact)
        },
        Term::FactorOp(left_fact, op, right_fact) => {
            let mut fact = gen_fact(left_fact);
            fact.push("push %rax".to_owned());
            fact.extend(gen_fact(right_fact));
            fact.push("pop %rcx".to_owned());

            match op {
                FactOp::Division => {
                    fact.push("mov %rax, %rbx".to_owned());
                    fact.push("mov %rcx, %rax".to_owned());
                    fact.push("mov %rbx, %rcx".to_owned());
                    fact.push("cqo".to_owned());
                    fact.push("idiv %rcx".to_owned());
                },
                FactOp::Multiplication => fact.push("imul %rcx, %rax".to_owned()),
            }

            fact
        },
    }
}

fn gen_fact(fact: &Factor) -> Vec<String> {
    match fact {
        Factor::Const(val) => {
            vec![format!("mov    ${}, %rax", val)]
        },
        Factor::Expr(expr) => {
            gen_expr(expr)
        },
        Factor::UnOp(op, factor) => {
            let mut fact_code = gen_fact(factor);
            match op {
                UnaryOp::BitwiseComplement => {
                    fact_code.push("not    %eax".to_owned());
                },
                UnaryOp::Negation => {
                    fact_code.push("neg    %rax".to_owned());
                },
                UnaryOp::LogicalNegation => {
                    fact_code.push("cmpl    $0, %eax".to_owned());
                    fact_code.push("movl    $0, %eax".to_owned());
                    fact_code.push("sete    %al".to_owned());
                },
            }
            
            fact_code
        },
    }
}

fn pretty_tokens(tokens: &Vec<Token>) -> String {
    format!(
        "{:?}",
        tokens
            .iter()
            .map(|t| t.token_type)
            .collect::<Vec<TokenType>>()
    )
}

fn pretty_program(program: &Program) -> String {
    pretty_decl(&program.0)
}

fn pretty_decl(d: &Declaration) -> String {
    match d {
        Declaration::Func(name, statement) => {
            format!("FUN {}:\n   body:\n      {}", name, pretty_statement(&statement))
        },
    }
}

fn pretty_statement(s: &Statement) -> String {
    match s {
        Statement::Return(expr) => format!("RETURN {}", pretty_expr(expr))
    }
}

fn pretty_expr(s: &Expression) -> String {
    match s {
        Expression::Term(term) => format!("Term<{}>", pretty_term(term)),
        Expression::BinOp(lt, op, rt) => format!("{} BinOp<{:?}> {}", pretty_term(lt), op, pretty_term(rt)),
    }
}

fn pretty_term(t: &Term) -> String {
    match t {
        Term::Fact(factor) => format!("Fact<{}>", pretty_fact(factor)),
        Term::FactorOp(lf, op, rf) => format!("{} Op<{:?}> {}", pretty_fact(lf), op, pretty_fact(rf)),
       }
}

fn pretty_fact(f: &Factor) -> String {
    match f {
        Factor::Const(e) => format!("Const<{}>", e),
        Factor::Expr(expr) => pretty_expr(expr),
        Factor::UnOp(op, f) => format!("UnOp<{:?}> Operation<{}>", op, pretty_fact(f)),
    }
}

fn main() {
    let file = std::env::args().collect::<Vec<String>>()[1].clone();
    let program = std::fs::File::open(file).unwrap();
    let lexer = Lexer::new();
    let mut tokens = lexer.lex(program);
    
    let program = Program::parse(&mut tokens).expect("Cannot parse program");
    
    println!("{}", pretty_program(&program));
    
    let mut asm_file = std::fs::File::create("assembly.s").expect("Cannot create assembler code");
    asm_file.write_all(gen(program, "main").as_ref()).unwrap();
}
