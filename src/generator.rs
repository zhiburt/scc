use crate::{ast};

pub fn gen(p: ast::Program, start_point: &str) -> String {
    let header = format!("\t.globl {}", start_point);
    let mut asm_func = AsmFunc::new();
    format!("{}\n{}", header, asm_func.gen(&p.0))
}

struct AsmFunc {}


impl AsmFunc {
    fn new() -> Self {
        AsmFunc {}
    }

    fn gen(&self, st: &ast::Declaration) -> String {
        match st {
            ast::Declaration::Func{name, statements} => {
                let prologue = vec![
                    "push %rbp".to_owned(),
                    "mov %rsp, %rbp".to_owned(),
                ];

                let epilogue = vec![
                    "mov %rbp, %rsp".to_owned(),
                    "pop %rbp".to_owned(),
                    "ret".to_owned(),
                ];
                
                let statements_code = self.gen_statement(&statements[0]);
                let mut code = Vec::new();
                code.extend(prologue);
                code.extend(statements_code);
                code.extend(epilogue);
                
                let mut pretty_code = code
                    .iter()
                    .map(|c| format!("\t{}", c))
                    .collect::<Vec<String>>();
                let func_name = format!("{}:", name);
                pretty_code.insert(0, func_name);
                pretty_code.join("\n")
            }
        }
    }

    fn gen_statement(&self, st: &ast::Statement) -> Vec<String> {
        match st {
            ast::Statement::Return{exp} => {
                self.gen_expr(&exp)
            }
            _ => unimplemented!(),
        }
    }

    fn gen_expr(&self, expr: &ast::Exp) -> Vec<String> {
        match expr {
            ast::Exp::Const(c) => self.gen_const(c),
            ast::Exp::UnOp(op, exp) => self.gen_unop(op, exp),
            ast::Exp::BinOp(op, exp1, exp2) => self.gen_binop(op, exp1, exp2),
            _ => unimplemented!(),
        }
    }

    fn gen_const(&self, c: &ast::Const) -> Vec<String> {
        match c {
            ast::Const::Int(val) => vec![format!("mov    ${}, %rax", val)]
        }
    }

    fn gen_unop(&self, op: &ast::UnOp, exp: &ast::Exp) -> Vec<String> {
        let mut code = self.gen_expr(exp);
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

    fn gen_binop(&self, op: &ast::BinOp, exp1: &ast::Exp, exp2: &ast::Exp) -> Vec<String> {
        let exp1 = self.gen_expr(exp1);
        let exp2 = self.gen_expr(exp2);

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
                let label = AsmFunc::unique_label("");
                let end_label = AsmFunc::unique_label("end");
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
                let label = AsmFunc::unique_label("");
                let end_label = AsmFunc::unique_label("end");
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
                code_with(exp1, exp2, &[
                    "cmp    %rax, %rcx".to_owned(),
                    "mov    $0, %eax".to_owned(),
                    "sete    %al".to_owned()
                ])
            },
            ast::BinOp::NotEqual => {
                code_with(exp1, exp2, &[
                    "cmp    %rax, %rcx".to_owned(),
                    "mov    $0, %eax".to_owned(),
                    "setne    %al".to_owned()
                ])
            },
            ast::BinOp::LessThan => {
                code_with(exp1, exp2, &[
                    "cmp    %rax, %rcx".to_owned(),
                    "mov    $0, %eax".to_owned(),
                    "setl    %al".to_owned()
                ])
            },
            ast::BinOp::LessThanOrEqual => {
                code_with(exp1, exp2, &[
                    "cmp    %rax, %rcx".to_owned(),
                    "mov    $0, %eax".to_owned(),
                    "setle    %al".to_owned()
                ])
            },
            ast::BinOp::GreaterThan => {
                code_with(exp1, exp2, &[
                    "cmp    %rax, %rcx".to_owned(),
                    "mov    $0, %eax".to_owned(),
                    "setg    %al".to_owned()
                ])
            },
            ast::BinOp::GreaterThanOrEqual => {
                code_with(exp1, exp2, &[
                    "cmp    %rax, %rcx".to_owned(),
                    "mov    $0, %eax".to_owned(),
                    "setge    %al".to_owned()
                ])
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
}