use crate::{ast};
use std::collections::{HashMap, HashSet};

pub fn gen(p: ast::Program, start_point: &str) -> Result<String> {
    let header = format!("\t.globl {}", start_point);
    Ok(format!("{}\n{}", header, gen_func(&p.0)?))
}

pub type Result<T> = std::result::Result<T, GenError>;

#[derive(Debug)]
pub enum GenError {
    InvalidVariableUsage(String),
    BreakWrongUsage,
    ContinueWrongUsage,
}

impl std::fmt::Display for GenError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            GenError::InvalidVariableUsage(var) => write!(f, "gen error {}", var),
            GenError::BreakWrongUsage => write!(f, "gen error break used not in loop scope"),
            GenError::ContinueWrongUsage => write!(f, "gen error continue used not in loop scope"),
        }
    }
}

impl std::error::Error for GenError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

const PLATFORM_WORD_SIZE: i64 = 8;

#[derive(Clone)]
struct AsmScope {
    variable_map: HashMap<String, i64>,
    current_scope: HashSet<String>,
    stack_index: i64,
    end_func_label: String,
    loop_context: Option<LoopContext>,
}

#[derive(Clone)]
struct LoopContext {
    begin_label: String,
    end_label: String,
}

fn gen_func(ast::FuncDecl{name, blocks}: &ast::FuncDecl) -> Result<String> {
    let scope = AsmScope{
        variable_map: HashMap::new(),
        stack_index: -PLATFORM_WORD_SIZE,
        current_scope: HashSet::new(),
        end_func_label: unique_label("_end_func_"),
        loop_context: None,
    };


    let mut code = Vec::new();
    code.extend(prologue());

    let return_exists = blocks.iter().any(|stat| match stat {
        ast::BlockItem::Statement(ast::Statement::Return{..}) => true,
        _ => false,
    });

    let c = gen_block(&blocks, &scope)?;
    code.extend(c);

    if !return_exists {
        code.push("ret $0".to_owned());
    }

    code.push(format!("{}:", scope.end_func_label));
    code.extend(epilogue());

    let mut pretty_code = code
        .iter()
        .map(|c| format!("\t{}", c))
        .collect::<Vec<String>>();
    let func_name = format!("{}:", name);
    pretty_code.insert(0, func_name);
    Ok(pretty_code.join("\n"))
}

fn gen_statement(st: &ast::Statement, scope: &AsmScope) -> Result<Vec<String>> {
    match st {
        ast::Statement::Exp{exp} => exp.as_ref().map_or(Ok(Vec::new()), |exp| gen_expr(exp, scope)),
        ast::Statement::Return{exp} => {
            let mut code = gen_expr(&exp, scope)?;
            code.push(format!("jmp {}", scope.end_func_label));

            Ok(code)
        },
        ast::Statement::Conditional{cond_expr, if_block, else_block} => {
            let cond = gen_expr(cond_expr, scope)?;
            let if_block = gen_statement(if_block, scope)?;
            let else_block = else_block.as_ref().map_or(None, |block| Some(gen_statement(block, scope).unwrap()));

            let if_label = unique_label("");
            let end_label = unique_label("end");
            let else_label = if else_block.is_some() {
                unique_label("")
            } else {
                end_label.clone()
            };

            let mut code = Vec::new();
            code.extend(cond);

            code.push("cmp $0, %rax".to_owned());
            code.push(format!("jne {}", if_label));
            code.push(format!("jmp {}", else_label));
            code.push(format!("{}:", if_label));
            code.extend(if_block);
            code.push(format!("jmp {}", end_label));
            if let Some(else_block) = else_block {
                code.push(format!("{}:", else_label));
                code.extend(else_block);
                code.push(format!("jmp {}", end_label));
            }
            code.push(format!("{}:", end_label));

            Ok(code)
        }
        ast::Statement::Compound{list} => {
            if list.is_none() {
                return Ok(Vec::new());
            }

            gen_block(list.as_ref().unwrap(), scope)
        }
        ast::Statement::ForDecl{decl, exp2, exp3, statement} => {
            let start_loop_label = unique_label("loop_");
            let end_loop_label = unique_label("000loop_");
            let continue_loop_label = unique_label("loop_");

            let mut header_scope = scope.clone();
            header_scope.current_scope = HashSet::new();

            let (decl_code, scope) = gen_decl(decl, header_scope)?;
            let dealocation_decl = format!("add ${}, %rsp", PLATFORM_WORD_SIZE);
            let exp_code = gen_expr(exp2, &scope)?;
            let exp3_code = exp3.as_ref().map_or(Ok(Vec::new()), |exp | gen_expr(exp, &scope))?;

            let mut body_scope = scope;
            body_scope.current_scope = HashSet::new();
            body_scope.loop_context = Some(LoopContext{
                begin_label: continue_loop_label.clone(),
                end_label: end_loop_label.clone(),
            });

            let statement_code = gen_statement(statement, &mut body_scope)?;
            
            let mut code = Vec::new();
            code.extend(decl_code);
            code.push(format!("{}:", start_loop_label));
            code.extend(exp_code);
            code.push("cmp $0, %rax".to_owned());
            code.push(format!("je {}", end_loop_label));
            code.extend(statement_code);
            code.push(format!("{}:", continue_loop_label));
            code.extend(exp3_code);
            code.push(format!("jmp {}", start_loop_label));
            code.push(format!("{}:", end_loop_label));
            code.push(dealocation_decl);
            
            Ok(code)
        }
        ast::Statement::Do{statement, exp} => {
            let start_loop_label = unique_label("loop_");
            let end_loop_label = unique_label("loop_");

            let mut scope = scope.clone();
            scope.loop_context = Some(LoopContext{
                begin_label: start_loop_label.clone(),
                end_label: end_loop_label.clone(),
            });

            let exp_code = gen_expr(exp, &scope)?;
            let statement_code = gen_statement(statement, &scope)?;

            let mut code = Vec::new();
            code.push(format!("{}:", start_loop_label));
            code.extend(statement_code);
            code.extend(exp_code);
            code.push("cmp $0, %rax".to_owned());
            code.push(format!("je {}", end_loop_label));
            code.push(format!("jmp {}", start_loop_label));
            code.push(format!("{}:", end_loop_label));

            Ok(code)
        },
        ast::Statement::While{exp, statement} => {
            let start_loop_label = unique_label("loop_");
            let end_loop_label = unique_label("loop_");

            let mut scope = scope.clone();
            scope.loop_context = Some(LoopContext{
                begin_label: start_loop_label.clone(),
                end_label: end_loop_label.clone(),
            });

            let exp_code = gen_expr(exp, &scope)?;
            let statement_code = gen_statement(statement, &scope)?;

            let mut code = Vec::new();
            code.push(format!("{}:", start_loop_label));
            code.extend(exp_code);
            code.push("cmp $0, %rax".to_owned());
            code.push(format!("je {}", end_loop_label));
            code.extend(statement_code);
            code.push(format!("jmp {}", start_loop_label));
            code.push(format!("{}:", end_loop_label));
            
            Ok(code)
        }
        ast::Statement::Break => {
            match &scope.loop_context {
                Some(loop_ctx) => {
                    Ok(vec![format!("jmp {}", loop_ctx.end_label)])
                }
                None => Err(GenError::BreakWrongUsage)
            }
        }
        ast::Statement::Continue => {
            match &scope.loop_context {
                Some(loop_ctx) => {
                    Ok(vec![format!("jmp {}", loop_ctx.begin_label)])
                }
                None => Err(GenError::ContinueWrongUsage)
            }
        }
        _ => unimplemented!(),
    }
}

fn gen_block(items: &[ast::BlockItem], scope: &AsmScope) -> Result<Vec<String>> {
    let mut scope = scope.clone();
    scope.current_scope = HashSet::new();

    let mut code = Vec::new();
    for item in items.iter() {
        let c = match item {
            ast::BlockItem::Declaration(decl) => {
                let (c, scp) = gen_decl(decl, scope)?;
                scope = scp;
                c
            }
            ast::BlockItem::Statement(stat) => {
                gen_statement(stat, &scope)?
            }
        };
        code.extend(c);
    }

    let bytes_to_deallocate = scope.current_scope.len() as i64 * PLATFORM_WORD_SIZE;
    code.push(format!("add ${}, %rsp", bytes_to_deallocate));

    Ok(code)
}

fn gen_decl(ast::Declaration::Declare{name, exp}: &ast::Declaration, mut scope: AsmScope) -> Result<(Vec<String>, AsmScope)> {
        if scope.current_scope.contains(name) {
            return Err(GenError::InvalidVariableUsage(name.clone()));
        }

        scope.variable_map.insert(name.clone(), scope.stack_index);
        scope.stack_index -= PLATFORM_WORD_SIZE;
        scope.current_scope.insert(name.clone());

        let code = match exp {
            Some(exp) => {
                let mut code = gen_expr(&exp, &scope)?;
                code.push("push %rax".to_owned());
                code
            }
            _ => vec!["push $0".to_owned()]
        };
        
        Ok((code, scope))
}

fn gen_expr(expr: &ast::Exp, scope: &AsmScope) -> Result<Vec<String>> {
    match expr {
        ast::Exp::Const(c) => Ok(gen_const(c)),
        ast::Exp::UnOp(op, exp) => gen_unop(op, exp, scope),
        ast::Exp::BinOp(op, exp1, exp2) => gen_binop(op, exp1, exp2, scope),
        ast::Exp::AssignOp(name, op, exp) => gen_assign_op(name, op, exp, scope),
        ast::Exp::Assign(name, exp) => {
            let mut code = gen_expr(exp, scope)?;

            let offset = scope.variable_map.get(name).ok_or(GenError::InvalidVariableUsage(name.clone()))?;
            code.push(format!("mov %rax, {}(%rbp)", offset));

            Ok(code)
        }
        ast::Exp::Var(name) => {
            let offset = scope.variable_map.get(name).ok_or(GenError::InvalidVariableUsage(name.clone()))?;

            Ok(vec![format!("mov {}(%rbp), %rax", offset)])
        }
        ast::Exp::CondExp(cond, exp1,exp2) => {
            let cond = gen_expr(cond, scope)?;
            let exp1 = gen_expr(exp1, scope)?;
            let exp2 = gen_expr(exp2, scope)?;

            let exp1_label = unique_label("");
            let exp2_label = unique_label("");
            let end_label = unique_label("end");

            let mut code = Vec::new();
            code.extend(cond);
            code.push("cmp $0, %rax".to_owned());
            code.push(format!("jne {}", exp1_label));
            code.push(format!("jmp {}", exp2_label));
            code.push(format!("{}:", exp1_label));
            code.extend(exp1);
            code.push(format!("jmp {}", end_label));
            code.push(format!("{}:", exp2_label));
            code.extend(exp2);
            code.push(format!("jmp {}", end_label));
            code.push(format!("{}:", end_label));

            Ok(code)
        }
    }
}

fn gen_const(c: &ast::Const) -> Vec<String> {
    match c {
        ast::Const::Int(val) => vec![format!("mov    ${}, %rax", val)]
    }
}

fn gen_unop(op: &ast::UnOp, exp: &ast::Exp, scope: &AsmScope) -> Result<Vec<String>> {
    let mut code = gen_expr(exp, scope)?;
    code = match op {
        ast::UnOp::Negation => {
            code.push("neg    %rax".to_owned());
            code
        }
        ast::UnOp::LogicalNegation => {
            code.extend(
                vec![
                    "cmpl    $0, %eax".to_owned(),
                    "movl    $0, %eax".to_owned(),
                    "sete    %al".to_owned()
                ]
            );
            code
        }
        ast::UnOp::BitwiseComplement => {
            code.push("not    %eax".to_owned());
            code
        }
        ast::UnOp::IncrementPrefix => {
            let offset = match exp {
                ast::Exp::Var(name) => {
                    scope.variable_map.get(name).ok_or(GenError::InvalidVariableUsage(name.clone()))?
                }
                _ => unreachable!(),
            };

            vec![
                format!("mov {}(%rbp), %rax", offset),
                "inc %rax".to_owned(),
                format!("mov    %rax, {}(%rbp)", offset),
            ]
        }
        ast::UnOp::IncrementPostfix => {
            let offset = match exp {
                ast::Exp::Var(name) => {
                    scope.variable_map.get(name).ok_or(GenError::InvalidVariableUsage(name.clone()))?
                }
                _ => unreachable!(),
            };

            vec![
                format!("mov {}(%rbp), %rax", offset),
                "inc %rax".to_owned(),
                format!("mov    %rax, {}(%rbp)", offset),
                "dec %rax".to_owned(),
            ]
        }
        ast::UnOp::DecrementPrefix => {
            let offset = match exp {
                ast::Exp::Var(name) => {
                    scope.variable_map.get(name).ok_or(GenError::InvalidVariableUsage(name.clone()))?
                }
                _ => unreachable!(),
            };

            vec![
                format!("mov {}(%rbp), %rax", offset),
                "dec %rax".to_owned(),
                format!("mov    %rax, {}(%rbp)", offset),
            ]
        }
        ast::UnOp::DecrementPostfix => {
            let offset = match exp {
                ast::Exp::Var(name) => {
                    scope.variable_map.get(name).ok_or(GenError::InvalidVariableUsage(name.clone()))?
                }
                _ => unreachable!(),
            };

            vec![
                format!("mov {}(%rbp), %rax", offset),
                "dec %rax".to_owned(),
                format!("mov    %rax, {}(%rbp)", offset),
                "inc %rax".to_owned(),
            ]
        }
    };
    Ok(code)
}

fn gen_binop(op: &ast::BinOp, exp1: &ast::Exp, exp2: &ast::Exp, scope: &AsmScope) -> Result<Vec<String>> {
    let exp1 = gen_expr(exp1, scope)?;
    let exp2 = gen_expr(exp2, scope)?;

    let code_with = |exp1: Vec<String>, exp2: Vec<String>, exp: &[String]| {
        let mut code = Vec::with_capacity(exp1.len() + exp2.len());
        code.extend(exp1);
        code.push("push %rax".to_owned());
        code.extend(exp2);
        code.push("pop %rcx".to_owned());
        code.extend_from_slice(exp);
        code
    };

    Ok(match op {
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
    })
}

fn gen_assign_op(var_name: &str, op: &ast::AssignmentOp, exp: &ast::Exp, scope: &AsmScope) -> Result<Vec<String>> {
    let mut code = gen_expr(exp, scope)?;

    let offset = scope.variable_map.get(var_name).ok_or(GenError::InvalidVariableUsage(var_name.to_owned()))?;

    match op {
        ast::AssignmentOp::Plus => code.push(format!("add {}(%rbp), %rax", offset)),
        ast::AssignmentOp::Sub => code.push(format!("sub {}(%rbp), %rax", offset)),
        ast::AssignmentOp::Mul => code.push(format!("imul {}(%rbp), %rax", offset)),
        ast::AssignmentOp::Div => {
            // is it correct?
            code.push(format!("mov {}(%rbp), %rcx", offset));
            code.extend(vec![
                "mov %rax, %rbx".to_owned(),
                "mov %rcx, %rax".to_owned(),
                "mov %rbx, %rcx".to_owned(),
                "cqo".to_owned(),
                "idiv %rcx".to_owned()
            ]);
        },
        ast::AssignmentOp::Mod => {
            // is it correct?
            code.push(format!("mov {}(%rbp), %rcx", offset));
            code.extend(vec![
                "mov %rax, %rbx".to_owned(),
                "mov %rcx, %rax".to_owned(),
                "mov %rbx, %rcx".to_owned(),
                "cqo".to_owned(),
                "idiv %rcx".to_owned(),
                "mov %rdx, %rax".to_owned()
            ]);
        },
        ast::AssignmentOp::BitLeftShift => code.push("sal %rcx, %rax".to_owned()),
        ast::AssignmentOp::BitRightShift => code.push("sar %rcx, %rax".to_owned()),
        ast::AssignmentOp::BitAnd => code.push(format!("and {}(%rbp), %rax", offset)),
        ast::AssignmentOp::BitOr => code.push(format!("or {}(%rbp), %rax", offset)),
        ast::AssignmentOp::BitXor => code.push(format!("xor {}(%rbp), %rax", offset)),
    };

    code.push(format!("mov %rax, {}(%rbp)", offset));

    Ok(code)
}

fn epilogue() -> Vec<String> {
    vec![
        "mov %rbp, %rsp".to_owned(),
        "pop %rbp".to_owned(),
        "ret".to_owned(),
    ]
}

fn prologue() -> Vec<String> {
    vec![
        "push %rbp".to_owned(),
        "mov %rsp, %rbp".to_owned(),
    ]
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