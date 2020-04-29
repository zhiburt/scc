use simple_c_compiler::ast::{BlockItem, Declaration, Exp, FuncDecl, Program, Statement, Visitor};

pub fn pretty_prog(prog: &Program) -> String {
    let mut printer = Printer::new(0);

    let mut out = Vec::new();
    for func in &prog.0 {
        out.push(printer.function(func));
    }

    out.join("\n\n")
}

struct Printer {
    buf: String,
    ident: usize,
    lines: Vec<String>,
}

impl Printer {
    fn new(ident: usize) -> Self {
        Self {
            buf: String::new(),
            ident,
            lines: Vec::new(),
        }
    }

    fn tab<F: FnMut(&mut Printer)>(&mut self, mut f: F) {
        self.ident += 2;
        f(self);
        self.ident -= 2;
    }

    fn save(&mut self, s: String) {
        self.buf = s;
    }

    fn line(&mut self, s: &str) {
        self.lines.push(str::repeat(" ", self.ident) + s);
    }

    fn expr(&mut self, exp: &Exp) -> String {
        Visitor::visit_expr(self, exp);
        self.buf.clone()
    }

    fn decl(&mut self, decl: &Declaration) -> String {
        Visitor::visit_decl(self, decl);
        self.buf.clone()
    }

    fn function(&mut self, func: &FuncDecl) -> String {
        self.visit_function(func);
        let body = self.lines.join("\n");

        let params = func
            .parameters
            .iter()
            .map(|p| format!("INT {}", p))
            .collect::<Vec<String>>()
            .join(", ");

        format!(
            "FUNCTION {}:\n  parameters: {}\n  body:\n{}",
            func.name, params, body
        )
    }
}

impl<'a> Visitor<'a> for Printer {
    fn visit_expr(&mut self, exp: &'a Exp) {
        match exp {
            Exp::BinOp(op, exp1, exp2) => {
                let left = self.expr(exp1);
                let right = self.expr(exp2);
                self.save(format!("{} BIN_OP<{:?}> {}", left, op, right));
            }
            Exp::Const(c) => self.save(format!("{:?}", c)),
            Exp::UnOp(op, exp) => {
                let exp = self.expr(exp);
                self.save(format!("UN_OP<{:?}> {}", op, exp));
            }
            Exp::IncOrDec(name, op) => self.save(format!("VAR[{}] {:?}", name, op)),
            Exp::Assign(name, exp) => {
                let exp = self.expr(exp);
                self.save(format!("VAR[{}] = {}", name, exp));
            }
            Exp::Var(name) => self.save(format!("VAR[{}]", name)),
            Exp::AssignOp(name, op, exp) => {
                let exp = self.expr(exp);
                self.save(format!("VAR[{}] ASSIGN_OP<{:?}> {}", name, op, exp));
            }
            Exp::CondExp(cond, exp1, exp2) => {
                let cond = self.expr(cond);
                let exp1 = self.expr(exp1);
                let exp2 = self.expr(exp2);
                self.save(format!(
                    "TERNAR_OP IF {} DO {} ELSE DO {}",
                    cond, exp1, exp2
                ));
            }
            Exp::FuncCall(name, params) => {
                let mut f = |e| self.expr(e);
                let params = params
                    .iter()
                    .map(|e| f(e))
                    .collect::<Vec<String>>()
                    .join(", ");
                self.save(format!("CALL {} WITH {}", name, params,));
            }
        }
    }

    fn visit_statement(&mut self, st: &'a Statement) {
        match st {
            Statement::Return { exp } => {
                let exp = self.expr(exp);
                self.line(&&format!("RETURN {}", exp));
            }
            Statement::Exp { exp } => {
                let exp = exp.as_ref().map_or("None".to_owned(), |exp| self.expr(exp));
                self.line(&exp);
            }
            Statement::Conditional {
                cond_expr,
                if_block,
                else_block,
            } => {
                let cond_expr = self.expr(cond_expr);
                self.line(&format!("IF {}:", cond_expr));

                self.tab(|s| s.visit_statement(if_block));

                if let Some(else_block) = else_block {
                    self.line("ELSE:");
                    self.tab(|p| p.visit_statement(else_block));
                }

                self.line("END");
            }
            Statement::Compound { list } => {
                if let Some(list) = list {
                    for block in list {
                        self.visit_block(block)
                    }
                }
            }
            Statement::While { exp, statement } => {
                let exp = self.expr(exp);
                self.line(&format!("WHILE {}:", exp));
                self.line("DO");
                self.tab(|p| p.visit_statement(statement));
                self.line("END");
            }
            Statement::Do { statement, exp } => {
                let exp = self.expr(exp);
                self.line(&format!("DO-WHILE {}:", exp));
                self.line("DO");
                self.tab(|p| p.visit_statement(statement));
                self.line("END");
            }
            Statement::ForDecl {
                decl,
                exp2,
                exp3,
                statement,
            } => {
                let decl = self.decl(decl);
                let cond = self.expr(exp2);
                let exp = exp3.as_ref().map_or("".to_string(), |e| self.expr(e));

                self.line("FOR:");
                self.line(&format!("decl: {}", decl));
                self.line(&format!("cond: {}", cond));
                self.line(&format!("exp: {}", exp));
                self.line("DO");
                self.tab(|p| p.visit_statement(statement));
                self.line("END");
            }
            Statement::For {
                exp1,
                exp2,
                exp3,
                statement,
            } => {
                let exp1 = exp1.as_ref().map_or(Default::default(), |e| self.expr(e));
                let cond = self.expr(exp2);
                let exp2 = exp3.as_ref().map_or("".to_string(), |e| self.expr(e));

                self.line("FOR:");
                self.line(&format!("exp: {}", exp1));
                self.line(&format!("cond: {}", cond));
                self.line(&format!("exp: {}", exp2));
                self.line("DO");
                self.tab(|p| p.visit_statement(statement));
                self.line("END");
            }
            Statement::Break => self.line(&"BREAK".to_owned()),
            Statement::Continue => self.line(&"CONTINUE".to_owned()),
        }
    }

    fn visit_decl(&mut self, decl: &'a Declaration) {
        let decl = match decl {
            Declaration::Declare { name, exp } => match exp {
                Some(exp) => {
                    let exp = self.expr(exp);
                    format!("INT {} = {}", name, exp)
                }
                None => format!("INT {}", name),
            },
        };
        self.save(decl);
    }

    fn visit_block(&mut self, block: &'a BlockItem) {
        match block {
            BlockItem::Declaration(decl) => {
                let decl = self.decl(decl);
                self.line(&decl);
            }
            BlockItem::Statement(st) => self.visit_statement(st),
        };
    }
}
