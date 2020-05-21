use crate::ast;

pub fn multi_definition(prog: &ast::Program) -> bool {
    let mut definitions = prog
        .0
        .iter()
        .flat_map(|top| match top {
            ast::TopLevel::Declaration(ast::Declaration::Declare { name, exp })
                if exp.is_some() =>
            {
                Some(name.clone())
            }
            _ => None,
        })
        .collect::<Vec<_>>();

    let count_definitions = definitions.len();
    definitions.sort();
    definitions.dedup();
    count_definitions == definitions.len()
}

pub fn name_check(prog: &ast::Program) -> bool {
    let funcs = prog
        .0
        .iter()
        .flat_map(|top| match top {
            ast::TopLevel::Function(f) => Some(f.name.clone()),
            _ => None,
        })
        .collect::<Vec<_>>();

    let vars = prog
        .0
        .iter()
        .flat_map(|top| match top {
            ast::TopLevel::Declaration(ast::Declaration::Declare { name, .. }) => {
                Some(name.clone())
            }
            _ => None,
        })
        .collect::<Vec<_>>();

    vars.iter().all(|var| !funcs.contains(var))
}

pub fn use_before_definition(prog: &ast::Program) -> bool {
    use ast::Visitor;
    struct Global {
        definitions: Vec<String>,
        globals: Vec<String>,
        issue: bool,
    }

    impl<'a> Visitor<'a> for Global {
        fn visit_global_item(&mut self, item: &'a ast::TopLevel) {
            if let ast::TopLevel::Declaration(ast::Declaration::Declare { name, .. }) = item {
                self.definitions.push(name.clone());
            }

            ast::visitor::visit_global_item(self, item);
        }

        fn visit_expr(&mut self, exp: &'a ast::Exp) {
            if let ast::Exp::Var(name) = exp {
                if self.globals.contains(name) && !self.definitions.contains(name) {
                    self.issue = true;
                }
            }

            ast::visitor::visit_expr(self, exp);
        }
    }

    let vars = prog
        .0
        .iter()
        .flat_map(|top| match top {
            ast::TopLevel::Declaration(ast::Declaration::Declare { name, .. }) => {
                Some(name.clone())
            }
            _ => None,
        })
        .collect::<Vec<_>>();

    let mut visitor = Global {
        definitions: Vec::new(),
        issue: false,
        globals: vars,
    };
    for item in &prog.0 {
        visitor.visit_global_item(item);
    }

    !visitor.issue
}
