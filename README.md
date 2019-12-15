use simple_c_compiler::tac;
use std::collections::HashMap;

pub fn pretty(fun: &tac::FuncDef) {
    println!("{}:", pretty_fun_name(&fun.name));
    println!("  BeginFunc {}", fun.frame_size);

    for inst in &fun.instructions {
        match inst {
            tac::Instruction::Op(id, op) => {
                let id = id.as_ref().unwrap();
                match op {
                    tac::Op::Assignment(.., val) => {
                        println!(
                            "  {}: {}",
                            pretty_id(&fun.vars, id),
                            pretty_val(&fun.vars, val)
                        );
                    }
                    tac::Op::Arithmetic(op, v1, v2) => {
                        println!(
                            "  {}: {} {} {}",
                            pretty_id(&fun.vars, id),
                            pretty_id(&fun.vars, v1),
                            pretty_arith_op(op),
                            pretty_id(&fun.vars, v2)
                        );
                    }
                    tac::Op::Relational(tac::RelationalOp::Equal, v1, v2) => {
                        println!(
                            "  {}: {} == {}",
                            pretty_id(&fun.vars, id),
                            pretty_id(&fun.vars, v1),
                            pretty_id(&fun.vars, v2)
                        );
                    }
                    tac::Op::Call(call) => {
                        for p in call.params.iter() {
                            println!("  PushParam {}", pretty_id(&fun.vars, p));
                        }

                        println!(
                            "  {}: LCall {}",
                            pretty_id(&fun.vars, id),
                            pretty_fun_name(&call.name)
                        );
                        println!("  PopParams {}", call.pop_size);
                    }
                    _ => unimplemented!(),
                };
            }
            tac::Instruction::ControllOp(cop) => match cop {
                tac::ControllOp::Branch(lb) => match lb {
                    tac::LabelBranch::Label(label) => {
                        println!("{}:", pretty_label(label));
                    }
                    tac::LabelBranch::GOTO(label) => {
                        println!("  Goto {}", pretty_label(label));
                    }
                    tac::LabelBranch::IfGOTO(id, label) => {
                        println!(
                            "  IfZ {} Goto {}",
                            pretty_id(&fun.vars, id),
                            pretty_label(label)
                        );
                    }
                },
                _ => unimplemented!(),
            },
        }
    }

    println!(
        "  Return {};",
        fun.ret
            .as_ref()
            .map_or("NO".to_owned(), |id| pretty_id(&fun.vars, id))
    );
    println!("  EndFunc;");
}

pub fn pretty_id(vars: &HashMap<usize, String>, id: &tac::ID) -> String {
    match id.tp {
        tac::IDType::Var => format!("{}", vars[&id.id]),
        tac::IDType::Temporary => format!("_t{}", id.id),
    }
}

pub fn pretty_val(vars: &HashMap<usize, String>, v: &tac::Val) -> String {
    match v {
        tac::Val::Var(id) => format!("{}", pretty_id(vars, id)),
        tac::Val::Const(tac::Const::Int(val)) => format!("{}", val),
    }
}

pub fn pretty_label(label: &tac::Label) -> String {
    format!("_L{}", label)
}

pub fn pretty_fun_name(name: &str) -> String {
    if name == "main" {
        name.to_string()
    } else {
        format!("_{}", name)
    }
}

pub fn pretty_arith_op(op: &tac::ArithmeticOp) -> String {
    match op {
        tac::ArithmeticOp::Add => "+".to_string(),
        tac::ArithmeticOp::Sub => "-".to_string(),
        tac::ArithmeticOp::Mul => "*".to_string(),
        tac::ArithmeticOp::Div => "/".to_string(),
        tac::ArithmeticOp::Mod => "%".to_string(),
    }
}























































use crate::ast;
use std::collections::HashMap;

pub fn il(p: &ast::Program) -> Vec<FuncDef> {
    let mut gen = Generator::new();
    let mut funcs = Vec::new();
    for fun in &p.0 {
        funcs.push(gen.parse(fun));
    }

    funcs
}
struct Generator {
    instructions: Vec<Instruction>,
    vars: HashMap<String, ID>,
    context_ret: Option<ID>,
    counters: [usize; 3],
    allocated: usize,
}

impl Generator {
    pub fn new() -> Self {
        Generator {
            counters: [0, 0, 0],
            allocated: 0,
            instructions: Vec::new(),
            vars: HashMap::new(),
            context_ret: None,
        }
    }

    pub fn from(g: &Generator) -> Self {
        let mut generator = Generator::new();
        // check is it copy or clone in sense of references.
        generator.counters = g.counters;
        generator
    }

    pub fn parse(&mut self, func: &ast::FuncDecl) -> FuncDef {
        if func.blocks.is_none() {
            // here we should somehow show that this function can be called
            // with some type of parameters
            // it representation of declaration without definition
            unimplemented!();
        }

        for p in func.parameters.iter() {
            self.recognize_var(&p);
        }

        let blocks = func.blocks.as_ref().unwrap();

        for block in blocks {
            emit_block(self, block);
        }

        let vars = self.vars
            .iter()
            .map(|(var, id)|(id.id, var.clone()))
            .collect::<HashMap<usize, String>>();
        
        self.vars.clear();
        FuncDef {
            name: func.name.clone(),
            frame_size: self.allocated_memory(),
            ret: self.context_ret.clone(),
            instructions: self.flush(),
            vars: vars,
        }
    }

    fn emit(&mut self, inst: Inst) -> Option<ID> {
        match inst {
            Inst::Op(op) => {
                let id = match &op {
                    Op::Assignment(Some(name), ..) => {
                        let id = self.var_id(name);
                        id
                    }
                    _ => {
                        let id = self.id(IDType::Temporary);
                        self.inc_tmp();
                        id
                    }
                };

                self.instructions
                    .push(Instruction::Op(Some(id.clone()), op));
                Some(id)
            }
            Inst::ControllOp(cop) => {
                self.instructions.push(Instruction::ControllOp(cop));
                None
            }
        }
    }

    pub fn var_id(&mut self, name: &str) -> ID {
        self.allocated += 1;
        self.recognize_var(name)
    }

    pub fn recognize_var(&mut self, name: &str) -> ID {
        match self.vars.get(name) {
            Some(id) => id.clone(),
            None => {
                let id = self.id(IDType::Var);
                self.inc_vars();
                self.vars.insert(name.to_owned(), id.clone());

                id
            }
        }
    }

    pub fn allocated_memory(&self) -> BytesSize {
        self.allocated * 4
    }

    pub fn flush(&mut self) -> Vec<Instruction> {
        self.allocated = 0;
        let mut v = Vec::new();
        std::mem::swap(&mut self.instructions, &mut v);
        v
    }

    fn inc_vars(&mut self) -> usize {
        self.counters[1] += 1;
        self.counters[1]
    }

    fn inc_tmp(&mut self) -> usize {
        self.allocated += 1;
        let i = self.counters[0];
        self.counters[0] += 1;
        i
    }

    fn uniq_label(&mut self) -> Label {
        let l = self.counters[2];
        self.counters[2] += 1;
        l
    }

    fn id(&self, tp: IDType) -> ID {
        match tp {
            IDType::Temporary => ID {
                id: self.counters[0],
                tp,
            },
            IDType::Var => ID {
                id: self.counters[1],
                tp,
            },
        }
    }
}

fn emit_block(mut gen: &mut Generator, block: &ast::BlockItem) -> Option<ID> {
    match block {
        ast::BlockItem::Declaration(decl) => emit_decl(&mut gen, decl),
        ast::BlockItem::Statement(st) => {
            emit_st(&mut gen, st);
            None
        }
    }
}

fn emit_st(mut gen: &mut Generator, st: &ast::Statement) {
    match st {
        ast::Statement::Compound { list } => {
            if let Some(blocks) = list {
                for block in blocks {
                    emit_block(&mut gen, block);
                }
            }
        }
        ast::Statement::Return { exp } => {
            gen.context_ret = Some(emit_exp(&mut gen, exp).unwrap());
        }
        ast::Statement::Exp { exp } => {
            if let Some(exp) = exp {
                emit_exp(&mut gen, exp);
            }
        }
        ast::Statement::Conditional {
            cond_expr,
            if_block,
            else_block,
        } => {
            let cond_id = emit_exp(&mut gen, cond_expr).unwrap();
            let end_label = gen.uniq_label();
            gen.emit(Inst::ControllOp(ControllOp::Branch(LabelBranch::IfGOTO(cond_id, end_label))));
            emit_st(&mut gen, if_block);
            if let Some(else_block) = else_block {
                let else_label = end_label;
                let end_label = gen.uniq_label();
                gen.emit(Inst::ControllOp(ControllOp::Branch(LabelBranch::GOTO(end_label))));
                gen.emit(Inst::ControllOp(ControllOp::Branch(LabelBranch::Label(else_label))));
                emit_st(&mut gen, else_block);
                gen.emit(Inst::ControllOp(ControllOp::Branch(LabelBranch::Label(end_label))));
            } else {
                gen.emit(Inst::ControllOp(ControllOp::Branch(LabelBranch::Label(end_label))));
            }
        }
        _ => unimplemented!(),
    }
}

fn emit_decl(mut gen: &mut Generator, decl: &ast::Declaration) -> Option<ID> {
    match decl {
        ast::Declaration::Declare { name, exp } => {
            match exp {
                Some(exp) => {
                    let id = emit_exp(&mut gen, exp).unwrap();
                    gen.emit(Inst::Op(Op::Assignment(Some(name.clone()), Val::Var(id))))
                }
                None => {
                    // we will create variable when
                    // get the first time of usage
                    //
                    // that is correct?
                    Some(gen.var_id(name))
                }
            }
        }
    }
}

fn emit_exp(mut gen: &mut Generator, exp: &ast::Exp) -> Option<ID> {
    match exp {
        ast::Exp::BinOp(op, exp1, exp2) => {
            let id1 = emit_exp(&mut gen, exp1).unwrap();
            let id2 = emit_exp(&mut gen, exp2).unwrap();
            match op {
                ast::BinOp::Equal => {
                    gen.emit(Inst::Op(Op::Relational(RelationalOp::Equal, id1, id2)))
                }
                _ => {
                    let op = ArithmeticOp::from(op).unwrap();
                    gen.emit(Inst::Op(Op::Arithmetic(op, id1, id2)))
                }
            }
        }
        ast::Exp::Assign(name, exp) => {
            let id = emit_exp(&mut gen, exp).unwrap();
            gen.emit(Inst::Op(Op::Assignment(Some(name.clone()), Val::Var(id))))
        }
        ast::Exp::Var(name) => {
            // should it create variable if it not exists?
            Some(gen.recognize_var(name))
        }
        ast::Exp::Const(ast::Const::Int(int_val)) => gen.emit(Inst::Op(Op::Assignment(
            None,
            Val::Const(Const::Int(*int_val as i32)),
        ))),
        ast::Exp::FuncCall(name, params) => {
            let params_ids = params
                .iter()
                .map(|p| emit_exp(&mut gen, p).unwrap())
                .collect::<Vec<ID>>();
            let call = Call {
                name: name.clone(),
                pop_size: params_ids.len() * 4,
                params: params_ids,
                tp: FnType::LCall,
            };
            
            gen.emit(Inst::Op(Op::Call(call)))
        }
        _ => unimplemented!(),
    }
}

#[derive(Debug)]
pub enum Instruction {
    Op(Option<ID>, Op),
    ControllOp(ControllOp),
}

#[derive(Debug)]
pub enum Inst {
    Op(Op),
    ControllOp(ControllOp),
}

#[derive(Clone, Debug)]
pub struct ID {
    pub id: usize,
    pub tp: IDType,
}

#[derive(Clone, Debug)]
pub enum IDType {
    Temporary,
    Var,
}

pub type Label = usize;

#[derive(Debug)]
pub enum Op {
    Arithmetic(ArithmeticOp, ID, ID),
    // here might be better used ID
    Assignment(Option<String>, Val),
    Relational(RelationalOp, ID, ID),
    Call(Call),
}

#[derive(Debug)]
pub enum ControllOp {
    FuncDef(FuncDef),
    Branch(LabelBranch),
}

type BytesSize = usize;

#[derive(Debug)]
pub enum Const {
    Int(i32),
}

#[derive(Debug)]
pub enum Val {
    Var(ID),
    Const(Const),
}

impl Val {
    fn to_var(self) -> Option<ID> {
        match self {
            Val::Var(id) => Some(id),
            _ => None,
        }
    }

    fn to_const(self) -> Option<Const> {
        match self {
            Val::Const(c) => Some(c),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum ArithmeticOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

impl ArithmeticOp {
    fn from(op: &ast::BinOp) -> Option<Self> {
        match op {
            ast::BinOp::Addition => Some(ArithmeticOp::Add),
            ast::BinOp::Sub => Some(ArithmeticOp::Sub),
            ast::BinOp::Multiplication => Some(ArithmeticOp::Mul),
            ast::BinOp::Division => Some(ArithmeticOp::Div),
            ast::BinOp::Modulo => Some(ArithmeticOp::Mod),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum RelationalOp {
    Equal,
    Less,
    And,
    Or,
}

#[derive(Debug)]
pub enum LabelBranch {
    Label(Label),
    GOTO(Label),
    // might here can be Val?
    IfGOTO(ID, Label),
}

#[derive(Debug)]
pub struct Call {
    pub name: String,
    pub params: Vec<ID>,
    pub pop_size: BytesSize,
    pub tp: FnType,
}

#[derive(Debug)]
pub enum FnType {
    LCall,
}

#[derive(Debug)]
pub struct FuncDef {
    pub name: String,
    pub frame_size: BytesSize,
    pub vars: HashMap<usize, String>,
    pub ret: Option<ID>,
    pub instructions: Vec<Instruction>,
}
