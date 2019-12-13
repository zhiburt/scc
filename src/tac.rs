use crate::ast;
use std::collections::HashMap;

pub fn il(program: &ast::Program) -> Vec<Vec<Instruction>> {
    let mut code = Vec::new();
    let mut generator = Generator::new();

    for func in &program.0 {
        emit_fn(func, &mut generator);
        code.push(generator.flush());
    }

    code
}

struct Generator {
    counters: [usize; 2],
    instructions: Vec<Instruction>,
    vars: HashMap<String, ID>,
}

impl Generator {
    pub fn new() -> Self {
        Generator {
            counters: [0, 0],
            instructions: Vec::new(),
            vars: HashMap::new(),
        }
    }

    pub fn from(g: &Generator) -> Self {
        let mut generator = Generator::new();
        // check is it copy or clone in sense of references.
        generator.counters = g.counters;
        generator
    }

    pub fn id(&mut self, name: &str) -> ID {
        match self.vars.get(name) {
            Some(id) => id.clone(),
            None => {
                let id = ID {
                    tp: IDType::Var,
                    id: self.counters[1],
                };
                self.inc_vars();

                self.vars.insert(name.to_owned(), id.clone());

                id
            }
        }
    }

    pub fn allocated_memory(&self) -> BytesSize {
        (self.counters[0] + self.counters[1]) * 4
    }

    pub fn flush(&mut self) -> Vec<Instruction> {
        let mut v = Vec::new();
        std::mem::swap(&mut self.instructions, &mut v);
        v
    }

    pub fn emit(&mut self, op: Step) -> ID {
        match op.tp {
            OpType::NoInstruction => {
                self.instructions.push(Instruction{id: None, op: op});
                ID {
                    id: 1111111111,
                    tp: IDType::Temporary,
                }
            },
            OpType::Inst => {
                match op.op {
                    Op::Arithmetic(ArithmeticOp::Add, ..) => {
                        self.instructions.push(Instruction::with_id(ID{id: self.counters[0], tp: IDType::Temporary}, op));
                        self.inc_tmp();
                        ID {
                            id: self.counters[0],
                            tp: IDType::Temporary,
                        }
                    }
                    Op::Assignment(ref id, ..) => {
                        self.instructions.push(Instruction::with_id(id.clone(), op));
                        self.inc_vars();
                        ID {
                            id: self.counters[1],
                            tp: IDType::Var,
                        }
                    }
                    _ => unimplemented!(),
                }
            }
        }


    }

    fn inc_vars(&mut self) -> usize {
        self.counters[1] += 1;
        self.counters[1]
    }

    fn inc_tmp(&mut self) -> usize {
        self.counters[0] += 1;
        self.counters[0]
    }
}

fn emit_fn(
    ast::FuncDecl {
        parameters,
        blocks,
        name,
    }: &ast::FuncDecl,
    gen: &mut Generator,
) {
    if blocks.is_none() {
        // here we should somehow show that this function can be called
        // with some type of parameters
        // it representation of declaration without definition
        unimplemented!();
    }

    let blocks = blocks.as_ref().unwrap();
    let mut body_generator = Generator::from(gen);
    for block in blocks {
        match block {
            ast::BlockItem::Declaration(decl) => emit_decl(decl, &mut body_generator),
            ast::BlockItem::Statement(st) => emit_st(st, &mut body_generator),
        }
    }

    let def = FuncDef::Begin(name.clone(), body_generator.allocated_memory());
    // let ret = FuncDef::Ret(Some(ID{id: 0, tp: IDType::Var}));
    
    gen.emit(Step{op: Op::FuncDef(def), tp: OpType::NoInstruction});

    for ist in body_generator.instructions {
        gen.instructions.push(ist);
    }

    // gen.emit(Step{op: Op::FuncDef(ret), tp: OpType::NoInstruction});
}

fn emit_decl(decl: &ast::Declaration, mut gen: &mut Generator) {
    match decl {
        ast::Declaration::Declare { name, exp } => {
            if let Some(exp) = exp {
                let id = gen.id(name); 
                let val = emit_exp(exp, &mut gen);
                gen.emit(Step{op: Op::Assignment(id, val), tp: OpType::Inst});
            }
        }
    }
}

fn emit_st(st: &ast::Statement, mut gen: &mut Generator) {
    match st {
        ast::Statement::Conditional {
            cond_expr,
            if_block,
            else_block,
        } => {

        }
        ast::Statement::Compound{list} => {
            if let Some(list) = list.as_ref() {
                for block in list {
                    match block {
                        ast::BlockItem::Declaration(decl) => emit_decl(decl, &mut gen),
                        ast::BlockItem::Statement(st) => emit_st(st, &mut gen),
                    }
                }
            }
        }
        ast::Statement::Return{exp} => {
            let id = emit_exp(exp, &mut gen);
            let id = match id {
                Val::Var(id) => id,
                _ => unimplemented!(),
            };
            gen.emit(Step{op: Op::FuncDef(FuncDef::Ret(Some(id))), tp: OpType::NoInstruction});
        }
        ast::Statement::Exp{exp} => {
            if let Some(exp) = exp {
                emit_exp(exp, &mut gen);
            }
        }
        _ => unimplemented!(),
    }
}

fn emit_exp(exp: &ast::Exp, mut gen: &mut Generator) -> Val {
    match exp {
        ast::Exp::BinOp(op, exp1, exp2) => match op {
            ast::BinOp::Addition => {
                let v1 = emit_exp(exp1, &mut gen);
                let v2 = emit_exp(exp2, &mut gen);
                Val::Var(gen.emit(Step{op: Op::Arithmetic(ArithmeticOp::Add, v1, v2), tp: OpType::Inst}))
            }
            _ => unimplemented!(),
        },
        ast::Exp::Assign(name, exp) => {
            let val = emit_exp(exp, &mut gen);
            let id = gen.id(name);
            Val::Var(gen.emit(Step{op: Op::Assignment(id, val), tp: OpType::Inst}))
        }
        ast::Exp::Var(name) => Val::Var(gen.id(&name)),
        ast::Exp::Const(ast::Const::Int(val)) => {
            Val::Const(Const::Int(*val as i32))
        },
        _ => unimplemented!(),
    }
}

#[derive(Debug)]
pub struct Instruction {
    pub id: Option<ID>,
    pub op: Step,
}

impl Instruction {
    pub fn with_id(id: ID, op: Step) -> Self {
        Instruction { id: Some(id), op }
    }
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

pub type Label = String;

#[derive(Debug)]
pub struct Step {
    pub op: Op,
    pub tp: OpType,
}

#[derive(Clone, Debug)]
pub enum OpType {
    NoInstruction,
    Inst,
}

#[derive(Debug)]
pub enum Op {
    Arithmetic(ArithmeticOp, Val, Val),
    Assignment(ID, Val),
    Relational(RelationalOp, Val, Val),
    Branch(LabelBranch),
    Call(FnCall, Label),
    FuncDef(FuncDef),
    Parameters(Parameters),
}

type BytesSize = usize;

#[derive(Debug)]
pub enum Parameters {
    Push(ID),
    Pop(BytesSize),
}

#[derive(Debug)]
pub enum Const {
    Int(i32),
}

#[derive(Debug)]
pub enum Val {
    Var(ID),
    Const(Const),
}

#[derive(Debug)]
pub enum ArithmeticOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
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
    IfGOTO(Val, Label),
}

#[derive(Debug)]
pub enum FnCall {
    LCall,
}

#[derive(Debug)]
pub enum FuncDef {
    Begin(String, BytesSize),
    Ret(Option<ID>),
}

// impl FuncDef {
//     fn new(size: BytesSize, ret: Option<ID>) -> Self {
//         FuncDef { begin: size, ret }
//     }
// }
