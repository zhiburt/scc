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
    context: Context,
    counters: [usize; 3],
    allocated: usize,
}

struct Context {
    ret_ctx: Option<ID>,
    loop_ctx: Option<LoopContext>,
}

struct LoopContext {
    begin: Label,
    end: Label,
}

impl Generator {
    pub fn new() -> Self {
        Generator {
            counters: [0, 0, 0],
            allocated: 0,
            instructions: Vec::new(),
            vars: HashMap::new(),
            context: Context{ret_ctx: None, loop_ctx: None},
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
            ret: self.context.ret_ctx.clone(),
            instructions: self.flush(),
            vars: vars,
        }
    }

    fn emit(&mut self, inst: PreInst) -> Option<ID> {
        match inst {
            PreInst::Op(pre_op) => {
                let id = match &pre_op {
                    PreOp::Assignment(Some(id), ..) => {
                        id.clone()
                    }
                    _ => {
                        let id = self.id(IDType::Temporary);
                        self.inc_tmp();
                        id
                    }
                };

                self.instructions
                    .push(Instruction::Op(Some(id.clone()), Op::from(pre_op)));
                Some(id)
            }
            PreInst::ControllOp(cop) => {
                self.instructions.push(Instruction::ControllOp(cop));
                None
            }
        }
    }

    pub fn var_id(&mut self, name: &str) -> ID {
        match self.vars.get(name) {
            Some(id) => id.clone(),
            None => {
                let id = self.id(IDType::Var);
                self.inc_vars();
                self.vars.insert(name.to_owned(), id.clone());
                self.allocated += 1;

                id
            }
        }
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
            gen.context.ret_ctx = Some(emit_exp(&mut gen, exp).unwrap());
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
            gen.emit(PreInst::ControllOp(ControllOp::Branch(LabelBranch::IfGOTO(cond_id, end_label))));
            emit_st(&mut gen, if_block);
            if let Some(else_block) = else_block {
                let else_label = end_label;
                let end_label = gen.uniq_label();
                gen.emit(PreInst::ControllOp(ControllOp::Branch(LabelBranch::GOTO(end_label))));
                gen.emit(PreInst::ControllOp(ControllOp::Branch(LabelBranch::Label(else_label))));
                emit_st(&mut gen, else_block);
                gen.emit(PreInst::ControllOp(ControllOp::Branch(LabelBranch::Label(end_label))));
            } else {
                gen.emit(PreInst::ControllOp(ControllOp::Branch(LabelBranch::Label(end_label))));
            }
        }
        ast::Statement::While{exp, statement} => {
            let begin_label = gen.uniq_label();
            let end_label = gen.uniq_label();
            gen.emit(PreInst::ControllOp(ControllOp::Branch(LabelBranch::Label(begin_label))));
            let cond_id = emit_exp(&mut gen, exp).unwrap();
            gen.emit(PreInst::ControllOp(ControllOp::Branch(LabelBranch::IfGOTO(cond_id, end_label))));
            emit_st(&mut gen, statement);
            gen.emit(PreInst::ControllOp(ControllOp::Branch(LabelBranch::GOTO(begin_label))));
            gen.emit(PreInst::ControllOp(ControllOp::Branch(LabelBranch::Label(end_label))));
        }
        ast::Statement::ForDecl{decl, exp2, exp3, statement} => {
            // there is a question with scope variables here
            let begin_label = gen.uniq_label();
            let end_label = gen.uniq_label();
            gen.context.loop_ctx = Some(LoopContext{begin: begin_label, end: end_label});
            emit_decl(&mut gen, decl);
            gen.emit(PreInst::ControllOp(ControllOp::Branch(LabelBranch::Label(begin_label))));
            let cond_id = emit_exp(&mut gen, exp2).unwrap();
            gen.emit(PreInst::ControllOp(ControllOp::Branch(LabelBranch::IfGOTO(cond_id, end_label))));
            emit_st(&mut gen, statement);
            if let Some(exp3) = exp3 {
                emit_exp(&mut gen, exp3).unwrap();
            }
            gen.emit(PreInst::ControllOp(ControllOp::Branch(LabelBranch::GOTO(begin_label))));
            gen.emit(PreInst::ControllOp(ControllOp::Branch(LabelBranch::Label(end_label))));
        }
        ast::Statement::For{exp1, exp2, exp3, statement} => {
            // there is a question with scope variables here
            let begin_label = gen.uniq_label();
            let end_label = gen.uniq_label();
            gen.context.loop_ctx = Some(LoopContext{begin: begin_label, end: end_label});
            if let Some(exp1) = exp1 {
                emit_exp(&mut gen, exp1).unwrap();
            }
            gen.emit(PreInst::ControllOp(ControllOp::Branch(LabelBranch::Label(begin_label))));
            let cond_id = emit_exp(&mut gen, exp2).unwrap();
            gen.emit(PreInst::ControllOp(ControllOp::Branch(LabelBranch::IfGOTO(cond_id, end_label))));
            emit_st(&mut gen, statement);
            if let Some(exp3) = exp3 {
                emit_exp(&mut gen, exp3).unwrap();
            }
            gen.emit(PreInst::ControllOp(ControllOp::Branch(LabelBranch::GOTO(begin_label))));
            gen.emit(PreInst::ControllOp(ControllOp::Branch(LabelBranch::Label(end_label))));
        }
        ast::Statement::Do{exp, statement} => {
            let begin_label = gen.uniq_label();
            let end_label = gen.uniq_label();
            gen.context.loop_ctx = Some(LoopContext{begin: begin_label, end: end_label});
            gen.emit(PreInst::ControllOp(ControllOp::Branch(LabelBranch::Label(begin_label))));
            emit_st(&mut gen, statement);
            let cond_id = emit_exp(&mut gen, exp).unwrap();
            gen.emit(PreInst::ControllOp(ControllOp::Branch(LabelBranch::IfGOTO(cond_id, end_label))));
            gen.emit(PreInst::ControllOp(ControllOp::Branch(LabelBranch::GOTO(begin_label))));
            gen.emit(PreInst::ControllOp(ControllOp::Branch(LabelBranch::Label(end_label))));
        }
        ast::Statement::Break => {
            if let Some(loop_ctx) = gen.context.loop_ctx.as_ref() {
                let end = loop_ctx.end;
                gen.emit(PreInst::ControllOp(ControllOp::Branch(LabelBranch::GOTO(end))));
            }
        }
        ast::Statement::Continue => {
            if let Some(loop_ctx) = gen.context.loop_ctx.as_ref() {
                let start = loop_ctx.begin;
                gen.emit(PreInst::ControllOp(ControllOp::Branch(LabelBranch::GOTO(start))));
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
                    let var_id = gen.recognize_var(name);
                    gen.emit(PreInst::Op(PreOp::Assignment(Some(var_id), Val::Var(id))))
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

            if let Some(b_op) = BitwiseOp::from(op) {
                gen.emit(PreInst::Op(PreOp::Bit(b_op, id1, id2)))
            } else {
                match op {
                    ast::BinOp::Equal => {
                        gen.emit(PreInst::Op(PreOp::Equality(EqualityOp::Equal, id1, id2)))
                    }
                    ast::BinOp::NotEqual => {
                        gen.emit(PreInst::Op(PreOp::Equality(EqualityOp::NotEq, id1, id2)))
                    }
                    ast::BinOp::GreaterThan => {
                        gen.emit(PreInst::Op(PreOp::Relational(RelationalOp::Greater, id1, id2)))
                    }
                    ast::BinOp::GreaterThanOrEqual => {
                        gen.emit(PreInst::Op(PreOp::Relational(RelationalOp::GreaterOrEq, id1, id2)))
                    }
                    ast::BinOp::LessThan => {
                        gen.emit(PreInst::Op(PreOp::Relational(RelationalOp::Less, id1, id2)))
                    }
                    ast::BinOp::LessThanOrEqual => {
                        gen.emit(PreInst::Op(PreOp::Relational(RelationalOp::LessOrEq, id1, id2)))
                    }
                    ast::BinOp::And => {
                        gen.emit(PreInst::Op(PreOp::Logical(LogicalOp::And, id1, id2)))
                    }
                    ast::BinOp::Or => {
                        gen.emit(PreInst::Op(PreOp::Logical(LogicalOp::Or, id1, id2)))
                    }
                    _ => {
                        let op = ArithmeticOp::from(op).unwrap();
                        gen.emit(PreInst::Op(PreOp::Arithmetic(op, id1, id2)))
                    }
                }
            }
        }
        ast::Exp::UnOp(op, exp) => {
            // this id is the variable's one
            let id = emit_exp(&mut gen, exp).unwrap();
            let un_op = UnOpInst::from(op);
            match op {
                ast::UnOp::IncrementPostfix | ast::UnOp::DecrementPostfix |
                ast::UnOp::IncrementPrefix | ast::UnOp::DecrementPrefix => {
                    let tmp_id = match op {
                        ast::UnOp::IncrementPostfix | ast::UnOp::DecrementPostfix => {
                            gen.emit(PreInst::Op(PreOp::Assignment(
                                None,
                                Val::Var(id.clone()),
                            )))
                        }
                        _ => None,
                    };
                    let inc_const = gen.emit(PreInst::Op(PreOp::Assignment(
                        None,
                        Val::Const(Const::Int(1)),
                    ))).unwrap();
                    let changed_id = match op {
                        ast::UnOp::IncrementPostfix | ast::UnOp::IncrementPrefix => {
                            gen.emit(PreInst::Op(PreOp::Arithmetic(ArithmeticOp::Add, id.clone(), inc_const))).unwrap()
                        }
                        ast::UnOp::DecrementPostfix | ast::UnOp::DecrementPrefix => {
                            gen.emit(PreInst::Op(PreOp::Arithmetic(ArithmeticOp::Sub, id.clone(), inc_const))).unwrap()
                        }
                        _ => unreachable!(),
                    };
                    let var_id = gen.emit(PreInst::Op(PreOp::Assignment(
                        Some(id),
                        Val::Var(changed_id),
                    )));
                    match op {
                        ast::UnOp::IncrementPostfix | ast::UnOp::DecrementPostfix => {
                            tmp_id
                        }
                        _ => var_id,
                    }
                }
            _ => gen.emit(PreInst::Op(PreOp::Unary(un_op, id)))
            }
        }
        ast::Exp::Assign(name, exp) => {
            let id = emit_exp(&mut gen, exp).unwrap();
            let var_id = gen.recognize_var(name);
            gen.emit(PreInst::Op(PreOp::Assignment(Some(var_id), Val::Var(id))))
        }
        ast::Exp::Var(name) => {
            // should it create variable if it not exists?
            Some(gen.recognize_var(name))
        }
        ast::Exp::Const(ast::Const::Int(int_val)) => gen.emit(PreInst::Op(PreOp::Assignment(
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
            
            gen.emit(PreInst::Op(PreOp::Call(call)))
        }
        ast::Exp::CondExp(cond, exp1, exp2) => {
            let cond_id = emit_exp(&mut gen, cond).unwrap();
            let else_label = gen.uniq_label();
            let end_label = gen.uniq_label();
            let mut val_id = gen.emit(PreInst::Op(PreOp::Assignment(None, Val::Const(Const::Int(0)))));
            gen.emit(PreInst::ControllOp(ControllOp::Branch(LabelBranch::IfGOTO(cond_id, end_label))));
            let exp1_id = emit_exp(&mut gen, exp1).unwrap();
            gen.emit(PreInst::Op(PreOp::Assignment(val_id.clone(), Val::Var(exp1_id))));
            gen.emit(PreInst::ControllOp(ControllOp::Branch(LabelBranch::GOTO(end_label))));
            gen.emit(PreInst::ControllOp(ControllOp::Branch(LabelBranch::Label(else_label))));
            let exp2_id = emit_exp(&mut gen, exp2).unwrap();
            gen.emit(PreInst::Op(PreOp::Assignment(val_id.clone(), Val::Var(exp2_id))));
            gen.emit(PreInst::ControllOp(ControllOp::Branch(LabelBranch::Label(end_label))));
            val_id
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
pub enum PreInst {
    Op(PreOp),
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
    // here might be better used ID
    Assignment(Option<ID>, Val),
    Op(TypeOp, ID, ID),
    Unary(UnOp, ID),
    Call(Call),
}

#[derive(Debug)]
pub enum TypeOp {
    Arithmetic(ArithmeticOp),
    Relational(RelationalOp),
    Logic(LogicalOp),
    Equality(EqualityOp),
    Bit(BitwiseOp),
}

impl Op {
    fn from(pre: PreOp) -> Self {
        match pre {
            PreOp::Arithmetic(op, id1, id2) => {
                Op::Op(TypeOp::Arithmetic(op), id1, id2)
            },
            PreOp::Relational(op, id1, id2) => {
                Op::Op(TypeOp::Relational(op), id1, id2)
            },
            PreOp::Bit(op, id1, id2) => {
                Op::Op(TypeOp::Bit(op), id1, id2)
            },
            PreOp::Logical(op, id1, id2) => {
                Op::Op(TypeOp::Logic(op), id1, id2)
            },
            PreOp::Equality(op, id1, id2) => {
                Op::Op(TypeOp::Equality(op), id1, id2)
            },
            PreOp::Assignment(name, val) => {
                Op::Assignment(name, val)
            },
            PreOp::Unary(op, id) => {
                Op::Unary(UnOp::from(op), id)
            },
            PreOp::Call(call) => {
                Op::Call(call)
            },
        }
    } 
}

#[derive(Debug)]
pub enum PreOp {
    Arithmetic(ArithmeticOp, ID, ID),
    // here might be better used ID
    Assignment(Option<ID>, Val),
    Relational(RelationalOp, ID, ID),
    Equality(EqualityOp, ID, ID),
    Logical(LogicalOp, ID, ID),
    Bit(BitwiseOp, ID, ID),
    Unary(UnOpInst, ID),
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
pub enum BitwiseOp {
    And,
    Or,
    Xor,
    LShift,
    RShift,
}

impl BitwiseOp {
    fn from(op: &ast::BinOp) -> Option<Self> {
        match op {
            ast::BinOp::BitwiseAnd => Some(BitwiseOp::And),
            ast::BinOp::BitwiseOr => Some(BitwiseOp::Or),
            ast::BinOp::BitwiseXor => Some(BitwiseOp::Xor),
            ast::BinOp::BitwiseLeftShift => Some(BitwiseOp::LShift),
            ast::BinOp::BitwiseRightShift => Some(BitwiseOp::RShift),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum UnOp {
    Neg,
    BitComplement,
    LogicNeg,
}

impl UnOp {
    fn from(op: UnOpInst) -> Self {
        match op {
            UnOpInst::Neg => UnOp::Neg,
            UnOpInst::BitComplement => UnOp::BitComplement,
            UnOpInst::LogicNeg => UnOp::LogicNeg,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub enum UnOpInst {
    Neg,
    BitComplement,
    LogicNeg,
    IncPre,
    IncPost,
    DecPre,
    DecPost,
}

impl UnOpInst {
    fn from(op: &ast::UnOp) -> Self {
        match op {
            ast::UnOp::Negation => UnOpInst::Neg,
            ast::UnOp::BitwiseComplement => UnOpInst::BitComplement,
            ast::UnOp::LogicalNegation => UnOpInst::LogicNeg,
            ast::UnOp::IncrementPrefix => UnOpInst::IncPre,
            ast::UnOp::IncrementPostfix => UnOpInst::IncPost,
            ast::UnOp::DecrementPrefix => UnOpInst::DecPre,
            ast::UnOp::DecrementPostfix => UnOpInst::DecPost,
        }
    }
}

#[derive(Debug)]
pub enum RelationalOp {
    Less,
    LessOrEq,
    Greater,
    GreaterOrEq,
}


#[derive(Debug)]
pub enum LogicalOp {
    And,
    Or,
}
#[derive(Debug)]
pub enum EqualityOp {
    Equal,
    NotEq,
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
