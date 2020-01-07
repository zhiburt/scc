use crate::ast;
use std::collections::HashMap;

pub fn il(p: &ast::Program) -> Vec<FuncDef> {
    let mut gen = Generator::new();
    let mut funcs = Vec::new();
    for fun in &p.0 {
        if let Some(func) = gen.parse(fun) {
            funcs.push(func);
        }
    }

    funcs
}
struct Generator {
    // TODO: certainly not sure about contains this tuple
    // it has been done only for pretty_output purposes right now
    instructions: Vec<InstructionLine>,
    vars: HashMap<String, ID>,
    context: Context,
    counters: [usize; 3],
    allocated: usize,
}

#[derive(Debug)]
pub struct InstructionLine(pub Instruction, pub Option<ID>);

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
            context: Context {
                ret_ctx: None,
                loop_ctx: None,
            },
        }
    }

    pub fn from(g: &Generator) -> Self {
        let mut generator = Generator::new();
        // check is it copy or clone in sense of references.
        generator.counters = g.counters;
        generator
    }

    pub fn parse(&mut self, func: &ast::FuncDecl) -> Option<FuncDef> {
        if func.blocks.is_none() {
            // here we should somehow show that this function can be called
            // with some type of parameters
            // it representation of declaration without definition
            //
            // unimplemented!()
            return None;
        }

        for p in func.parameters.iter() {
            self.recognize_var(&p);
        }

        let blocks = func.blocks.as_ref().unwrap();

        for block in blocks {
            self.emit_block(&block);
        }

        let vars = self
            .vars
            .iter()
            .map(|(var, id)| (id.id, var.clone()))
            .collect::<HashMap<usize, String>>();

        self.vars.clear();
        Some(FuncDef {
            name: func.name.clone(),
            frame_size: self.allocated_memory(),
            ret: self.context.ret_ctx.clone(),
            instructions: self.flush(),
            vars: vars,
        })
    }

    fn emit(&mut self, inst: Instruction) -> Option<ID> {
        let id = match &inst {
            Instruction::Op(..) => Some(self.alloc_tmp()),
            Instruction::Assignment(id, ..) => Some(id.clone()),
            Instruction::Alloc(..) => Some(self.alloc_tmp()),
            Instruction::Call(..) => {
                // TODO: we should handle somehow
                // the initial assignment to variable,
                // so might the best solution here is move call to Op type,
                // but not all calls has assignment pre operation
                //
                // It seems possible if we will have a small information about that in AST
                //
                // TODO: And what is the result unused?
                //
                // might it can be solved on some stage of optimization
                Some(self.alloc_tmp())
            }
            _ => None,
        };
        self.instructions.push(InstructionLine(inst, id.clone()));
        id
    }

    fn emit_expr(&mut self, exp: &ast::Exp) -> ID {
        match exp {
            ast::Exp::Var(name) => self.recognize_var(name),
            ast::Exp::Const(ast::Const::Int(val)) => {
                // TODO: might it should be changed since we whant to handle expresions like this
                // in this manner.
                //
                // x = 2 * a -> x := a * 2
                //
                // Without a temporary variable, but its deservers a major discussion
                self.emit(Instruction::Alloc(Const::Int(*val as i32)))
                    .unwrap()
            }
            ast::Exp::FuncCall(name, params) => {
                // Notion: it might be useful if we don't work with IDs itself here,
                // instead we could handle types which contains its size and id
                let ids = params.iter().map(|exp| self.emit_expr(exp)).collect();

                let types_size = params.len() * 4;

                self.emit(Instruction::Call(Call::new(&name, ids, types_size)))
                    .unwrap()
            }
            ast::Exp::UnOp(op, exp) => {
                let exp_id = self.emit_expr(exp);
                // TODO: looks like here the problem with additional tmp variable
                self.emit(Instruction::Op(Op::Unary(UnOp::from(op), exp_id)))
                    .unwrap()
            }
            ast::Exp::BinOp(op, exp1, exp2) => {
                let id1 = self.emit_expr(exp1);
                let id2 = self.emit_expr(exp2);
                self.emit(Instruction::Op(Op::Op(TypeOp::from(op), id1, id2)))
                    .unwrap()
            }
            ast::Exp::Assign(name, exp) => {
                let var_id = self.recognize_var(name);
                let exp_id = self.emit_expr(exp);
                self.emit(Instruction::Assignment(var_id, exp_id)).unwrap()
            }
            _ => unimplemented!(),
        }
    }

    fn emit_decl(&mut self, decl: &ast::Declaration) {
        match decl {
            ast::Declaration::Declare { name, exp } => {
                let var_id = self.var_id(name);
                if let Some(exp) = exp {
                    let exp_id = self.emit_expr(exp);
                    self.emit(Instruction::Assignment(var_id, exp_id));
                }
            }
        }
    }

    fn emit_block(&mut self, block: &ast::BlockItem) {
        match block {
            ast::BlockItem::Declaration(decl) => self.emit_decl(decl),
            ast::BlockItem::Statement(st) => self.emit_statement(st),
        }
    }

    fn emit_statement(&mut self, st: &ast::Statement) {
        match st {
            ast::Statement::Exp { exp: Some(exp) } => {
                self.emit_expr(exp);
            }
            ast::Statement::Return { exp } => {
                let id = self.emit_expr(exp);
                self.emit(Instruction::ControlOp(ControlOp::Return(id)));
            }
            ast::Statement::Conditional {
                cond_expr,
                if_block,
                else_block,
            } => {
                let cond_id = self.emit_expr(cond_expr);
                let end_label = self.uniq_label();

                self.emit(Instruction::ControlOp(ControlOp::Branch(Branch::IfGOTO(
                    cond_id, end_label,
                ))));
                self.emit_statement(if_block);
                if let Some(else_block) = else_block {
                    let else_label = end_label;
                    let end_label = self.uniq_label();

                    self.emit(Instruction::ControlOp(ControlOp::Branch(Branch::GOTO(
                        end_label,
                    ))));
                    self.emit(Instruction::ControlOp(ControlOp::Label(else_label)));
                    self.emit_statement(if_block);
                    self.emit(Instruction::ControlOp(ControlOp::Label(end_label)));
                } else {
                    self.emit(Instruction::ControlOp(ControlOp::Label(end_label)));
                }
            }
            ast::Statement::Compound { list: Some(list) } => {
                for block in list {
                    self.emit_block(block);
                }
            }
            _ => unimplemented!(),
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

    pub fn flush(&mut self) -> Vec<InstructionLine> {
        self.allocated = 0;
        let mut v = Vec::new();
        std::mem::swap(&mut self.instructions, &mut v);
        v
    }

    fn alloc_tmp(&mut self) -> ID {
        self.allocated += 1;
        ID::new(self.inc_tmp(), IDType::Temporary)
    }

    fn inc_vars(&mut self) -> usize {
        self.counters[1] += 1;
        self.counters[1]
    }

    fn inc_tmp(&mut self) -> usize {
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

#[derive(Debug)]
pub enum Instruction {
    // TODO: shake off this ID,
    // it represents assignment to a variable or a temporary one
    //
    // we would like to accomplish that since this operation is not represented by ID
    // it means that in the ID of this command will be the same as ID in parameter
    //
    // TODO: it does not support assignment of const, and constants now and Call Itself
    // the possible way is
    //
    // #[derive(Debug)]
    // enum Exp {
    //     Id(ID),
    //     Call(Call),
    //     Op(Op),
    // }
    Assignment(ID, ID),
    // Notion: Can alloc be responsible not only for tmp variables?
    Alloc(Const),
    Op(Op),
    Call(Call),
    ControlOp(ControlOp),
}

#[derive(Debug)]
enum Exp {
    Id(ID),
    Call(Call),
    Op(Op),
}

#[derive(Clone, Debug)]
pub struct ID {
    pub id: usize,
    pub tp: IDType,
}

impl ID {
    fn tmp() -> Self {
        ID {
            id: 0,
            tp: IDType::Temporary,
        }
    }

    fn new(id: usize, tp: IDType) -> Self {
        ID { id, tp }
    }
}

#[derive(Clone, Debug)]
pub enum IDType {
    Temporary,
    Var,
}

pub type Label = usize;

#[derive(Debug)]
pub enum Op {
    // TODO: it seems can be a Val
    Op(TypeOp, ID, ID),
    Unary(UnOp, ID),
}

#[derive(Debug)]
pub enum TypeOp {
    Arithmetic(ArithmeticOp),
    Relational(RelationalOp),
    Equality(EqualityOp),
    Bit(BitwiseOp),
}

impl TypeOp {
    fn from(op: &ast::BinOp) -> Self {
        match op {
            ast::BinOp::Addition => TypeOp::Arithmetic(ArithmeticOp::Add),
            ast::BinOp::Sub => TypeOp::Arithmetic(ArithmeticOp::Sub),
            ast::BinOp::Multiplication => TypeOp::Arithmetic(ArithmeticOp::Mul),
            ast::BinOp::Division => TypeOp::Arithmetic(ArithmeticOp::Div),
            ast::BinOp::Modulo => TypeOp::Arithmetic(ArithmeticOp::Mod),

            ast::BinOp::BitwiseAnd => TypeOp::Bit(BitwiseOp::And),
            ast::BinOp::BitwiseOr => TypeOp::Bit(BitwiseOp::Or),
            ast::BinOp::BitwiseXor => TypeOp::Bit(BitwiseOp::Xor),
            ast::BinOp::BitwiseLeftShift => TypeOp::Bit(BitwiseOp::LShift),
            ast::BinOp::BitwiseRightShift => TypeOp::Bit(BitwiseOp::RShift),

            ast::BinOp::Equal => TypeOp::Equality(EqualityOp::Equal),
            ast::BinOp::NotEqual => TypeOp::Equality(EqualityOp::NotEq),

            ast::BinOp::GreaterThan => TypeOp::Relational(RelationalOp::Greater),
            ast::BinOp::GreaterThanOrEqual => TypeOp::Relational(RelationalOp::GreaterOrEq),
            ast::BinOp::LessThan => TypeOp::Relational(RelationalOp::Less),
            ast::BinOp::LessThanOrEqual => TypeOp::Relational(RelationalOp::LessOrEq),

            ast::BinOp::And => unimplemented!(),
            ast::BinOp::Or => unimplemented!(),
        }
    }
}

#[derive(Debug)]
pub enum ControlOp {
    Label(Label),
    Branch(Branch),
    Return(ID),
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

#[derive(Debug)]
pub enum BitwiseOp {
    And,
    Or,
    Xor,
    LShift,
    RShift,
}

#[derive(Debug)]
pub enum UnOp {
    Neg,
    BitComplement,
    LogicNeg,
}

impl UnOp {
    fn from(op: &ast::UnOp) -> Self {
        match op {
            ast::UnOp::Negation => UnOp::Neg,
            ast::UnOp::BitwiseComplement => UnOp::BitComplement,
            ast::UnOp::LogicalNegation => UnOp::LogicNeg,
            _ => unreachable!(),
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
pub enum EqualityOp {
    Equal,
    NotEq,
}

#[derive(Debug)]
pub enum Branch {
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

impl Call {
    fn new(name: &str, params: Vec<ID>, params_size: BytesSize) -> Self {
        Call {
            name: name.to_owned(),
            tp: FnType::LCall,
            params,
            pop_size: params_size,
        }
    }
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
    pub instructions: Vec<InstructionLine>,
}
