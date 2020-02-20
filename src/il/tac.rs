use crate::ast;
use std::collections::{HashMap, HashSet};

pub fn il(p: &ast::Program) -> Vec<FuncDef> {
    let mut gen = Generator::new();
    let mut funcs = Vec::new();
    for fun in &p.0 {
        if let Some(func) = gen.parse(fun) {
            funcs.push(func);
        }
        gen.clear_vars();
    }

    funcs
}

struct Generator {
    // TODO: certainly not sure about contains this tuple
    // it has been done only for pretty_output purposes right now
    instructions: Vec<InstructionLine>,
    // used to get deal with scopes
    instruction_buffer: Vec<Vec<InstructionLine>>,
    is_buffering: bool,
    context: Context,
    label_counter: usize,
    allocated: usize,
}

// TODO: change the type make the files private and create method instead
#[derive(Debug)]
pub struct InstructionLine(pub Instruction, pub Option<ID>);

struct Context {
    /*
        NOTION: take away from ID as a dependency
    */
    symbols: HashMap<String, Vec<ID>>,
    list_symbols: HashMap<String, Vec<ID>>,
    symbols_counter: usize,
    scopes: Vec<HashSet<String>>,
    loop_ctx: Vec<LoopContext>,
    ret_ctx: Option<ReturnContext>,
}

impl Context {
    fn new() -> Self {
        Context {
            symbols: HashMap::new(),
            list_symbols: HashMap::new(),
            symbols_counter: 0,
            scopes: vec![HashSet::new()],
            loop_ctx: Vec::new(),
            ret_ctx: None,
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashSet::new());
    }

    fn pop_scope(&mut self) {
        let scope = self.scopes.pop().unwrap();
        // TODO: might we should change the data structure since
        // It got much more complicated for such type of typical needs
        //
        // Including the fact that we store the duplicates
        // to be able to display them in pretty way it takes blazingly many resources.
        //
        // list_symbols was created to support pretty_output logic
        // since it expected to get all names by IDs when we remove here the names
        // it just cannot find them.
        for symbol in scope {
            self.symbols.get_mut(&symbol).unwrap().pop();
        }
    }

    fn add_symbol(&mut self, name: &str) -> ID {
        if !self.add_symbol_to_scope(name) {
            /*
                TODO: Here should be raised a error since we have added the same variable to scope
                what is error

                it may be implemented as a feature, what means that we can pass here a config of polices to such type of behavior

                It's not handled anywhere above in the chain of compilation process
            */
            unimplemented!()
        }

        let id = ID::new(self.symbols_counter, IDType::Var);
        self.symbols_counter += 1;
        self.symbols
            .entry(name.to_owned())
            .or_default()
            .push(id.clone());
        self.list_symbols
            .entry(name.to_owned())
            .or_default()
            .push(id.clone());

        id
    }

    // add_tmp method was developed in regard to have the same counter for id
    // for Var and Tmp types even though might it's not the best place to realize this method.
    // Might we have to switch to another approach with ID.
    // The main concerned about that is pretty_output needs an uniq indicator for tmp and var as well.
    // But the translation code have a strong needs in uniq one for both.
    // It may be a smelt code, since context does not deal with tmp at all currently.
    fn add_tmp(&mut self) -> ID {
        let id = ID::new(self.symbols_counter, IDType::Temporary);
        self.symbols_counter += 1;
        id
    }

    fn get_symbol(&self, name: &str) -> Option<&ID> {
        self.symbols.get(name).and_then(|ids| ids.last())
    }

    fn add_symbol_to_scope(&mut self, name: &str) -> bool {
        let last_scope = self.scopes.last_mut().unwrap();
        last_scope.insert(name.to_owned())
    }

    /*
        NOTION: could we store in context more useful information?
        e.g variables context

        on that regard we could develop a more convenient approach like

        let ctx = generator.push_ctx();
        ...

        do some stuff with context, and then it goes off the scope drop will be called
    */
    fn push_loop(&mut self, ctx: LoopContext) {
        self.loop_ctx.push(ctx);
    }

    fn pop_loop(&mut self) {
        self.loop_ctx.pop();
    }

    fn loop_end(&self) -> Label {
        self.loop_ctx.last().as_ref().unwrap().end
    }

    fn loop_start(&self) -> Label {
        self.loop_ctx.last().as_ref().unwrap().begin
    }

    fn clear(&mut self) {
        self.symbols.clear();
        self.scopes.clear();
        self.scopes.push(HashSet::new());
        self.loop_ctx.clear();
    }
}

struct LoopContext {
    begin: Label,
    end: Label,
}

impl LoopContext {
    fn new(begin: Label, end: Label) -> Self {
        LoopContext { begin, end }
    }
}

struct ReturnContext {
    save_id: ID,
    label: Label,
}

impl Generator {
    pub fn new() -> Self {
        Generator {
            label_counter: 0,
            allocated: 0,
            instructions: Vec::new(),
            instruction_buffer: Vec::new(),
            is_buffering: false,
            context: Context::new(),
        }
    }

    pub fn from(g: &Generator) -> Self {
        let mut generator = Generator::new();
        // check is it copy or clone in sense of references.
        generator.label_counter = g.label_counter;
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

        let mut params = Vec::new();
        for p in func.parameters.iter() {
            /*
                Don't allocate memory for parameters since
                this memory was prepared by caller
            */
            let id = self.remember_var(&p);
            params.push(id.id);
        }

        let blocks = func.blocks.as_ref().unwrap();

        let count_returns = count_returns(blocks);
        if count_returns > 0 {
            let ret_id = self
                .emit(Instruction::Alloc(Value::Const(Const::Int(0))))
                .unwrap();
            let label = self.uniq_label();
            self.context.ret_ctx = Some(ReturnContext {
                save_id: ret_id,
                label,
            });
        }

        for block in blocks {
            self.emit_block(&block);
        }

        if count_returns == 0 {
            self.emit(Instruction::ControlOp(ControlOp::Return(Value::Const(
                Const::Int(0),
            ))));
        } else if count_returns != 1 {
            let v = self.context.ret_ctx.as_ref().unwrap().save_id.clone();
            let l = self.context.ret_ctx.as_ref().unwrap().label.clone();
            self.emit(Instruction::ControlOp(ControlOp::Label(l)));
            self.emit(Instruction::ControlOp(ControlOp::Return(Value::ID(v))));
        }

        let vars = self
            .context
            .list_symbols
            .iter()
            .map(|(var, ids)| {
                ids.iter()
                    .map(|id| (id.id, var.clone()))
                    .collect::<Vec<_>>()
            })
            .flatten()
            .collect::<HashMap<usize, String>>();

        self.context.symbols.clear();
        Some(FuncDef {
            name: func.name.clone(),
            frame_size: self.allocated_memory(),
            instructions: self.flush(),
            vars: vars,
            parameters: params,
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

        if self.is_buffering {
            self.instruction_buffer
                .last_mut()
                .unwrap()
                .push(InstructionLine(inst, id.clone()));
        } else {
            self.instructions.push(InstructionLine(inst, id.clone()));
        }

        id
    }

    fn emit_expr(&mut self, exp: &ast::Exp) -> Value {
        match exp {
            ast::Exp::Var(name) => Value::from(self.recognize_var(name)),
            ast::Exp::Const(ast::Const::Int(val)) => {
                // TODO: might it should be changed since we whant to handle expresions like this
                // in this manner.
                //
                // x = 2 * a -> x := a * 2
                //
                // Without a temporary variable, but its deservers a major discussion
                Value::from(Const::Int(*val as i32))
            }
            ast::Exp::FuncCall(name, params) => {
                // Notion: it might be useful if we don't work with IDs itself here,
                // instead we could handle types which contains its size and id
                let values = params.iter().map(|exp| self.emit_expr(exp)).collect();

                let types_size = params.len() * 4;

                let id = self
                    .emit(Instruction::Call(Call::new(&name, values, types_size)))
                    .unwrap();
                Value::from(id)
            }
            ast::Exp::UnOp(op, exp) => {
                let val = self.emit_expr(exp);
                // TODO: looks like here the problem with additional tmp variable
                let id = self
                    .emit(Instruction::Op(Op::Unary(UnOp::from(op), val)))
                    .unwrap();
                Value::from(id)
            }
            ast::Exp::IncOrDec(name, op) => {
                let var_id = self.recognize_var(name);
                let one = Value::Const(Const::Int(1));

                let arithmetic_op = match op {
                    ast::IncOrDec::Inc(..) => TypeOp::Arithmetic(ArithmeticOp::Add),
                    ast::IncOrDec::Dec(..) => TypeOp::Arithmetic(ArithmeticOp::Sub),
                };

                if op.is_postfix() {
                    let var_copy = self
                        .emit(Instruction::Alloc(Value::from(var_id.clone())))
                        .unwrap();
                    let changed_id = self
                        .emit(Instruction::Op(Op::Op(
                            arithmetic_op,
                            Value::from(var_id.clone()),
                            one,
                        )))
                        .unwrap();
                    self.emit(Instruction::Assignment(var_id, Value::from(changed_id)))
                        .unwrap();
                    Value::from(var_copy)
                } else {
                    let changed_id = self
                        .emit(Instruction::Op(Op::Op(
                            arithmetic_op,
                            Value::from(var_id.clone()),
                            one,
                        )))
                        .unwrap();
                    self.emit(Instruction::Assignment(
                        var_id,
                        Value::from(changed_id.clone()),
                    ))
                    .unwrap();
                    Value::from(changed_id)
                }
            }
            ast::Exp::BinOp(op, exp1, exp2) => {
                if let ast::BinOp::And = op {
                    let end_label = self.uniq_label();
                    let val1 = self.emit_expr(exp1);
                    let tmp_var = self
                        .emit(Instruction::Alloc(Value::from(Const::Int(0))))
                        .unwrap();
                    self.emit(Instruction::ControlOp(ControlOp::Branch(Branch::IfGOTO(
                        val1, end_label,
                    ))));
                    let val2 = self.emit_expr(exp2);
                    self.emit(Instruction::ControlOp(ControlOp::Branch(Branch::IfGOTO(
                        val2, end_label,
                    ))));
                    let false_var = self
                        .emit(Instruction::Alloc(Value::from(Const::Int(1))))
                        .unwrap();
                    self.emit(Instruction::Assignment(
                        tmp_var.clone(),
                        Value::from(false_var),
                    ));
                    self.emit(Instruction::ControlOp(ControlOp::Label(end_label)));
                    Value::from(tmp_var)
                } else if let ast::BinOp::Or = op {
                    let second_branch = self.uniq_label();
                    let false_branch = self.uniq_label();
                    let end_label = self.uniq_label();
                    let val1 = self.emit_expr(exp1);
                    let tmp_var = self
                        .emit(Instruction::Alloc(Value::from(Const::Int(1))))
                        .unwrap();
                    self.emit(Instruction::ControlOp(ControlOp::Branch(Branch::IfGOTO(
                        val1,
                        second_branch,
                    ))));
                    self.emit(Instruction::ControlOp(ControlOp::Branch(Branch::GOTO(
                        end_label,
                    ))));
                    self.emit(Instruction::ControlOp(ControlOp::Label(second_branch)));
                    let val2 = self.emit_expr(exp2);
                    self.emit(Instruction::ControlOp(ControlOp::Branch(Branch::IfGOTO(
                        val2,
                        false_branch,
                    ))));
                    self.emit(Instruction::ControlOp(ControlOp::Branch(Branch::GOTO(
                        end_label,
                    ))));
                    self.emit(Instruction::ControlOp(ControlOp::Label(false_branch)));
                    let false_var = self
                        .emit(Instruction::Alloc(Value::from(Const::Int(0))))
                        .unwrap();
                    self.emit(Instruction::Assignment(
                        tmp_var.clone(),
                        Value::from(false_var),
                    ));
                    self.emit(Instruction::ControlOp(ControlOp::Label(end_label)));
                    Value::from(tmp_var)
                } else {
                    let id1 = self.emit_expr(exp1);
                    let val = self.emit_expr(exp2);
                    Value::from(
                        self.emit(Instruction::Op(Op::Op(TypeOp::from(op), id1, val)))
                            .unwrap(),
                    )
                }
            }
            ast::Exp::Assign(name, exp) => {
                let var_id = self.recognize_var(name);
                let exp_id = self.emit_expr(exp);
                Value::from(
                    self.emit(Instruction::Assignment(var_id, Value::from(exp_id)))
                        .unwrap(),
                )
            }
            ast::Exp::CondExp(cond, exp1, exp2) => {
                /*
                    NOTION: if we will get a track with assign id an operator
                    it can be simplified by removing tmp_id
                */
                let end_label = self.uniq_label();
                let exp2_label = self.uniq_label();

                let tmp_id = self.alloc_tmp();

                let cond_val = self.emit_expr(cond);
                self.emit(Instruction::ControlOp(ControlOp::Branch(Branch::IfGOTO(
                    cond_val, exp2_label,
                ))));
                let exp_id = self.emit_expr(exp1);
                self.emit(Instruction::Assignment(tmp_id.clone(), exp_id));
                self.emit(Instruction::ControlOp(ControlOp::Branch(Branch::GOTO(
                    end_label,
                ))));
                self.emit(Instruction::ControlOp(ControlOp::Label(exp2_label)));
                let exp_id = self.emit_expr(exp2);
                self.emit(Instruction::Assignment(tmp_id.clone(), exp_id));
                self.emit(Instruction::ControlOp(ControlOp::Label(end_label)));

                Value::from(tmp_id)
            }
            ast::Exp::AssignOp(name, op, exp) => {
                let id = self.recognize_var(name);
                let op = assign_op_to_type_op(op);
                let val = self.emit_expr(exp);
                let resp = self
                    .emit(Instruction::Op(Op::Op(op, Value::from(id.clone()), val)))
                    .unwrap();
                self.emit(Instruction::Assignment(id, Value::from(resp.clone())));
                Value::from(resp)
            }
        }
    }

    fn emit_decl(&mut self, decl: &ast::Declaration) {
        match decl {
            ast::Declaration::Declare { name, exp } => {
                if let Some(exp) = exp {
                    let exp_id = self.emit_expr(exp);
                    let var_id = self.alloc_var(name);
                    self.emit(Instruction::Assignment(var_id, exp_id));
                } else {
                    // Allocate the value to be able to recognize it.
                    // Do that after processing expression since there may be
                    // a variable with the same name in the above scope
                    let var_id = self.alloc_var(name);
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
            ast::Statement::Exp { exp: exp } => {
                if let Some(exp) = exp {
                    self.emit_expr(exp);
                }
            }
            ast::Statement::Return { exp } => {
                let val = self.emit_expr(exp);
                if let Some(ret) = self.context.ret_ctx.as_ref() {
                    let save_id = ret.save_id.clone();
                    let l = ret.label;
                    self.emit(Instruction::Assignment(save_id, val));
                    self.emit(Instruction::ControlOp(ControlOp::Branch(Branch::GOTO(l))));
                } else {
                    self.emit(Instruction::ControlOp(ControlOp::Return(val)));
                }
            }
            ast::Statement::Conditional {
                cond_expr,
                if_block,
                else_block,
            } => {
                let cond_val = self.emit_expr(cond_expr);
                let end_label = self.uniq_label();

                self.emit(Instruction::ControlOp(ControlOp::Branch(Branch::IfGOTO(
                    cond_val, end_label,
                ))));
                self.emit_statement(if_block);
                if let Some(else_block) = else_block {
                    let else_label = end_label;
                    let end_label = self.uniq_label();

                    self.emit(Instruction::ControlOp(ControlOp::Branch(Branch::GOTO(
                        end_label,
                    ))));
                    self.emit(Instruction::ControlOp(ControlOp::Label(else_label)));
                    self.emit_statement(else_block);
                    self.emit(Instruction::ControlOp(ControlOp::Label(end_label)));
                } else {
                    self.emit(Instruction::ControlOp(ControlOp::Label(end_label)));
                }
            }
            ast::Statement::Compound { list: list } => {
                self.start_scope();

                if let Some(list) = list {
                    for block in list {
                        self.emit_block(block);
                    }
                }

                self.end_scope();
            }
            ast::Statement::While { exp, statement } => {
                let begin_label = self.uniq_label();
                let end_label = self.uniq_label();

                self.context
                    .push_loop(LoopContext::new(begin_label, end_label));

                self.emit(Instruction::ControlOp(ControlOp::Label(begin_label)));
                let cond_val = self.emit_expr(exp);
                self.emit(Instruction::ControlOp(ControlOp::Branch(Branch::IfGOTO(
                    cond_val, end_label,
                ))));

                self.start_scope();
                self.emit_statement(statement);
                self.end_scope();

                self.emit(Instruction::ControlOp(ControlOp::Branch(Branch::GOTO(
                    begin_label,
                ))));
                self.emit(Instruction::ControlOp(ControlOp::Label(end_label)));

                self.context.pop_loop();
            }
            ast::Statement::Do { exp, statement } => {
                let begin_label = self.uniq_label();
                let end_label = self.uniq_label();

                self.context
                    .push_loop(LoopContext::new(begin_label, end_label));

                self.emit(Instruction::ControlOp(ControlOp::Label(begin_label)));

                self.start_scope();
                self.emit_statement(statement);
                self.end_scope();

                let cond_val = self.emit_expr(exp);
                self.emit(Instruction::ControlOp(ControlOp::Branch(Branch::IfGOTO(
                    cond_val, end_label,
                ))));
                self.emit(Instruction::ControlOp(ControlOp::Branch(Branch::GOTO(
                    begin_label,
                ))));
                self.emit(Instruction::ControlOp(ControlOp::Label(end_label)));

                self.context.pop_loop();
            }
            ast::Statement::ForDecl {
                decl,
                exp2,
                exp3,
                statement,
            } => {
                let begin_label = self.uniq_label();
                let end_label = self.uniq_label();
                let continue_label = if exp3.is_some() {
                    self.uniq_label()
                } else {
                    begin_label.clone()
                };

                self.context
                    .push_loop(LoopContext::new(continue_label, end_label));

                self.start_scope();
                self.emit_decl(decl);

                self.emit(Instruction::ControlOp(ControlOp::Label(begin_label)));
                let cond_val = self.emit_expr(exp2);
                self.emit(Instruction::ControlOp(ControlOp::Branch(Branch::IfGOTO(
                    cond_val, end_label,
                ))));

                if let Some(exp3) = exp3 {
                    self.start_buffering();
                    self.emit_expr(exp3);
                    self.stop_buffering();
                }

                self.start_scope();
                self.emit_statement(statement);
                self.end_scope();
                self.end_scope();

                if exp3.is_some() {
                    self.emit(Instruction::ControlOp(ControlOp::Label(continue_label)));

                    self.flush_buffer();
                }

                self.emit(Instruction::ControlOp(ControlOp::Branch(Branch::GOTO(
                    begin_label,
                ))));
                self.emit(Instruction::ControlOp(ControlOp::Label(end_label)));

                self.context.pop_loop();
            }
            ast::Statement::For {
                exp1,
                exp2,
                exp3,
                statement,
            } => {
                let begin_label = self.uniq_label();
                let end_label = self.uniq_label();
                let continue_label = if exp3.is_some() {
                    self.uniq_label()
                } else {
                    begin_label.clone()
                };

                self.context
                    .push_loop(LoopContext::new(continue_label, end_label));

                if let Some(exp) = exp1 {
                    self.emit_expr(exp);
                }
                self.emit(Instruction::ControlOp(ControlOp::Label(begin_label)));
                let cond_val = self.emit_expr(exp2);
                self.emit(Instruction::ControlOp(ControlOp::Branch(Branch::IfGOTO(
                    cond_val, end_label,
                ))));

                self.start_scope();
                self.emit_statement(statement);
                self.end_scope();

                if let Some(exp3) = exp3 {
                    self.emit(Instruction::ControlOp(ControlOp::Label(continue_label)));

                    self.emit_expr(exp3);
                }
                self.emit(Instruction::ControlOp(ControlOp::Branch(Branch::GOTO(
                    begin_label,
                ))));
                self.emit(Instruction::ControlOp(ControlOp::Label(end_label)));

                self.context.pop_loop();
            }
            ast::Statement::Break => {
                self.emit(Instruction::ControlOp(ControlOp::Branch(Branch::GOTO(
                    self.context.loop_end(),
                ))));
            }
            ast::Statement::Continue => {
                self.emit(Instruction::ControlOp(ControlOp::Branch(Branch::GOTO(
                    self.context.loop_start(),
                ))));
            }
        }
    }

    fn start_buffering(&mut self) {
        self.instruction_buffer.push(Vec::new());
        self.is_buffering = true;
    }

    fn stop_buffering(&mut self) {
        self.is_buffering = false;
    }
    fn flush_buffer(&mut self) {
        self.instructions
            .extend(self.instruction_buffer.pop().unwrap());
    }

    // TODO: implement a a function which call something in scope
    fn start_scope(&mut self) {
        self.context.push_scope();
    }

    fn end_scope(&mut self) {
        self.context.pop_scope();
    }

    pub fn recognize_var(&mut self, name: &str) -> ID {
        self.context.get_symbol(name).unwrap().clone()
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

    pub fn clear_vars(&mut self) {
        self.context.clear();
    }

    fn alloc_tmp(&mut self) -> ID {
        self.allocated += 1;
        self.context.add_tmp()
    }

    fn alloc_var(&mut self, name: &str) -> ID {
        self.allocated += 1;
        self.remember_var(name)
    }

    fn remember_var(&mut self, name: &str) -> ID {
        self.context.add_symbol(name)
    }

    fn uniq_label(&mut self) -> Label {
        let l = self.label_counter;
        self.label_counter += 1;
        l
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
    // #[derive(Debug)]
    // enum Exp {
    //     Id(ID),
    //     Call(Call),
    //     Op(Op),
    // }
    //
    Assignment(ID, Value),
    // Notion: Can alloc be responsible not only for tmp variables?
    Alloc(Value),
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

    pub fn new(id: usize, tp: IDType) -> Self {
        ID { id, tp }
    }

    pub fn is_var(&self) -> bool {
        match self.tp {
            IDType::Var => true,
            IDType::Temporary => false,
        }
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
    Op(TypeOp, Value, Value),
    Unary(UnOp, Value),
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
    Return(Value),
}

type BytesSize = usize;

#[derive(Debug)]
pub enum Const {
    Int(i32),
}

#[derive(Debug)]
pub enum Value {
    ID(ID),
    Const(Const),
}

impl Value {
    pub fn id(self) -> Option<ID> {
        match self {
            Value::ID(id) => Some(id),
            _ => None,
        }
    }

    pub fn constant(self) -> Option<Const> {
        match self {
            Value::Const(c) => Some(c),
            _ => None,
        }
    }

    pub fn as_id(&self) -> Option<&ID> {
        match self {
            Value::ID(id) => Some(id),
            _ => None,
        }
    }

    pub fn is_id(&self) -> bool {
        match self {
            Value::ID(..) => true,
            _ => false,
        }
    }
}

impl From<Const> for Value {
    fn from(c: Const) -> Self {
        Value::Const(c)
    }
}

impl From<ID> for Value {
    fn from(id: ID) -> Self {
        Value::ID(id)
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
    IfGOTO(Value, Label),
}

#[derive(Debug)]
pub struct Call {
    pub name: String,
    pub params: Vec<Value>,
    pub pop_size: BytesSize,
    pub tp: FnType,
}

impl Call {
    fn new(name: &str, params: Vec<Value>, params_size: BytesSize) -> Self {
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
    pub parameters: Vec<usize>,
    pub frame_size: BytesSize,
    pub vars: HashMap<usize, String>,
    pub instructions: Vec<InstructionLine>,
}

fn assign_op_to_type_op(op: &ast::AssignmentOp) -> TypeOp {
    match op {
        ast::AssignmentOp::Plus => TypeOp::Arithmetic(ArithmeticOp::Add),
        ast::AssignmentOp::Mul => TypeOp::Arithmetic(ArithmeticOp::Mul),
        ast::AssignmentOp::Sub => TypeOp::Arithmetic(ArithmeticOp::Sub),
        ast::AssignmentOp::Div => TypeOp::Arithmetic(ArithmeticOp::Div),
        ast::AssignmentOp::Mod => TypeOp::Arithmetic(ArithmeticOp::Mod),
        ast::AssignmentOp::BitAnd => TypeOp::Bit(BitwiseOp::And),
        ast::AssignmentOp::BitOr => TypeOp::Bit(BitwiseOp::Or),
        ast::AssignmentOp::BitXor => TypeOp::Bit(BitwiseOp::Xor),
        ast::AssignmentOp::BitLeftShift => TypeOp::Bit(BitwiseOp::LShift),
        ast::AssignmentOp::BitRightShift => TypeOp::Bit(BitwiseOp::RShift),
    }
}

fn count_returns(blocks: &[ast::BlockItem]) -> usize {
    let mut i = 0;
    for b in blocks {
        i += count_returns_bl(b)
    }

    i
}

fn count_returns_bl(b: &ast::BlockItem) -> usize {
    match b {
        ast::BlockItem::Statement(st) => count_returns_st(st),
        _ => 0,
    }
}

fn count_returns_st(st: &ast::Statement) -> usize {
    match st {
        ast::Statement::Return { .. } => 1,
        ast::Statement::Conditional {
            if_block,
            else_block,
            ..
        } => count_returns_st(if_block) + else_block.as_ref().map_or(0, |st| count_returns_st(st)),
        ast::Statement::Compound {
            list: list @ Some(..),
        } => count_returns(list.as_ref().unwrap()),
        ast::Statement::For { statement, .. } => count_returns_st(statement),
        ast::Statement::ForDecl { statement, .. } => count_returns_st(statement),
        ast::Statement::While { statement, .. } => count_returns_st(statement),
        ast::Statement::Do { statement, .. } => count_returns_st(statement),
        _ => 0,
    }
}
