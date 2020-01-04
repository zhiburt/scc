use crate::tac;
use std::collections::HashSet;

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

pub fn gen(foo: tac::FuncDef) -> Result<String> {
    let mut code = Vec::new();
    code.push(format!("{}:", foo.name));

    let ctx = MemoryContext::new();
    for i in foo.instructions {
        translate(i);
    }

    Ok(String::from(""))
}

fn translate(i: tac::Instruction) {
    match i {
        tac::Instruction::Op(id, op) => {
            translate_op(id, op);
        }
        tac::Instruction::ControllOp(op) => {
            translate_controll(op);
        }
    }
}

fn translate_op(id: Option<tac::ID>, op: tac::Op) {
    match op {
        tac::Op::Assignment(_, val) => {
            let id = id.unwrap();
            let val = match val {
                tac::Val::Const(tac::Const::Int(int)) => format!("int {}", int),
                tac::Val::Var(val_id) => format!("id {}", val_id.id),
            };

            println!("assigh id {} {}", id.id, val);
        }
        tac::Op::Call(tac::Call{name, params, pop_size, ..}) => {
            println!("call {} with len {:?}", name, params.iter().map(|p| p.id).collect::<Vec<_>>());
            println!("pop {}", pop_size);
        }
        tac::Op::Unary(op, id1) => {
            let op = match op {
                tac::UnOp::BitComplement => "bit complement",
                tac::UnOp::LogicNeg => "logic neg",
                tac::UnOp::Neg => "neg",
            };

            println!("unary {} {}", op, id1.id);
        }
        tac::Op::Op(op, id1, id2) => {
            let op = match op {
                tac::TypeOp::Arithmetic(op) => {
                    match op {
                        tac::ArithmeticOp::Add => "+",
                        tac::ArithmeticOp::Sub => "-",
                        tac::ArithmeticOp::Div => "/",
                        tac::ArithmeticOp::Mod => "%",
                        tac::ArithmeticOp::Mul => "*",
                    }
                }
                tac::TypeOp::Bit(op) => {
                    match op {
                            tac::BitwiseOp::And => "and",
                            tac::BitwiseOp::LShift => "left shift",
                            tac::BitwiseOp::RShift => "right shift",
                            tac::BitwiseOp::Or => "or",
                            tac::BitwiseOp::Xor => "xor",
                        }
                }
                tac::TypeOp::Equality(op) => {
                    match op {
                        tac::EqualityOp::Equal => "equal",
                        tac::EqualityOp::NotEq => "not equal",
                    }
                }
                tac::TypeOp::Relational(op) => {
                    match op {
                        tac::RelationalOp::Greater => "greater",
                        tac::RelationalOp::GreaterOrEq => "greater or equal",
                        tac::RelationalOp::Less => "less",
                        tac::RelationalOp::LessOrEq => "less or equal",
                    }
                }
            };

            println!("{} op {} {}", id1.id, op, id2.id);
        }
    }
}

fn translate_controll(op: tac::ControllOp) {
    match op {
        tac::ControllOp::Branch(branch) => {
            match branch {
                tac::LabelBranch::GOTO(label) => {
                    println!("goto _label{}", label);
                }
                tac::LabelBranch::IfGOTO(id, label) => {
                    println!("ifgoto id{} _label{}", id.id, label);
                }
                tac::LabelBranch::Label(label) => {
                    println!("goto _label{}", label);
                }
            }
        }
        _ => unreachable!(),
    }
}

pub struct MemoryContext {
    registers: Vec<State<Reg>>,
    allocated:  usize,
}

enum State<T> {
    Free(T),
    Used(T),
}

impl<T> State<T> {
    fn is_used(&self) -> bool {
        match self {
            State::Used(..) => true,
            State::Free(..) => false,
        }
    }

    fn switch(self) -> State<T> {
        match self {
            State::Used(v) => State::Free(v),
            State::Free(v) => State::Used(v),
        }
    }
}

impl<T> std::ops::Deref for State<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        match self {
            State::Used(v) => v,
            State::Free(v) => v,
        }
    }
}

impl MemoryContext {
    fn new() -> Self {
        MemoryContext {
            registers: Vec::new(),
            allocated: 0,
        }
    }

    fn alloc(&mut self) -> Space {
        Space::OnStack(0)
    }

    fn free(&mut self, s: Space) {

    }

    fn allowed_register(&self) -> Option<&Reg> {
        for r in self.registers.iter() {
            if !r.is_used() {
                return Some(r);
            }
        }

        None
    }
}

enum Space<'a> {
    OnStack(usize),
    InRegister(&'a Reg),
}

type Reg = String;
