use simple_c_compiler::{tac};

pub fn pretty(instr_list: &Vec<tac::Instruction>) {
    for inst in instr_list {
        if let Some(id) = inst.id.as_ref() {
            let inst = match &inst.op.op {
                tac::Op::Arithmetic(tac::ArithmeticOp::Add, v1, v2) => {
                    format!("{} + {}", pretty_val(v1), pretty_val(v2))
                }
                tac::Op::Assignment(id, val) => {
                    format!("{}", /*pretty_id(id),*/ pretty_val(val))
                }
                _ => unimplemented!(),
            };
            println!("  {}: {}", pretty_id(id), inst);
        } else {
            match &inst.op.op {
                tac::Op::FuncDef(func) => {
                    match func {
                        tac::FuncDef::Begin(name, size) => {
                            println!("{}:\n  BeginFunc {}", name, size);
                        }
                        tac::FuncDef::Ret(val) => {
                            println!("  Return {};\n  EndFunc", val.as_ref().map_or("NONE".to_owned(), |id| pretty_id(id)));
                        }
                    }
                }
                _ => unimplemented!(),
            }
        }
    }
}

pub fn pretty_id(id: &tac::ID) -> String {
    match id.tp {
        tac::IDType::Var => format!("a{}", id.id),
        tac::IDType::Temporary => format!("_t{}", id.id),
    }
}

pub fn pretty_val(v: &tac::Val) -> String {
    match v {
        tac::Val::Var(id) => format!("{}", pretty_id(id)),
        tac::Val::Const(tac::Const::Int(val)) => format!("{}", val), 
    }
}
