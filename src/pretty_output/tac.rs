use simple_c_compiler::tac;

pub fn pretty(fun: &tac::FuncDef) {
    println!("{}:", pretty_fun_name(&fun.name));
    println!("  BeginFunc {}", fun.frame_size);

    for inst in &fun.instructions {
        match inst {
            tac::Instruction::Op(id, op) => {
                let id = id.as_ref().unwrap();
                match op {
                    tac::Op::Assignment(.., val) => {
                        println!("  {}: {}", pretty_id(id), pretty_val(val));
                    }
                    tac::Op::Arithmetic(op, v1, v2) => {
                        println!("  {}: {} {} {}", pretty_id(id), pretty_id(v1), pretty_arith_op(op), pretty_id(v2));
                    }
                    tac::Op::Relational(tac::RelationalOp::Equal, v1, v2) => {
                        println!("  {}: {} == {}", pretty_id(id), pretty_id(v1), pretty_id(v2));
                    }
                    tac::Op::Call(call) => {
                        for p in call.params.iter() {
                            println!("  PushParam {}", pretty_id(id));
                        }

                        println!("  {}: LCall {}", pretty_id(id), pretty_fun_name(&call.name));
                        println!("  PopParams {}", call.pop_size);
                    }
                    _ => unimplemented!(),
                };
            }
            tac::Instruction::ControllOp(cop) => {
                match cop {
                    tac::ControllOp::Branch(lb) => {
                        match lb {
                            tac::LabelBranch::Label(label) => {
                                println!("{}:", pretty_label(label));
                            },
                            tac::LabelBranch::GOTO(label) => {
                                println!("  Goto {}", pretty_label(label));
                            },
                            tac::LabelBranch::IfGOTO(id, label) => {
                                println!("  IfZ {} Goto {}", pretty_id(id), pretty_label(label));
                            },
                        }
                    }
                    _ => unimplemented!(),
                }
            }
        }
    }

    println!(
        "  Return {};",
        fun.ret.as_ref().map_or("NO".to_owned(), |id| pretty_id(id))
    );
    println!("  EndFunc;");
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
