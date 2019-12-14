use simple_c_compiler::tac;

pub fn pretty(fun: &tac::FuncDef) {
    println!("{}:", fun.name);
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
                    _ => unimplemented!(),
                };
            }
            tac::Instruction::ControllOp(cop) => {}
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

pub fn pretty_arith_op(op: &tac::ArithmeticOp) -> String {
    match op {
        tac::ArithmeticOp::Add => "+".to_string(),
        tac::ArithmeticOp::Sub => "-".to_string(),
        tac::ArithmeticOp::Mul => "*".to_string(),
        tac::ArithmeticOp::Div => "/".to_string(),
        tac::ArithmeticOp::Mod => "%".to_string(),
    }
}
