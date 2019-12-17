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
                    tac::Op::Relational(op, v1, v2) => {
                        println!(
                            "  {}: {} {} {}",
                            pretty_id(&fun.vars, id),
                            pretty_id(&fun.vars, v1),
                            pretty_rel_op(op),
                            pretty_id(&fun.vars, v2),
                        );
                    }
                    tac::Op::Bit(op, v1, v2) => {
                        println!(
                            "  {}: {} {} {}",
                            pretty_id(&fun.vars, id),
                            pretty_id(&fun.vars, v1),
                            pretty_bit_op(op),
                            pretty_id(&fun.vars, v2),
                        );
                    }
                    tac::Op::Unary(op, v1) => {
                        println!(
                            "  {}: {}",
                            pretty_id(&fun.vars, id),
                            pretty_unary_op_with(op, &pretty_id(&fun.vars, v1)),
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

pub fn pretty_rel_op(op: &tac::RelationalOp) -> String {
    match op {
        tac::RelationalOp::Equal => "==".to_string(),
        tac::RelationalOp::NotEq => "!=".to_string(),
        tac::RelationalOp::Less => "<".to_string(),
        tac::RelationalOp::LessOrEq => "<=".to_string(),
        tac::RelationalOp::Greater => ">".to_string(),
        tac::RelationalOp::GreaterOrEq => ">=".to_string(),
        tac::RelationalOp::And => "&&".to_string(),
        tac::RelationalOp::Or => "||".to_string(),
    }
}

pub fn pretty_bit_op(op: &tac::BitwiseOp) -> String {
    match op {
    tac::BitwiseOp::And => "&".to_string(),
    tac::BitwiseOp::Or => "|".to_string(),
    tac::BitwiseOp::Xor => "^".to_string(),
    tac::BitwiseOp::LShift => "<<".to_string(),
    tac::BitwiseOp::RShift => ">>".to_string(),
    }
}

pub fn pretty_unary_op_with(op: &tac::UnOp, id: &str) -> String {
    match op {
        tac::UnOp::Neg => format!("-{}", id),
        tac::UnOp::BitComplement => format!("~{}", id),
        tac::UnOp::LogicNeg => format!("!{}", id),
        tac::UnOp::IncPre => format!("++{}", id),
        tac::UnOp::IncPost => format!("{}++", id),
        tac::UnOp::DecPre => format!("--{}", id),
        tac::UnOp::DecPost => format!("{}--", id),
    }
}
