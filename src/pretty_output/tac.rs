use std::collections::HashMap;
use std::io::Write;

use simple_c_compiler::tac;

pub fn pretty<W: Write>(mut w: W, fun: &tac::FuncDef) {
    writeln!(w, "{}:", pretty_fun_name(&fun.name));
    writeln!(w, "  BeginFunc {}", fun.frame_size);

    for inst in &fun.instructions {
        match inst {
            tac::Instruction::Op(id, op) => {
                let id = id.as_ref().unwrap();
                match op {
                    tac::Op::Assignment(.., val) => {
                        writeln!(
                            w,
                            "  {}: {}",
                            pretty_id(&fun.vars, id),
                            pretty_val(&fun.vars, val)
                        );
                    }
                    tac::Op::Op(t, v1, v2) => {
                        writeln!(
                            w,
                            "  {}: {} {} {}",
                            pretty_id(&fun.vars, id),
                            pretty_id(&fun.vars, v1),
                            pretty_type(t),
                            pretty_id(&fun.vars, v2)
                        );
                    }
                    tac::Op::Unary(op, v1) => {
                        writeln!(
                            w,
                            "  {}: {} {}",
                            pretty_id(&fun.vars, id),
                            pretty_unary_op(op),
                            pretty_id(&fun.vars, v1),
                        );
                    }
                    tac::Op::Call(call) => {
                        for p in call.params.iter() {
                            writeln!(w, "  PushParam {}", pretty_id(&fun.vars, p));
                        }

                        writeln!(
                            w,
                            "  {}: LCall {}",
                            pretty_id(&fun.vars, id),
                            pretty_fun_name(&call.name)
                        );
                        writeln!(w, "  PopParams {}", call.pop_size);
                    }
                };
            }
            tac::Instruction::ControllOp(cop) => match cop {
                tac::ControllOp::Branch(lb) => match lb {
                    tac::LabelBranch::Label(label) => {
                        writeln!(w, "{}:", pretty_label(label));
                    }
                    tac::LabelBranch::GOTO(label) => {
                        writeln!(w, "  Goto {}", pretty_label(label));
                    }
                    tac::LabelBranch::IfGOTO(id, label) => {
                        writeln!(
                            w,
                            "  IfZ {} Goto {}",
                            pretty_id(&fun.vars, id),
                            pretty_label(label)
                        );
                    }
                },
                tac::ControllOp::Return(id) => {
                    match id {
                        Some(id) => writeln!(w, "  Return {}", pretty_id(&fun.vars, id)),
                        None => writeln!(w, "  Return void"),
                    }
                }
                _ => unimplemented!(),
            },
        }
    }
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

pub fn pretty_type(op: &tac::TypeOp) -> String {
    match op {
        tac::TypeOp::Arithmetic(op) => pretty_arith_op(op),
        tac::TypeOp::Relational(op) => pretty_rel_op(op),
        tac::TypeOp::Equality(op) => pretty_eq_op(op),
        tac::TypeOp::Bit(op) => pretty_bit_op(op),
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
        tac::RelationalOp::Less => "<".to_string(),
        tac::RelationalOp::LessOrEq => "<=".to_string(),
        tac::RelationalOp::Greater => ">".to_string(),
        tac::RelationalOp::GreaterOrEq => ">=".to_string(),
    }
}

pub fn pretty_eq_op(op: &tac::EqualityOp) -> String {
    match op {
        tac::EqualityOp::Equal => "==".to_string(),
        tac::EqualityOp::NotEq => "!=".to_string(),
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

pub fn pretty_unary_op(op: &tac::UnOp) -> String {
    match op {
        tac::UnOp::Neg => "-".to_string(),
        tac::UnOp::LogicNeg => "!".to_string(),
        tac::UnOp::BitComplement => "~".to_string(),
    }
}
