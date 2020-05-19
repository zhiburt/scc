use std::io::Write;

use simple_c_compiler::il::tac;

pub fn pretty<W: Write>(mut w: W, fun: &tac::FuncDef) {
    writeln!(w, "{}:", pretty_fun_name(&fun.name));
    fun.parameters
        .iter()
        .map(|id| format!("param {}", pretty_id(id, &fun.ctx)))
        .for_each(|p| {
            writeln!(w, "  {}", p);
        });
    writeln!(w, "  BeginFunc {}", fun.frame_size);

    for tac::InstructionLine(inst, id) in &fun.instructions {
        match inst {
            tac::Instruction::Alloc(val) => {
                writeln!(
                    w,
                    "  {}: {}",
                    pretty_id(id.as_ref().unwrap(), &fun.ctx),
                    pretty_value(val, &fun.ctx),
                )
                .unwrap();
            }
            tac::Instruction::Assignment(id1, v) => {
                writeln!(
                    w,
                    "  {}: {}",
                    pretty_id(id1, &fun.ctx),
                    pretty_value(v, &fun.ctx),
                );
            }
            tac::Instruction::Call(call) => {
                for p in call.params.iter() {
                    writeln!(w, "  PushParam {}", pretty_value(p, &fun.ctx));
                }

                writeln!(
                    w,
                    "  {}: LCall {}",
                    pretty_id(id.as_ref().unwrap(), &fun.ctx),
                    pretty_fun_name(&call.name)
                );
                writeln!(w, "  PopParams {}", call.pop_size);
            }
            tac::Instruction::Op(op) => {
                match op {
                    tac::Op::Op(t, v1, v2) => {
                        writeln!(
                            w,
                            "  {}: {} {} {}",
                            pretty_id(id.as_ref().unwrap(), &fun.ctx),
                            pretty_value(v1, &fun.ctx),
                            pretty_type(t),
                            pretty_value(v2, &fun.ctx)
                        );
                    }
                    tac::Op::Unary(op, v1) => {
                        writeln!(
                            w,
                            "  {}: {} {}",
                            pretty_id(id.as_ref().unwrap(), &fun.ctx),
                            pretty_unary_op(op),
                            pretty_value(v1, &fun.ctx),
                        );
                    }
                };
            }
            tac::Instruction::ControlOp(cop) => match cop {
                tac::ControlOp::Label(label) => {
                    writeln!(w, "{}:", pretty_label(label));
                }
                tac::ControlOp::Branch(lb) => match lb {
                    tac::Branch::GOTO(label) => {
                        writeln!(w, "  Goto {}", pretty_label(label));
                    }
                    tac::Branch::IfGOTO(v, label) => {
                        writeln!(
                            w,
                            "  IfZ {} Goto {}",
                            pretty_value(v, &fun.ctx),
                            pretty_label(label)
                        );
                    }
                },
                tac::ControlOp::Return(v) => {
                    writeln!(w, "  Return {}", pretty_value(v, &fun.ctx)).unwrap()
                }
            },
        }
    }
}

pub fn pretty_value(v: &tac::Value, ctx: &tac::Context) -> String {
    match v {
        tac::Value::Const(tac::Const::Int(c)) => format!("{}", c),
        tac::Value::ID(id) => pretty_id(id, &ctx),
    }
}

pub fn pretty_id(id: &tac::ID, ctx: &tac::Context) -> String {
    match ctx.ident_by_id(*id) {
        Some(name) => format!("{}", name),
        None => format!("t{}", id),
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
