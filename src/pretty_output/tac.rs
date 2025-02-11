use std::{io::Result, io::Write};

use scc::il::tac::{
    self, ArithmeticOp, BitwiseOp, Const, Context, EqualityOp, FuncDef, Instruction,
    InstructionLine, Label, RelationalOp, TypeOp, UnOp, Value, ID,
};

pub fn pretty<W>(mut w: W, fun: &FuncDef) -> Result<()>
where
    W: Write,
{
    writeln!(w, "{}:", pretty_fun_name(&fun.name))?;

    for id in &fun.parameters {
        let id = pretty_id(id, &fun.ctx);
        writeln!(w, "  param {}", id)?;
    }

    writeln!(w, "  BeginFunc {}", fun.frame_size)?;

    for InstructionLine(inst, id) in &fun.instructions {
        match inst {
            Instruction::Alloc(val) => {
                let id = pretty_id(id.as_ref().unwrap(), &fun.ctx);
                let value = pretty_value(val, &fun.ctx);
                writeln!(w, "  {}: {}", id, value,)?;
            }
            Instruction::Assignment(id1, v) => {
                let id = pretty_id(id1, &fun.ctx);
                let value = pretty_value(v, &fun.ctx);
                writeln!(w, "  {}: {}", id, value,)?;
            }
            Instruction::Call(call) => {
                for param in call.params.iter() {
                    let value = pretty_value(param, &fun.ctx);
                    writeln!(w, "  PushParam {}", value)?;
                }

                let id = pretty_id(id.as_ref().unwrap(), &fun.ctx);
                let func_name = pretty_fun_name(&call.name);
                writeln!(w, "  {}: LCall {}", id, func_name)?;
                writeln!(w, "  PopParams {}", call.pop_size)?;
            }
            Instruction::Op(op) => {
                match op {
                    tac::Op::Op(t, v1, v2) => {
                        let id = pretty_id(id.as_ref().unwrap(), &fun.ctx);
                        let op = pretty_type(t);
                        let val1 = pretty_value(v1, &fun.ctx);
                        let val2 = pretty_value(v2, &fun.ctx);

                        writeln!(w, "  {}: {} {} {}", id, val1, op, val2)?;
                    }
                    tac::Op::Unary(op, v1) => {
                        let id = pretty_id(id.as_ref().unwrap(), &fun.ctx);
                        let op = pretty_unary_op(op);
                        let val = pretty_value(v1, &fun.ctx);
                        writeln!(w, "  {}: {} {}", id, op, val)?;
                    }
                };
            }
            Instruction::ControlOp(cop) => match cop {
                tac::ControlOp::Label(label) => {
                    let label = pretty_label(label);
                    writeln!(w, "{}:", label)?;
                }
                tac::ControlOp::Branch(lb) => match lb {
                    tac::Branch::GOTO(label) => {
                        let label = pretty_label(label);
                        writeln!(w, "  Goto {}", label)?;
                    }
                    tac::Branch::IfGOTO(v, label) => {
                        let value = pretty_value(v, &fun.ctx);
                        let label = pretty_label(label);
                        writeln!(w, "  IfZ {} Goto {}", value, label,)?;
                    }
                },
                tac::ControlOp::Return(v) => {
                    let value = pretty_value(v, &fun.ctx);
                    writeln!(w, "  Return {}", value)?;
                }
            },
        }
    }

    Ok(())
}

pub fn pretty_value(v: &Value, ctx: &Context) -> String {
    match v {
        Value::Const(Const::Int(c)) => format!("{}", c),
        Value::ID(id) => pretty_id(id, &ctx),
    }
}

pub fn pretty_id(id: &ID, ctx: &Context) -> String {
    match ctx.ident_by_id(*id) {
        Some(name) => format!("{}", name),
        None => format!("t{}", id),
    }
}

pub fn pretty_label(label: &Label) -> String {
    format!("_L{}", label)
}

pub fn pretty_fun_name(name: &str) -> String {
    if name == "main" {
        name.to_string()
    } else {
        format!("_{}", name)
    }
}

pub fn pretty_type(op: &TypeOp) -> String {
    match op {
        TypeOp::Arithmetic(op) => pretty_arith_op(op),
        TypeOp::Relational(op) => pretty_rel_op(op),
        TypeOp::Equality(op) => pretty_eq_op(op),
        TypeOp::Bit(op) => pretty_bit_op(op),
    }
}

pub fn pretty_arith_op(op: &ArithmeticOp) -> String {
    match op {
        ArithmeticOp::Add => "+".to_string(),
        ArithmeticOp::Sub => "-".to_string(),
        ArithmeticOp::Mul => "*".to_string(),
        ArithmeticOp::Div => "/".to_string(),
        ArithmeticOp::Mod => "%".to_string(),
    }
}

pub fn pretty_rel_op(op: &RelationalOp) -> String {
    match op {
        RelationalOp::Less => "<".to_string(),
        RelationalOp::LessOrEq => "<=".to_string(),
        RelationalOp::Greater => ">".to_string(),
        RelationalOp::GreaterOrEq => ">=".to_string(),
    }
}

pub fn pretty_eq_op(op: &EqualityOp) -> String {
    match op {
        EqualityOp::Equal => "==".to_string(),
        EqualityOp::NotEq => "!=".to_string(),
    }
}

pub fn pretty_bit_op(op: &BitwiseOp) -> String {
    match op {
        BitwiseOp::And => "&".to_string(),
        BitwiseOp::Or => "|".to_string(),
        BitwiseOp::Xor => "^".to_string(),
        BitwiseOp::LShift => "<<".to_string(),
        BitwiseOp::RShift => ">>".to_string(),
    }
}

pub fn pretty_unary_op(op: &UnOp) -> String {
    match op {
        UnOp::Neg => "-".to_string(),
        UnOp::LogicNeg => "!".to_string(),
        UnOp::BitComplement => "~".to_string(),
    }
}
