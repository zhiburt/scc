use std::collections::HashMap;
use std::io::Write;

use simple_c_compiler::il::tac;

pub fn pretty<W: Write>(mut w: W, fun: &tac::FuncDef) {
    let mut vars = VarView{ tmp_vars: HashMap::new(), vars: &fun.vars, tpm_counter: 0 };

    writeln!(w, "{}:", pretty_fun_name(&fun.name));
    writeln!(w, "  BeginFunc {}", fun.frame_size);

    for tac::InstructionLine(inst, id) in &fun.instructions {
        match inst {
            tac::Instruction::Alloc(val) => {
                writeln!(
                    w,
                    "  {}: {}",
                    vars.pretty_id(id.as_ref().unwrap()),
                    vars.pretty_val(val),
                )
                .unwrap();
            }
            tac::Instruction::Assignment(id1, v) => {
                writeln!(
                    w,
                    "  {}: {}",
                    vars.pretty_id(id1),
                    vars.pretty_val(v),
                );
            }
            tac::Instruction::Call(call) => {
                for p in call.params.iter() {
                    writeln!(w, "  PushParam {}", vars.pretty_val(p));
                }

                writeln!(
                    w,
                    "  {}: LCall {}",
                    vars.pretty_id(id.as_ref().unwrap()),
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
                            vars.pretty_id(id.as_ref().unwrap()),
                            vars.pretty_val(v1),
                            pretty_type(t),
                            vars.pretty_val(v2)
                        );
                    }
                    tac::Op::Unary(op, v1) => {
                        writeln!(
                            w,
                            "  {}: {} {}",
                            vars.pretty_id(id.as_ref().unwrap()),
                            pretty_unary_op(op),
                            vars.pretty_val(v1),
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
                    tac::Branch::IfGOTO(val, label) => {
                        writeln!(
                            w,
                            "  IfZ {} Goto {}",
                            vars.pretty_val(val),
                            pretty_label(label)
                        );
                    }
                },
                tac::ControlOp::Return(value) => {
                    writeln!(w, "  Return {}", vars.pretty_val(value)).unwrap()
                }
            },
        }
    }
}

struct VarView<'a> {
    vars: &'a HashMap<usize, String>,
    tmp_vars: HashMap<usize, usize>,
    tpm_counter: usize,
}

impl<'a> VarView<'a> {
    fn pretty_id(&mut self, id: &tac::ID) -> String {
        match id.tp {
            tac::IDType::Var => format!("{}", self.vars[&id.id]),
            tac::IDType::Temporary => {
                let tmp_id = if self.tmp_vars.contains_key(&id.id) {
                    self.tmp_vars[&id.id]
                } else {
                    let tmp_id = self.tpm_counter;
                    self.tmp_vars.insert(id.id, tmp_id);
                    self.tpm_counter += 1;
                    tmp_id
                };
                format!("_t{}", tmp_id)
            },
        }
    }
    
    fn pretty_val(&mut self, v: &tac::Value) -> String {
        match v {
            tac::Value::ID(id) => format!("{}", self.pretty_id(id)),
            tac::Value::Const(tac::Const::Int(val)) => format!("{}", val),
        }
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
