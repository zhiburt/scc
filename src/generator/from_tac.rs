use std::collections::HashMap;

use super::translator::{self, Id, Translator, Type, Value};
use crate::il::tac;

pub struct Transit<T: Translator> {
    translator: T,
}

impl<T: Translator> Transit<T> {
    pub fn new(translator: T) -> Self {
        Transit { translator }
    }

    pub fn gen(&mut self, func: tac::FuncDef) -> String {
        self.translator.func_begin(&func.name);

        for instruction in func.instructions {
            translate(&mut self.translator, instruction);
        }

        self.translator.func_end();

        self.translator.stash()
    }
}

fn translate(translator: &mut impl Translator, line: tac::InstructionLine) {
    match line.0 {
        tac::Instruction::Op(op) => match op {
            tac::Op::Op(op, v1, v2) => match op {
                tac::TypeOp::Arithmetic(op) => match op {
                    tac::ArithmeticOp::Add => translator.add(
                        parse_id(line.1.unwrap()),
                        Type::Doubleword,
                        parse_value(v1),
                        parse_value(v2),
                    ),
                    tac::ArithmeticOp::Sub => translator.sub(
                        parse_id(line.1.unwrap()),
                        Type::Doubleword,
                        parse_value(v1),
                        parse_value(v2),
                    ),
                    tac::ArithmeticOp::Mul => translator.mul(
                        parse_id(line.1.unwrap()),
                        Type::Doubleword,
                        parse_value(v1),
                        parse_value(v2),
                    ),
                    tac::ArithmeticOp::Div => translator.div(
                        parse_id(line.1.unwrap()),
                        Type::Doubleword,
                        parse_value(v1),
                        parse_value(v2),
                    ),
                    tac::ArithmeticOp::Mod => translator.div_reminder(
                        parse_id(line.1.unwrap()),
                        Type::Doubleword,
                        parse_value(v1),
                        parse_value(v2),
                    ),
                },
                tac::TypeOp::Bit(op) => match op {
                    tac::BitwiseOp::And => translator.bit_and(
                        parse_id(line.1.unwrap()),
                        Type::Doubleword,
                        parse_value(v1),
                        parse_value(v2),
                    ),
                    tac::BitwiseOp::Or => translator.bit_or(
                        parse_id(line.1.unwrap()),
                        Type::Doubleword,
                        parse_value(v1),
                        parse_value(v2),
                    ),
                    tac::BitwiseOp::Xor => translator.bit_xor(
                        parse_id(line.1.unwrap()),
                        Type::Doubleword,
                        parse_value(v1),
                        parse_value(v2),
                    ),
                    _ => unimplemented!(),
                },
                tac::TypeOp::Equality(op) => match op {
                    tac::EqualityOp::Equal => translator.eq(
                        parse_id(line.1.unwrap()),
                        Type::Doubleword,
                        parse_value(v1),
                        parse_value(v2),
                    ),
                    tac::EqualityOp::NotEq => translator.not_eq(
                        parse_id(line.1.unwrap()),
                        Type::Doubleword,
                        parse_value(v1),
                        parse_value(v2),
                    ),
                },
                tac::TypeOp::Relational(op) => match op {
                    tac::RelationalOp::Greater => translator.gt(
                        parse_id(line.1.unwrap()),
                        Type::Doubleword,
                        parse_value(v1),
                        parse_value(v2),
                    ),
                    tac::RelationalOp::GreaterOrEq => translator.ge(
                        parse_id(line.1.unwrap()),
                        Type::Doubleword,
                        parse_value(v1),
                        parse_value(v2),
                    ),
                    tac::RelationalOp::Less => translator.lt(
                        parse_id(line.1.unwrap()),
                        Type::Doubleword,
                        parse_value(v1),
                        parse_value(v2),
                    ),
                    tac::RelationalOp::LessOrEq => translator.le(
                        parse_id(line.1.unwrap()),
                        Type::Doubleword,
                        parse_value(v1),
                        parse_value(v2),
                    ),
                }
            },
            tac::Op::Unary(op, value) => {
                // assembly does not support the unary operators with constants
                // so currently we unwrap it.
                // Might it will be dismantle in IL pre-processing part(optimizations)
                //
                // TODO: including corresponding observations, it makes sense to support comments in IL
                // to translate them into asm.
                // It can be comments provided by types not only by strings
                // such as `Comment::SimplifiedExp(exp)`
                match value {
                    // Even though it's not very well to implement it here
                    // but while we still don't have an optimization stage
                    // it's been implemented to be better testable.
                    tac::Value::Const(tac::Const::Int(int)) => {
                        let value = match op {
                            tac::UnOp::Neg => -int as i64,
                            tac::UnOp::LogicNeg => (int == 0) as i64,
                            tac::UnOp::BitComplement => !int as i64,
                        };

                        translator.save(parse_id(line.1.unwrap()), Type::Doubleword, Some(Value::Const(value)));
                    },
                    tac::Value::ID(id) => match op {
                        tac::UnOp::Neg => translator.neg(
                            parse_id(line.1.unwrap()),
                            id.id as u32,
                        ),
                        tac::UnOp::LogicNeg => translator.logical_neg(
                                parse_id(line.1.unwrap()),
                                id.id as u32 ,
                        ),
                        tac::UnOp::BitComplement => translator.bitwise(
                            parse_id(line.1.unwrap()),
                            id.id as u32,
                        ),
                    },

                }
            }
        },
        tac::Instruction::Alloc(v) => {
            translator.save(
                parse_id(line.1.unwrap()),
                Type::Doubleword,
                Some(parse_value(v)),
            );
        }
        tac::Instruction::Assignment(id, v) => {
            translator.save(
                parse_id(line.1.unwrap()),
                Type::Doubleword,
                Some(parse_value(v)),
            );
        }
        tac::Instruction::ControlOp(op) => match op {
            tac::ControlOp::Return(v) => translator.ret(Type::Doubleword, parse_value(v)),
            tac::ControlOp::Label(index) => translator.label(index),
            tac::ControlOp::Branch(branch) => match branch {
                tac::Branch::GOTO(label) => {
                    translator.jump(label);
                }
                tac::Branch::IfGOTO(v, label) => {
                    translator.if_goto(Type::Doubleword, parse_value(v), label);
                }
            },
        },
        _ => {
            println!("{:?}", line.0);
            unimplemented!()
        }
    }
}

fn parse_id(id: tac::ID) -> translator::Id {
    id.id as translator::Id
}

fn parse_value(v: tac::Value) -> translator::Value {
    match v {
        tac::Value::Const(tac::Const::Int(int)) => translator::Value::Const(int as i64),
        tac::Value::ID(id) => translator::Value::Ref(parse_id(id)),
    }
}
