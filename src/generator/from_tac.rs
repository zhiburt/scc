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
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Arithmetic(tac::ArithmeticOp::Add),
            v1,
            v2,
        )) => {
            translator.add(
                parse_id(line.1.unwrap()),
                Type::Doubleword,
                parse_value(v1),
                parse_value(v2),
            );
        }
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
            _ => unimplemented!(),
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
