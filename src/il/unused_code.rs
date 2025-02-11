use super::tac::{self, Instruction, InstructionLine, Op, Value, ID};
use std::collections::HashSet;

pub fn remove_unused(mut func: tac::FuncDef) -> tac::FuncDef {
    let mut u: HashSet<ID> = HashSet::new();
    for index in (0..func.instructions.len()).rev() {
        let InstructionLine(i, id) = &func.instructions[index];
        if let Some(id) = id {
            // we check if the id isn't a variable because
            // it's possible that it's used in loop as a counter.
            //
            // the check of function is caused by a design of tac function.
            // it stores the result in an tmp id even thought it's unused.
            if !u.contains(id)
                && !func.ctx.is_variable(*id)
                && !matches!(i, tac::Instruction::Call(..))
            {
                func.instructions.remove(index);
            }
        }

        let InstructionLine(i, ..) = &func.instructions[index];
        used(i).iter().for_each(|id| {
            u.insert(*id);
        });
    }

    func
}

fn used(i: &Instruction) -> Vec<ID> {
    let mut ids = Vec::new();
    match i {
        Instruction::Assignment(.., v) => {
            v.as_id().map(|id| ids.push(*id));
        }
        Instruction::Op(Op::Op(_, v1, v2)) => {
            v1.as_id().map(|id| ids.push(*id));
            v2.as_id().map(|id| ids.push(*id));
        }
        Instruction::Op(Op::Unary(_, v)) => {
            v.as_id().map(|id| ids.push(*id));
        }
        Instruction::ControlOp(tac::ControlOp::Return(Value::ID(id))) => ids.push(*id),
        Instruction::ControlOp(tac::ControlOp::Branch(tac::Branch::IfGOTO(Value::ID(id), ..))) => {
            ids.push(*id)
        }
        Instruction::Call(tac::Call { params, .. }) => params
            .iter()
            .filter_map(|v| match v {
                Value::ID(id) => Some(*id),
                _ => None,
            })
            .for_each(|id| ids.push(id)),
        Instruction::Alloc(..)
        | Instruction::ControlOp(tac::ControlOp::Label(..))
        | Instruction::ControlOp(tac::ControlOp::Return(Value::Const(..)))
        | Instruction::ControlOp(tac::ControlOp::Branch(tac::Branch::IfGOTO(
            Value::Const(..),
            ..,
        )))
        | Instruction::ControlOp(tac::ControlOp::Branch(tac::Branch::GOTO(..))) => (),
    }

    ids
}
