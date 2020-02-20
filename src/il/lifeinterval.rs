use super::tac::{Branch, Call, ControlOp, Instruction, InstructionLine, Op, Value, ID};
use std::collections::BTreeMap;

/// Range represents an life range of id(variable).
#[derive(Debug)]
pub struct Range {
    /// start is a first index where a id showed up
    start: usize,
    /// end is a last index where a id showed up
    /// 
    /// It's considered that on this index
    /// a variable can be safely utilized.
    end: usize,
}

impl Range {
    /// length returns range's lifetime
    pub fn length(&self) -> usize {
        self.end - self.start
    }
}

pub fn life_intervals(instructions: &[InstructionLine]) -> BTreeMap<usize, Range> {
    let mut intervals = BTreeMap::new();
    for (index, InstructionLine(i, id)) in instructions.iter().enumerate().rev() {
        match id {
            Some(id) if id.is_var() => {
                // end is equal index + 1
                // to show that it's should live more then index
                // but on index + 1 it can be utilized as well
                //
                // Otherwise we could not recognize difference
                // between both these expressions
                //
                // `a = b + c` where let's imagine it's a place of `b`'s last absence 
                // and `d = e + f` where let's imagine all of these live after this expression
                intervals
                    .entry(id.id)
                    .and_modify(|e: &mut Range| e.start = index)
                    .or_insert(Range {
                        start: index,
                        end: index + 1,
                    });
            }
            _ => (),
        }

        for id in instruction_ids(i) {
            if !id.is_var() {
                continue;
            }
            intervals
                .entry(id.id)
                .and_modify(|e: &mut Range| e.start = index)
                .or_insert(Range {
                    start: index,
                    end: index,
                });
        }
    }

    intervals
}

fn instruction_ids(i: &Instruction) -> Vec<ID> {
    let mut ids = Vec::new();
    for v in instruction_values(i) {
        if v.is_id() {
            ids.push(v.as_id().unwrap().clone())
        }
    }

    ids
}

fn instruction_values(i: &Instruction) -> Vec<&Value> {
    let mut values = Vec::new();
    match i {
        Instruction::Alloc(v) => values.push(v),
        Instruction::Assignment(.., v) => values.push(v),
        Instruction::Op(Op::Unary(.., v)) => values.push(v),
        Instruction::Op(Op::Op(.., v1, v2)) => {
            values.push(v1);
            values.push(v2);
        }
        Instruction::Call(Call { params, .. }) => {
            for v in params.iter() {
                values.push(v);
            }
        }
        Instruction::ControlOp(op) => match op {
            ControlOp::Branch(Branch::IfGOTO(v, ..)) => values.push(v),
            ControlOp::Return(v) => values.push(v),
            _ => (),
        },
    };

    values
}
