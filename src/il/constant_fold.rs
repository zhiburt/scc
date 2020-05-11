use super::tac::{self, Const, Instruction, InstructionLine, Op, TypeOp, UnOp, Value, ID};
use std::collections::HashMap;

pub fn fold(instructions: &mut [InstructionLine]) {
    let mut constants = HashMap::new();
    for InstructionLine(i, id) in instructions {
        op_fold(&constants, i);
        if let Instruction::Alloc(Value::Const(Const::Int(c))) = i {
            constants.insert(id.unwrap(), *c);
        }
    }
}

fn op_fold(constants: &HashMap<ID, i32>, i: &mut Instruction) {
    use tac::{ArithmeticOp::*, BitwiseOp::*, EqualityOp::*, RelationalOp::*, TypeOp::*};

    let checks: Vec<(TypeOp, Box<dyn FnOnce(i32, i32) -> i32>)> = vec![
        (Arithmetic(Add), Box::new(|lhs: i32, rhs: i32| lhs + rhs)),
        (Arithmetic(Sub), Box::new(|lhs: i32, rhs: i32| lhs - rhs)),
        (Arithmetic(Mul), Box::new(|lhs: i32, rhs: i32| lhs * rhs)),
        (Arithmetic(Mod), Box::new(|lhs: i32, rhs: i32| lhs % rhs)),
        (Arithmetic(Div), Box::new(|lhs: i32, rhs: i32| lhs / rhs)),
        (Bit(And), Box::new(|lhs: i32, rhs: i32| lhs & rhs)),
        (Bit(Or), Box::new(|lhs: i32, rhs: i32| lhs | rhs)),
        (Bit(Xor), Box::new(|lhs: i32, rhs: i32| lhs ^ rhs)),
        (Bit(LShift), Box::new(|lhs: i32, rhs: i32| lhs << rhs)),
        (Bit(RShift), Box::new(|lhs: i32, rhs: i32| lhs >> rhs)),
        (
            Equality(Equal),
            Box::new(|lhs: i32, rhs: i32| (lhs == rhs) as i32),
        ),
        (
            Equality(NotEq),
            Box::new(|lhs: i32, rhs: i32| (lhs != rhs) as i32),
        ),
        (
            Relational(Greater),
            Box::new(|lhs: i32, rhs: i32| (lhs > rhs) as i32),
        ),
        (
            Relational(GreaterOrEq),
            Box::new(|lhs: i32, rhs: i32| (lhs >= rhs) as i32),
        ),
        (
            Relational(Less),
            Box::new(|lhs: i32, rhs: i32| (lhs < rhs) as i32),
        ),
        (
            Relational(LessOrEq),
            Box::new(|lhs: i32, rhs: i32| (lhs <= rhs) as i32),
        ),
    ];

    for check in checks {
        if let Some(v) = check_bin_op(constants, i, check.0, check.1) {
            *i = Instruction::Alloc(Value::Const(Const::Int(v)));
            return;
        }
    }

    use UnOp::*;
    let checks: Vec<(UnOp, Box<dyn FnOnce(i32) -> i32>)> = vec![
        (Neg, Box::new(|v: i32| -v)),
        (BitComplement, Box::new(|v: i32| !v)),
        (LogicNeg, Box::new(|v: i32| i32::from(v == 0))),
    ];

    for check in checks {
        if let Some(v) = check_un_op(constants, i, check.0, check.1) {
            *i = Instruction::Alloc(Value::Const(Const::Int(v)));
            return;
        }
    }
}

fn check_bin_op<F: FnOnce(i32, i32) -> i32>(
    constants: &HashMap<ID, i32>,
    i: &Instruction,
    exp_op: TypeOp,
    does: F,
) -> Option<i32> {
    match i {
        Instruction::Op(Op::Op(op, lhs, rhs)) if op == &exp_op => {
            let lhs = value_to_const(constants, lhs);
            let rhs = value_to_const(constants, rhs);

            if lhs.is_none() || rhs.is_none() {
                return None;
            }

            Some(does(lhs.unwrap(), rhs.unwrap()))
        }
        _ => None,
    }
}

fn check_un_op<F: FnOnce(i32) -> i32>(
    constants: &HashMap<ID, i32>,
    i: &Instruction,
    exp_op: UnOp,
    does: F,
) -> Option<i32> {
    match i {
        Instruction::Op(Op::Unary(op, v)) if op == &exp_op => {
            let v = value_to_const(constants, v);

            if v.is_none() {
                return None;
            }

            Some(does(v.unwrap()))
        }
        _ => None,
    }
}

fn value_to_const(constants: &HashMap<ID, i32>, v: &Value) -> Option<i32> {
    match v {
        Value::Const(Const::Int(v)) => Some(*v),
        Value::ID(id) => constants.get(&id).cloned(),
    }
}
