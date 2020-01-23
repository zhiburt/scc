use std::collections::HashMap;

use crate::il::tac;

pub struct Translator {
    vars: HashMap<u64, Place>,
    registers: Vec<RegisterWithState>,
    instructions: IList,
    stack_index: u64,
}

impl Translator {
    pub fn new() -> Self {
        Translator {
            vars: HashMap::new(),
            registers: Vec::new(),
            instructions: IList::new(),
            stack_index: 0,
        }
    }

    pub fn translate(&mut self, line: tac::InstructionLine) {
        match line.0 {
            tac::Instruction::Op(tac::Op::Op(
                tac::TypeOp::Arithmetic(tac::ArithmeticOp::Add),
                v1,
                v2,
            )) => {
                let p1 = self.alloc_const(v1);
                let p2 = self.alloc_const(v2);
                self.push_instruction(AsmInstruction::Add(p1, Value::Place(p2)));
            }
            tac::Instruction::Alloc(v) => {
                let p = self.alloc_const(v);
                self.remember(line.1.unwrap(), p);
            }
            tac::Instruction::Assignment(id, v) => {
                let p = self.alloc_const(v);
                self.remember(id, p);
            }
            tac::Instruction::ControlOp(tac::ControlOp::Return(v)) => {
                self.push_instruction(AsmInstruction::Ret);
            }
            _ => {
                println!("{:?}", line.0);
                unimplemented!()
            }
        }
    }

    fn push_instruction(&mut self, i: AsmInstruction) {
        self.instructions.push(i);
    }

    fn alloc_const(&mut self, v: tac::Value) -> Place {
        match v {
            tac::Value::Const(c) => {
                let place = self.place_on_stack();
                let v = match c {
                    tac::Const::Int(int) => Value::Const(int as i64),
                };
                self.push_instruction(AsmInstruction::Mov(place.clone(), v));
                place
            }
            tac::Value::ID(id) => {
                self.look_up(&id).unwrap().clone()
            }
        }
    }

    fn place_on_stack(&mut self) -> Place {
        self.stack_index += 4;
        let place = Place::on_stack(self.stack_index);
        place
    }

    fn look_up(&self, id: &tac::ID) -> Option<&Place> {
        self.vars.get(&(id.id as u64))
    }

    fn remember(&mut self, id: tac::ID, place: Place) {
        self.vars.insert(id.id as u64, place);
    }

    fn free_register(&mut self) -> Option<&Register> {
        let reg = self.registers.iter_mut().filter(|r| r.is_free()).next();
        match reg {
            Some(reg) => {
                reg.switch_state();
                Some(reg.register())
            }
            None => None,
        }
    }
}

#[derive(Debug)]
pub struct IList(Vec<AsmInstruction>);

impl IList {
    pub fn new() -> Self {
        IList(Vec::new())
    }
}

impl From<Vec<AsmInstruction>> for IList {
    fn from(l: Vec<AsmInstruction>) -> Self {
        IList(l)
    }
}

impl std::ops::Deref for IList {
    type Target = Vec<AsmInstruction>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for IList {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl PartialEq for IList {
    fn eq(&self, other: &Self) -> bool {
        let matching = self
            .iter()
            .zip(other.iter())
            .filter(|&(a, b)| a == b)
            .count();
        matching == self.len() && matching == other.len()
    }
}

#[derive(PartialEq, Eq, Debug)]
pub enum AsmInstruction {
    Metadata(String),
    Mov(Place, Value),
    Add(Place, Value),
    Ret,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Place {
    OnStack(u64),
    InRegister(Register),
}

impl Place {
    fn on_stack(offset: u64) -> Self {
        Place::OnStack(offset)
    }
}

#[derive(PartialEq, Eq, Debug)]
pub enum Value {
    Const(i64),
    Place(Place),
}

enum RegisterState {
    Free,
    Used,
}

type Register = String;

struct RegisterWithState(Register, RegisterState);

impl RegisterWithState {
    fn is_free(&self) -> bool {
        match self.1 {
            RegisterState::Free => true,
            _ => false,
        }
    }

    fn switch_state(&mut self) {
        match self.1 {
            RegisterState::Free => self.1 = RegisterState::Used,
            RegisterState::Used => self.1 = RegisterState::Free,
        }
    }

    fn register(&self) -> &Register {
        &self.0
    }
}

mod tests {
    use super::*;

    #[test]
    fn translate_operation_sum_2_and_3() {
        let mut translator = Translator::new();
        let line = tac::InstructionLine(
            tac::Instruction::Op(tac::Op::Op(
                tac::TypeOp::Arithmetic(tac::ArithmeticOp::Add),
                tac::Value::Const(tac::Const::Int(2)),
                tac::Value::Const(tac::Const::Int(3)),
            )),
            Some(tac::ID::new(0, tac::IDType::Temporary)),
        );

        translator.translate(line);
        let instructions = translator.instructions;

        let expected = IList::from(vec![
            AsmInstruction::Mov(Place::on_stack(4), Value::Const(2)),
            AsmInstruction::Mov(Place::on_stack(8), Value::Const(3)),
            AsmInstruction::Add(Place::on_stack(4), Value::Place(Place::on_stack(8))),
        ]);
        assert_eq!(expected, instructions);
    }

    #[test]
    fn translate_operation_sum_va1_and_var2() {
        let mut translator = Translator::new();
        let var1 = tac::InstructionLine(
            tac::Instruction::Alloc(tac::Value::Const(tac::Const::Int(2))),
            Some(tac::ID::new(0, tac::IDType::Var)),
        );
        let var2 = tac::InstructionLine(
            tac::Instruction::Alloc(tac::Value::Const(tac::Const::Int(3))),
            Some(tac::ID::new(1, tac::IDType::Var)),
        );
        let line = tac::InstructionLine(
            tac::Instruction::Op(tac::Op::Op(
                tac::TypeOp::Arithmetic(tac::ArithmeticOp::Add),
                tac::Value::ID(var1.1.clone().unwrap()),
                tac::Value::ID(var2.1.clone().unwrap()),
            )),
            Some(tac::ID::new(0, tac::IDType::Temporary)),
        );

        translator.translate(var1);
        translator.translate(var2);
        translator.translate(line);
        let instructions = translator.instructions;

        let expected = IList::from(vec![
            AsmInstruction::Mov(Place::on_stack(4), Value::Const(2)),
            AsmInstruction::Mov(Place::on_stack(8), Value::Const(3)),
            AsmInstruction::Add(Place::on_stack(4), Value::Place(Place::on_stack(8))),
        ]);
        assert_eq!(expected, instructions);
    }


    #[test]
    fn translate_operation_sum_const_and_var2() {
        let mut translator = Translator::new();
        let var = tac::InstructionLine(
            tac::Instruction::Alloc(tac::Value::Const(tac::Const::Int(2))),
            Some(tac::ID::new(1, tac::IDType::Var)),
        );
        let line = tac::InstructionLine(
            tac::Instruction::Op(tac::Op::Op(
                tac::TypeOp::Arithmetic(tac::ArithmeticOp::Add),
                tac::Value::ID(var.1.clone().unwrap()),
                tac::Value::Const(tac::Const::Int(3)),
            )),
            Some(tac::ID::new(0, tac::IDType::Temporary)),
        );

        translator.translate(var);
        translator.translate(line);
        let instructions = translator.instructions;

        let expected = IList::from(vec![
            AsmInstruction::Mov(Place::on_stack(4), Value::Const(2)),
            AsmInstruction::Mov(Place::on_stack(8), Value::Const(3)),
            AsmInstruction::Add(Place::on_stack(4), Value::Place(Place::on_stack(8))),
        ]);
        assert_eq!(expected, instructions);
    }
}
