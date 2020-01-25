use std::collections::HashMap;

use crate::il::tac;
use super::syntax::{AsmInstruction, Place, Value, Register, IList};

pub fn gen(func: tac::FuncDef) -> IList {
    let mut translator = Translator::new();
    for instruction in func.instructions {
        translator.translate(instruction);
    }

    let mut asm = IList::new();
    asm.push(AsmInstruction::Metadata(format!(".globl {}", func.name)));
    asm.push(AsmInstruction::Label(func.name));
    
    asm.push(AsmInstruction::Push(Value::Place(Place::Register("rbp".to_owned()))));
    asm.push(AsmInstruction::Mov(Place::Register("rbp".to_owned()), Value::Place(Place::Register("rsp".to_owned()))));

    asm.extend(translator.instructions);
    asm.push(AsmInstruction::Pop(Place::Register("rbp".to_owned())));
    
    asm
}

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
                // TODO: I guess that if we implement some allocation algorithm
                // here we could save the often used variable to register or whatever,
                //
                // let p2 = self.alloc_const(v2);
                //
                // NOTION: Might it is not so well that we rid the initialization each variable in tac/3ac
                // since I am not sure that on the analyse stage it will be simple to handle the structures we have now
                //
                // It'd better for now to allocate all variables in a similar manner
                let p1 = self.alloc_const(v1);
                let p2 = self.alloc_const(v2);
                self.push_instruction(AsmInstruction::Add(p1.clone(), Value::Place(p2)));
                self.remember(line.1.unwrap(), Place::Register("eax".to_owned()));
            }
            tac::Instruction::Alloc(v) => {
                let p = self.alloc_const(v);
                self.remember(line.1.unwrap(), p);
            }
            tac::Instruction::Assignment(id, v) => {
                let p = self.alloc_const(v);
                self.remember(id, p);
            }
            tac::Instruction::ControlOp(op) => {
                match op {
                    tac::ControlOp::Return(v) => {
                        let value = match v {
                            tac::Value::ID(id) => Value::Place(self.look_up(&id).unwrap().clone()),
                            tac::Value::Const(tac::Const::Int(int)) => Value::Const(int as i64),
                        };
                        self.push_instruction(AsmInstruction::Mov(Place::Register("eax".to_owned()), value));
                        self.push_instruction(AsmInstruction::Ret);
                    }
                    _ => unimplemented!(),
                }
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
        let place = Place::Stack(self.stack_index);
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

enum RegisterState {
    Free,
    Used,
}

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
            AsmInstruction::Mov(Place::Stack(4), Value::Const(2)),
            AsmInstruction::Mov(Place::Stack(8), Value::Const(3)),
            AsmInstruction::Add(Place::Stack(4), Value::Place(Place::Stack(8))),
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
            AsmInstruction::Mov(Place::Stack(4), Value::Const(2)),
            AsmInstruction::Mov(Place::Stack(8), Value::Const(3)),
            AsmInstruction::Add(Place::Stack(4), Value::Place(Place::Stack(8))),
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
            AsmInstruction::Mov(Place::Stack(4), Value::Const(2)),
            AsmInstruction::Mov(Place::Stack(8), Value::Const(3)),
            AsmInstruction::Add(Place::Stack(4), Value::Place(Place::Stack(8))),
        ]);
        assert_eq!(expected, instructions);
    }

    #[test]
    fn translate_operation_sum_const_and_const_and_var2() {
        let mut translator = Translator::new();
        let var = tac::InstructionLine(
            tac::Instruction::Alloc(tac::Value::Const(tac::Const::Int(2))),
            Some(tac::ID::new(1, tac::IDType::Var)),
        );
        let first_sum = tac::InstructionLine(
            tac::Instruction::Op(tac::Op::Op(
                tac::TypeOp::Arithmetic(tac::ArithmeticOp::Add),
                tac::Value::ID(var.1.clone().unwrap()),
                tac::Value::Const(tac::Const::Int(3)),
            )),
            Some(tac::ID::new(0, tac::IDType::Temporary)),
        );
        let second_sum = tac::InstructionLine(
            tac::Instruction::Op(tac::Op::Op(
                tac::TypeOp::Arithmetic(tac::ArithmeticOp::Add),
                tac::Value::ID(first_sum.1.clone().unwrap()),
                tac::Value::Const(tac::Const::Int(4)),
            )),
            Some(tac::ID::new(1, tac::IDType::Temporary)),
        );

        translator.translate(var);
        translator.translate(first_sum);
        translator.translate(second_sum);
        let instructions = translator.instructions;

        let expected = IList::from(vec![
            AsmInstruction::Mov(Place::Stack(4), Value::Const(2)),
            AsmInstruction::Mov(Place::Stack(8), Value::Const(3)),
            AsmInstruction::Add(Place::Stack(4), Value::Place(Place::Stack(8))),
            AsmInstruction::Mov(Place::Stack(12), Value::Const(4)),
            AsmInstruction::Add(Place::Register("eax".to_owned()), Value::Place(Place::Stack(12))),
        ]);
        assert_eq!(expected, instructions);
    }

    #[test]
    fn translate_operation_sum_const_three_times() {
        let mut translator = Translator::new();
        let first_sum = tac::InstructionLine(
            tac::Instruction::Op(tac::Op::Op(
                tac::TypeOp::Arithmetic(tac::ArithmeticOp::Add),
                tac::Value::Const(tac::Const::Int(2)),
                tac::Value::Const(tac::Const::Int(3)),
            )),
            Some(tac::ID::new(0, tac::IDType::Temporary)),
        );
        let second_sum = tac::InstructionLine(
            tac::Instruction::Op(tac::Op::Op(
                tac::TypeOp::Arithmetic(tac::ArithmeticOp::Add),
                tac::Value::ID(first_sum.1.clone().unwrap()),
                tac::Value::Const(tac::Const::Int(4)),
            )),
            Some(tac::ID::new(1, tac::IDType::Temporary)),
        );

        translator.translate(first_sum);
        translator.translate(second_sum);
        let instructions = translator.instructions;

        let expected = IList::from(vec![
            AsmInstruction::Mov(Place::Stack(4), Value::Const(2)),
            AsmInstruction::Mov(Place::Stack(8), Value::Const(3)),
            AsmInstruction::Add(Place::Stack(4), Value::Place(Place::Stack(8))),
            AsmInstruction::Mov(Place::Stack(12), Value::Const(4)),
            AsmInstruction::Add(Place::Register("eax".to_owned()), Value::Place(Place::Stack(12))),
        ]);
        assert_eq!(expected, instructions);
    }

    #[test]
    fn translate_operation_sum_const_four_times() {
        let mut translator = Translator::new();
        let first_sum = tac::InstructionLine(
            tac::Instruction::Op(tac::Op::Op(
                tac::TypeOp::Arithmetic(tac::ArithmeticOp::Add),
                tac::Value::Const(tac::Const::Int(2)),
                tac::Value::Const(tac::Const::Int(3)),
            )),
            Some(tac::ID::new(0, tac::IDType::Temporary)),
        );
        let second_sum = tac::InstructionLine(
            tac::Instruction::Op(tac::Op::Op(
                tac::TypeOp::Arithmetic(tac::ArithmeticOp::Add),
                tac::Value::ID(first_sum.1.clone().unwrap()),
                tac::Value::Const(tac::Const::Int(4)),
            )),
            Some(tac::ID::new(1, tac::IDType::Temporary)),
        );
        let third_sum = tac::InstructionLine(
            tac::Instruction::Op(tac::Op::Op(
                tac::TypeOp::Arithmetic(tac::ArithmeticOp::Add),
                tac::Value::ID(second_sum.1.clone().unwrap()),
                tac::Value::Const(tac::Const::Int(5)),
            )),
            Some(tac::ID::new(1, tac::IDType::Temporary)),
        );

        translator.translate(first_sum);
        translator.translate(second_sum);
        translator.translate(third_sum);
        let instructions = translator.instructions;

        let expected = IList::from(vec![
            AsmInstruction::Mov(Place::Stack(4), Value::Const(2)),
            AsmInstruction::Mov(Place::Stack(8), Value::Const(3)),
            AsmInstruction::Add(Place::Stack(4), Value::Place(Place::Stack(8))),
            AsmInstruction::Mov(Place::Stack(12), Value::Const(4)),
            AsmInstruction::Add(Place::Register("eax".to_owned()), Value::Place(Place::Stack(12))),
            AsmInstruction::Mov(Place::Stack(16), Value::Const(5)),
            AsmInstruction::Add(Place::Register("eax".to_owned()), Value::Place(Place::Stack(16))),
        ]);
        assert_eq!(expected, instructions);
    }

    #[test]
    fn translate_operation_return() {
        let mut translator = Translator::new();
        let assign = tac::InstructionLine(
            tac::Instruction::Assignment(tac::ID::new(0, tac::IDType::Var), tac::Value::Const(tac::Const::Int(1))),
            Some(tac::ID::new(0, tac::IDType::Var)),
        );
        let ret = tac::InstructionLine(
            tac::Instruction::ControlOp(tac::ControlOp::Return(tac::Value::ID(assign.1.clone().unwrap()))),
            None,
        );

        translator.translate(assign);
        translator.translate(ret);
        let instructions = translator.instructions;

        let expected = IList::from(vec![
            AsmInstruction::Mov(Place::Stack(4), Value::Const(1)),
            AsmInstruction::Mov(Place::Register("eax".to_owned()), Value::Place(Place::Stack(4))),
            AsmInstruction::Ret,
        ]);
        assert_eq!(expected, instructions);
    }
}
