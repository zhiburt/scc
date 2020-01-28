use std::collections::HashMap;

use super::syntax::{self, AsmInstruction, IList, Params, Place, Register, Value};
use crate::il::tac;

pub fn gen(func: tac::FuncDef) -> IList {
    let mut translator = Translator::new();
    for instruction in func.instructions {
        translator.translate(instruction);
    }

    let mut asm = IList::new();
    asm.push(AsmInstruction::Metadata(format!(".globl {}", func.name)));
    asm.push(AsmInstruction::Label(func.name));

    asm.push(AsmInstruction::Push(Value::Place(Place::Register(
        syntax::X64Memory::address_previous_frame(),
    ))));
    asm.push(AsmInstruction::Mov(
        Params::new(
            Place::Register(syntax::X64Memory::address_previous_frame()),
            Value::Place(Place::Register(syntax::X64Memory::address_current_frame())),
        )
        .unwrap(),
    ));

    asm.extend(translator.instructions);
    asm.push(AsmInstruction::Pop(Place::Register(
        syntax::X64Memory::address_previous_frame(),
    )));
    asm.push(AsmInstruction::Ret);
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
                tac::TypeOp::Arithmetic(operation_type),
                v1,
                v2,
            )) => {
                // We could return a place of this id
                // but as we decided to follow the style of clang and gcc compilations
                // we move the operand to add register before.
                //
                // Instead of `addl    -8(%rbp), $2` we make two instructions
                // `movl    -8(%rbp), %eax`
                // `addl    $2, %eax`
                //
                // In detail earlier we got this result for this expression `a + 1` where `a` is declared variable
                //
                // AsmInstruction::Mov(Params::new(Place::Stack(4, syntax::Type::Doubleword), Value::Const(2, syntax::Type::Doubleword)).unwrap()),
                // AsmInstruction::Add(Params::new(Place::Stack(4, syntax::Type::Doubleword), Value::Const(3, syntax::Type::Doubleword)).unwrap()),
                // 
                // But now we get this
                // 
                // AsmInstruction::Mov(Params::new(Place::Stack(4, syntax::Type::Doubleword), Value::Const(2, syntax::Type::Doubleword)).unwrap()),
                // AsmInstruction::Mov(Params::new(Place::Register("eax"), Value::Place(Place::Stack(4, syntax::Type::Doubleword))).unwrap()),
                // AsmInstruction::Add(Params::new(Place::Register("eax"), Value::Const(3, syntax::Type::Doubleword)).unwrap()),
                //
                // Certainly, I can not say which variate is better.
                let p2 = self.to_value(v2);
                let result_place = Place::Register(syntax::X64Memory::result_of(syntax::OpType::Add, &p2));
                let p1 = self.alloc_on(v1, result_place.clone());
                let params = Params::new(p1.clone(), p2.clone()).unwrap_or_else(|| {
                    self.push_instruction(AsmInstruction::Mov(
                        Params::new(result_place.clone(), Value::Place(p1)).unwrap(),
                    ));
                    Params::new(p2.as_place().unwrap(), Value::Place(result_place.clone())).unwrap()
                });
                let instruction = map_arithmetic_op_to_asm(operation_type, params);
                self.remember(
                    line.1.unwrap(),
                    result_place,
                );
                self.push_instruction(instruction);
            }
            tac::Instruction::Alloc(v) => {
                let p = self.alloc_value(v);
                self.remember(line.1.unwrap(), p);
            }
            tac::Instruction::Assignment(id, v) => {
                let p = self.alloc_value(v);
                self.remember(id, p);
            }
            tac::Instruction::ControlOp(op) => {
                match op {
                    tac::ControlOp::Return(v) => {
                        let value = match v {
                            tac::Value::ID(id) => Value::Place(self.look_up(&id).unwrap().clone()),
                            tac::Value::Const(tac::Const::Int(int)) => Value::Const(int as i64, syntax::Type::Doubleword),
                        };
                        let ret_register = syntax::X64Memory::return_register(&value);
                        // we ignore move to return register when its already there
                        match value {
                            Value::Place(Place::Stack(..)) | Value::Const(..) => {
                                self.push_instruction(AsmInstruction::Mov(
                                    Params::new(Place::Register(ret_register), value).unwrap(),
                                ));
                            }
                            Value::Place(Place::Register(reg)) if ret_register != reg => {
                                // TODO: we not always need to move data to return register
                                // for example when we operate with int
                                // we can move it to `eax`
                                self.push_instruction(AsmInstruction::Mov(
                                    Params::new(Place::Register(ret_register), value).unwrap(),
                                ));
                                // TODO: here we should handle multiply returns by label
                            }
                            _ => (),
                        }
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

    fn to_value(&self, v: tac::Value) -> Value {
        match v {
            tac::Value::Const(tac::Const::Int(int)) => Value::Const(int as i64, syntax::Type::Doubleword),
            tac::Value::ID(id) => Value::Place(self.look_up(&id).unwrap().clone()),
        }
    }

    fn alloc_value(&mut self, v: tac::Value) -> Place {
        let place = self.place_on_stack();
        self.alloc_on(v, place)
    }

    fn alloc_on(&mut self, v: tac::Value, place: Place) -> Place {
        let v = self.to_value(v);
        match v {
            Value::Place(ref p) if *p == place => place,
            _ => {
                self.push_instruction(AsmInstruction::Mov(Params::new(place.clone(), v).unwrap()));
                place
            }
        }
    }

    fn place_on_stack(&mut self) -> Place {
        self.stack_index += 4;
        let place = Place::Stack(self.stack_index, syntax::Type::Doubleword);
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

fn map_arithmetic_op_to_asm(op: tac::ArithmeticOp, params: Params) -> AsmInstruction {
    match op {
        tac::ArithmeticOp::Add => AsmInstruction::Add(params),
        tac::ArithmeticOp::Sub => AsmInstruction::Sub(params),
        tac::ArithmeticOp::Mul => AsmInstruction::Mul(params),
        tac::ArithmeticOp::Div => AsmInstruction::Div(params),
        tac::ArithmeticOp::Mod => AsmInstruction::Mod(params),
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
    fn translate_operation_sum_const_and_const() {
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
            AsmInstruction::Mov(Params::new(Place::Register("eax"), Value::Const(2, syntax::Type::Doubleword)).unwrap()),
            AsmInstruction::Add(Params::new(Place::Register("eax"), Value::Const(3, syntax::Type::Doubleword)).unwrap()),
        ]);
        assert_eq!(expected, instructions);
    }

    #[test]
    fn translate_operation_sum_var_and_var() {
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
            AsmInstruction::Mov(Params::new(Place::Stack(4, syntax::Type::Doubleword), Value::Const(2, syntax::Type::Doubleword)).unwrap()),
            AsmInstruction::Mov(Params::new(Place::Stack(8, syntax::Type::Doubleword), Value::Const(3, syntax::Type::Doubleword)).unwrap()),
            AsmInstruction::Mov(Params::new(Place::Register("eax"), Value::Place(Place::Stack(4, syntax::Type::Doubleword))).unwrap()),
            AsmInstruction::Add(Params::new(Place::Register("eax"), Value::Place(Place::Stack(8, syntax::Type::Doubleword))).unwrap()),
        ]);
        assert_eq!(expected, instructions);
    }

    #[test]
    fn translate_operation_sum_var_and_const() {
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
            AsmInstruction::Mov(Params::new(Place::Stack(4, syntax::Type::Doubleword), Value::Const(2, syntax::Type::Doubleword)).unwrap()),
            AsmInstruction::Mov(Params::new(Place::Register("eax"), Value::Place(Place::Stack(4, syntax::Type::Doubleword))).unwrap()),
            AsmInstruction::Add(Params::new(Place::Register("eax"), Value::Const(3, syntax::Type::Doubleword)).unwrap()),
        ]);
        assert_eq!(expected, instructions);
    }

    #[test]
    fn translate_operation_sum_var_and_const_and_const() {
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
            AsmInstruction::Mov(Params::new(Place::Stack(4, syntax::Type::Doubleword), Value::Const(2, syntax::Type::Doubleword)).unwrap()),
            AsmInstruction::Mov(Params::new(Place::Register("eax"), Value::Place(Place::Stack(4, syntax::Type::Doubleword))).unwrap()),
            AsmInstruction::Add(Params::new(Place::Register("eax"), Value::Const(3, syntax::Type::Doubleword)).unwrap()),
            AsmInstruction::Add(Params::new(Place::Register("eax"), Value::Const(4, syntax::Type::Doubleword)).unwrap()),
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
            AsmInstruction::Mov(Params::new(Place::Register("eax"), Value::Const(2, syntax::Type::Doubleword)).unwrap()),
            AsmInstruction::Add(Params::new(Place::Register("eax"), Value::Const(3, syntax::Type::Doubleword)).unwrap()),
            AsmInstruction::Add(Params::new(Place::Register("eax"), Value::Const(4, syntax::Type::Doubleword)).unwrap()),
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
            AsmInstruction::Mov(Params::new(Place::Register("eax"), Value::Const(2, syntax::Type::Doubleword)).unwrap()),
            AsmInstruction::Add(Params::new(Place::Register("eax"), Value::Const(3, syntax::Type::Doubleword)).unwrap()),
            AsmInstruction::Add(Params::new(Place::Register("eax"), Value::Const(4, syntax::Type::Doubleword)).unwrap()),
            AsmInstruction::Add(Params::new(Place::Register("eax"), Value::Const(5, syntax::Type::Doubleword)).unwrap()),
        ]);
        assert_eq!(expected, instructions);
    }

    #[test]
    fn translate_operation_return() {
        let mut translator = Translator::new();
        let assign = tac::InstructionLine(
            tac::Instruction::Assignment(
                tac::ID::new(0, tac::IDType::Var),
                tac::Value::Const(tac::Const::Int(1)),
            ),
            Some(tac::ID::new(0, tac::IDType::Var)),
        );
        let ret = tac::InstructionLine(
            tac::Instruction::ControlOp(tac::ControlOp::Return(tac::Value::ID(
                assign.1.clone().unwrap(),
            ))),
            None,
        );

        translator.translate(assign);
        translator.translate(ret);
        let instructions = translator.instructions;

        let expected = IList::from(vec![
            AsmInstruction::Mov(Params::new(Place::Stack(4, syntax::Type::Doubleword), Value::Const(1, syntax::Type::Doubleword)).unwrap()),
            AsmInstruction::Mov(
                Params::new(Place::Register("eax"), Value::Place(Place::Stack(4, syntax::Type::Doubleword))).unwrap(),
            ),
        ]);
        assert_eq!(expected, instructions);
    }

    #[test]
    fn translate_operation_return_const() {
        let mut translator = Translator::new();
        let ret = tac::InstructionLine(
            tac::Instruction::ControlOp(tac::ControlOp::Return(tac::Value::Const(tac::Const::Int(1)))),
            None,
        );

        translator.translate(ret);
        let instructions = translator.instructions;

        let expected = IList::from(vec![
            AsmInstruction::Mov(
                Params::new(Place::Register("eax"), Value::Const(1, syntax::Type::Doubleword)).unwrap(),
            ),
        ]);
        assert_eq!(expected, instructions);
    }

    #[test]
    fn translate_operation_assign_var_sum_var_and_const() {
        let mut translator = Translator::new();
        let var = tac::InstructionLine(
            tac::Instruction::Alloc(tac::Value::Const(tac::Const::Int(1))),
            Some(tac::ID::new(0, tac::IDType::Var)),
        );
        let sum = tac::InstructionLine(
            tac::Instruction::Op(tac::Op::Op(
                tac::TypeOp::Arithmetic(tac::ArithmeticOp::Add), tac::Value::ID(var.1.clone().unwrap()), tac::Value::Const(tac::Const::Int(2))
            )),
            Some(tac::ID::new(0, tac::IDType::Temporary)),
        );
        let var2 = tac::InstructionLine(
            tac::Instruction::Assignment(tac::ID::new(1, tac::IDType::Var), tac::Value::ID(sum.1.clone().unwrap())),
            Some(tac::ID::new(1, tac::IDType::Var)),
        );

        translator.translate(var);
        translator.translate(sum);
        translator.translate(var2);
        let instructions = translator.instructions;

        let expected = IList::from(vec![
            AsmInstruction::Mov(Params::new(Place::Stack(4, syntax::Type::Doubleword), Value::Const(1, syntax::Type::Doubleword)).unwrap()),
            AsmInstruction::Mov(Params::new(Place::Register("eax"), Value::Place(Place::Stack(4, syntax::Type::Doubleword))).unwrap()),
            AsmInstruction::Add(Params::new(Place::Register("eax"), Value::Const(2, syntax::Type::Doubleword)).unwrap()),
            AsmInstruction::Mov(Params::new(Place::Stack(8, syntax::Type::Doubleword), Value::Place(Place::Register("eax"))).unwrap()),
        ]);
        assert_eq!(expected, instructions);
    }

    #[test]
    fn translate_operation_assign_var_sum_var_and_var() {
        let mut translator = Translator::new();
        let var1 = tac::InstructionLine(
            tac::Instruction::Alloc(tac::Value::Const(tac::Const::Int(1))),
            Some(tac::ID::new(0, tac::IDType::Var)),
        );
        let var2 = tac::InstructionLine(
            tac::Instruction::Alloc(tac::Value::Const(tac::Const::Int(2))),
            Some(tac::ID::new(1, tac::IDType::Var)),
        );
        let sum = tac::InstructionLine(
            tac::Instruction::Op(tac::Op::Op(
                tac::TypeOp::Arithmetic(tac::ArithmeticOp::Add), tac::Value::ID(var1.1.clone().unwrap()), tac::Value::ID(var2.1.clone().unwrap())
            )),
            Some(tac::ID::new(0, tac::IDType::Temporary)),
        );
        let var3 = tac::InstructionLine(
            tac::Instruction::Assignment(tac::ID::new(1, tac::IDType::Var), tac::Value::ID(sum.1.clone().unwrap())),
            Some(tac::ID::new(2, tac::IDType::Var)),
        );

        translator.translate(var1);
        translator.translate(var2);
        translator.translate(sum);
        translator.translate(var3);
        let instructions = translator.instructions;

        let expected = IList::from(vec![
            AsmInstruction::Mov(Params::new(Place::Stack(4, syntax::Type::Doubleword), Value::Const(1, syntax::Type::Doubleword)).unwrap()),
            AsmInstruction::Mov(Params::new(Place::Stack(8, syntax::Type::Doubleword), Value::Const(2, syntax::Type::Doubleword)).unwrap()),
            AsmInstruction::Mov(Params::new(Place::Register("eax"), Value::Place(Place::Stack(4, syntax::Type::Doubleword))).unwrap()),
            AsmInstruction::Add(Params::new(Place::Register("eax"), Value::Place(Place::Stack(8, syntax::Type::Doubleword))).unwrap()),
            AsmInstruction::Mov(Params::new(Place::Stack(12, syntax::Type::Doubleword), Value::Place(Place::Register("eax"))).unwrap()),
        ]);
        assert_eq!(expected, instructions);
    }
}
