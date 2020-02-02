use super::translator::{Id, Translator, Type, Value};
use std::collections::HashMap;

#[derive(Debug, PartialEq, Eq)]
pub enum AsmX32 {
    Metadata(String),
    Label(String),
    Mov(Place, AsmValue),
    Add(Place, AsmValue),
    Sub(Place, AsmValue),
    Jmp(String),
    Je(String),
    Jne(String),
    Cmp(Place, AsmValue),
    Push(AsmValue),
    Pop(Place),
    Ret,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Place {
    // TODO: might better store just size?
    // Type could implement from usize for instance
    Stack(u64, Type),
    Register(Register),
}

impl Place {
    pub fn size(&self) -> Type {
        match self {
            Place::Register(reg) => reg.size(),
            Place::Stack(.., t) => t.clone(),
        }
    }
}

// TODO: after move rename to Value
#[derive(Debug, PartialEq, Eq)]
pub enum AsmValue {
    Const(i64, Type),
    Place(Place),
}

impl AsmValue {
    pub fn size(&self) -> Type {
        match self {
            AsmValue::Const(.., t) => t.clone(),
            AsmValue::Place(p) => p.size(),
        }
    }

    pub fn is_place(&self) -> bool {
        match self {
            AsmValue::Place(..) => true,
            _ => false,
        }
    }

    pub fn is_const(&self) -> bool {
        match self {
            AsmValue::Const(..) => true,
            _ => false,
        }
    }
}

#[derive(PartialEq, Eq, Clone)]
pub struct Register(usize);

use std::iter::FromIterator;

impl Register {
    // Since nowadays we don't use others registers the are not support them yet
    const REGISTERS: &'static [&'static str] = &[
        "", "", // to be able to provide size by index (see method `size`)
        "rax", "eax", "rbx", "ebx", "rcx", "ecx", "rdx", "edx", "rsi", "esi", "rdi", "edi", "rbp",
        "ebp", "rsp", "esp", "r8", "r8d", "r9", "r9d", "r10", "r10d", "r11", "r11d", "r12", "r12d",
        "r13", "r13d", "r14", "r14d", "r15", "r15d",
    ];

    pub fn new(reg_str: &'static str) -> Register {
        let index = Register::reg_index(reg_str).unwrap();
        Register(index)
    }

    pub fn size(&self) -> Type {
        if self.0 % 2 == 0 {
            Type::Quadword
        } else if self.0 % 3 == 0 {
            Type::Doubleword
        } else {
            unimplemented!()
        }
    }

    fn cast(&self, size: &Type) -> Register {
        let self_size = self.size();
        if self_size == *size {
            return self.clone();
        }

        match self_size {
            Type::Quadword if *size == Type::Doubleword => Register(self.0 + 1),
            Type::Doubleword if *size == Type::Quadword => Register(self.0 - 1),
            _ => unimplemented!(),
        }
    }

    fn reg_index(reg: &str) -> Option<usize> {
        for (i, r) in Register::REGISTERS.iter().enumerate() {
            if r == &reg {
                return Some(i);
            }
        }
        None
    }
}

impl std::fmt::Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", Register::REGISTERS[self.0])
    }
}

impl std::fmt::Debug for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\"{}\"", self)
    }
}

impl Into<Register> for &'static str {
    fn into(self) -> Register {
        Register::new(self)
    }
}

pub struct X64Backend {
    stack_index: u64,
    memory: HashMap<Id, Place>,
    asm: Vec<AsmX32>,
}

impl X64Backend {
    pub fn new() -> Self {
        X64Backend {
            memory: HashMap::new(),
            stack_index: 0,
            asm: Vec::new(),
        }
    }
}

impl Translator for X64Backend {
    fn func_begin(&mut self, name: &str) {
        self.push_asm(AsmX32::Metadata(format!(".globl {}", name)));
        self.push_asm(AsmX32::Label(name.to_owned()));

        self.push_asm(AsmX32::Push(AsmValue::Place(Place::Register("rbp".into()))));
        self.push_asm(AsmX32::Mov(
            Place::Register("rbp".into()),
            AsmValue::Place(Place::Register("rsp".into())),
        ));
    }

    fn func_end(&mut self) {
        self.push_asm(AsmX32::Pop(Place::Register("rbp".into())));
        self.push_asm(AsmX32::Ret);
    }

    fn label(&mut self, label: usize) {
        self.push_asm(AsmX32::Label(format!(".L_{}", label)));
    }

    // TODO: should we handle situation when there are not such type of label?
    fn jump(&mut self, label: usize) {
        self.push_asm(AsmX32::Jmp(format!(".L_{}", label)));
    }

    fn if_goto(&mut self, t: Type, value: Value, label: usize) {
        let value = self.const_or_allocated(t.clone(), value);
        let place = match value {
            AsmValue::Place(place) => place,
            AsmValue::Const(..) => {
                let space = self.alloc(&t);
                self.copy_value_on(value, space)
            }
        };

        self.push_asm(AsmX32::Cmp(place, AsmValue::Const(0, t)));
        self.push_asm(AsmX32::Je(format!(".L_{}", label)));
    }

    fn save(&mut self, id: Id, t: Type, value: Option<Value>) {
        if let Some(place) = self.place(&id) {
            let place = place.clone();
            let value = self.const_or_allocated(t.clone(), value.unwrap());
            if value.is_place() {
                let register = Place::Register(Register::new("rax").cast(&t));
                let reg_place = self.copy_value_on(value, register);
                self.copy_value_on(AsmValue::Place(reg_place), place);
            } else {
                self.copy_value_on(value, place);
            }
        } else {
            let place = self.alloc(&t);
            self.save_place(id, &place);
            self.copy_on(t, value.unwrap(), place);
        }
    }

    fn add(&mut self, id: Id, t: Type, a: Value, b: Value) {
        let first = self.const_or_allocated(t.clone(), b);
        let add_register = Place::Register(Register::new("rax").cast(&t));
        let second = self.copy_on(t, a, add_register);
        self.save_place(id, &second);
        self.push_asm(AsmX32::Add(second, first));
    }

    fn sub(&mut self, id: Id, t: Type, a: Value, b: Value) {
        let first = self.const_or_allocated(t.clone(), b);
        let add_register = Place::Register(Register::new("rax").cast(&t));
        let second = self.copy_on(t, a, add_register);
        self.save_place(id, &second);
        self.push_asm(AsmX32::Sub(second, first));
    }

    fn ret(&mut self, t: Type, v: Value) {
        let value = self.const_or_allocated(t, v);
        let size = X64Backend::value_size(&value);
        let return_reg = Register::new("rax").cast(&size);
        match value {
            AsmValue::Place(Place::Register(ref reg)) if *reg == return_reg => (),
            _ => {
                self.push_asm(AsmX32::Mov(Place::Register(return_reg), value));
            }
        }
    }

    fn stash(&self) -> String {
        let mut buf = String::new();
        for i in &self.asm {
            buf += &super::syntax::GASMx64::to_string(i);
            buf += "\n";
        }
        buf
    }
}

impl X64Backend {
    fn push_asm(&mut self, i: AsmX32) {
        self.asm.push(i);
    }

    fn copy_on(&mut self, t: Type, v: Value, p: Place) -> Place {
        let value = self.const_or_allocated(t, v);
        self.copy_value_on(value, p)
    }

    fn copy_value_on(&mut self, value: AsmValue, place: Place) -> Place {
        if let AsmValue::Place(p) = &value {
            if *p == place {
                return place;
            }
        }

        self.push_asm(AsmX32::Mov(place.clone(), value));
        place
    }

    fn alloc(&mut self, t: &Type) -> Place {
        let size = match t {
            Type::Doubleword => 4,
            _ => unimplemented!(),
        };

        self.stack_index += size;

        Place::Stack(self.stack_index, t.clone())
    }

    fn const_or_allocated(&self, t: Type, v: Value) -> AsmValue {
        match v {
            Value::Const(int) => AsmValue::Const(int, t),
            Value::Ref(id) => AsmValue::Place(self.place(&id).unwrap().clone()),
        }
    }

    fn place(&self, id: &Id) -> Option<&Place> {
        self.memory.get(id)
    }

    fn save_place(&mut self, id: Id, place: &Place) {
        self.memory.insert(id, place.clone());
    }

    fn value_size(v: &AsmValue) -> Type {
        match v {
            AsmValue::Const(.., t) => t.clone(),
            AsmValue::Place(Place::Stack(.., t)) => t.clone(),
            AsmValue::Place(Place::Register(reg)) => reg.size(),
        }
    }
}

mod translator_tests {
    use super::*;

    #[test]
    fn return_doubleword() {
        let mut trans = X64Backend::new();
        trans.ret(Type::Doubleword, Value::Const(10));
        let asm = trans.asm;

        assert_eq!(
            vec![AsmX32::Mov(
                Place::Register("eax".into()),
                AsmValue::Const(10, Type::Doubleword)
            )],
            asm
        )
    }

    #[test]
    fn return_quadword() {
        let mut trans = X64Backend::new();
        trans.ret(Type::Quadword, Value::Const(10));
        let asm = trans.asm;

        assert_eq!(
            vec![AsmX32::Mov(
                Place::Register("rax".into()),
                AsmValue::Const(10, Type::Quadword)
            )],
            asm
        )
    }

    #[test]
    fn add_const_to_const() {
        let mut trans = X64Backend::new();
        trans.add(0, Type::Doubleword, Value::Const(10), Value::Const(20));
        let asm = trans.asm;

        assert_eq!(
            vec![
                AsmX32::Mov(
                    Place::Register("eax".into()),
                    AsmValue::Const(10, Type::Doubleword)
                ),
                AsmX32::Add(
                    Place::Register("eax".into()),
                    AsmValue::Const(20, Type::Doubleword)
                )
            ],
            asm
        )
    }

    #[test]
    fn assign_var_then_add_const() {
        let mut trans = X64Backend::new();
        trans.save(0, Type::Doubleword, Some(Value::Const(10)));
        trans.add(1, Type::Doubleword, Value::Ref(0), Value::Const(20));
        let asm = trans.asm;

        assert_eq!(
            vec![
                AsmX32::Mov(
                    Place::Stack(4, Type::Doubleword),
                    AsmValue::Const(10, Type::Doubleword)
                ),
                AsmX32::Mov(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Stack(4, Type::Doubleword)),
                ),
                AsmX32::Add(
                    Place::Register("eax".into()),
                    AsmValue::Const(20, Type::Doubleword)
                )
            ],
            asm
        );
    }

    #[test]
    fn add_var_add_var() {
        let mut trans = X64Backend::new();
        trans.save(0, Type::Doubleword, Some(Value::Const(10)));
        trans.save(1, Type::Doubleword, Some(Value::Const(20)));
        trans.add(1, Type::Doubleword, Value::Ref(0), Value::Ref(1));
        let asm = trans.asm;

        assert_eq!(
            vec![
                AsmX32::Mov(
                    Place::Stack(4, Type::Doubleword),
                    AsmValue::Const(10, Type::Doubleword)
                ),
                AsmX32::Mov(
                    Place::Stack(8, Type::Doubleword),
                    AsmValue::Const(20, Type::Doubleword)
                ),
                AsmX32::Mov(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Stack(4, Type::Doubleword)),
                ),
                AsmX32::Add(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Stack(8, Type::Doubleword)),
                )
            ],
            asm
        );
    }

    #[test]
    fn add_var_add_var_3_times() {
        let mut trans = X64Backend::new();
        trans.save(0, Type::Doubleword, Some(Value::Const(10)));
        trans.add(1, Type::Doubleword, Value::Ref(0), Value::Ref(0));
        trans.add(2, Type::Doubleword, Value::Ref(1), Value::Ref(0));
        let asm = trans.asm;

        assert_eq!(
            vec![
                AsmX32::Mov(
                    Place::Stack(4, Type::Doubleword),
                    AsmValue::Const(10, Type::Doubleword)
                ),
                AsmX32::Mov(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Stack(4, Type::Doubleword)),
                ),
                AsmX32::Add(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Stack(4, Type::Doubleword)),
                ),
                AsmX32::Add(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Stack(4, Type::Doubleword)),
                )
            ],
            asm
        );
    }

    #[test]
    fn add_var_and_var_then_add_the_result_and_itself() {
        let mut trans = X64Backend::new();
        trans.save(0, Type::Doubleword, Some(Value::Const(10)));
        trans.add(1, Type::Doubleword, Value::Ref(0), Value::Ref(0));
        trans.add(2, Type::Doubleword, Value::Ref(1), Value::Ref(1));
        let asm = trans.asm;

        assert_eq!(
            vec![
                AsmX32::Mov(
                    Place::Stack(4, Type::Doubleword),
                    AsmValue::Const(10, Type::Doubleword)
                ),
                AsmX32::Mov(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Stack(4, Type::Doubleword)),
                ),
                AsmX32::Add(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Stack(4, Type::Doubleword)),
                ),
                AsmX32::Add(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Register("eax".into())),
                )
            ],
            asm
        );
    }

    #[test]
    fn add_const_to_const_quadword() {
        let mut trans = X64Backend::new();
        trans.add(0, Type::Quadword, Value::Const(10), Value::Const(20));
        let asm = trans.asm;

        assert_eq!(
            vec![
                AsmX32::Mov(
                    Place::Register("rax".into()),
                    AsmValue::Const(10, Type::Quadword)
                ),
                AsmX32::Add(
                    Place::Register("rax".into()),
                    AsmValue::Const(20, Type::Quadword)
                )
            ],
            asm
        )
    }

    #[test]
    fn sub_const_and_const() {
        let mut trans = X64Backend::new();
        trans.sub(0, Type::Doubleword, Value::Const(20), Value::Const(10));
        let asm = trans.asm;

        assert_eq!(
            vec![
                AsmX32::Mov(
                    Place::Register("eax".into()),
                    AsmValue::Const(20, Type::Doubleword)
                ),
                AsmX32::Sub(
                    Place::Register("eax".into()),
                    AsmValue::Const(10, Type::Doubleword)
                )
            ],
            asm
        )
    }

    #[test]
    fn sub_var_and_var() {
        let mut trans = X64Backend::new();
        trans.save(0, Type::Doubleword, Some(Value::Const(20)));
        trans.save(1, Type::Doubleword, Some(Value::Const(10)));
        trans.sub(2, Type::Doubleword, Value::Ref(0), Value::Ref(1));
        let asm = trans.asm;

        assert_eq!(
            vec![
                AsmX32::Mov(
                    Place::Stack(4, Type::Doubleword),
                    AsmValue::Const(20, Type::Doubleword)
                ),
                AsmX32::Mov(
                    Place::Stack(8, Type::Doubleword),
                    AsmValue::Const(10, Type::Doubleword)
                ),
                AsmX32::Mov(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Stack(4, Type::Doubleword)),
                ),
                AsmX32::Sub(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Stack(8, Type::Doubleword)),
                )
            ],
            asm
        );
    }

    #[test]
    fn sub_var_add_var_3_times() {
        let mut trans = X64Backend::new();
        trans.save(0, Type::Doubleword, Some(Value::Const(10)));
        trans.sub(1, Type::Doubleword, Value::Ref(0), Value::Ref(0));
        trans.sub(2, Type::Doubleword, Value::Ref(1), Value::Ref(0));
        let asm = trans.asm;

        assert_eq!(
            vec![
                AsmX32::Mov(
                    Place::Stack(4, Type::Doubleword),
                    AsmValue::Const(10, Type::Doubleword)
                ),
                AsmX32::Mov(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Stack(4, Type::Doubleword)),
                ),
                AsmX32::Sub(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Stack(4, Type::Doubleword)),
                ),
                AsmX32::Sub(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Stack(4, Type::Doubleword)),
                )
            ],
            asm
        );
    }

    #[test]
    fn sub_var_and_var_then_sub_the_result_and_itself() {
        let mut trans = X64Backend::new();
        trans.save(0, Type::Doubleword, Some(Value::Const(10)));
        trans.sub(1, Type::Doubleword, Value::Ref(0), Value::Ref(0));
        trans.sub(2, Type::Doubleword, Value::Ref(1), Value::Ref(1));
        let asm = trans.asm;

        assert_eq!(
            vec![
                AsmX32::Mov(
                    Place::Stack(4, Type::Doubleword),
                    AsmValue::Const(10, Type::Doubleword)
                ),
                AsmX32::Mov(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Stack(4, Type::Doubleword)),
                ),
                AsmX32::Sub(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Stack(4, Type::Doubleword)),
                ),
                AsmX32::Sub(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Register("eax".into())),
                )
            ],
            asm
        );
    }

    #[test]
    fn sub_const_to_const_quadword() {
        let mut trans = X64Backend::new();
        trans.sub(0, Type::Quadword, Value::Const(20), Value::Const(10));
        let asm = trans.asm;

        assert_eq!(
            vec![
                AsmX32::Mov(
                    Place::Register("rax".into()),
                    AsmValue::Const(20, Type::Quadword)
                ),
                AsmX32::Sub(
                    Place::Register("rax".into()),
                    AsmValue::Const(10, Type::Quadword)
                )
            ],
            asm
        )
    }

    #[test]
    fn assign_var_to_var() {
        let mut trans = X64Backend::new();
        trans.save(0, Type::Doubleword, Some(Value::Const(10)));
        trans.save(1, Type::Doubleword, Some(Value::Const(20)));
        trans.save(0, Type::Doubleword, Some(Value::Ref(1)));
        let asm = trans.asm;

        assert_eq!(
            vec![
                AsmX32::Mov(
                    Place::Stack(4, Type::Doubleword),
                    AsmValue::Const(10, Type::Doubleword)
                ),
                AsmX32::Mov(
                    Place::Stack(8, Type::Doubleword),
                    AsmValue::Const(20, Type::Doubleword)
                ),
                AsmX32::Mov(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Stack(8, Type::Doubleword)),
                ),
                AsmX32::Mov(
                    Place::Stack(4, Type::Doubleword),
                    AsmValue::Place(Place::Register("eax".into())),
                ),
            ],
            asm
        );
    }

    #[test]
    fn assign_var_to_const_after_initialization() {
        let mut trans = X64Backend::new();
        trans.save(0, Type::Doubleword, Some(Value::Const(10)));
        trans.save(0, Type::Doubleword, Some(Value::Const(20)));
        let asm = trans.asm;

        assert_eq!(
            vec![
                AsmX32::Mov(
                    Place::Stack(4, Type::Doubleword),
                    AsmValue::Const(10, Type::Doubleword)
                ),
                AsmX32::Mov(
                    Place::Stack(4, Type::Doubleword),
                    AsmValue::Const(20, Type::Doubleword),
                ),
            ],
            asm
        );
    }

    #[test]
    fn label_creation() {
        let mut trans = X64Backend::new();
        trans.label(0);

        let asm = trans.asm;

        assert_eq!(vec![AsmX32::Label(".L_0".to_owned())], asm)
    }

    #[test]
    fn goto_creation() {
        let mut trans = X64Backend::new();
        trans.jump(0);

        let asm = trans.asm;

        assert_eq!(vec![AsmX32::Jmp(".L_0".to_owned())], asm)
    }

    #[test]
    fn if_constant() {
        let mut trans = X64Backend::new();
        trans.if_goto(Type::Doubleword, Value::Const(1), 0);

        let asm = trans.asm;

        assert_eq!(
            vec![
                AsmX32::Mov(
                    Place::Stack(4, Type::Doubleword),
                    AsmValue::Const(1, Type::Doubleword)
                ),
                AsmX32::Cmp(
                    Place::Stack(4, Type::Doubleword),
                    AsmValue::Const(0, Type::Doubleword)
                ),
                AsmX32::Je(".L_0".to_owned()),
            ],
            asm
        );
    }

    #[test]
    fn if_place() {
        let mut trans = X64Backend::new();
        trans.save(0, Type::Doubleword, Some(Value::Const(1)));
        trans.if_goto(Type::Doubleword, Value::Ref(0), 0);

        let asm = trans.asm;

        assert_eq!(
            vec![
                AsmX32::Mov(
                    Place::Stack(4, Type::Doubleword),
                    AsmValue::Const(1, Type::Doubleword)
                ),
                AsmX32::Cmp(
                    Place::Stack(4, Type::Doubleword),
                    AsmValue::Const(0, Type::Doubleword)
                ),
                AsmX32::Je(".L_0".to_owned()),
            ],
            asm
        );
    }
}

mod register_tests {
    use super::*;

    #[test]
    fn register_size() {
        assert_eq!(Register::new("rax").size(), Type::Quadword);
        assert_eq!(Register::new("eax").size(), Type::Doubleword);
    }

    #[test]
    #[should_panic]
    fn wrong_construction() {
        Register::new("not_supported_register");
    }
}
