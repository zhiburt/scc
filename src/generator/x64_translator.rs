
use std::collections::HashMap;
use super::translator::{Translator, Type, Value, Id};

#[derive(Debug, PartialEq, Eq)]
pub enum AsmX32 {
    Metadata(String),
    Label(String),
    Mov(Place, AsmValue),
    Add(Place, AsmValue),
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

// TODO: after move rename to Value
#[derive(Debug, PartialEq, Eq)]
pub enum AsmValue {
    Const(i64, Type),
    Place(Place),
}

// TODO: might it's better to provide it by enum?
//
// TODO: we should create big map <key: register, value: size>
pub type Register = &'static str;

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

        self.push_asm(AsmX32::Push(AsmValue::Place(Place::Register("rbp"))));
        self.push_asm(AsmX32::Mov(Place::Register("rbp"), AsmValue::Place(Place::Register("rsp"))));
    }

    fn func_end(&mut self) {
        self.push_asm(AsmX32::Pop(Place::Register("rbp")));
    }

    fn save(&mut self, id: Id, t: Type, value: Option<Value>) {
        
    }

    fn add(&mut self, id: Id, t: Type, a: Value, b: Value) {
        let first = self.const_or_allocated(t.clone(), a);
        // TODO: the shoce of eax, rax etc should be better 
        let second = self.copy_on(t, b, Place::Register("eax"));
        self.push_asm(AsmX32::Add(second, first));
    }

    fn ret(&mut self, t: Type, v: Value) {
        let value = self.const_or_allocated(t, v);
        let size = X64Backend::value_size(&value);
        let return_reg = X64Backend::register_by_size("rax", size);
        match value {
            AsmValue::Place(Place::Register(reg)) if reg == return_reg => (),
            _ => {
                self.push_asm(AsmX32::Mov(Place::Register(return_reg), value));
            },
        }
    }

    fn stash(&self) -> String {
        let mut buf = String::new();
        for i in &self.asm {
            buf += &super::syntax::GASMx64::to_string(i);
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
        // TODO: might the check on the same value on the same place
        self.push_asm(AsmX32::Mov(p.clone(), value));
        p
    }

    fn alloc(&mut self, t: Type) -> Place {
        let size = match &t {
            Type::Doubleword => 4,
            Type::Quadword => 8,
            _ => unimplemented!(),
        };

        self.stack_index += size;

        Place::Stack(self.stack_index, t)
    }

    fn const_or_allocated(&self, t: Type, v: Value) -> AsmValue {
        match v {
            Value::Const(int) => AsmValue::Const(int, t),
            Value::Ref(id) => AsmValue::Place(self.place(id).unwrap().clone())
        }
    }

    fn place(&self, id: Id) -> Option<&Place> {
        self.memory.get(&id)
    }

    fn value_size(v: &AsmValue) -> Type {
        match v {
            AsmValue::Const(.., t) => t.clone(),
            AsmValue::Place(Place::Stack(.., t)) => t.clone(),
            AsmValue::Place(Place::Register(reg)) => unimplemented!(),
        }
    }

    // TODO: reimplement it;
    fn register_by_size(r: Register, t: Type) -> Register {
        match r {
            "rax" if t == Type::Doubleword => "eax",
            "rax" if t == Type::Quadword => "rax",
            _ => unimplemented!(),
        }
    }
}

mod tests {
    use super::*;

    #[test]
    fn return_doubleword() {
        let mut trans = X64Backend::new();
        
        trans.ret(Type::Doubleword, Value::Const(10));
        let asm = trans.asm;

        assert_eq!(vec![AsmX32::Mov(Place::Register("eax"), AsmValue::Const(10, Type::Doubleword))], asm)
    }

    #[test]
    fn return_quadword() {
        let mut trans = X64Backend::new();
        
        trans.ret(Type::Quadword, Value::Const(10));
        let asm = trans.asm;

        assert_eq!(vec![AsmX32::Mov(Place::Register("rax"), AsmValue::Const(10, Type::Quadword))], asm)
    }
}