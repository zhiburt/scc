use super::translator::{Id, Translator, Type, Value};
use std::collections::HashMap;

#[derive(Debug, PartialEq, Eq)]
pub enum AsmX32 {
    Metadata(String),
    Label(String),
    Mov(Place, AsmValue),
    Movzx(Place, AsmValue),
    And(Place, AsmValue),
    Or(Place, AsmValue),
    Xor(Place, AsmValue),
    Add(Place, AsmValue),
    Sub(Place, AsmValue),
    Mul(Place, AsmValue),
    Div(Place),
    Neg(Place),
    Not(Place),
    Convert(Type),
    Sete(Place),
    Setne(Place),
    Setl(Place),
    Setle(Place),
    Setg(Place),
    Setge(Place),
    Jmp(String),
    Je(String),
    Jne(String),
    Cmp(Place, AsmValue),
    Push(AsmValue),
    Pop(Place),
    Call(String),
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
pub struct Register {
    table_index: usize,
    reg_pos: usize,
}

use std::iter::FromIterator;

impl Register {
    // Since nowadays we don't use others registers the are not support them yet
    const REGISTERS: &'static [&'static [&'static str]] = &[
        &["rax", "eax", "ax", "al"],
        &["rbx", "ebx"],
        &["rcx", "ecx"],
        &["rdx", "edx"],
        &["rsi", "esi"],
        &["rdi", "edi"],
        &["rbp", "ebp"],
        &["rsp", "esp"],
        &["r8", "r8d"],
        &["r9", "r9d"],
        &["r10", "r10d"],
        &["r11", "r11d"],
        &["r12", "r12d"],
        &["r13", "r13d"],
        &["r14", "r14d"],
        &["r15", "r15d"],
    ];

    pub fn new(reg_str: &'static str) -> Register {
        let (table_index, reg_pos) = Register::reg_index(reg_str).unwrap();
        Register{ table_index, reg_pos }
    }

    pub fn size(&self) -> Type {
        match self.reg_pos {
            0 => Type::Quadword,
            1 => Type::Doubleword,
            3 => Type::Byte,
            _ => unimplemented!(),
        }
    }

    fn cast(&self, size: &Type) -> Register {
        let self_size = self.size();
        if self_size == *size {
            return self.clone();
        }

        let mut new_register = self.clone();
        match self_size {
            Type::Quadword if *size == Type::Doubleword => new_register.reg_pos += 1,
            Type::Doubleword if *size == Type::Quadword => new_register.reg_pos -= 1,
            _ => unimplemented!(),
        }

        new_register
    }

    fn reg_index(reg: &str) -> Option<(usize, usize)> {
        for (i, r) in Register::REGISTERS.iter().enumerate() {
            if let Some(index) = r.iter().position(|r| r == &reg) {
                return Some((i, index));
            }
        }
        None
    }
}

impl std::fmt::Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", Register::REGISTERS[self.table_index][self.reg_pos])
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
    ret_label: Option<String>, // It's blazingly bad design step ???
    ret_label_index: usize,
    frame_size: usize,
    asm: Vec<AsmX32>,
}

impl X64Backend {
    pub fn new() -> Self {
        X64Backend {
            memory: HashMap::new(),
            stack_index: 0,
            ret_label: None,
            ret_label_index: 0,
            frame_size: 0,
            asm: Vec::new(),
        }
    }
}

impl Translator for X64Backend {
    fn func_begin(&mut self, name: &str, params: &[(Type, Id)], has_multi_ret: bool, alloc_size: usize) {
        self.push_asm(AsmX32::Metadata(format!(".globl {}", name)));
        self.push_asm(AsmX32::Label(name.to_owned()));

        self.push_asm(AsmX32::Push(AsmValue::Place(Place::Register("rbp".into()))));
        self.push_asm(AsmX32::Mov(
            Place::Register("rbp".into()),
            AsmValue::Place(Place::Register("rsp".into())),
        ));

        self.frame_size = alloc_size;
        if alloc_size != 0 {
            self.push_asm(AsmX32::Sub(Place::Register(Register::new("rsp")), AsmValue::Const(alloc_size as i64, Type::Quadword)));
        }

        let registers: [Register; 6] = ["rdi".into(), "rsi".into(), "rdx".into(), "rcx".into(), "r8".into(), "r9".into()];
        let mut i = 0;
        for (t, id) in params {
            if i < registers.len() {
                let place = self.alloc(&t);
                self.save_place(*id, &place);
                self.copy_value_on(AsmValue::Place(Place::Register(registers[i].cast(&t))), place);
                i += 1;
            } else {
                // TODO: there's a difference in handling stack paramaters
                unimplemented!()
            }
        }

        if has_multi_ret {
            self.ret_label = Some(self.create_label())
        }
    }

    fn func_end(&mut self, alloc_size: usize) {
        if let Some(ret_label) = self.ret_label.clone() {
            self.push_asm(AsmX32::Label(ret_label));
            self.ret_label = None;
        }

        if alloc_size != 0 {
            self.push_asm(AsmX32::Add(Place::Register(Register::new("rsp")), AsmValue::Const(alloc_size as i64, Type::Quadword)));
        }

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
            let value = if value.as_ref().unwrap().is_ref() {
                // TODO: clang has much more attractive strategy to handle
                // expressions like `a = b = 4`
                // but stick with easiest one since the former one is
                // quite hard to implement right now.
                let register = Place::Register(Register::new("rax").cast(&t));
                AsmValue::Place(self.copy_on(t.clone(), value.unwrap(), register))
            } else {
                self.const_or_allocated(t.clone(), value.unwrap())
            };

            let place = self.alloc(&t);
            self.save_place(id, &place);
            self.copy_value_on(value, place);
        }
    }

    fn add(&mut self, id: Id, t: Type, a: Value, b: Value) {
        let place = Place::Register(Register::new("rax").cast(&t));
        let value = self.const_or_allocated(t.clone(), b);
        self.copy_on(t.clone(), a, place.clone());
        self.push_asm(AsmX32::Add(place.clone(), value));
        let copy_place = self.alloc(&t);
        self.copy_value_on(AsmValue::Place(place), copy_place.clone());
        self.save_place(id, &copy_place);
    }

    fn sub(&mut self, id: Id, t: Type, a: Value, b: Value) {
        let place = Place::Register(Register::new("rax").cast(&t));
        let value = self.const_or_allocated(t.clone(), b);
        self.copy_on(t.clone(), a, place.clone());
        self.push_asm(AsmX32::Sub(place.clone(), value));
        let copy_place = self.alloc(&t);
        self.copy_value_on(AsmValue::Place(place), copy_place.clone());
        self.save_place(id, &copy_place);
    }

    fn mul(&mut self, id: Id, t: Type, a: Value, b: Value) {
        let place = Place::Register(Register::new("rax").cast(&t));
        let value = self.const_or_allocated(t.clone(), b);
        self.copy_on(t.clone(), a, place.clone());
        self.push_asm(AsmX32::Mul(place.clone(), value));
        let copy_place = self.alloc(&t);
        self.copy_value_on(AsmValue::Place(place), copy_place.clone());
        self.save_place(id, &copy_place);
    }

    fn div(&mut self, id: Id, t: Type, a: Value, b: Value) {
        let sub_register = Place::Register(Register::new("rax").cast(&t));
        self.copy_on(t.clone(), a, sub_register.clone());
        // To be comparable with clang, it does this operation
        // before possible move of constant
        self.push_asm(AsmX32::Convert(t.clone()));

        let second = self.const_or_allocated(t.clone(), b);
        let divisor_place = match second {
            AsmValue::Place(place) => place,
            AsmValue::Const(..) => {
                // TODO: why do we use rcx directly why not any of others registers?
                let remain_register = Place::Register(Register::new("rcx").cast(&t));
                self.copy_value_on(second, remain_register.clone())
            }
        };

        self.push_asm(AsmX32::Div(divisor_place.clone()));

        let copy_place = self.alloc(&t);
        self.save_place(id, &copy_place);
        self.copy_value_on(AsmValue::Place(sub_register), copy_place);
    }

    fn div_reminder(&mut self, id: Id, t: Type, a: Value, b: Value) {
        let sub_register = Place::Register(Register::new("rax").cast(&t));
        let first = self.copy_on(t.clone(), a, sub_register);
        // To be comparable with clang, it does this operation
        // before possible move of constant
        self.push_asm(AsmX32::Convert(t.clone()));

        let second = self.const_or_allocated(t.clone(), b);
        let divisor_place = match second {
            AsmValue::Place(place) => place,
            AsmValue::Const(..) => {
                // TODO: why do we use rcx directly why not any of others registers?
                let remain_register = Place::Register(Register::new("rcx").cast(&t));
                self.copy_value_on(second, remain_register.clone())
            }
        };

        self.push_asm(AsmX32::Div(divisor_place.clone()));

        let copy_place = self.alloc(&t);
        self.save_place(id, &copy_place);
        self.copy_value_on(AsmValue::Place(Place::Register(Register::new("rdx").cast(&t))), copy_place);
    }

    fn bit_and(&mut self, id: Id, t: Type, a: Value, b: Value) {
        let place = Place::Register(Register::new("rax").cast(&t));
        let value = self.const_or_allocated(t.clone(), b);
        self.copy_on(t.clone(), a, place.clone());
        self.push_asm(AsmX32::And(place.clone(), value));
        let copy_place = self.alloc(&t);
        self.copy_value_on(AsmValue::Place(place), copy_place.clone());
        self.save_place(id, &copy_place);
    }

    fn bit_or(&mut self, id: Id, t: Type, a: Value, b: Value) {
        let place = Place::Register(Register::new("rax").cast(&t));
        let value = self.const_or_allocated(t.clone(), b);
        self.copy_on(t.clone(), a, place.clone());
        self.push_asm(AsmX32::Or(place.clone(), value));
        let copy_place = self.alloc(&t);
        self.copy_value_on(AsmValue::Place(place), copy_place.clone());
        self.save_place(id, &copy_place);
    }

    fn bit_xor(&mut self, id: Id, t: Type, a: Value, b: Value) {
        let place = Place::Register(Register::new("rax").cast(&t));
        let value = self.const_or_allocated(t.clone(), b);
        self.copy_on(t.clone(), a, place.clone());
        self.push_asm(AsmX32::Xor(place.clone(), value));
        let copy_place = self.alloc(&t);
        self.copy_value_on(AsmValue::Place(place), copy_place.clone());
        self.save_place(id, &copy_place);
    }

    fn neg(&mut self, id: Id, a: Id) {
        let place = self.place(&a).unwrap().clone();
        let t = place.size();
        let add_register = Place::Register(Register::new("rax").cast(&t));
        let value_place = AsmValue::Place(self.copy_value_on(AsmValue::Place(place), add_register.clone()));
        self.push_asm(AsmX32::Neg(add_register));
        // TODO: a critical issue appears here
        // when we try to pass a il as
        // ```
        //   _t0: - a
        //   _t1: - b
        //   _t2: _t0 + _t1
        // ```
        // We would save the values on the %rax by default,
        // but it will cause error error since t0 and t1 placed in the same storage
        // so we just copy it on stack here instead of
        // using something like `self.save_place(id, &place);`
        // The same issue appears for all unary operations.
        //
        // There's no easy way to resolve it.
        // There's only one I can see now is life interval calculation
        //
        // Also `clang` has different approach for this calculation
        let place = self.alloc(&t);
        self.save_place(id, &place);
        self.copy_value_on(value_place, place.clone());
    }

    fn logical_neg(&mut self, id: Id, a: Id) {
        // TODO: look at neg function
        let place = self.place(&a).unwrap().clone();
        let t = place.size();
        self.push_asm(AsmX32::Cmp(place, AsmValue::Const(0, t.clone())));
        self.push_asm(AsmX32::Setne(Place::Register("al".into())));
        self.push_asm(AsmX32::Xor(Place::Register("al".into()), AsmValue::Const(-1, Type::Byte)));
        self.push_asm(AsmX32::And(Place::Register("al".into()), AsmValue::Const(1, Type::Byte)));
        self.push_asm(AsmX32::Movzx(Place::Register("eax".into()), AsmValue::Place(Place::Register("al".into()))));

        let place = self.alloc(&t);
        self.save_place(id, &place);
        self.copy_value_on(AsmValue::Place(Place::Register("eax".into())), place.clone());
        self.save_place(id, &place);
    }

    fn bitwise(&mut self, id: Id, a: Id) {
        // TODO: look at neg function
        let place = self.place(&a).unwrap().clone();
        let t = place.size();
        let add_register = Place::Register(Register::new("rax").cast(&t));
        let value_place = AsmValue::Place(self.copy_value_on(AsmValue::Place(place), add_register.clone()));
        self.push_asm(AsmX32::Not(add_register));
        let place = self.alloc(&t);
        self.save_place(id, &place);
        self.copy_value_on(value_place, place.clone());
    }

    fn eq(&mut self, id: Id, t: Type, a: Value, b: Value) 
    {
        let (place, value) = self.parse_with_move_place(t.clone(), a, b);
        self.push_asm(AsmX32::Cmp(place.clone(), value));
        self.push_asm(AsmX32::Sete(Place::Register("al".into())));
        self.push_asm(AsmX32::Movzx(Place::Register("eax".into()), AsmValue::Place(Place::Register("al".into()))));
        let copy_place = self.alloc(&t);
        self.save_place(id, &copy_place);
        self.copy_value_on(AsmValue::Place(Place::Register("eax".into())), copy_place);
    }

    fn not_eq(&mut self, id: Id, t: Type, a: Value, b: Value) {
        let (place, value) = self.parse_with_move_place(t.clone(), a, b);
        self.save_place(id, &place);
        self.push_asm(AsmX32::Cmp(place.clone(), value));
        self.push_asm(AsmX32::Setne(Place::Register("al".into())));
        self.push_asm(AsmX32::Movzx(Place::Register("eax".into()), AsmValue::Place(Place::Register("al".into()))));
        let copy_place = self.alloc(&t);
        self.save_place(id, &copy_place);
        self.copy_value_on(AsmValue::Place(Place::Register("eax".into())), copy_place);
    }

    fn lt(&mut self, id: Id, t: Type, a: Value, b: Value) {
        let place = Place::Register(Register::new("rax").cast(&t));
        let value = self.const_or_allocated(t.clone(), b);
        self.copy_on(t.clone(), a, place.clone());

        self.push_asm(AsmX32::Cmp(place.clone(), value));
        self.push_asm(AsmX32::Setl(Place::Register("al".into())));
        self.push_asm(AsmX32::And(Place::Register("al".into()), AsmValue::Const(1, Type::Doubleword)));
        self.push_asm(AsmX32::Movzx(Place::Register("eax".into()), AsmValue::Place(Place::Register(Register::new("al")))));
        self.save_place(id, &Place::Register("eax".into()));
        let copy_place = self.alloc(&t);
        self.save_place(id, &copy_place);
        self.copy_value_on(AsmValue::Place(place), copy_place);
    }

    fn le(&mut self, id: Id, t: Type, a: Value, b: Value) {
        let place = Place::Register(Register::new("rax").cast(&t));
        let value = self.const_or_allocated(t.clone(), b);
        self.copy_on(t.clone(), a, place.clone());

        self.push_asm(AsmX32::Cmp(place.clone(), value));
        self.push_asm(AsmX32::Setle(Place::Register("al".into())));
        self.push_asm(AsmX32::And(Place::Register("al".into()), AsmValue::Const(1, Type::Doubleword)));
        self.push_asm(AsmX32::Movzx(Place::Register("eax".into()), AsmValue::Place(Place::Register(Register::new("al")))));
        self.save_place(id, &Place::Register("eax".into()));
        let copy_place = self.alloc(&t);
        self.save_place(id, &copy_place);
        self.copy_value_on(AsmValue::Place(place), copy_place);
    }

    fn gt(&mut self, id: Id, t: Type, a: Value, b: Value) {
        let place = Place::Register(Register::new("rax").cast(&t));
        let value = self.const_or_allocated(t.clone(), b);
        self.copy_on(t.clone(), a, place.clone());

        self.push_asm(AsmX32::Cmp(place.clone(), value));
        self.push_asm(AsmX32::Setg(Place::Register("al".into())));
        self.push_asm(AsmX32::And(Place::Register("al".into()), AsmValue::Const(1, Type::Doubleword)));
        self.push_asm(AsmX32::Movzx(Place::Register("eax".into()), AsmValue::Place(Place::Register(Register::new("al")))));
        self.save_place(id, &Place::Register("eax".into()));
        let copy_place = self.alloc(&t);
        self.save_place(id, &copy_place);
        self.copy_value_on(AsmValue::Place(place), copy_place);
    }

    fn ge(&mut self, id: Id, t: Type, a: Value, b: Value) {
        let place = Place::Register(Register::new("rax").cast(&t));
        let value = self.const_or_allocated(t.clone(), b);
        self.copy_on(t.clone(), a, place.clone());

        self.push_asm(AsmX32::Cmp(place.clone(), value));
        self.push_asm(AsmX32::Setge(Place::Register("al".into())));
        self.push_asm(AsmX32::And(Place::Register("al".into()), AsmValue::Const(1, Type::Doubleword)));
        self.push_asm(AsmX32::Movzx(Place::Register("eax".into()), AsmValue::Place(Place::Register(Register::new("al")))));
        self.save_place(id, &Place::Register("eax".into()));
        let copy_place = self.alloc(&t);
        self.save_place(id, &copy_place);
        self.copy_value_on(AsmValue::Place(place), copy_place);
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
        if let Some(ret_label) = self.ret_label.clone() {
            self.push_asm(AsmX32::Jmp(ret_label));
        }
    }

    fn call(&mut self, id: Option<Id>, t: Type, name: &str, params: &[(Type, Value)]) {
        // alignment
        if self.frame_size != 0 {
            let padding = self.frame_size % 16;
            self.push_asm(AsmX32::Sub(Place::Register(Register::new("rsp")), AsmValue::Const(padding as i64, Type::Quadword)));
        }

        let registers: [Register; 6] = ["rdi".into(), "rsi".into(), "rdx".into(), "rcx".into(), "r8".into(), "r9".into()];

        let mut stack_allocated = 0;
        let mut i = 0;
        for (t, p) in params {
            if i < registers.len() {
                self.push_asm(AsmX32::Mov(Place::Register(registers[i].cast(&t)), self.const_or_allocated(t.clone(), p.clone())));
                i += 1;
            } else {
                stack_allocated += t.size();
                self.push_asm(AsmX32::Push(self.const_or_allocated(t.clone(), p.clone())));
            }
        }

        self.push_asm(AsmX32::Call(name.to_owned()));

        if stack_allocated > 0 {
            self.push_asm(AsmX32::Add(Place::Register(Register::new("rsp")), AsmValue::Const(stack_allocated as i64, Type::Quadword)));
        }

        if self.frame_size != 0 {
            let padding = self.frame_size % 16;
            self.push_asm(AsmX32::Add(Place::Register(Register::new("rsp")), AsmValue::Const(padding as i64, Type::Quadword)));
        }

        if let Some(id) = id {
            let place = self.alloc(&t);
            self.save_place(id, &place);
            self.copy_value_on(AsmValue::Place(Place::Register(Register::new("rax").cast(&t))), place);
        }
    }

    fn stash(&mut self) -> String {
        let mut buf = String::new();
        for i in &self.asm {
            buf += &super::syntax::GASMx64::to_string(i);
            buf += "\n";
        }

        self.asm.clear();
        self.stack_index = 0;

        buf
    }
}

impl X64Backend {
    fn parse_values(&mut self, t: Type, a: Value, b: Value) -> (Place, AsmValue) {
        if a.is_ref() && b.is_const() {
            (
                self.place(&a.as_ref().unwrap()).unwrap().clone(),
                self.const_or_allocated(t.clone(), b),
            )
        } else {
            (
                self.copy_on(t.clone(), a, Place::Register(Register::new("rax").cast(&t))),
                self.const_or_allocated(t, b)
            )
        }
    }

    fn parse_with_move_place(&mut self, t: Type, a: Value, b: Value) -> (Place, AsmValue) {
        let a = self.const_or_allocated(t.clone(), a);
        let b = self.const_or_allocated(t.clone(), b);
        if let AsmValue::Place(Place::Stack(..)) = a {
            if let AsmValue::Place(Place::Stack(..)) = b {
                (
                    self.copy_value_on(a, Place::Register(Register::new("rax").cast(&t))),
                    b,
                )
            } else {
                let place = match a {
                    AsmValue::Place(place) => place,
                    _ => unreachable!()
                };

                (
                    place,
                    b,
                )
            }
        } else {
            (
                self.copy_value_on(a, Place::Register(Register::new("rax").cast(&t))),
                b,
            )
        }
    }

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

    fn create_label(&mut self) -> String {
        self.ret_label_index += 1;
        format!(".RR_{}", self.ret_label_index)
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
    fn sum_const_and_var() {
        let mut trans = X64Backend::new();
        trans.save(0, Type::Doubleword, Some(Value::Const(10)));
        trans.add(1, Type::Doubleword, Value::Const(20), Value::Ref(0));
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
    fn sum_var_and_const() {
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
                    AsmValue::Const(20, Type::Quadword)
                ),
                AsmX32::Add(
                    Place::Register("rax".into()),
                    AsmValue::Const(10, Type::Quadword)
                )
            ],
            asm
        )
    }

    #[test]
    fn and_const_and_const() {
        let mut trans = X64Backend::new();
        trans.bit_and(0, Type::Doubleword, Value::Const(10), Value::Const(20));
        let asm = trans.asm;

        assert_eq!(
            vec![
                AsmX32::Mov(
                    Place::Register("eax".into()),
                    AsmValue::Const(20, Type::Doubleword)
                ),
                AsmX32::And(
                    Place::Register("eax".into()),
                    AsmValue::Const(10, Type::Doubleword)
                )
            ],
            asm
        )
    }

    #[test]
    fn assign_var_then_bit_and_const() {
        let mut trans = X64Backend::new();
        trans.save(0, Type::Doubleword, Some(Value::Const(10)));
        trans.bit_and(1, Type::Doubleword, Value::Ref(0), Value::Const(20));
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
                AsmX32::And(
                    Place::Register("eax".into()),
                    AsmValue::Const(20, Type::Doubleword)
                )
            ],
            asm
        );
    }

    #[test]
    fn bit_and_var_bit_and_var() {
        let mut trans = X64Backend::new();
        trans.save(0, Type::Doubleword, Some(Value::Const(10)));
        trans.save(1, Type::Doubleword, Some(Value::Const(20)));
        trans.bit_and(1, Type::Doubleword, Value::Ref(0), Value::Ref(1));
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
                AsmX32::And(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Stack(8, Type::Doubleword)),
                )
            ],
            asm
        );
    }

    #[test]
    fn bit_and_var_bit_and_var_3_times() {
        let mut trans = X64Backend::new();
        trans.save(0, Type::Doubleword, Some(Value::Const(10)));
        trans.bit_and(1, Type::Doubleword, Value::Ref(0), Value::Ref(0));
        trans.bit_and(2, Type::Doubleword, Value::Ref(1), Value::Ref(0));
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
                AsmX32::And(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Stack(4, Type::Doubleword)),
                ),
                AsmX32::And(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Stack(4, Type::Doubleword)),
                )
            ],
            asm
        );
    }

    #[test]
    fn bit_and_var_and_var_then_bit_and_the_result_and_itself() {
        let mut trans = X64Backend::new();
        trans.save(0, Type::Doubleword, Some(Value::Const(10)));
        trans.bit_and(1, Type::Doubleword, Value::Ref(0), Value::Ref(0));
        trans.bit_and(2, Type::Doubleword, Value::Ref(1), Value::Ref(1));
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
                AsmX32::And(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Stack(4, Type::Doubleword)),
                ),
                AsmX32::And(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Register("eax".into())),
                )
            ],
            asm
        );
    }

    #[test]
    fn bit_and_const_to_const_quadword() {
        let mut trans = X64Backend::new();
        trans.bit_and(0, Type::Quadword, Value::Const(10), Value::Const(20));
        let asm = trans.asm;

        assert_eq!(
            vec![
                AsmX32::Mov(
                    Place::Register("rax".into()),
                    AsmValue::Const(20, Type::Quadword)
                ),
                AsmX32::And(
                    Place::Register("rax".into()),
                    AsmValue::Const(10, Type::Quadword)
                )
            ],
            asm
        )
    }

    #[test]
    fn bit_or_const_and_const() {
        let mut trans = X64Backend::new();
        trans.bit_or(0, Type::Doubleword, Value::Const(10), Value::Const(20));
        let asm = trans.asm;

        assert_eq!(
            vec![
                AsmX32::Mov(
                    Place::Register("eax".into()),
                    AsmValue::Const(20, Type::Doubleword)
                ),
                AsmX32::Or(
                    Place::Register("eax".into()),
                    AsmValue::Const(10, Type::Doubleword)
                )
            ],
            asm
        )
    }

    #[test]
    fn assign_var_then_bit_or_const() {
        let mut trans = X64Backend::new();
        trans.save(0, Type::Doubleword, Some(Value::Const(10)));
        trans.bit_or(1, Type::Doubleword, Value::Ref(0), Value::Const(20));
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
                AsmX32::Or(
                    Place::Register("eax".into()),
                    AsmValue::Const(20, Type::Doubleword)
                )
            ],
            asm
        );
    }

    #[test]
    fn bit_or_var_bit_or_var() {
        let mut trans = X64Backend::new();
        trans.save(0, Type::Doubleword, Some(Value::Const(10)));
        trans.save(1, Type::Doubleword, Some(Value::Const(20)));
        trans.bit_or(1, Type::Doubleword, Value::Ref(0), Value::Ref(1));
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
                AsmX32::Or(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Stack(8, Type::Doubleword)),
                )
            ],
            asm
        );
    }

    #[test]
    fn bit_or_var_bit_or_var_3_times() {
        let mut trans = X64Backend::new();
        trans.save(0, Type::Doubleword, Some(Value::Const(10)));
        trans.bit_or(1, Type::Doubleword, Value::Ref(0), Value::Ref(0));
        trans.bit_or(2, Type::Doubleword, Value::Ref(1), Value::Ref(0));
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
                AsmX32::Or(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Stack(4, Type::Doubleword)),
                ),
                AsmX32::Or(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Stack(4, Type::Doubleword)),
                )
            ],
            asm
        );
    }

    #[test]
    fn bit_or_var_and_var_then_bit_or_the_result_and_itself() {
        let mut trans = X64Backend::new();
        trans.save(0, Type::Doubleword, Some(Value::Const(10)));
        trans.bit_or(1, Type::Doubleword, Value::Ref(0), Value::Ref(0));
        trans.bit_or(2, Type::Doubleword, Value::Ref(1), Value::Ref(1));
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
                AsmX32::Or(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Stack(4, Type::Doubleword)),
                ),
                AsmX32::Or(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Register("eax".into())),
                )
            ],
            asm
        );
    }

    #[test]
    fn bit_or_const_to_const_quadword() {
        let mut trans = X64Backend::new();
        trans.bit_or(0, Type::Quadword, Value::Const(10), Value::Const(20));
        let asm = trans.asm;

        assert_eq!(
            vec![
                AsmX32::Mov(
                    Place::Register("rax".into()),
                    AsmValue::Const(20, Type::Quadword)
                ),
                AsmX32::Or(
                    Place::Register("rax".into()),
                    AsmValue::Const(10, Type::Quadword)
                )
            ],
            asm
        )
    }

    #[test]
    fn bit_xor_const_and_const() {
        let mut trans = X64Backend::new();
        trans.bit_xor(0, Type::Doubleword, Value::Const(10), Value::Const(20));
        let asm = trans.asm;

        assert_eq!(
            vec![
                AsmX32::Mov(
                    Place::Register("eax".into()),
                    AsmValue::Const(20, Type::Doubleword)
                ),
                AsmX32::Xor(
                    Place::Register("eax".into()),
                    AsmValue::Const(10, Type::Doubleword)
                )
            ],
            asm
        )
    }

    #[test]
    fn assign_var_then_bit_xor_const() {
        let mut trans = X64Backend::new();
        trans.save(0, Type::Doubleword, Some(Value::Const(10)));
        trans.bit_xor(1, Type::Doubleword, Value::Ref(0), Value::Const(20));
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
                AsmX32::Xor(
                    Place::Register("eax".into()),
                    AsmValue::Const(20, Type::Doubleword)
                )
            ],
            asm
        );
    }

    #[test]
    fn bit_xor_var_bit_xor_var() {
        let mut trans = X64Backend::new();
        trans.save(0, Type::Doubleword, Some(Value::Const(10)));
        trans.save(1, Type::Doubleword, Some(Value::Const(20)));
        trans.bit_xor(1, Type::Doubleword, Value::Ref(0), Value::Ref(1));
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
                AsmX32::Xor(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Stack(8, Type::Doubleword)),
                )
            ],
            asm
        );
    }

    #[test]
    fn bit_xor_var_bit_xor_var_3_times() {
        let mut trans = X64Backend::new();
        trans.save(0, Type::Doubleword, Some(Value::Const(10)));
        trans.bit_xor(1, Type::Doubleword, Value::Ref(0), Value::Ref(0));
        trans.bit_xor(2, Type::Doubleword, Value::Ref(1), Value::Ref(0));
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
                AsmX32::Xor(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Stack(4, Type::Doubleword)),
                ),
                AsmX32::Xor(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Stack(4, Type::Doubleword)),
                )
            ],
            asm
        );
    }

    #[test]
    fn bit_xor_var_and_var_then_bit_xor_the_result_and_itself() {
        let mut trans = X64Backend::new();
        trans.save(0, Type::Doubleword, Some(Value::Const(10)));
        trans.bit_xor(1, Type::Doubleword, Value::Ref(0), Value::Ref(0));
        trans.bit_xor(2, Type::Doubleword, Value::Ref(1), Value::Ref(1));
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
                AsmX32::Xor(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Stack(4, Type::Doubleword)),
                ),
                AsmX32::Xor(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Register("eax".into())),
                )
            ],
            asm
        );
    }

    #[test]
    fn bit_xor_const_to_const_quadword() {
        let mut trans = X64Backend::new();
        trans.bit_xor(0, Type::Quadword, Value::Const(10), Value::Const(20));
        let asm = trans.asm;

        assert_eq!(
            vec![
                AsmX32::Mov(
                    Place::Register("rax".into()),
                    AsmValue::Const(20, Type::Quadword)
                ),
                AsmX32::Xor(
                    Place::Register("rax".into()),
                    AsmValue::Const(10, Type::Quadword)
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
                    AsmValue::Const(10, Type::Doubleword)
                ),
                AsmX32::Sub(
                    Place::Register("eax".into()),
                    AsmValue::Const(20, Type::Doubleword)
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
    fn sub_var_and_var_3_times() {
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
                    AsmValue::Const(10, Type::Quadword)
                ),
                AsmX32::Sub(
                    Place::Register("rax".into()),
                    AsmValue::Const(20, Type::Quadword)
                )
            ],
            asm
        )
    }

    #[test]
    fn mul_const_to_const() {
        let mut trans = X64Backend::new();
        trans.mul(0, Type::Doubleword, Value::Const(10), Value::Const(20));
        let asm = trans.asm;

        assert_eq!(
            vec![
                AsmX32::Mov(
                    Place::Register("eax".into()),
                    AsmValue::Const(20, Type::Doubleword)
                ),
                AsmX32::Mul(
                    Place::Register("eax".into()),
                    AsmValue::Const(10, Type::Doubleword)
                )
            ],
            asm
        )
    }

    #[test]
    fn assign_var_then_mul_const() {
        let mut trans = X64Backend::new();
        trans.save(0, Type::Doubleword, Some(Value::Const(10)));
        trans.mul(1, Type::Doubleword, Value::Ref(0), Value::Const(20));
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
                AsmX32::Mul(
                    Place::Register("eax".into()),
                    AsmValue::Const(20, Type::Doubleword)
                )
            ],
            asm
        );
    }

    #[test]
    fn mul_var_and_var() {
        let mut trans = X64Backend::new();
        trans.save(0, Type::Doubleword, Some(Value::Const(10)));
        trans.save(1, Type::Doubleword, Some(Value::Const(20)));
        trans.mul(1, Type::Doubleword, Value::Ref(0), Value::Ref(1));
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
                AsmX32::Mul(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Stack(8, Type::Doubleword)),
                )
            ],
            asm
        );
    }

    #[test]
    fn mul_var_mul_var_3_times() {
        let mut trans = X64Backend::new();
        trans.save(0, Type::Doubleword, Some(Value::Const(10)));
        trans.mul(1, Type::Doubleword, Value::Ref(0), Value::Ref(0));
        trans.mul(2, Type::Doubleword, Value::Ref(1), Value::Ref(0));
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
                AsmX32::Mul(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Stack(4, Type::Doubleword)),
                ),
                AsmX32::Mul(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Stack(4, Type::Doubleword)),
                )
            ],
            asm
        );
    }

    #[test]
    fn mul_var_and_var_then_mul_the_result_and_itself() {
        let mut trans = X64Backend::new();
        trans.save(0, Type::Doubleword, Some(Value::Const(10)));
        trans.mul(1, Type::Doubleword, Value::Ref(0), Value::Ref(0));
        trans.mul(2, Type::Doubleword, Value::Ref(1), Value::Ref(1));
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
                AsmX32::Mul(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Stack(4, Type::Doubleword)),
                ),
                AsmX32::Mul(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Register("eax".into())),
                )
            ],
            asm
        );
    }

    #[test]
    fn mul_const_to_const_quadword() {
        let mut trans = X64Backend::new();
        trans.mul(0, Type::Quadword, Value::Const(10), Value::Const(20));
        let asm = trans.asm;

        assert_eq!(
            vec![
                AsmX32::Mov(
                    Place::Register("rax".into()),
                    AsmValue::Const(20, Type::Quadword)
                ),
                AsmX32::Mul(
                    Place::Register("rax".into()),
                    AsmValue::Const(10, Type::Quadword)
                )
            ],
            asm
        )
    }

    #[test]
    fn div_const_to_const() {
        let mut trans = X64Backend::new();
        trans.div(0, Type::Doubleword, Value::Const(20), Value::Const(10));
        let asm = trans.asm;

        assert_eq!(
            vec![
                AsmX32::Mov(
                    Place::Register("eax".into()),
                    AsmValue::Const(20, Type::Doubleword)
                ),
                AsmX32::Convert(Type::Doubleword),
                AsmX32::Mov(
                    Place::Register("ecx".into()),
                    AsmValue::Const(10, Type::Doubleword)
                ),
                AsmX32::Div(
                    Place::Register("ecx".into()),
                )
            ],
            asm
        )
    }

    #[test]
    fn assign_var_then_div_const() {
        let mut trans = X64Backend::new();
        trans.save(0, Type::Doubleword, Some(Value::Const(20)));
        trans.div(1, Type::Doubleword, Value::Ref(0), Value::Const(10));
        let asm = trans.asm;

        assert_eq!(
            vec![
                AsmX32::Mov(
                    Place::Stack(4, Type::Doubleword),
                    AsmValue::Const(20, Type::Doubleword)
                ),
                AsmX32::Mov(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Stack(4, Type::Doubleword)),
                ),
                AsmX32::Convert(Type::Doubleword),
                AsmX32::Mov(
                    Place::Register("ecx".into()),
                    AsmValue::Const(10, Type::Doubleword),
                ),    
                AsmX32::Div(
                    Place::Register("ecx".into()),
                )
            ],
            asm
        );
    }

    #[test]
    fn div_var_and_var() {
        let mut trans = X64Backend::new();
        trans.save(0, Type::Doubleword, Some(Value::Const(20)));
        trans.save(1, Type::Doubleword, Some(Value::Const(10)));
        trans.div(1, Type::Doubleword, Value::Ref(0), Value::Ref(1));
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
                AsmX32::Convert(Type::Doubleword),
                AsmX32::Div(
                    Place::Stack(8, Type::Doubleword),
                )
            ],
            asm
        );
    }

    #[test]
    fn div_var_div_var_3_times() {
        let mut trans = X64Backend::new();
        trans.save(0, Type::Doubleword, Some(Value::Const(10)));
        trans.div(1, Type::Doubleword, Value::Ref(0), Value::Ref(0));
        trans.div(2, Type::Doubleword, Value::Ref(1), Value::Ref(0));
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
                AsmX32::Convert(Type::Doubleword),
                AsmX32::Div(
                    Place::Stack(4, Type::Doubleword),
                ),
                AsmX32::Convert(Type::Doubleword),
                AsmX32::Div(
                    Place::Stack(4, Type::Doubleword),
                )
            ],
            asm
        );
    }

    #[test]
    fn div_var_and_var_then_div_the_result_and_itself() {
        let mut trans = X64Backend::new();
        trans.save(0, Type::Doubleword, Some(Value::Const(10)));
        trans.div(1, Type::Doubleword, Value::Ref(0), Value::Ref(0));
        trans.div(2, Type::Doubleword, Value::Ref(1), Value::Ref(1));
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
                AsmX32::Convert(Type::Doubleword),
                AsmX32::Div(
                    Place::Stack(4, Type::Doubleword),
                ),
                AsmX32::Convert(Type::Doubleword),
                AsmX32::Div(
                    Place::Register("eax".into()),
                )
            ],
            asm
        );
    }

    #[test]
    fn div_const_to_const_quadword() {
        let mut trans = X64Backend::new();
        trans.div(0, Type::Quadword, Value::Const(20), Value::Const(10));
        let asm = trans.asm;

        assert_eq!(
            vec![
                AsmX32::Mov(
                    Place::Register("rax".into()),
                    AsmValue::Const(20, Type::Quadword),
                ),
                AsmX32::Convert(Type::Quadword),
                AsmX32::Mov(
                    Place::Register("rcx".into()),
                    AsmValue::Const(10, Type::Quadword),
                ),
                AsmX32::Div(
                    Place::Register("rcx".into()),
                )
            ],
            asm
        )
    }

    #[test]
    fn mod_const_to_const() {
        let mut trans = X64Backend::new();
        trans.div_reminder(0, Type::Doubleword, Value::Const(10), Value::Const(20));
        let asm = trans.asm;

        assert_eq!(
            vec![
                AsmX32::Mov(
                    Place::Register("eax".into()),
                    AsmValue::Const(10, Type::Doubleword)
                ),
                AsmX32::Convert(Type::Doubleword),
                AsmX32::Mov(
                    Place::Register("ecx".into()),
                    AsmValue::Const(20, Type::Doubleword),
                ),
                AsmX32::Div(
                    Place::Register("ecx".into()),
                ),
                AsmX32::Mov(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Register("edx".into())),
                ),
            ],
            asm
        )
    }

    #[test]
    fn assign_var_then_mod_const() {
        let mut trans = X64Backend::new();
        trans.save(0, Type::Doubleword, Some(Value::Const(10)));
        trans.div_reminder(1, Type::Doubleword, Value::Ref(0), Value::Const(20));
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
                AsmX32::Convert(Type::Doubleword),
                AsmX32::Mov(
                    Place::Register("ecx".into()),
                    AsmValue::Const(20, Type::Doubleword),
                ),
                AsmX32::Div(
                    Place::Register("ecx".into()),
                ),
                AsmX32::Mov(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Register("edx".into())),
                ),
            ],
            asm
        );
    }

    #[test]
    fn mod_var_and_var() {
        let mut trans = X64Backend::new();
        trans.save(0, Type::Doubleword, Some(Value::Const(10)));
        trans.save(1, Type::Doubleword, Some(Value::Const(20)));
        trans.div_reminder(1, Type::Doubleword, Value::Ref(0), Value::Ref(1));
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
                AsmX32::Convert(Type::Doubleword),
                AsmX32::Div(
                    Place::Stack(8, Type::Doubleword),
                ),
                AsmX32::Mov(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Register("edx".into())),
                ),
            ],
            asm
        );
    }

    #[test]
    fn mod_var_and_var_3_times() {
        let mut trans = X64Backend::new();
        trans.save(0, Type::Doubleword, Some(Value::Const(10)));
        trans.div_reminder(1, Type::Doubleword, Value::Ref(0), Value::Ref(0));
        trans.div_reminder(2, Type::Doubleword, Value::Ref(1), Value::Ref(0));
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
                AsmX32::Convert(Type::Doubleword),
                AsmX32::Div(
                    Place::Stack(4, Type::Doubleword),
                ),
                AsmX32::Mov(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Register("edx".into())),
                ),
                AsmX32::Convert(Type::Doubleword),
                AsmX32::Div(
                    Place::Stack(4, Type::Doubleword),
                ),
                AsmX32::Mov(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Register("edx".into())),
                ),
            ],
            asm
        );
    }

    #[test]
    fn mod_var_and_var_then_mod_the_result_and_itself() {
        let mut trans = X64Backend::new();
        trans.save(0, Type::Doubleword, Some(Value::Const(10)));
        trans.div_reminder(1, Type::Doubleword, Value::Ref(0), Value::Ref(0));
        trans.div_reminder(2, Type::Doubleword, Value::Ref(1), Value::Ref(1));
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
                AsmX32::Convert(Type::Doubleword),
                AsmX32::Div(
                    Place::Stack(4, Type::Doubleword),
                ),
                AsmX32::Mov(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Register("edx".into())),
                ),
                AsmX32::Convert(Type::Doubleword),
                AsmX32::Div(
                    Place::Register("eax".into()),
                ),
                AsmX32::Mov(
                    Place::Register("eax".into()),
                    AsmValue::Place(Place::Register("edx".into())),
                ),
            ],
            asm
        );
    }

    #[test]
    fn mod_const_to_const_quadword() {
        let mut trans = X64Backend::new();
        trans.div_reminder(0, Type::Quadword, Value::Const(10), Value::Const(20));
        let asm = trans.asm;

        assert_eq!(
            vec![
                AsmX32::Mov(
                    Place::Register("rax".into()),
                    AsmValue::Const(10, Type::Quadword),
                ),
                AsmX32::Convert(Type::Quadword),
                AsmX32::Mov(
                    Place::Register("rcx".into()),
                    AsmValue::Const(20, Type::Quadword),
                ),
                AsmX32::Div(
                    Place::Register("rcx".into()),
                ),
                AsmX32::Mov(
                    Place::Register("rax".into()),
                    AsmValue::Place(Place::Register("rdx".into())),
                ),
            ],
            asm
        )
    }

    #[test]
    fn equal_operation() {
        let mut trans = X64Backend::new();
        trans.save(0, Type::Doubleword, Some(Value::Const(10)));
        trans.eq(0, Type::Doubleword, Value::Ref(0), Value::Const(20));
        let asm = trans.asm;

        assert_eq!(
            vec![
                AsmX32::Mov(
                    Place::Stack(4, Type::Doubleword),
                    AsmValue::Const(10, Type::Doubleword),
                ),
                AsmX32::Mov(
                    Place::Register(Register::new("eax")),
                    AsmValue::Place(Place::Stack(4, Type::Doubleword))
                ),
                AsmX32::Cmp(
                    Place::Register(Register::new("eax")),
                    AsmValue::Const(20, Type::Doubleword)
                ),
                AsmX32::Sete(Place::Register(Register::new("al"))),
                AsmX32::Movzx(
                    Place::Register(Register::new("eax")),
                    AsmValue::Place(Place::Register(Register::new("al")))
                )
            ],
            asm
        )
    }

    #[test]
    fn equal_operation_consts() {
        let mut trans = X64Backend::new();
        trans.eq(0, Type::Doubleword, Value::Const(10), Value::Const(20));
        let asm = trans.asm;

        assert_eq!(
            vec![
                AsmX32::Mov(
                    Place::Register(Register::new("eax")),
                    AsmValue::Const(20, Type::Doubleword)
                ),
                AsmX32::Cmp(
                    Place::Register(Register::new("eax")),
                    AsmValue::Const(10, Type::Doubleword)
                ),
                AsmX32::Sete(Place::Register(Register::new("al"))),
                AsmX32::Movzx(
                    Place::Register(Register::new("eax")),
                    AsmValue::Place(Place::Register(Register::new("al")))
                )
            ],
            asm
        )
    }

    #[test]
    fn not_equal_operation() {
        let mut trans = X64Backend::new();
        trans.save(0, Type::Doubleword, Some(Value::Const(10)));
        trans.not_eq(0, Type::Doubleword, Value::Ref(0), Value::Const(20));
        let asm = trans.asm;

        assert_eq!(
            vec![
                AsmX32::Mov(
                    Place::Stack(4, Type::Doubleword),
                    AsmValue::Const(10, Type::Doubleword),
                ),
                AsmX32::Mov(
                    Place::Register(Register::new("eax")),
                    AsmValue::Place(Place::Stack(4, Type::Doubleword))
                ),
                AsmX32::Cmp(
                    Place::Register(Register::new("eax")),
                    AsmValue::Const(20, Type::Doubleword)
                ),
                AsmX32::Setne(Place::Register(Register::new("al"))),
                AsmX32::Movzx(
                    Place::Register(Register::new("eax")),
                    AsmValue::Place(Place::Register(Register::new("al")))
                )
            ],
            asm
        )
    }

    #[test]
    fn not_equal_operation_consts() {
        let mut trans = X64Backend::new();
        trans.not_eq(0, Type::Doubleword, Value::Const(10), Value::Const(20));
        let asm = trans.asm;

        assert_eq!(
            vec![
                AsmX32::Mov(
                    Place::Register(Register::new("eax")),
                    AsmValue::Const(20, Type::Doubleword)
                ),
                AsmX32::Cmp(
                    Place::Register(Register::new("eax")),
                    AsmValue::Const(10, Type::Doubleword)
                ),
                AsmX32::Setne(Place::Register(Register::new("al"))),
                AsmX32::Movzx(
                    Place::Register(Register::new("eax")),
                    AsmValue::Place(Place::Register(Register::new("al")))
                )
            ],
            asm
        )
    }

    #[test]
    fn var_less_const() {
        let mut trans = X64Backend::new();
        trans.save(0, Type::Doubleword, Some(Value::Const(0)));
        trans.lt(1, Type::Doubleword, Value::Ref(0), Value::Const(1));
        let asm = trans.asm;

        assert_eq!(
            vec![
                AsmX32::Mov(
                    Place::Stack(4, Type::Doubleword),
                    AsmValue::Const(0, Type::Doubleword)
                ),
                AsmX32::Cmp(
                    Place::Stack(4, Type::Doubleword),
                    AsmValue::Const(1, Type::Doubleword)
                ),
                AsmX32::Setl(Place::Register(Register::new("al"))),
                AsmX32::And(Place::Register(Register::new("al")), AsmValue::Const(1, Type::Doubleword)),
                AsmX32::Movzx(
                    Place::Register(Register::new("eax")),
                    AsmValue::Place(Place::Register(Register::new("al")))
                )
            ],
            asm
        )
    }

    #[test]
    fn const_less_var() {
        let mut trans = X64Backend::new();
        trans.save(0, Type::Doubleword, Some(Value::Const(0)));
        trans.lt(1, Type::Doubleword, Value::Const(1), Value::Ref(0));
        let asm = trans.asm;

        assert_eq!(
            vec![
                AsmX32::Mov(
                    Place::Stack(4, Type::Doubleword),
                    AsmValue::Const(0, Type::Doubleword)
                ),
                AsmX32::Mov(
                    Place::Register(Register::new("eax")),
                    AsmValue::Const(1, Type::Doubleword),
                ),
                AsmX32::Cmp(
                    Place::Register(Register::new("eax")),
                    AsmValue::Place(Place::Stack(4, Type::Doubleword)),
                ),
                AsmX32::Setl(Place::Register(Register::new("al"))),
                AsmX32::And(Place::Register(Register::new("al")), AsmValue::Const(1, Type::Doubleword)),
                AsmX32::Movzx(
                    Place::Register(Register::new("eax")),
                    AsmValue::Place(Place::Register(Register::new("al")))
                )
            ],
            asm
        )
    }

    #[test]
    fn var_less_var() {
        let mut trans = X64Backend::new();
        trans.save(0, Type::Doubleword, Some(Value::Const(0)));
        trans.save(1, Type::Doubleword, Some(Value::Const(1)));
        trans.lt(1, Type::Doubleword, Value::Ref(0), Value::Ref(1));
        let asm = trans.asm;

        assert_eq!(
            vec![
                AsmX32::Mov(
                    Place::Stack(4, Type::Doubleword),
                    AsmValue::Const(0, Type::Doubleword)
                ),
                AsmX32::Mov(
                    Place::Stack(8, Type::Doubleword),
                    AsmValue::Const(1, Type::Doubleword)
                ),
                AsmX32::Mov(
                    Place::Register(Register::new("eax")),
                    AsmValue::Place(Place::Stack(4, Type::Doubleword)),
                ),
                AsmX32::Cmp(
                    Place::Register(Register::new("eax")),
                    AsmValue::Place(Place::Stack(8, Type::Doubleword)),
                ),
                AsmX32::Setl(Place::Register(Register::new("al"))),
                AsmX32::And(Place::Register(Register::new("al")), AsmValue::Const(1, Type::Doubleword)),
                AsmX32::Movzx(
                    Place::Register(Register::new("eax")),
                    AsmValue::Place(Place::Register(Register::new("al")))
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
    fn neg_doubleword_var() {
        let mut trans = X64Backend::new();
        trans.save(0, Type::Doubleword, Some(Value::Const(10)));
        trans.neg(1, 0);
        let asm = trans.asm;

        assert_eq!(
            vec![
                AsmX32::Mov(
                    Place::Stack(4, Type::Doubleword),
                    AsmValue::Const(10, Type::Doubleword)
                ),
                AsmX32::Mov(
                    Place::Register(Register::new("eax")),
                    AsmValue::Place(Place::Stack(4, Type::Doubleword)),
                ),
                AsmX32::Neg(Place::Register(Register::new("eax"))),
                AsmX32::Mov(
                    Place::Stack(8, Type::Doubleword),
                    AsmValue::Place(Place::Register(Register::new("eax"))),
                ),
            ],
            asm
        );
    }

    #[test]
    fn bitwise_doubleword_var() {
        let mut trans = X64Backend::new();
        trans.save(0, Type::Doubleword, Some(Value::Const(10)));
        trans.bitwise(1, 0);
        let asm = trans.asm;

        assert_eq!(
            vec![
                AsmX32::Mov(
                    Place::Stack(4, Type::Doubleword),
                    AsmValue::Const(10, Type::Doubleword)
                ),
                AsmX32::Mov(
                    Place::Register(Register::new("eax")),
                    AsmValue::Place(Place::Stack(4, Type::Doubleword)),
                ),
                AsmX32::Not(Place::Register(Register::new("eax"))),
                AsmX32::Mov(
                    Place::Stack(8, Type::Doubleword),
                    AsmValue::Place(Place::Register(Register::new("eax"))),
                ),
            ],
            asm
        );
    }

    #[test]
    fn logical_neg_doubleword_var() {
        let mut trans = X64Backend::new();
        trans.save(0, Type::Doubleword, Some(Value::Const(10)));
        trans.logical_neg(1, 0);
        let asm = trans.asm;

        assert_eq!(
            vec![
                AsmX32::Mov(
                    Place::Stack(4, Type::Doubleword),
                    AsmValue::Const(10, Type::Doubleword)
                ),
                AsmX32::Cmp(Place::Stack(4, Type::Doubleword), AsmValue::Const(0, Type::Doubleword)),
                AsmX32::Setne(Place::Register(Register::new("al"))),
                AsmX32::Movzx(
                    Place::Register(Register::new("eax")),
                    AsmValue::Place(Place::Register(Register::new("al"))),
                ),
                AsmX32::Mov(
                    Place::Stack(8, Type::Doubleword),
                    AsmValue::Place(Place::Register(Register::new("eax"))),
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
