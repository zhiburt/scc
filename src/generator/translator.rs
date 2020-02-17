pub type Id = u32;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Value {
    Const(i64),
    Ref(Id),
}

impl Value {
    pub fn is_ref(&self) -> bool {
        match self {
            Value::Ref(..) => true,
            Value::Const(..) => false,
        }
    }

    pub fn is_const(&self) -> bool {
        !self.is_ref()
    }


    pub fn as_ref(self) -> Option<Id> {
        match self {
            Value::Ref(id) => Some(id),
            Value::Const(..) => None,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Type {
    Byte,
    Word,
    Doubleword,
    Quadword,
}

impl Type {
    pub fn size(&self) -> usize {
        match self {
            Type::Doubleword => 4,
            Type::Quadword => 8,
            Type::Word => 2,
            Type::Byte => 1,
        }
    }
}

pub trait Translator {
    fn func_begin(&mut self, name: &str, params: &[(Type, Id)], has_multi_ret: bool, alloc_size: usize);
    fn func_end(&mut self, alloc_size: usize);
    // TODO: might better supply &str instead of usize?
    fn label(&mut self, label: usize);
    fn jump(&mut self, label: usize);
    fn if_goto(&mut self, t: Type, value: Value, label: usize);
    fn save(&mut self, id: Id, t: Type, value: Option<Value>);
    // TODO: investigate the same type
    fn add(&mut self, id: Id, t: Type, a: Value, b: Value);
    fn sub(&mut self, id: Id, t: Type, a: Value, b: Value);
    fn mul(&mut self, id: Id, t: Type, a: Value, b: Value);
    fn div(&mut self, id: Id, t: Type, a: Value, b: Value);
    fn div_reminder(&mut self, id: Id, t: Type, a: Value, b: Value);
    fn bit_and(&mut self, id: Id, t: Type, a: Value, b: Value);
    fn bit_or(&mut self, id: Id, t: Type, a: Value, b: Value);
    fn bit_xor(&mut self, id: Id, t: Type, a: Value, b: Value);
    fn neg(&mut self, id: Id, a: Id);
    fn logical_neg(&mut self, id: Id, a: Id);
    fn bitwise(&mut self, id: Id, a: Id);
    fn eq(&mut self, id: Id, t: Type, a: Value, b: Value);
    fn not_eq(&mut self, id: Id, t: Type, a: Value, b: Value);
    fn lt(&mut self, id: Id, t: Type, a: Value, b: Value);
    fn le(&mut self, id: Id, t: Type, a: Value, b: Value);
    fn gt(&mut self, id: Id, t: Type, a: Value, b: Value);
    fn ge(&mut self, id: Id, t: Type, a: Value, b: Value);
    fn call(&mut self, id: Option<Id>, t: Type, name: &str, params: &[(Type, Value)]);
    // fn mul(&mut self, id: Id, t: Type, a: Value, b: Value);
    // fn div(&mut self, id: Id, t: Type, a: Value, b: Value);
    // fn module(&mut self, id: Id, t: Type, a: Value, b: Value);
    fn ret(&mut self, t: Type, v: Value);
    // fn stash<S: Syntax>(&self, syntax: S);
    fn stash(&mut self) -> String;
}
