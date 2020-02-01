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
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Type {
    Byte,
    Word,
    Doubleword,
    Quadword,
}

pub trait Translator {
    fn func_begin(&mut self, name: &str);
    fn func_end(&mut self);
    // TODO: might better supply &str instead of usize?
    fn label(&mut self, label: usize);
    fn goto(&mut self, label: usize);
    fn save(&mut self, id: Id, t: Type, value: Option<Value>);
    // TODO: investigate the same type
    fn add(&mut self, id: Id, t: Type, a: Value, b: Value);
    // fn mul(&mut self, id: Id, t: Type, a: Value, b: Value);
    // fn div(&mut self, id: Id, t: Type, a: Value, b: Value);
    // fn module(&mut self, id: Id, t: Type, a: Value, b: Value);
    fn ret(&mut self, t: Type, v: Value);
    // fn stash<S: Syntax>(&self, syntax: S);
    fn stash(&self) -> String;
}
