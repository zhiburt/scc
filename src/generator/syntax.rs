#[derive(PartialEq, Eq, Debug)]
pub enum AsmInstruction {
    Metadata(String),
    Label(String),
    Mov(Place, Value),
    Add(Place, Value),
    Push(Value),
    Pop(Place),
    Ret,
}

#[derive(PartialEq, Eq, Debug)]
pub enum Value {
    Const(i64),
    Place(Place),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Place {
    Stack(u64),
    Register(Register),
}

pub type Register = String;

#[derive(Debug)]
pub struct IList(Vec<AsmInstruction>);

impl IList {
    pub fn new() -> Self {
        IList(Vec::new())
    }

    pub fn extend(&mut self, l: IList) {
        self.0.extend(l.0);
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
