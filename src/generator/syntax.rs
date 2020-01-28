#[derive(PartialEq, Eq, Debug)]
pub enum AsmInstruction {
    Metadata(String),
    Label(String),
    Mov(Params),
    Add(Params),
    Sub(Params),
    Mul(Params),
    Div(Params),
    Mod(Params),
    Push(Value),
    Pop(Place),
    Ret,
}

#[derive(PartialEq, Eq, Debug)]
pub struct Params {
    place: Place,
    value: Value,
}

impl Params {
    pub fn new(place: Place, value: Value) -> Option<Self> {
        if let Place::Stack(..) = place {
            if let Value::Place(Place::Stack(..)) = value {
                return None;
            }
        }

        Some(Params { place, value })
    }

    pub fn place(&self) -> &Place {
        &self.place
    }

    pub fn value(&self) -> &Value {
        &self.value
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Value {
    Const(i64, Type),
    Place(Place),
}

impl Value {
    pub fn as_place(self) -> Option<Place> {
        match self {
            Value::Place(p) => Some(p),
            Value::Const(..) => None,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Place {
    Stack(u64, Type),
    Register(Register),
}

pub type Register = &'static str;

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

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Type {
    Byte,
    Word,
    Doubleword,
    Quadword,
}

pub struct X64Memory;

#[derive(Debug)]
pub enum OpType {
    Add,
}

impl X64Memory {
    const REGISTERS: &'static [Register] = &[
        "rax", "rbx", "rcx", "rdx", "rsi", "rdi", "rbp", "rsp", "r8", "r9", "r10", "r11", "r12",
        "r13", "r14", "r15",
    ];

    const LOWER_32BITS: &'static [Register] = &[
        "eax", "ebx", "ecx", "edx", "esi", "edi", "ebp", "esp", "r8d", "r9d", "r10d", "r11d",
        "r12d", "r13d", "r14d", "r15d",
    ];

    pub fn result_in(i: &AsmInstruction) -> Option<Register> {
        match i {
            // TODO: nowadays it supports only one type,
            // but we have to handle multiply types
            //
            // it means that add will store its result in rax
            //
            // example -> for type long long we should store it in `rax`
            //
            AsmInstruction::Add(..) => Some("eax"),
            _ => None,
        }
    }

    pub fn result_of(o: OpType, v: &Value) -> Register {
        match o {
            OpType::Add => match X64Memory::value_type(v) {
                Type::Doubleword => "eax",
                Type::Quadword => "rax",
                _ => unimplemented!(),
            },
        }
    }

    pub fn address_current_frame() -> Register {
        "rsp"
    }

    pub fn address_previous_frame() -> Register {
        "rbp"
    }

    pub fn return_register(v: &Value) -> Register {
        match X64Memory::value_type(v) {
            Type::Doubleword => "eax",
            Type::Quadword => "rax",
            _ => unimplemented!(),
        }
    }

    pub fn value_type(v: &Value) -> Type {
        match v {
            // TODO: at least now it is OK according the fact we support one type
            // but in the nearest it might be managed to be changed
            Value::Const(.., t) => t.clone(),
            Value::Place(p) => X64Memory::place_type(p),
        }
    }

    fn place_type(p: &Place) -> Type {
        match p {
            Place::Stack(.., t) => t.clone(),
            Place::Register(reg) => {
                if X64Memory::is_main_register(reg) {
                    Type::Quadword
                } else {
                    Type::Doubleword
                }
            }
        }
    }

    fn is_main_register(reg: &Register) -> bool {
        for r in X64Memory::REGISTERS {
            if reg == r {
                return true;
            }
        }

        false
    }
}
