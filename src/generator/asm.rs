use super::syntax::GASM;
use std::collections::HashMap;

pub struct Assembly {
    pub funcs: HashMap<String, Func>,
    pub data: Block,
}

impl Assembly {
    pub fn new() -> Self {
        Self {
            funcs: HashMap::new(),
            data: Block::new(),
        }
    }

    pub fn emit_function(&mut self, name: &str, code: Vec<Block>) {
        self.funcs.insert(name.to_owned(), Func::new(code));
    }

    pub fn set_data(&mut self, data: Block) {
        self.data = data;
    }

    pub fn code(&self) -> String {
        let mut buf = String::new();
        for i in self.data.into_iter() {
            buf.push_str(&GASM::translate(i));
            buf.push('\n');
        }

        for func in self.funcs.values() {
            for i in func.instructions() {
                buf.push_str(&GASM::translate(i));
                buf.push('\n');
            }

            buf.push('\n');
        }

        buf
    }
}

pub struct Func {
    pub(super) blocks: Vec<Block>,
}

impl Func {
    fn new(code: Vec<Block>) -> Self {
        Self { blocks: code }
    }

    pub fn instructions(&self) -> impl Iterator<Item = &Line> {
        self.blocks.iter().map(|b| b.into_iter()).flatten()
    }
}

pub struct Block {
    pub code: Vec<Line>,
}

impl Block {
    pub fn new() -> Self {
        Self { code: Vec::new() }
    }

    pub fn emit(&mut self, i: AsmX32) {
        self.code.push(Line::Instruction(i));
    }

    pub fn emit_directive(&mut self, s: &str) {
        self.code.push(Line::Directive(s.to_owned()));
    }

    pub fn emit_label(&mut self, s: &str) {
        self.code.push(Line::Label(s.to_owned()));
    }
}

impl std::ops::AddAssign<Block> for Block {
    fn add_assign(&mut self, other: Self) {
        self.code.extend(other.code);
    }
}

impl std::ops::AddAssign<Line> for Block {
    fn add_assign(&mut self, i: Line) {
        self.code.push(i);
    }
}

impl<'a> IntoIterator for &'a Block {
    type Item = &'a Line;
    type IntoIter = std::slice::Iter<'a, Line>;

    fn into_iter(self) -> Self::IntoIter {
        self.code.iter()
    }
}
pub enum Line {
    Instruction(AsmX32),
    Directive(Directive),
    Label(Label),
}

impl Line {
    pub fn instruction_mut(&mut self) -> Option<&mut AsmX32> {
        match self {
            Line::Instruction(i) => Some(i),
            _ => None,
        }
    }
}

pub enum AsmX32 {
    Metadata(String),
    Label(String),
    Mov(Place, Value),
    Movzx(Place, Value),
    And(Place, Value),
    Or(Place, Value),
    Xor(Place, Value),
    Add(Place, Value),
    Sub(Place, Value),
    Mul(Place, Value),
    Imul(Const, Value, Register),
    Div(Place),
    Neg(Place),
    Not(Place),
    Convert(Size),
    Sete(Place),
    Setne(Place),
    Setl(Place),
    Setle(Place),
    Setg(Place),
    Setge(Place),
    Jmp(String),
    Je(String),
    Jne(String),
    Cmp(Place, Value),
    Push(Value),
    Pop(Place),
    Call(String),
    Ret,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Register(Register),
    Indirect(Indirect),
    Const(Const),
    Static(Label, Size),
}

impl Value {
    pub fn size(&self) -> Size {
        match self {
            Self::Register(Register::Register(..)) => Size::Quadword,
            Self::Register(Register::Sub(.., Part::Doubleword)) => Size::Doubleword,
            Self::Register(Register::Sub(.., Part::Word)) => Size::Word,
            Self::Register(Register::Sub(.., Part::Byte)) => Size::Byte,
            Self::Indirect(Indirect { size, .. }) => size.clone(),
            Self::Const(..) => Size::Doubleword,
            Self::Static(.., size) => size.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Place {
    Register(Register),
    Indirect(Indirect),
    Static(Label, Size),
}

impl Place {
    pub fn size(&self) -> Size {
        match self {
            Self::Register(Register::Register(..)) => Size::Quadword,
            Self::Register(Register::Sub(.., Part::Doubleword)) => Size::Doubleword,
            Self::Register(Register::Sub(.., Part::Word)) => Size::Word,
            Self::Register(Register::Sub(.., Part::Byte)) => Size::Byte,
            Self::Indirect(Indirect { size, .. }) => size.clone(),
            Self::Static(.., size) => size.clone(),
        }
    }
}

pub type Const = i32;
pub type Label = String;
pub type Directive = String;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Register {
    Register(RegisterX64),
    Sub(RegisterX64, Part),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Indirect {
    pub reg: Register,
    pub offset: Offset,
    pub size: Size,
}

impl Indirect {
    pub fn new(reg: Register, offset: usize, size: Size) -> Self {
        Self {
            offset: Offset::Static(offset),
            reg,
            size,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Offset {
    Static(usize),
    Label(Label),
}

#[derive(Debug, Clone, PartialEq, Eq, std::hash::Hash)]
pub enum RegisterX64 {
    RAX,
    RBX,
    RCX,
    RDX,
    RSI,
    RDI,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    RSP,
    RBP,
    RIP,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Size {
    Quadword,
    Doubleword,
    Word,
    Byte,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Part {
    Doubleword,
    Word,
    Byte,
}

impl std::fmt::Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Sub(reg, part) => {
                use Part::*;
                use RegisterX64::*;
                let reg = match reg {
                    RAX => match part {
                        Doubleword => "eax",
                        Word => "ax",
                        Byte => "al",
                    },
                    RBX => match part {
                        Doubleword => "ebx",
                        Word => "bx",
                        Byte => "bl",
                    },
                    RCX => match part {
                        Doubleword => "ecx",
                        Word => "cx",
                        Byte => "cl",
                    },
                    RDX => match part {
                        Doubleword => "edx",
                        Word => "dx",
                        Byte => "dl",
                    },
                    RSI => match part {
                        Doubleword => "esi",
                        Word => "si",
                        Byte => "sil",
                    },
                    RDI => match part {
                        Doubleword => "edi",
                        Word => "di",
                        Byte => "dil",
                    },
                    RBP => match part {
                        Doubleword => "ebp",
                        Word => "bp",
                        Byte => "bpl",
                    },
                    RSP => match part {
                        Doubleword => "esp",
                        Word => "sp",
                        Byte => "spl",
                    },
                    R8 => match part {
                        Doubleword => "r8d",
                        Word => "r8w",
                        Byte => "r8b",
                    },
                    R9 => match part {
                        Doubleword => "r9d",
                        Word => "r9w",
                        Byte => "r9b",
                    },
                    R10 => match part {
                        Doubleword => "r10d",
                        Word => "r10w",
                        Byte => "r10b",
                    },
                    R11 => match part {
                        Doubleword => "r11d",
                        Word => "r11w",
                        Byte => "r11b",
                    },
                    R12 => match part {
                        Doubleword => "r12d",
                        Word => "r12w",
                        Byte => "r12b",
                    },
                    R13 => match part {
                        Doubleword => "r13d",
                        Word => "r13w",
                        Byte => "r13b",
                    },
                    R14 => match part {
                        Doubleword => "r14d",
                        Word => "r14w",
                        Byte => "r14b",
                    },
                    R15 => match part {
                        Doubleword => "r15d",
                        Word => "r15w",
                        Byte => "r15b",
                    },
                    RIP => match part {
                        Doubleword => "eip",
                        _ => unreachable!(),
                    },
                };

                write!(f, "{}", reg)
            }
            Self::Register(reg) => write!(f, "{}", format!("{:?}", reg).to_lowercase()),
        }
    }
}

impl Register {
    pub fn base(&self) -> RegisterX64 {
        match self {
            Self::Sub(reg, ..) | Self::Register(reg) => reg.clone(),
        }
    }
}

impl Into<Value> for Place {
    fn into(self) -> Value {
        match self {
            Place::Indirect(i) => Value::Indirect(i),
            Place::Register(reg) => Value::Register(reg),
            Place::Static(label, size) => Value::Static(label, size),
        }
    }
}
