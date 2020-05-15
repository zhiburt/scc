use super::syntax::translate;
use std::collections::HashMap;

pub struct Assembly {
    funcs: HashMap<String, Func>,
}

impl Assembly {
    pub fn new() -> Self {
        Self {
            funcs: HashMap::new(),
        }
    }

    pub fn emit_function(&mut self, name: &str, code: Vec<Block>) {
        self.funcs.insert(name.to_owned(), Func::new(code));
    }

    pub fn code(&self) -> String {
        let mut buf = String::new();
        for func in self.funcs.values() {
            for i in func.instructions() {
                buf.push_str(&translate(i));
                buf.push('\n');
            }

            buf.push('\n');
        }

        buf
    }

    pub fn funcs_mut(&mut self) -> impl Iterator<Item = &mut Func> {
        self.funcs.values_mut()
    }
}

pub struct Func {
    pub(super) blocks: Vec<Block>,
}

impl Func {
    fn new(code: Vec<Block>) -> Self {
        Self { blocks: code }
    }

    pub fn instructions(&self) -> impl Iterator<Item = &InstructionLine> {
        self.blocks.iter().map(|b| b.into_iter()).flatten()
    }

    pub fn blocks_mut(&mut self) -> impl Iterator<Item = &mut Block> {
        self.blocks.iter_mut()
    }
}

pub struct Block {
    pub code: Vec<InstructionLine>,
}

impl Block {
    pub fn new() -> Self {
        Self { code: Vec::new() }
    }

    pub fn emit<Line: Into<InstructionLine>>(&mut self, l: Line) {
        self.code.push(l.into());
    }

    pub fn emit_directive(&mut self, s: &str) {
        self.code.push(InstructionLine::Directive(s.to_owned()));
    }

    pub fn emit_label(&mut self, s: &str) {
        self.code.push(InstructionLine::Label(s.to_owned()));
    }
}

impl std::ops::AddAssign<Block> for Block {
    fn add_assign(&mut self, other: Self) {
        self.code.extend(other.code);
    }
}

impl std::ops::AddAssign<InstructionLine> for Block {
    fn add_assign(&mut self, i: InstructionLine) {
        self.code.push(i);
    }
}

impl<'a> IntoIterator for &'a Block {
    type Item = &'a InstructionLine;
    type IntoIter = std::slice::Iter<'a, InstructionLine>;

    fn into_iter(self) -> Self::IntoIter {
        self.code.iter()
    }
}

#[derive(Clone)]
pub enum InstructionLine {
    Instruction(Instruction),
    Directive(Directive),
    Label(Label),
}

impl InstructionLine {
    pub fn instruction_mut(&mut self) -> Option<&mut Instruction> {
        match self {
            InstructionLine::Instruction(i) => Some(i),
            _ => None,
        }
    }
}

impl Into<InstructionLine> for Instruction {
    fn into(self) -> InstructionLine {
        InstructionLine::Instruction(self)
    }
}

#[derive(Clone)]
pub struct Instruction {
    pub mnemonic: &'static str,
    pub args: Vec<Arg>,
}

impl Instruction {
    pub fn new<Iter>(mnemonic: &'static str, args: Iter) -> Self
    where
        Iter: IntoIterator<Item = Arg>,
    {
        Self {
            mnemonic,
            args: args.into_iter().collect(),
        }
    }
}

#[derive(Clone)]
pub enum Arg {
    Register(Register),
    Const(Const),
    Label(Label),
}

impl Into<Arg> for Register {
    fn into(self) -> Arg {
        Arg::Register(self)
    }
}

impl Into<Arg> for Const {
    fn into(self) -> Arg {
        Arg::Const(self)
    }
}

#[derive(Clone)]
pub struct Const(pub(crate) i32);

pub type Label = String;
pub type Directive = String;

pub type MachineRegister = &'static str;

impl Const {
    fn size(&self) -> Size {
        Size::Doubleword
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Register {
    pub(crate) rg: RegisterBackend,
    pub(crate) size: Size,
}

impl Register {
    pub fn as_byte(&self) -> Register {
        match self.rg {
            RegisterBackend::Machine("eax") => Register::machine("al", Size::Byte),
            RegisterBackend::Machine("ebx") => Register::machine("bl", Size::Byte),
            RegisterBackend::Machine("ecx") => Register::machine("cl", Size::Byte),
            RegisterBackend::Machine("edx") => Register::machine("dl", Size::Byte),
            _ => unimplemented!(),
        }
    }

    pub fn as_machine(&self) -> MachineRegister {
        match self.rg {
            RegisterBackend::Machine(reg) => reg,
            _ => unimplemented!(),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum RegisterBackend {
    Machine(MachineRegister),
    StackOffset(usize),
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Size {
    Byte,
    Word,
    Doubleword,
    Quadword,
}

impl Register {
    pub fn new(rg: RegisterBackend, size: Size) -> Self {
        Self { rg, size }
    }

    pub fn machine(r: MachineRegister, size: Size) -> Self {
        Self {
            rg: RegisterBackend::Machine(r),
            size,
        }
    }

    pub fn size(&self) -> Size {
        self.size.clone()
    }

    pub fn base(&self) -> &RegisterBackend {
        &self.rg
    }
}
