use crate::il::tac;
use std::io;

pub fn write<W, Asm, Format>(mut w: W, functions: &[tac::FuncDef])
where
    W: io::Write,
    Asm: AsmTranslator,
    Format: AsmFormatter,
{
    let mut memory = MemoryContext::new(0, &["rax".to_owned(), "rbx".to_owned()]);
    for f in functions {
        let mut list = epilogue();
        list.extend(gen_function(&mut memory, f));
        list.extend(prologue());

        w.write(Format::format_function_metadata(&format!(".globl {}", f.name)).as_bytes())
            .unwrap();
        w.write(Format::format_function_header(&f.name).as_bytes())
            .unwrap();
        for i in list.iter() {
            w.write(Format::format(i, &Asm::translate(&i)).as_bytes())
                .unwrap();
        }
    }
}

fn epilogue() -> Vec<Instruction> {
    vec![
        Instruction::Push(Const::Register("rbp".to_owned())),
        Instruction::Mov(
            Place::Register("rbp".to_owned()),
            Const::Register("rsp".to_owned()),
        ),
    ]
}

fn prologue() -> Vec<Instruction> {
    vec![Instruction::Pop(Place::Register("rbp".to_owned()))]
}

fn gen_function(memory: &mut MemoryContext, function: &tac::FuncDef) -> Vec<Instruction> {
    let mut instructions = Vec::new();
    for i in function.instructions.iter() {
        if let Some(instr) = gen_instruction(memory, i) {
            instructions.push(instr);
        }
    }

    instructions
}

fn gen_instruction(memory: &mut MemoryContext, line: &tac::InstructionLine) -> Option<Instruction> {
    let tac::InstructionLine(instruction, id) = line;
    match instruction {
        tac::Instruction::Alloc(tac::Const::Int(i)) => Some(Instruction::Mov(
            memory.alloc_on_stack(),
            Const::Int(*i as i64),
        )),
        tac::Instruction::ControlOp(tac::ControlOp::Return(id)) =>
        /* Instruction::Ret */
        {
            None
        }
        _ => unimplemented!(),
    }
}

struct MemoryContext {
    stack_index: usize,
    free_registers: Vec<Register>,
}

impl MemoryContext {
    fn new(stack_start: usize, registers: &[Register]) -> Self {
        MemoryContext {
            stack_index: stack_start,
            free_registers: registers.to_vec(),
        }
    }

    fn alloc_on_stack(&mut self) -> Place {
        self.stack_index += 4;
        Place::OnStack(self.stack_index)
    }
}

/*
    TODO: Should be added metadata

        .globl main
    main:

    And label can be general of the function representaion which can be formated by others methods

*/
pub enum Instruction {
    Mov(Place, Const),
    Add(Const, Const),
    Sub(Const, Const),
    Push(Const),
    Pop(Place),
    Ret,
}

pub enum Const {
    Int(i64),
    Place(Place),
    OffsetOf(Register, isize),
    Register(Register),
}

pub enum Place {
    OnStack(StackIndex),
    Register(Register),
    OffsetOf(Register, isize),
}

type StackIndex = usize;
/* TODO: might better to build a register table type */
type Register = String;

pub struct GASM;

impl AsmTranslator for GASM {
    fn translate(i: &Instruction) -> String {
        match i {
            Instruction::Ret => "ret".to_owned(),
            Instruction::Mov(ref to, ref from) => format!(
                "mov {}, {}",
                GASM::translate_place(to),
                GASM::translate_const(from)
            ),
            Instruction::Pop(to) => format!("pop {}", GASM::translate_place(to)),
            Instruction::Push(what) => format!("push {}", GASM::translate_const(what)),
            _ => unimplemented!(),
        }
    }
}

impl GASM {
    fn translate_place(p: &Place) -> String {
        match p {
            Place::Register(reg) => format!("%{}", reg),
            /* TODO: rbp here should be changed to some variable which responcible for the begging of the stack */
            Place::OnStack(offset) => format!("-{}(%rbp)", offset),
            Place::OffsetOf(reg, offset) => format!("{}(%{})", offset, reg),
        }
    }

    fn translate_const(c: &Const) -> String {
        match c {
            Const::Int(v) => format!("${}", v),
            Const::Place(p) => GASM::translate_place(p),
            Const::Register(reg) => format!("%{}", reg),
            Const::OffsetOf(reg, offset) => format!("{}(%{})", offset, reg),
        }
    }
}

struct NASM;

impl AsmTranslator for NASM {
    fn translate(i: &Instruction) -> String {
        "".to_owned()
    }
}

pub trait AsmTranslator {
    fn translate(i: &Instruction) -> String;
}

pub trait AsmFormatter {
    fn format(i: &Instruction, line: &str) -> String;
    fn format_function_header(name: &str) -> String;
    fn format_function_metadata(metadata: &str) -> String;
}

pub struct DefaultFormatter;

impl AsmFormatter for DefaultFormatter {
    fn format(i: &Instruction, line: &str) -> String {
        format!("    {}\n", line)
    }

    fn format_function_header(name: &str) -> String {
        format!("{}:\n", name)
    }

    fn format_function_metadata(metadata: &str) -> String {
        format!("    {}\n", metadata)
    }
}
