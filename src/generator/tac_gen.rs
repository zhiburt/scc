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
        let instructions = gen_function(&mut memory, f);

        for i in instructions.iter() {
            w.write(Format::format(i, &Asm::translate(&i)).as_bytes())
                .unwrap();
        }
    }
}

fn gen_function(memory: &mut MemoryContext, function: &tac::FuncDef) -> Vec<Instruction> {
    let mut instructions = Vec::new();
    for i in function.instructions.iter() {
        instructions.push(gen_instruction(memory, i));
    }

    instructions
}

fn gen_instruction(memory: &mut MemoryContext, line: &tac::InstructionLine) -> Instruction {
    let tac::InstructionLine(instruction, id) = line;
    match instruction {
        tac::Instruction::Alloc(tac::Const::Int(i)) => {
            Instruction::Mov(memory.alloc_on_stack(), Const::Int(*i as i64))
        }
        tac::Instruction::ControlOp(tac::ControlOp::Return(id)) => Instruction::Ret,
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
    Ret,
}

pub enum Const {
    Int(i64),
    Place(Place),
}

pub enum Place {
    OnStack(StackIndex),
    InRegister(Register),
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
            _ => unimplemented!(),
        }
    }
}

impl GASM {
    fn translate_place(p: &Place) -> String {
        match p {
            Place::InRegister(reg) => reg.clone(),
            /* TODO: rbp here should be changed to some variable which responcible for the begging of the stack */
            Place::OnStack(offset) => format!("-{}(%rbp)", offset),
        }
    }

    fn translate_const(c: &Const) -> String {
        match c {
            Const::Int(v) => format!("${}", v),
            Const::Place(p) => GASM::translate_place(p),
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
}

pub struct DefaultFormatter;

impl AsmFormatter for DefaultFormatter {
    fn format(i: &Instruction, line: &str) -> String {
        format!("{}\n", line)
    }
}
