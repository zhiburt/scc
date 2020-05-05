use super::asm::{Arg, Const, Instruction, InstructionLine, Register, RegisterBackend, Size};
use std::fmt::{self, Display};

pub fn translate(instruction: &InstructionLine) -> String {
    match instruction {
        InstructionLine::Directive(d) => format!("  {}", d),
        InstructionLine::Label(l) => format!("{}:", l),
        InstructionLine::Instruction(i) => format!("  {}", translate_instruction(i)),
    }
}

fn translate_instruction(i: &Instruction) -> String {
    let args = i
        .args
        .iter()
        .map(|arg| translate_arg(arg))
        .collect::<Vec<_>>()
        .join(", ");
    format!("{} {}", i.mnemonic, args)
}

fn translate_arg(arg: &Arg) -> String {
    use RegisterBackend::*;
    match arg {
        Arg::Register(Register { rg, .. }) => match rg {
            Machine(reg) => format!("%{}", reg),
            StackOffset(offset) => format!("-{}(%rbp)", offset),
        },
        Arg::Const(Const(c)) => format!("${}", c),
        Arg::Label(label) => format!("{}", label),
    }
}
