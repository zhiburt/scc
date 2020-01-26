use std::io;

use super::syntax::{self, IList, AsmInstruction, Place, Value};

pub fn format<W: io::Write>(mut w: W, asm: IList) -> io::Result<()> {
    for i in asm.iter() {
        writeln!(w, "{}", format_instruction(i))?;
    }

    Ok(())
}

fn format_instruction(i: &AsmInstruction) -> String {
    match i {
        AsmInstruction::Add(p, v) => format!("    add {}, {}", format_place(p), format_value(v)),
        AsmInstruction::Mov(p, v) => {
            let suffix = match syntax::X64Memory::value_type(v) {
                syntax::Type::Doubleword => "l",
                syntax::Type::Quadword => "q",
                _ => unimplemented!(),
            };

            format!("    mov{} {}, {}", suffix, format_value(v), format_place(p))
        }
        AsmInstruction::Ret => "    ret".to_string(),
        AsmInstruction::Label(label) => format!("{}:", label),
        AsmInstruction::Metadata(line) => format!("    {}", line),
        AsmInstruction::Push(value) => format!("    push {}", format_value(value)),
        AsmInstruction::Pop(place) => format!("    pop {}", format_place(place))
    }
}

fn format_place(p: &Place) -> String {
    match p {
        Place::Register(reg) => format!("%{}", reg),
        Place::Stack(offset) => format!("-{}(%rbp)", offset)
    }
}

fn format_value(v: &Value) -> String {
    match v {
        Value::Place(p) => format_place(p),
        Value::Const(int) => format!("${}", int)
    }
}