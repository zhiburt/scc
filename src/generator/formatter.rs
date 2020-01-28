use std::io;

use super::syntax::{self, IList, AsmInstruction, Place, Value, Params};

pub fn format<W: io::Write>(mut w: W, asm: IList) -> io::Result<()> {
    for i in asm.iter() {
        writeln!(w, "{}", format_instruction(i))?;
    }

    Ok(())
}

fn format_instruction(i: &AsmInstruction) -> String {
    match i {
        AsmInstruction::Add(args) => format!("    add{} {}, {}", suffix(args), format_value(args.value()), format_place(args.place())),
        AsmInstruction::Mov(args) => {
            let suffix = suffix(args);
            format!("    mov{} {}, {}", suffix, format_value(args.value()), format_place(args.place()))
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
        Place::Stack(offset, t) => format!("-{}(%rbp)", offset)
    }
}

fn format_value(v: &Value) -> String {
    match v {
        Value::Place(p) => format_place(p),
        Value::Const(int, t) => format!("${}", int)
    }
}

fn suffix(args: &Params) -> String {
    match syntax::X64Memory::value_type(args.value()) {
        syntax::Type::Doubleword => "l".to_owned(),
        syntax::Type::Quadword => "q".to_owned(),
        _ => unimplemented!(),
    }
}