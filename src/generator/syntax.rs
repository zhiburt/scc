use super::x64_translator::{self, AsmX32};

// TODO: it's better to be pure structure
pub struct GASMx64;

impl GASMx64 {
    pub fn to_string(asm: &AsmX32) -> String {
        match asm {
            AsmX32::Label(label) => format!("{}:", label),
            AsmX32::Metadata(data) => format!("  {}", data),
            AsmX32::Mov(p, v) => format!("  mov {}, {}", format_value(&v), format_place(&p)),
            AsmX32::Add(p, v) => format!("  add {}, {}", format_value(&v), format_place(&p)),
            AsmX32::Push(v) => format!("  push {}", format_value(&v)),
            AsmX32::Pop(p) => format!("  pop {}", format_place(&p)),
            AsmX32::Ret => format!("  ret"),
            _ => { println!("{:?}", asm); unimplemented!() },
        }
    }
}

fn format_place(p: &x64_translator::Place) -> String {
    match p {
        x64_translator::Place::Register(reg) => format!("%{}", reg),
        x64_translator::Place::Stack(offset, ..) => format!("-{}(%rbp)", offset)
    }
}

fn format_value(v: &x64_translator::AsmValue) -> String {
    match v {
        x64_translator::AsmValue::Place(p) => format_place(p),
        x64_translator::AsmValue::Const(int, t) => format!("${}", int)
    }
}
