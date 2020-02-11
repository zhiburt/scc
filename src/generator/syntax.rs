use super::{
    translator::Type,
    x64_translator::{self, AsmX32},
};

// TODO: it's better to be pure structure
pub struct GASMx64;

impl GASMx64 {
    pub fn to_string(asm: &AsmX32) -> String {
        match asm {
            AsmX32::Label(label) => format!("{}:", label),
            AsmX32::Metadata(data) => format!("  {}", data),
            AsmX32::Mov(p, v) => format!(
                "  mov{} {}, {}",
                suffix(&v.size()),
                format_value(&v),
                format_place(&p)
            ),
            AsmX32::Movzx(p, v) => {
                let instruction = match v.size() {
                    Type::Byte => "movzb",
                    _ => unimplemented!(),
                };

                format!(
                    "  {}{} {}, {}",
                    instruction,
                    suffix(&p.size()),
                    format_value(&v),
                    format_place(&p)
                )
            },
            AsmX32::And(p, v) => format!(
                "  and{} {}, {}",
                suffix(&p.size()),
                format_value(&v),
                format_place(&p)
            ),
            AsmX32::Or(p, v) => format!(
                "  or{} {}, {}",
                suffix(&v.size()),
                format_value(&v),
                format_place(&p)
            ),
            AsmX32::Xor(p, v) => format!(
                "  xor{} {}, {}",
                suffix(&v.size()),
                format_value(&v),
                format_place(&p)
            ),
            AsmX32::Add(p, v) => format!(
                "  add{} {}, {}",
                suffix(&v.size()),
                format_value(&v),
                format_place(&p)
            ),
            AsmX32::Sub(p, v) => format!(
                "  sub{} {}, {}",
                suffix(&v.size()),
                format_value(&v),
                format_place(&p)
            ),
            AsmX32::Mul(p, v) => format!(
                "  imul{} {}, {}",
                suffix(&v.size()),
                format_value(&v),
                format_place(&p)
            ),
            AsmX32::Div(p) => format!(
                "  idiv{} {}",
                suffix(&p.size()),
                format_place(&p),
            ),
            AsmX32::Sete(p) => format!(
                "  sete {}",
                format_place(&p),
            ),
            AsmX32::Setn(p) => format!(
                "  setn {}",
                format_place(&p),
            ),
            AsmX32::Setl(p) => format!(
                "  setl {}",
                format_place(&p),
            ),
            AsmX32::Setle(p) => format!(
                "  setle {}",
                format_place(&p),
            ),
            AsmX32::Setg(p) => format!(
                "  setg {}",
                format_place(&p),
            ),
            AsmX32::Setge(p) => format!(
                "  setge {}",
                format_place(&p),
            ),
            AsmX32::Neg(p) => format!(
                "  neg{} {}",
                suffix(&p.size()),
                format_place(&p),
            ),
            AsmX32::Not(p) => format!(
                "  not{} {}",
                suffix(&p.size()),
                format_place(&p),
            ),
            AsmX32::Convert(t) => match t {
                Type::Doubleword => format!("  cltd"),
                Type::Quadword => format!("  cqto"),
                _ => unimplemented!(),
            }
            AsmX32::Push(v) => format!("  push{} {}", suffix(&v.size()), format_value(&v)),
            AsmX32::Pop(p) => format!("  pop{} {}", suffix(&p.size()), format_place(&p)),
            AsmX32::Cmp(p, v) => format!(
                "  cmp{} {}, {}",
                suffix(&p.size()),
                format_value(&v),
                format_place(&p),
            ),
            AsmX32::Jmp(label) => format!("  jmp {}", label),
            AsmX32::Je(label) => format!("  je {}", label),
            AsmX32::Jne(label) => format!("  jne {}", label),
            AsmX32::Ret => format!("  ret"),
            AsmX32::Call(name) => format!( "  call {}", name),
        }
    }
}

fn format_place(p: &x64_translator::Place) -> String {
    match p {
        x64_translator::Place::Register(reg) => format!("%{}", reg),
        x64_translator::Place::Stack(offset, ..) => format!("-{}(%rbp)", offset),
    }
}

fn format_value(v: &x64_translator::AsmValue) -> String {
    match v {
        x64_translator::AsmValue::Place(p) => format_place(p),
        x64_translator::AsmValue::Const(int, t) => format!("${}", int),
    }
}

fn suffix(t: &Type) -> &'static str {
    match t {
        Type::Doubleword => "l",
        Type::Quadword => "q",
        Type::Byte => "b",
        _ => unimplemented!(),
    }
}

mod tests {
    use super::x64_translator::{AsmValue, Place, Register};
    use super::*;

    #[test]
    fn suffix() {
        let full_double = AsmX32::Add(
            Place::Register(Register::new("eax")),
            AsmValue::Const(1, Type::Doubleword),
        );
        let full_quad = AsmX32::Add(
            Place::Register(Register::new("rax")),
            AsmValue::Const(2, Type::Quadword),
        );
        let const_quad = AsmX32::Add(
            Place::Register(Register::new("eax")),
            AsmValue::Const(3, Type::Quadword),
        );
        let place_quad = AsmX32::Add(
            Place::Register(Register::new("rax")),
            AsmValue::Const(4, Type::Doubleword),
        );

        assert_eq!("  addl $1, %eax", GASMx64::to_string(&full_double));
        assert_eq!("  addq $2, %rax", GASMx64::to_string(&full_quad));
        assert_eq!("  addq $3, %eax", GASMx64::to_string(&const_quad));
        assert_eq!("  addl $4, %rax", GASMx64::to_string(&place_quad));
    }
}
