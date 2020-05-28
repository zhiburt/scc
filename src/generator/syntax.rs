use super::asm::{AsmX32, Indirect, Line, Place, Size, Value};
// TODO: it's better to be pure structure
pub struct GASM;

impl GASM {
    pub fn translate(l: &Line) -> String {
        match l {
            Line::Label(label) => format!("{}:", label),
            Line::Directive(directive) => format!("  {}", directive),
            Line::Instruction(i) => format!("  {}", Self::translate_instruction(i)),
        }
    }

    fn translate_instruction(asm: &AsmX32) -> String {
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
                    Size::Byte => "movzb",
                    _ => unimplemented!(),
                };

                format!(
                    "  {}{} {}, {}",
                    instruction,
                    suffix(&p.size()),
                    format_value(&v),
                    format_place(&p)
                )
            }
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
                suffix(&p.size()),
                format_value(&v),
                format_place(&p)
            ),
            AsmX32::Add(p, v) => format!(
                "  add{} {}, {}",
                suffix(&p.size()),
                format_value(&v),
                format_place(&p)
            ),
            AsmX32::Sub(p, v) => format!(
                "  sub{} {}, {}",
                suffix(&p.size()),
                format_value(&v),
                format_place(&p)
            ),
            AsmX32::Mul(p, v) => format!(
                "  imul{} {}, {}",
                suffix(&v.size()),
                format_value(&v),
                format_place(&p)
            ),
            AsmX32::Imul(c, v, reg) => format!(
                "  imul{} ${}, {}, %{}",
                suffix(&v.size()),
                c,
                format_value(&v),
                reg
            ),
            AsmX32::Div(p) => format!("  idiv{} {}", suffix(&p.size()), format_place(&p),),
            AsmX32::Sete(p) => format!("  sete {}", format_place(&p),),
            AsmX32::Setne(p) => format!("  setne {}", format_place(&p),),
            AsmX32::Setl(p) => format!("  setl {}", format_place(&p),),
            AsmX32::Setle(p) => format!("  setle {}", format_place(&p),),
            AsmX32::Setg(p) => format!("  setg {}", format_place(&p),),
            AsmX32::Setge(p) => format!("  setge {}", format_place(&p),),
            AsmX32::Neg(p) => format!("  neg{} {}", suffix(&p.size()), format_place(&p),),
            AsmX32::Not(p) => format!("  not{} {}", suffix(&p.size()), format_place(&p),),
            AsmX32::Convert(t) => match t {
                Size::Doubleword => format!("  cltd"),
                Size::Quadword => format!("  cqto"),
                _ => unimplemented!(),
            },
            AsmX32::Push(v) => format!("  push{} {}", suffix(&v.size()), format_value(&v)),
            AsmX32::Pop(p) => format!("  pop{} {}", suffix(&p.size()), format_place(&p)),
            AsmX32::Cmp(rhs, lhs) => format!(
                "  cmp{} {}, {}",
                suffix(&rhs.size()),
                format_value(&lhs),
                format_place(&rhs),
            ),
            AsmX32::Jmp(label) => format!("  jmp {}", label),
            AsmX32::Je(label) => format!("  je {}", label),
            AsmX32::Jne(label) => format!("  jne {}", label),
            AsmX32::Ret => format!("  ret"),
            AsmX32::Call(name) => format!("  call {}", name),
        }
    }
}

fn format_place(p: &Place) -> String {
    match p {
        Place::Register(reg) => format!("%{}", reg),
        Place::Indirect(Indirect { offset, reg, .. }) => format!("-{}(%{})", offset, reg),
        Place::Static(label, ..) => label.to_owned(),
    }
}

fn format_value(v: &Value) -> String {
    match v {
        Value::Register(reg) => format_place(&Place::Register(reg.clone())),
        Value::Indirect(indirect) => format_place(&Place::Indirect(indirect.clone())),
        Value::Const(int) => format!("${}", int),
        Value::Static(label, ..) => label.to_owned(),
    }
}

fn suffix(t: &Size) -> &'static str {
    match t {
        Size::Doubleword => "l",
        Size::Quadword => "q",
        Size::Byte => "b",
        _ => unimplemented!(),
    }
}

mod tests {
    use super::*;
    use super::super::{RegisterX64, Part, Register};

    #[test]
    fn suffix() {
        let full_double = AsmX32::Add(
            Place::Register(Register::Sub(RegisterX64::RAX, Part::Doubleword)),
            Value::Const(1),
        );
        let full_quad = AsmX32::Add(
            Place::Register(Register::Register(RegisterX64::RAX)),
            Value::Const(2),
        );
        let const_quad = AsmX32::Add(
            Place::Register(Register::Sub(RegisterX64::RAX, Part::Doubleword)),
            Value::Const(3),
        );
        let place_quad = AsmX32::Add(
            Place::Register(Register::Register(RegisterX64::RAX)),
            Value::Const(4),
        );

        assert_eq!("  addl $1, %eax", GASM::translate_instruction(&full_double));
        assert_eq!("  addq $2, %rax", GASM::translate_instruction(&full_quad));
        assert_eq!("  addq $3, %eax", GASM::translate_instruction(&const_quad));
        assert_eq!("  addl $4, %rax", GASM::translate_instruction(&place_quad));
    }
}
