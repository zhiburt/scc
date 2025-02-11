use super::asm::{AsmX32, Assembly, Indirect, Line, Offset, Place, Size, Value};

pub trait Syntax {
    fn asm(asm: &Assembly) -> String;
    fn translate(l: &Line) -> String;
}

pub struct GASM;

impl Syntax for GASM {
    fn asm(asm: &Assembly) -> String {
        let mut buf = String::new();
        for i in asm.data.into_iter() {
            buf.push_str(&Self::translate(i));
            buf.push('\n');
        }

        for func in asm.funcs.values() {
            for i in func.instructions() {
                buf.push_str(&Self::translate(i));
                buf.push('\n');
            }

            buf.push('\n');
        }

        buf
    }

    fn translate(l: &Line) -> String {
        match l {
            Line::Label(label) => format!("{}:", label),
            Line::Directive(directive) => format!("  {}", directive),
            Line::Instruction(i) => format!("  {}", Self::translate_instruction(i)),
        }
    }
}

impl GASM {
    fn translate_instruction(asm: &AsmX32) -> String {
        match asm {
            AsmX32::Label(label) => format!("{}:", label),
            AsmX32::Metadata(data) => format!("  {}", data),
            AsmX32::Mov(p, v) => format!(
                "  mov{} {}, {}",
                Self::suffix(&v.size()),
                Self::fmt_value(&v),
                Self::fmt_place(&p)
            ),
            AsmX32::Movzx(p, v) => {
                let instruction = match v.size() {
                    Size::Byte => "movzb",
                    _ => unimplemented!(),
                };

                format!(
                    "  {}{} {}, {}",
                    instruction,
                    Self::suffix(&p.size()),
                    Self::fmt_value(&v),
                    Self::fmt_place(&p)
                )
            }
            AsmX32::And(p, v) => format!(
                "  and{} {}, {}",
                Self::suffix(&p.size()),
                Self::fmt_value(&v),
                Self::fmt_place(&p)
            ),
            AsmX32::Or(p, v) => format!(
                "  or{} {}, {}",
                Self::suffix(&v.size()),
                Self::fmt_value(&v),
                Self::fmt_place(&p)
            ),
            AsmX32::Xor(p, v) => format!(
                "  xor{} {}, {}",
                Self::suffix(&p.size()),
                Self::fmt_value(&v),
                Self::fmt_place(&p)
            ),
            AsmX32::Add(p, v) => format!(
                "  add{} {}, {}",
                Self::suffix(&p.size()),
                Self::fmt_value(&v),
                Self::fmt_place(&p)
            ),
            AsmX32::Sub(p, v) => format!(
                "  sub{} {}, {}",
                Self::suffix(&p.size()),
                Self::fmt_value(&v),
                Self::fmt_place(&p)
            ),
            AsmX32::Mul(p, v) => format!(
                "  imul{} {}, {}",
                Self::suffix(&v.size()),
                Self::fmt_value(&v),
                Self::fmt_place(&p)
            ),
            AsmX32::Imul(c, v, reg) => format!(
                "  imul{} ${}, {}, %{}",
                Self::suffix(&v.size()),
                c,
                Self::fmt_value(&v),
                reg
            ),
            AsmX32::Div(p) => format!("  idiv{} {}", Self::suffix(&p.size()), Self::fmt_place(&p),),
            AsmX32::Sete(p) => format!("  sete {}", Self::fmt_place(&p),),
            AsmX32::Setne(p) => format!("  setne {}", Self::fmt_place(&p),),
            AsmX32::Setl(p) => format!("  setl {}", Self::fmt_place(&p),),
            AsmX32::Setle(p) => format!("  setle {}", Self::fmt_place(&p),),
            AsmX32::Setg(p) => format!("  setg {}", Self::fmt_place(&p),),
            AsmX32::Setge(p) => format!("  setge {}", Self::fmt_place(&p),),
            AsmX32::Neg(p) => format!("  neg{} {}", Self::suffix(&p.size()), Self::fmt_place(&p),),
            AsmX32::Not(p) => format!("  not{} {}", Self::suffix(&p.size()), Self::fmt_place(&p),),
            AsmX32::Convert(t) => match t {
                Size::Doubleword => format!("  cltd"),
                Size::Quadword => format!("  cqto"),
                _ => unimplemented!(),
            },
            AsmX32::Push(v) => format!("  push{} {}", Self::suffix(&v.size()), Self::fmt_value(&v)),
            AsmX32::Pop(p) => format!("  pop{} {}", Self::suffix(&p.size()), Self::fmt_place(&p)),
            AsmX32::Cmp(rhs, lhs) => format!(
                "  cmp{} {}, {}",
                Self::suffix(&rhs.size()),
                Self::fmt_value(&lhs),
                Self::fmt_place(&rhs),
            ),
            AsmX32::Jmp(label) => format!("  jmp {}", label),
            AsmX32::Je(label) => format!("  je {}", label),
            AsmX32::Jne(label) => format!("  jne {}", label),
            AsmX32::Ret => format!("  ret"),
            AsmX32::Call(name) => format!("  call {}", name),
        }
    }

    fn fmt_place(p: &Place) -> String {
        match p {
            Place::Register(reg) => format!("%{}", reg),
            Place::Indirect(Indirect { offset, reg, .. }) => match offset {
                Offset::Label(offset) => format!("{}(%{})", offset, reg),
                Offset::Static(offset) => format!("-{}(%{})", offset, reg),
            },
            Place::Static(label, ..) => label.to_owned(),
        }
    }

    fn fmt_value(v: &Value) -> String {
        match v {
            Value::Register(reg) => Self::fmt_place(&Place::Register(reg.clone())),
            Value::Indirect(indirect) => Self::fmt_place(&Place::Indirect(indirect.clone())),
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
}

pub struct Intel;

impl Syntax for Intel {
    fn asm(asm: &Assembly) -> String {
        let mut buf = String::new();
        for i in asm.data.into_iter() {
            buf.push_str(&Self::translate(i));
            buf.push('\n');
        }

        buf.push('\n');

        for func in asm.funcs.values() {
            buf.push_str(&Self::translate(&Line::Directive(
                ".intel_syntax noprefix".to_owned(),
            )));
            buf.push('\n');
            for i in func.instructions() {
                buf.push_str(&Self::translate(i));
                buf.push('\n');
            }

            buf.push('\n');
        }

        buf
    }

    fn translate(l: &Line) -> String {
        match l {
            Line::Label(label) => format!("{}:", label),
            Line::Directive(directive) => format!("\t{}", directive),
            Line::Instruction(i) => format!("\t{}", Self::translate_instruction(i)),
        }
    }
}

impl Intel {
    fn translate_instruction(asm: &AsmX32) -> String {
        match asm {
            AsmX32::Label(label) => format!("{}:", label),
            AsmX32::Metadata(data) => format!("{}", data),
            AsmX32::Mov(p, v) => {
                format!("mov {1}, {0}", Self::fmt_value(&v), Self::fmt_place(&p))
            }
            AsmX32::Movzx(p, v) => {
                let instruction = match v.size() {
                    Size::Byte => "movzx",
                    _ => unimplemented!(),
                };

                format!(
                    "{} {2}, {1}",
                    instruction,
                    Self::fmt_value(&v),
                    Self::fmt_place(&p)
                )
            }
            AsmX32::And(p, v) => {
                format!("and {1}, {0}", Self::fmt_value(&v), Self::fmt_place(&p))
            }
            AsmX32::Or(p, v) => format!("  or {1}, {0}", Self::fmt_value(&v), Self::fmt_place(&p)),
            AsmX32::Xor(p, v) => {
                format!("xor {1}, {0}", Self::fmt_value(&v), Self::fmt_place(&p))
            }
            AsmX32::Add(p, v) => {
                format!("add {1}, {0}", Self::fmt_value(&v), Self::fmt_place(&p))
            }
            AsmX32::Sub(p, v) => {
                format!("sub {1}, {0}", Self::fmt_value(&v), Self::fmt_place(&p))
            }
            AsmX32::Mul(p, v) => {
                format!("imul {1}, {0}", Self::fmt_value(&v), Self::fmt_place(&p))
            }
            AsmX32::Imul(c, v, reg) => format!("imul {2}, {1}, {0}", c, Self::fmt_value(&v), reg),
            AsmX32::Div(p) => format!("idiv {}", Self::fmt_place(&p),),
            AsmX32::Sete(p) => format!("sete {}", Self::fmt_place(&p),),
            AsmX32::Setne(p) => format!("setne {}", Self::fmt_place(&p),),
            AsmX32::Setl(p) => format!("setl {}", Self::fmt_place(&p),),
            AsmX32::Setle(p) => format!("setle {}", Self::fmt_place(&p),),
            AsmX32::Setg(p) => format!("setg {}", Self::fmt_place(&p),),
            AsmX32::Setge(p) => format!("setge {}", Self::fmt_place(&p),),
            AsmX32::Neg(p) => format!("neg {}", Self::fmt_place(&p),),
            AsmX32::Not(p) => format!("not {}", Self::fmt_place(&p),),
            AsmX32::Convert(t) => match t {
                Size::Doubleword => format!("cltd"),
                Size::Quadword => format!("cqto"),
                _ => unimplemented!(),
            },
            AsmX32::Push(v) => format!("push {}", Self::fmt_value(&v)),
            AsmX32::Pop(p) => format!("pop {}", Self::fmt_place(&p)),
            AsmX32::Cmp(rhs, lhs) => {
                format!("cmp {1}, {0}", Self::fmt_value(&lhs), Self::fmt_place(&rhs),)
            }
            AsmX32::Jmp(label) => format!("jmp {}", label),
            AsmX32::Je(label) => format!("je {}", label),
            AsmX32::Jne(label) => format!("jne {}", label),
            AsmX32::Ret => format!("ret"),
            AsmX32::Call(name) => format!("call {}", name),
        }
    }

    fn fmt_place(p: &Place) -> String {
        match p {
            Place::Register(reg) => format!("{}", reg),
            Place::Indirect(Indirect { offset, reg, .. }) => match offset {
                Offset::Label(offset) => format!("dword ptr {1}[{0}]", reg, offset),
                Offset::Static(offset) => format!("dword ptr [{} - {}]", reg, offset),
            },
            Place::Static(label, ..) => label.to_owned(),
        }
    }

    fn fmt_value(v: &Value) -> String {
        match v {
            Value::Register(reg) => Self::fmt_place(&Place::Register(reg.clone())),
            Value::Indirect(indirect) => Self::fmt_place(&Place::Indirect(indirect.clone())),
            Value::Const(int) => format!("{}", int),
            Value::Static(label, ..) => label.to_owned(),
        }
    }
}

#[cfg(test)]
mod gasm {
    use super::super::{Part, Register, RegisterX64};
    use super::*;

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
