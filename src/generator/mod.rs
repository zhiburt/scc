mod allocator;
mod asm;
pub mod syntax;

use super::il::tac::{self, File, InstructionLine};
use asm::{AsmX32, Indirect, Part, Place, Register, RegisterX64, Size, Value};
use std::collections::HashMap;

pub fn gen<S: syntax::Syntax>(ir: File) -> String {
    let g = Generator::new(ir);
    let asm = g.gen();
    // allocator::alloc(&mut asm);

    asm.code::<S>()
}

struct Generator {
    ir: File,
    code: asm::Assembly,
}

impl Generator {
    fn new(ir: File) -> Self {
        Self {
            ir,
            code: asm::Assembly::new(),
        }
    }

    fn gen_function(&mut self, func: tac::FuncDef) {
        let (mut allocator, params) = allocator::Allocator::new(&self.ir, &func);
        let mut code = Vec::new();
        code.push(params);

        for (line, i) in func.instructions.into_iter().enumerate() {
            code.push(translate(line, &mut allocator, i));
        }

        let header = {
            let mut header = asm::Block::new();
            header.emit_directive(&format!(".globl {}", func.name));
            header.emit_directive(&format!(".text"));
            header.emit_label(&func.name);
            header
        };

        let (prologue, epilogue) = {
            let mut prologue = asm::Block::new();
            prologue.emit(AsmX32::Push(Value::Register(Register::Register(
                RegisterX64::RBP,
            ))));
            prologue.emit(AsmX32::Mov(
                Place::Register(Register::Register(RegisterX64::RBP)),
                Value::Register(Register::Register(RegisterX64::RSP)),
            ));

            let mut epilogue = asm::Block::new();
            if func.has_function_call {
                // todo: stack alignment
                // comment: now it's always allocated by 4 bytes so its got to be ok
                let stack_size = allocator.stack_size;
                prologue.emit(AsmX32::Sub(
                    Place::Register(Register::Register(RegisterX64::RSP)),
                    Value::Const(stack_size as i32),
                ));
                epilogue.emit(AsmX32::Add(
                    Place::Register(Register::Register(RegisterX64::RSP)),
                    Value::Const(stack_size as i32),
                ));
                epilogue.emit(AsmX32::Mov(
                    Place::Register(Register::Register(RegisterX64::RSP)),
                    Value::Register(Register::Register(RegisterX64::RBP)),
                ));
                epilogue.emit(AsmX32::Pop(Place::Register(Register::Register(
                    RegisterX64::RBP,
                ))));
                epilogue.emit(AsmX32::Ret);
            } else {
                epilogue.emit(AsmX32::Pop(Place::Register(Register::Register(
                    RegisterX64::RBP,
                ))));
                epilogue.emit(AsmX32::Ret);
            }

            (prologue, epilogue)
        };

        let mut c = vec![header];
        c.push(prologue);
        c.extend(code);
        c.push(epilogue);

        self.code.emit_function(&func.name, c);
    }

    fn gen_data_section(data: &HashMap<tac::ID, Option<tac::Const>>) -> asm::Block {
        let mut block = asm::Block::new();
        for (var, value) in data {
            match value {
                Some(tac::Const::Int(value)) => {
                    block.emit_directive(&format!(".globl _var_{}", var));
                    block.emit_directive(&format!(".data"));
                    block.emit_directive(&format!(".align 8"));
                    block.emit_directive(&format!("_var_{}:", var));
                    block.emit_directive(&format!(".long {}", value));
                }
                None => {
                    block.emit_directive(&format!(".globl _var_{}", var));
                    block.emit_directive(&format!(".bss"));
                    block.emit_directive(&format!(".align 8"));
                    block.emit_directive(&format!("_var_{}:", var));
                    block.emit_directive(&format!(".zero 4"));
                }
            }
        }

        block
    }

    fn gen(mut self) -> asm::Assembly {
        let data = Self::gen_data_section(&self.ir.global_data);

        self.code.set_data(data);

        let code = std::mem::replace(&mut self.ir.code, Vec::new());
        for func in code {
            self.gen_function(func);
        }

        self.code
    }
}

fn checked_add(
    line: usize,
    al: &mut allocator::Allocator,
    from: tac::ID,
    to: tac::ID,
) -> asm::Block {
    let mut b = asm::Block::new();
    if matches!(al.get(from), Place::Indirect(..)) && matches!(al.get(to), Place::Indirect(..)) {
        let (reg, spill, unspill) = get_register(line, al);
        b += spill;
        b.emit(AsmX32::Mov(
            Place::Register(Register::Sub(reg.clone(), Part::Doubleword)),
            al.get(from).into(),
        ));
        b.emit(AsmX32::Add(
            al.get(to),
            Value::Register(Register::Sub(reg, Part::Doubleword)),
        ));
        b += unspill;
    } else {
        b.emit(AsmX32::Add(al.get(to), al.get(from).into()));
    }
    b
}

fn checked_sub(
    line: usize,
    al: &mut allocator::Allocator,
    from: tac::ID,
    to: tac::ID,
) -> asm::Block {
    let mut b = asm::Block::new();
    if matches!(al.get(from), Place::Indirect(..)) && matches!(al.get(to), Place::Indirect(..)) {
        let (reg, spill, unspill) = get_register(line, al);
        b += spill;
        b.emit(AsmX32::Mov(
            Place::Register(Register::Sub(reg.clone(), Part::Doubleword)),
            al.get(from).into(),
        ));
        b.emit(AsmX32::Sub(
            al.get(to),
            Value::Register(Register::Sub(reg, Part::Doubleword)),
        ));
        b += unspill;
    } else {
        b.emit(AsmX32::Sub(al.get(to), al.get(from).into()));
    }
    b
}

fn checked_mov(
    line: usize,
    al: &mut allocator::Allocator,
    from: tac::ID,
    to: tac::ID,
) -> asm::Block {
    let mut b = asm::Block::new();
    if matches!(al.get(from), Place::Indirect(..)) && matches!(al.get(to), Place::Indirect(..)) {
        let (reg, spill, unspill) = get_register(line, al);
        b += spill;
        b.emit(AsmX32::Mov(
            Place::Register(Register::Sub(reg.clone(), Part::Doubleword)),
            al.get(from).into(),
        ));
        b.emit(AsmX32::Mov(
            al.get(to),
            Value::Register(Register::Sub(reg, Part::Doubleword)),
        ));
        b += unspill;
    } else {
        b.emit(AsmX32::Mov(al.get(to), al.get(from).into()));
    }
    b
}

fn checked_cmp(
    line: usize,
    al: &mut allocator::Allocator,
    lhs: tac::ID,
    rhs: tac::ID,
) -> asm::Block {
    let mut b = asm::Block::new();
    if matches!(al.get(lhs), Place::Indirect(..)) && matches!(al.get(rhs), Place::Indirect(..)) {
        let (reg, spill, unspill) = get_register(line, al);
        b += spill;
        b.emit(AsmX32::Mov(
            Place::Register(Register::Sub(reg.clone(), Part::Doubleword)),
            al.get(lhs).into(),
        ));
        b.emit(AsmX32::Cmp(
            Place::Register(Register::Sub(reg, Part::Doubleword)),
            al.get(rhs).into(),
        ));
        b += unspill;
    } else {
        b.emit(AsmX32::Cmp(al.get(lhs), al.get(rhs).into()));
    }
    b
}

fn get_register(
    line: usize,
    al: &mut allocator::Allocator,
) -> (RegisterX64, asm::Block, asm::Block) {
    match al.find_free_at(line) {
        Some(reg) => (reg, asm::Block::new(), asm::Block::new()),
        None => {
            let reg = al
                .live_at(line)
                .into_iter()
                .filter_map(|reg| match reg {
                    Place::Register(reg) => Some(reg),
                    _ => None,
                })
                .next()
                .unwrap()
                .clone();
            let offset = al.alloc_stack();

            let mut spill = asm::Block::new();
            spill.emit(AsmX32::Mov(
                Place::Indirect(Indirect::new(
                    Register::Register(RegisterX64::RBP),
                    offset,
                    Size::Doubleword,
                )),
                Value::Register(reg.clone()),
            ));

            let mut unspill = asm::Block::new();
            unspill.emit(AsmX32::Mov(
                Place::Register(reg.clone()),
                Value::Indirect(Indirect::new(
                    Register::Register(RegisterX64::RBP),
                    offset,
                    Size::Doubleword,
                )),
            ));

            (reg.base(), spill, unspill)
        }
    }
}

fn imul_constant(
    line: usize,
    al: &mut allocator::Allocator,
    lhs: tac::ID,
    rhs: i32,
    id: tac::ID,
) -> asm::Block {
    let mut b = asm::Block::new();
    match al.get(id) {
        Place::Register(reg) => {
            b.emit(AsmX32::Imul(rhs, al.get(lhs).into(), reg));
        }
        _ => {
            let (reg, spill, unspill) = get_register(line, al);
            b += spill;
            b.emit(AsmX32::Mov(
                Place::Register(Register::Sub(reg.clone(), Part::Doubleword)),
                al.get(id).into(),
            ));
            b.emit(AsmX32::Imul(
                rhs,
                al.get(lhs).into(),
                Register::Sub(reg.clone(), Part::Doubleword),
            ));
            b.emit(AsmX32::Mov(
                al.get(id).into(),
                Value::Register(Register::Sub(reg, Part::Doubleword)),
            ));
            b += unspill;
        }
    }

    b
}

fn space_for_divisor(
    line: usize,
    al: &mut allocator::Allocator,
    rhs: i32,
) -> (Place, asm::Block, asm::Block) {
    if let Some(reg) = al
        .free_at(line)
        .iter()
        .find(|reg| reg != &&RegisterX64::RAX && reg != &&RegisterX64::RDX)
    {
        let mut spill = asm::Block::new();
        let place = Place::Register(Register::Sub(reg.clone(), Part::Doubleword));
        spill.emit(AsmX32::Mov(place.clone(), Value::Const(rhs)));

        (place, spill, asm::Block::new())
    } else {
        let offset = al.alloc_stack();
        let mut spill = asm::Block::new();
        let place = Place::Indirect(Indirect::new(
            Register::Register(RegisterX64::RBP),
            offset,
            Size::Doubleword,
        ));
        spill.emit(AsmX32::Mov(place.clone(), Value::Const(rhs)));

        (place, spill, asm::Block::new())
    }
}

fn spill_eax(line: usize, al: &mut allocator::Allocator) -> (asm::Block, asm::Block) {
    if al.free_at(line).contains(&RegisterX64::RAX) {
        (asm::Block::new(), asm::Block::new())
    } else {
        let offset = al.alloc_stack();
        let mut spill = asm::Block::new();
        spill.emit(AsmX32::Mov(
            Place::Indirect(Indirect::new(
                Register::Register(RegisterX64::RBP),
                offset,
                Size::Doubleword,
            )),
            Value::Register(Register::Sub(RegisterX64::RAX, Part::Doubleword)),
        ));

        let mut unspill = asm::Block::new();
        unspill.emit(AsmX32::Mov(
            Place::Register(Register::Sub(RegisterX64::RAX, Part::Doubleword)),
            Value::Indirect(Indirect::new(
                Register::Register(RegisterX64::RBP),
                offset,
                Size::Doubleword,
            )),
        ));

        (spill, unspill)
    }
}

fn spill_eax_div(
    line: usize,
    al: &mut allocator::Allocator,
    lhs: tac::ID,
    id: tac::ID,
) -> (asm::Block, asm::Block) {
    let (mut spill, unspill) = if matches!(
        al.get(lhs),
        Place::Register(Register::Sub(RegisterX64::RAX, Part::Doubleword))
    ) || al.free_at(line).contains(&RegisterX64::RAX)
        || matches!(
            al.get(id),
            Place::Register(Register::Sub(RegisterX64::RAX, Part::Doubleword))
        ) {
        (asm::Block::new(), asm::Block::new())
    } else {
        spill_eax(line, al)
    };

    spill.emit(AsmX32::Mov(
        Place::Register(Register::Sub(RegisterX64::RAX, Part::Doubleword)),
        al.get(lhs).into(),
    ));

    (spill, unspill)
}

fn spill_edx_if_not(
    line: usize,
    al: &mut allocator::Allocator,
    not_in: &[tac::ID],
) -> (asm::Block, asm::Block) {
    if not_in.iter().any(|id| {
        matches!(
            al.get(*id),
            Place::Register(Register::Sub(RegisterX64::RDX, Part::Doubleword))
        )
    }) {
        (asm::Block::new(), asm::Block::new())
    } else {
        spill_edx_div_c(line, al)
    }
}

fn spill_edx_div_c(line: usize, al: &mut allocator::Allocator) -> (asm::Block, asm::Block) {
    if al.live_at(line).contains(&Place::Register(Register::Sub(
        RegisterX64::RDX,
        Part::Doubleword,
    ))) {
        let offset = al.alloc_stack();
        let mut spill = asm::Block::new();
        spill.emit(AsmX32::Mov(
            Place::Indirect(Indirect::new(
                Register::Register(RegisterX64::RBP),
                offset,
                Size::Doubleword,
            )),
            Value::Register(Register::Sub(RegisterX64::RDX, Part::Doubleword)),
        ));

        let mut unspill = asm::Block::new();
        unspill.emit(AsmX32::Mov(
            Place::Register(Register::Sub(RegisterX64::RDX, Part::Doubleword)),
            Value::Indirect(Indirect::new(
                Register::Register(RegisterX64::RBP),
                offset,
                Size::Doubleword,
            )),
        ));

        (spill, unspill)
    } else {
        (asm::Block::new(), asm::Block::new())
    }
}

fn spill_eax_div_cc(
    line: usize,
    al: &mut allocator::Allocator,
    lhs: i32,
    rhs: tac::ID,
    id: tac::ID,
) -> (asm::Block, asm::Block) {
    let (mut spill, unspill) = if matches!(
        al.get(rhs),
        Place::Register(Register::Sub(RegisterX64::RAX, Part::Doubleword))
    ) || al.free_at(line).contains(&RegisterX64::RAX)
        || matches!(
            al.get(id),
            Place::Register(Register::Sub(RegisterX64::RAX, Part::Doubleword))
        ) {
        (asm::Block::new(), asm::Block::new())
    } else {
        spill_eax(line, al)
    };

    spill.emit(AsmX32::Mov(
        Place::Register(Register::Sub(RegisterX64::RAX, Part::Doubleword)),
        Value::Const(lhs),
    ));

    (spill, unspill)
}

fn spill_eax_div_ccc(
    line: usize,
    al: &mut allocator::Allocator,
    lhs: i32,
    id: tac::ID,
) -> (asm::Block, asm::Block) {
    let (mut spill, unspill) = if al.free_at(line).contains(&RegisterX64::RAX)
        || matches!(
            al.get(id),
            Place::Register(Register::Sub(RegisterX64::RAX, Part::Doubleword))
        ) {
        (asm::Block::new(), asm::Block::new())
    } else {
        spill_eax(line, al)
    };

    spill.emit(AsmX32::Mov(
        Place::Register(Register::Sub(RegisterX64::RAX, Part::Doubleword)),
        Value::Const(lhs),
    ));

    (spill, unspill)
}

fn translate(
    line: usize,
    mut map: &mut allocator::Allocator,
    InstructionLine(i, id): InstructionLine,
) -> asm::Block {
    let mut b = asm::Block::new();
    match i {
        // ADD
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Arithmetic(tac::ArithmeticOp::Add),
            tac::Value::ID(lhs),
            tac::Value::ID(rhs),
        )) => {
            b += checked_mov(line, &mut map, lhs, id.unwrap());
            b += checked_add(line, &mut map, rhs, id.unwrap());
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Arithmetic(tac::ArithmeticOp::Add),
            tac::Value::ID(lhs),
            tac::Value::Const(tac::Const::Int(rhs)),
        )) => {
            b += checked_mov(line, &mut map, lhs, id.unwrap());
            b.emit(AsmX32::Add(map.get(id.unwrap()), Value::Const(rhs)));
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Arithmetic(tac::ArithmeticOp::Add),
            tac::Value::Const(tac::Const::Int(lhs)),
            tac::Value::ID(rhs),
        )) => {
            b.emit(AsmX32::Mov(map.get(id.unwrap()), map.get(rhs).into()));
            b.emit(AsmX32::Add(map.get(id.unwrap()), Value::Const(lhs)));
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Arithmetic(tac::ArithmeticOp::Add),
            tac::Value::Const(tac::Const::Int(lhs)),
            tac::Value::Const(tac::Const::Int(rhs)),
        )) => {
            b.emit(AsmX32::Mov(map.get(id.unwrap()), Value::Const(rhs)));
            b.emit(AsmX32::Add(map.get(id.unwrap()), Value::Const(lhs)));
        }
        // SUB
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Arithmetic(tac::ArithmeticOp::Sub),
            tac::Value::ID(lhs),
            tac::Value::ID(rhs),
        )) => {
            b += checked_mov(line, &mut map, lhs, id.unwrap());
            b += checked_sub(line, &mut map, rhs, id.unwrap());
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Arithmetic(tac::ArithmeticOp::Sub),
            tac::Value::ID(lhs),
            tac::Value::Const(tac::Const::Int(rhs)),
        )) => {
            b += checked_mov(line, &mut map, lhs, id.unwrap());
            b.emit(AsmX32::Sub(map.get(id.unwrap()), Value::Const(rhs)));
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Arithmetic(tac::ArithmeticOp::Sub),
            tac::Value::Const(tac::Const::Int(lhs)),
            tac::Value::ID(rhs),
        )) => {
            b.emit(AsmX32::Mov(map.get(id.unwrap()), Value::Const(lhs).into()));
            b.emit(AsmX32::Sub(map.get(id.unwrap()), map.get(rhs).into()));
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Arithmetic(tac::ArithmeticOp::Sub),
            tac::Value::Const(tac::Const::Int(lhs)),
            tac::Value::Const(tac::Const::Int(rhs)),
        )) => {
            b.emit(AsmX32::Mov(map.get(id.unwrap()), Value::Const(lhs).into()));
            b.emit(AsmX32::Sub(map.get(id.unwrap()), Value::Const(rhs).into()));
        }
        // MUL
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Arithmetic(tac::ArithmeticOp::Mul),
            tac::Value::ID(lhs),
            tac::Value::ID(rhs),
        )) => {
            b += checked_mov(line, &mut map, lhs, id.unwrap());
            b.emit(AsmX32::Mul(map.get(id.unwrap()), map.get(rhs).into()));
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Arithmetic(tac::ArithmeticOp::Mul),
            tac::Value::ID(lhs),
            tac::Value::Const(tac::Const::Int(rhs)),
        )) => {
            b += imul_constant(line, map, lhs, rhs, id.unwrap());
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Arithmetic(tac::ArithmeticOp::Mul),
            tac::Value::Const(tac::Const::Int(lhs)),
            tac::Value::ID(rhs),
        )) => {
            b += imul_constant(line, map, rhs, lhs, id.unwrap());
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Arithmetic(tac::ArithmeticOp::Mul),
            tac::Value::Const(tac::Const::Int(lhs)),
            tac::Value::Const(tac::Const::Int(rhs)),
        )) => {
            b.emit(AsmX32::Mov(map.get(id.unwrap()), Value::Const(lhs).into()));
            b.emit(AsmX32::Mul(map.get(id.unwrap()), Value::Const(rhs).into()));
        }
        // DIV
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Arithmetic(tac::ArithmeticOp::Div),
            tac::Value::ID(lhs),
            tac::Value::ID(rhs),
        )) => {
            let (eax_spill, eax_un_spill) = spill_eax_div(line, map, lhs, id.unwrap());
            let (spill_edx, un_spill_edx) = spill_edx_if_not(line, map, &[rhs, id.unwrap()]);

            b += eax_spill;
            b += spill_edx;

            b.emit(AsmX32::Convert(Size::Doubleword));
            b.emit(AsmX32::Div(map.get(rhs).into()));
            b.emit(AsmX32::Mov(
                map.get(id.unwrap()),
                Value::Register(Register::Sub(RegisterX64::RAX, Part::Doubleword)),
            ));

            b += eax_un_spill;
            b += un_spill_edx;
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Arithmetic(tac::ArithmeticOp::Div),
            tac::Value::ID(lhs),
            tac::Value::Const(tac::Const::Int(rhs)),
        )) => {
            let (eax_spill, eax_un_spill) = spill_eax_div(line, map, lhs, id.unwrap());
            let (spill_edx, un_spill_edx) = spill_edx_if_not(line, map, &[id.unwrap()]);
            let (divisor, divisor_spill, divisor_unspill) = space_for_divisor(line, map, rhs);

            b += eax_spill;
            b += spill_edx;
            b += divisor_spill;

            b.emit(AsmX32::Convert(Size::Doubleword));
            b.emit(AsmX32::Div(divisor));
            b.emit(AsmX32::Mov(
                map.get(id.unwrap()),
                Value::Register(Register::Sub(RegisterX64::RAX, Part::Doubleword)),
            ));

            b += eax_un_spill;
            b += un_spill_edx;
            b += divisor_unspill;
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Arithmetic(tac::ArithmeticOp::Div),
            tac::Value::Const(tac::Const::Int(lhs)),
            tac::Value::ID(rhs),
        )) => {
            let (eax_spill, eax_un_spill) = spill_eax_div_cc(line, map, lhs, rhs, id.unwrap());
            let (spill_edx, un_spill_edx) = spill_edx_if_not(line, map, &[id.unwrap()]);

            b += eax_spill;
            b += spill_edx;

            b.emit(AsmX32::Convert(Size::Doubleword));
            b.emit(AsmX32::Div(map.get(rhs).into()));
            b.emit(AsmX32::Mov(
                map.get(id.unwrap()),
                Value::Register(Register::Sub(RegisterX64::RAX, Part::Doubleword)),
            ));

            b += eax_un_spill;
            b += un_spill_edx;
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Arithmetic(tac::ArithmeticOp::Div),
            tac::Value::Const(tac::Const::Int(lhs)),
            tac::Value::Const(tac::Const::Int(rhs)),
        )) => {
            let (eax_spill, eax_un_spill) = spill_eax_div_ccc(line, map, lhs, id.unwrap());
            let (spill_edx, un_spill_edx) = spill_edx_if_not(line, map, &[id.unwrap()]);
            let (divisor, divisor_spill, divisor_unspill) = space_for_divisor(line, map, rhs);

            b += eax_spill;
            b += spill_edx;
            b += divisor_spill;

            b.emit(AsmX32::Convert(Size::Doubleword));
            b.emit(AsmX32::Div(divisor));
            b.emit(AsmX32::Mov(
                map.get(id.unwrap()),
                Value::Register(Register::Sub(RegisterX64::RAX, Part::Doubleword)),
            ));

            b += eax_un_spill;
            b += un_spill_edx;
            b += divisor_unspill;
        }
        // MOD
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Arithmetic(tac::ArithmeticOp::Mod),
            tac::Value::ID(lhs),
            tac::Value::ID(rhs),
        )) => {
            let (eax_spill, eax_un_spill) = spill_eax_div(line, map, lhs, id.unwrap());
            let (spill_edx, un_spill_edx) = spill_edx_if_not(line, map, &[rhs, id.unwrap()]);

            b += eax_spill;
            b += spill_edx;

            b.emit(AsmX32::Convert(Size::Doubleword));
            b.emit(AsmX32::Div(map.get(rhs).into()));
            b.emit(AsmX32::Mov(
                map.get(id.unwrap()),
                Value::Register(Register::Sub(RegisterX64::RDX, Part::Doubleword)),
            ));

            b += eax_un_spill;
            b += un_spill_edx;
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Arithmetic(tac::ArithmeticOp::Mod),
            tac::Value::ID(lhs),
            tac::Value::Const(tac::Const::Int(rhs)),
        )) => {
            let (eax_spill, eax_un_spill) = spill_eax_div(line, map, lhs, id.unwrap());
            let (spill_edx, un_spill_edx) = spill_edx_if_not(line, map, &[id.unwrap()]);
            let (divisor, divisor_spill, divisor_unspill) = space_for_divisor(line, map, rhs);

            b += eax_spill;
            b += spill_edx;
            b += divisor_spill;

            b.emit(AsmX32::Convert(Size::Doubleword));
            b.emit(AsmX32::Div(divisor));
            b.emit(AsmX32::Mov(
                map.get(id.unwrap()),
                Value::Register(Register::Sub(RegisterX64::RDX, Part::Doubleword)),
            ));

            b += eax_un_spill;
            b += un_spill_edx;
            b += divisor_unspill;
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Arithmetic(tac::ArithmeticOp::Mod),
            tac::Value::Const(tac::Const::Int(lhs)),
            tac::Value::ID(rhs),
        )) => {
            let (eax_spill, eax_un_spill) = spill_eax_div_cc(line, map, lhs, rhs, id.unwrap());
            let (spill_edx, un_spill_edx) = spill_edx_if_not(line, map, &[id.unwrap()]);

            b += eax_spill;
            b += spill_edx;

            b.emit(AsmX32::Convert(Size::Doubleword));
            b.emit(AsmX32::Div(map.get(rhs)));
            b.emit(AsmX32::Mov(
                map.get(id.unwrap()),
                Value::Register(Register::Sub(RegisterX64::RDX, Part::Doubleword)),
            ));

            b += eax_un_spill;
            b += un_spill_edx;
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Arithmetic(tac::ArithmeticOp::Mod),
            tac::Value::Const(tac::Const::Int(lhs)),
            tac::Value::Const(tac::Const::Int(rhs)),
        )) => {
            let (eax_spill, eax_un_spill) = spill_eax_div_ccc(line, map, lhs, id.unwrap());
            let (spill_edx, un_spill_edx) = spill_edx_if_not(line, map, &[id.unwrap()]);
            let (divisor, divisor_spill, divisor_unspill) = space_for_divisor(line, map, rhs);

            b += eax_spill;
            b += spill_edx;
            b += divisor_spill;

            b.emit(AsmX32::Convert(Size::Doubleword));
            b.emit(AsmX32::Div(divisor));
            b.emit(AsmX32::Mov(
                map.get(id.unwrap()),
                Value::Register(Register::Sub(RegisterX64::RDX, Part::Doubleword)),
            ));

            b += eax_un_spill;
            b += un_spill_edx;
            b += divisor_unspill;
        }
        // NEG
        tac::Instruction::Op(tac::Op::Unary(tac::UnOp::Neg, tac::Value::ID(v))) => {
            // comment: clang approach with 1 line instruction(sub) require to have
            // prepared register(equal 0)
            b.emit(AsmX32::Xor(
                map.get(id.unwrap()),
                map.get(id.unwrap()).into(),
            ));
            b.emit(AsmX32::Sub(map.get(id.unwrap()), map.get(v).into()));
        }
        tac::Instruction::Op(tac::Op::Unary(
            tac::UnOp::Neg,
            tac::Value::Const(tac::Const::Int(v)),
        )) => {
            b.emit(AsmX32::Mov(map.get(id.unwrap()), Value::Const(v)));
            b.emit(AsmX32::Neg(map.get(id.unwrap())));
        }
        // Bitwise
        tac::Instruction::Op(tac::Op::Unary(tac::UnOp::BitComplement, tac::Value::ID(v))) => {
            b.emit(AsmX32::Mov(map.get(id.unwrap()), map.get(v).into()));
            b.emit(AsmX32::Xor(map.get(id.unwrap()), Value::Const(-1)));
        }
        tac::Instruction::Op(tac::Op::Unary(
            tac::UnOp::BitComplement,
            tac::Value::Const(tac::Const::Int(v)),
        )) => {
            b.emit(AsmX32::Mov(map.get(id.unwrap()), Value::Const(v)));
            b.emit(AsmX32::Xor(map.get(id.unwrap()), Value::Const(-1)));
        }
        // Logicneg
        tac::Instruction::Op(tac::Op::Unary(tac::UnOp::LogicNeg, tac::Value::ID(v))) => {
            let (reg, spill, unspill) = match map.get(id.unwrap()) {
                Place::Indirect(..) => {
                    let (reg, spill, mut unspill) = get_register(line, map);
                    unspill.emit(AsmX32::Mov(
                        map.get(id.unwrap()),
                        Value::Register(Register::Sub(reg.clone(), Part::Doubleword)),
                    ));
                    (reg, spill, unspill)
                }
                Place::Register(Register::Register(reg)) => {
                    (reg.clone(), asm::Block::new(), asm::Block::new())
                }
                Place::Register(Register::Sub(reg, ..)) => {
                    (reg.clone(), asm::Block::new(), asm::Block::new())
                }
                _ => unreachable!(),
            };

            b += spill;

            b.emit(AsmX32::Cmp(map.get(v), Value::Const(0)));
            b.emit(AsmX32::Setne(Place::Register(Register::Sub(
                reg.clone(),
                Part::Byte,
            ))));
            b.emit(AsmX32::Xor(
                Place::Register(Register::Sub(reg.clone(), Part::Byte)),
                Value::Const(-1),
            ));
            b.emit(AsmX32::And(
                Place::Register(Register::Sub(reg.clone(), Part::Byte)),
                Value::Const(1),
            ));
            b.emit(AsmX32::Movzx(
                Place::Register(Register::Sub(reg.clone(), Part::Doubleword)),
                Value::Register(Register::Sub(reg.clone(), Part::Byte)),
            ));

            b += unspill;
        }
        tac::Instruction::Op(tac::Op::Unary(
            tac::UnOp::LogicNeg,
            tac::Value::Const(tac::Const::Int(v)),
        )) => {
            let (reg, spill, unspill) = match map.get(id.unwrap()) {
                Place::Indirect(..) => {
                    let (reg, spill, mut unspill) = get_register(line, map);
                    unspill.emit(AsmX32::Mov(
                        map.get(id.unwrap()),
                        Value::Register(Register::Sub(reg.clone(), Part::Doubleword)),
                    ));
                    (reg, spill, unspill)
                }
                Place::Register(Register::Register(reg)) => {
                    (reg.clone(), asm::Block::new(), asm::Block::new())
                }
                Place::Register(Register::Sub(reg, ..)) => {
                    (reg.clone(), asm::Block::new(), asm::Block::new())
                }
                _ => unreachable!(),
            };

            b += spill;

            let reg_place = Place::Register(Register::Sub(reg.clone(), Part::Doubleword));
            b.emit(AsmX32::Mov(reg_place.clone(), Value::Const(v)));

            b.emit(AsmX32::Cmp(reg_place.clone(), Value::Const(0)));
            b.emit(AsmX32::Setne(Place::Register(Register::Sub(
                reg.clone(),
                Part::Byte,
            ))));
            b.emit(AsmX32::Xor(
                Place::Register(Register::Sub(reg.clone(), Part::Byte)),
                Value::Const(-1),
            ));
            b.emit(AsmX32::And(
                Place::Register(Register::Sub(reg.clone(), Part::Byte)),
                Value::Const(1),
            ));
            b.emit(AsmX32::Movzx(
                reg_place,
                Value::Register(Register::Sub(reg.clone(), Part::Byte)),
            ));

            b += unspill;
        }
        // EQ
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Equality(tac::EqualityOp::Equal),
            tac::Value::ID(lhs),
            tac::Value::ID(rhs),
        )) => {
            let (reg, spill, unspill) = match map.get(id.unwrap()) {
                Place::Register(Register::Register(reg))
                | Place::Register(Register::Sub(reg, ..)) => {
                    (reg, asm::Block::new(), asm::Block::new())
                }
                _ => get_register(line, map),
            };

            b += spill;

            b += checked_cmp(line, &mut map, lhs, rhs);
            b.emit(AsmX32::Sete(Place::Register(Register::Sub(
                reg.clone(),
                Part::Byte,
            ))));
            b.emit(AsmX32::And(
                Place::Register(Register::Sub(reg.clone(), Part::Byte)),
                Value::Const(1),
            ));
            b.emit(AsmX32::Movzx(
                Place::Register(Register::Sub(reg.clone(), Part::Doubleword)),
                Value::Register(Register::Sub(reg.clone(), Part::Byte)),
            ));
            b.emit(AsmX32::Mov(
                map.get(id.unwrap()),
                Value::Register(Register::Sub(reg, Part::Doubleword)),
            ));

            b += unspill;
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Equality(tac::EqualityOp::Equal),
            tac::Value::ID(lhs),
            tac::Value::Const(tac::Const::Int(rhs)),
        )) => {
            let (reg, spill, unspill) = match map.get(id.unwrap()) {
                Place::Register(Register::Register(reg))
                | Place::Register(Register::Sub(reg, ..)) => {
                    (reg, asm::Block::new(), asm::Block::new())
                }
                _ => get_register(line, map),
            };

            b += spill;

            b.emit(AsmX32::Cmp(map.get(lhs), Value::Const(rhs)));
            b.emit(AsmX32::Sete(Place::Register(Register::Sub(
                reg.clone(),
                Part::Byte,
            ))));
            b.emit(AsmX32::And(
                Place::Register(Register::Sub(reg.clone(), Part::Byte)),
                Value::Const(1),
            ));
            b.emit(AsmX32::Movzx(
                Place::Register(Register::Sub(reg.clone(), Part::Doubleword)),
                Value::Register(Register::Sub(reg.clone(), Part::Byte)),
            ));
            b.emit(AsmX32::Mov(
                map.get(id.unwrap()),
                Value::Register(Register::Sub(reg, Part::Doubleword)),
            ));

            b += unspill;
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Equality(tac::EqualityOp::Equal),
            tac::Value::Const(tac::Const::Int(lhs)),
            tac::Value::ID(rhs),
        )) => {
            let (reg, spill, unspill) = match map.get(id.unwrap()) {
                Place::Register(Register::Register(reg))
                | Place::Register(Register::Sub(reg, ..)) => {
                    (reg, asm::Block::new(), asm::Block::new())
                }
                _ => get_register(line, map),
            };

            b += spill;

            b.emit(AsmX32::Cmp(map.get(rhs), Value::Const(lhs)));
            b.emit(AsmX32::Sete(Place::Register(Register::Sub(
                reg.clone(),
                Part::Byte,
            ))));
            b.emit(AsmX32::And(
                Place::Register(Register::Sub(reg.clone(), Part::Byte)),
                Value::Const(1),
            ));
            b.emit(AsmX32::Movzx(
                Place::Register(Register::Sub(reg.clone(), Part::Doubleword)),
                Value::Register(Register::Sub(reg.clone(), Part::Byte)),
            ));
            b.emit(AsmX32::Mov(
                map.get(id.unwrap()),
                Value::Register(Register::Sub(reg, Part::Doubleword)),
            ));

            b += unspill;
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Equality(tac::EqualityOp::Equal),
            tac::Value::Const(tac::Const::Int(lhs)),
            tac::Value::Const(tac::Const::Int(rhs)),
        )) => {
            let (reg, spill, unspill) = match map.get(id.unwrap()) {
                Place::Register(Register::Register(reg))
                | Place::Register(Register::Sub(reg, ..)) => {
                    (reg, asm::Block::new(), asm::Block::new())
                }
                _ => get_register(line, map),
            };

            let offset = map.alloc_stack();
            let tmp = Place::Indirect(Indirect::new(
                Register::Register(RegisterX64::RBP),
                offset,
                Size::Doubleword,
            ));

            b += spill;

            b.emit(AsmX32::Mov(tmp.clone(), Value::Const(rhs)));
            b.emit(AsmX32::Cmp(tmp.clone(), Value::Const(lhs).into()));
            b.emit(AsmX32::Sete(Place::Register(Register::Sub(
                reg.clone(),
                Part::Byte,
            ))));
            b.emit(AsmX32::And(
                Place::Register(Register::Sub(reg.clone(), Part::Byte)),
                Value::Const(1),
            ));
            b.emit(AsmX32::Movzx(
                Place::Register(Register::Sub(reg.clone(), Part::Doubleword)),
                Value::Register(Register::Sub(reg.clone(), Part::Byte)),
            ));
            b.emit(AsmX32::Mov(
                map.get(id.unwrap()),
                Value::Register(Register::Sub(reg, Part::Doubleword)),
            ));

            b += unspill;
        }
        // NOTEQ
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Equality(tac::EqualityOp::NotEq),
            tac::Value::ID(lhs),
            tac::Value::ID(rhs),
        )) => {
            let (reg, spill, unspill) = match map.get(id.unwrap()) {
                Place::Register(Register::Register(reg))
                | Place::Register(Register::Sub(reg, ..)) => {
                    (reg, asm::Block::new(), asm::Block::new())
                }
                _ => get_register(line, map),
            };

            b += spill;

            b += checked_cmp(line, &mut map, lhs, rhs);
            b.emit(AsmX32::Setne(Place::Register(Register::Sub(
                reg.clone(),
                Part::Byte,
            ))));
            b.emit(AsmX32::And(
                Place::Register(Register::Sub(reg.clone(), Part::Byte)),
                Value::Const(1),
            ));
            b.emit(AsmX32::Movzx(
                Place::Register(Register::Sub(reg.clone(), Part::Doubleword)),
                Value::Register(Register::Sub(reg.clone(), Part::Byte)),
            ));
            b.emit(AsmX32::Mov(
                map.get(id.unwrap()),
                Value::Register(Register::Sub(reg, Part::Doubleword)),
            ));

            b += unspill;
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Equality(tac::EqualityOp::NotEq),
            tac::Value::ID(lhs),
            tac::Value::Const(tac::Const::Int(rhs)),
        )) => {
            let (reg, spill, unspill) = match map.get(id.unwrap()) {
                Place::Register(Register::Register(reg))
                | Place::Register(Register::Sub(reg, ..)) => {
                    (reg, asm::Block::new(), asm::Block::new())
                }
                _ => get_register(line, map),
            };

            b += spill;

            b.emit(AsmX32::Cmp(map.get(lhs), Value::Const(rhs)));
            b.emit(AsmX32::Setne(Place::Register(Register::Sub(
                reg.clone(),
                Part::Byte,
            ))));
            b.emit(AsmX32::And(
                Place::Register(Register::Sub(reg.clone(), Part::Byte)),
                Value::Const(1),
            ));
            b.emit(AsmX32::Movzx(
                Place::Register(Register::Sub(reg.clone(), Part::Doubleword)),
                Value::Register(Register::Sub(reg.clone(), Part::Byte)),
            ));
            b.emit(AsmX32::Mov(
                map.get(id.unwrap()),
                Value::Register(Register::Sub(reg, Part::Doubleword)),
            ));

            b += unspill;
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Equality(tac::EqualityOp::NotEq),
            tac::Value::Const(tac::Const::Int(lhs)),
            tac::Value::ID(rhs),
        )) => {
            let (reg, spill, unspill) = match map.get(id.unwrap()) {
                Place::Register(Register::Register(reg))
                | Place::Register(Register::Sub(reg, ..)) => {
                    (reg, asm::Block::new(), asm::Block::new())
                }
                _ => get_register(line, map),
            };

            b += spill;

            b.emit(AsmX32::Cmp(map.get(rhs), Value::Const(lhs)));
            b.emit(AsmX32::Sete(Place::Register(Register::Sub(
                reg.clone(),
                Part::Byte,
            ))));
            b.emit(AsmX32::And(
                Place::Register(Register::Sub(reg.clone(), Part::Byte)),
                Value::Const(1),
            ));
            b.emit(AsmX32::Movzx(
                Place::Register(Register::Sub(reg.clone(), Part::Doubleword)),
                Value::Register(Register::Sub(reg.clone(), Part::Byte)),
            ));
            b.emit(AsmX32::Mov(
                map.get(id.unwrap()),
                Value::Register(Register::Sub(reg, Part::Doubleword)),
            ));

            b += unspill;
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Equality(tac::EqualityOp::NotEq),
            tac::Value::Const(tac::Const::Int(lhs)),
            tac::Value::Const(tac::Const::Int(rhs)),
        )) => {
            let (reg, spill, unspill) = match map.get(id.unwrap()) {
                Place::Register(Register::Register(reg))
                | Place::Register(Register::Sub(reg, ..)) => {
                    (reg, asm::Block::new(), asm::Block::new())
                }
                _ => get_register(line, map),
            };

            let offset = map.alloc_stack();
            let tmp = Place::Indirect(Indirect::new(
                Register::Register(RegisterX64::RBP),
                offset,
                Size::Doubleword,
            ));

            b += spill;

            b.emit(AsmX32::Mov(tmp.clone(), Value::Const(rhs)));
            b.emit(AsmX32::Cmp(tmp.clone(), Value::Const(lhs).into()));
            b.emit(AsmX32::Setne(Place::Register(Register::Sub(
                reg.clone(),
                Part::Byte,
            ))));
            b.emit(AsmX32::And(
                Place::Register(Register::Sub(reg.clone(), Part::Byte)),
                Value::Const(1),
            ));
            b.emit(AsmX32::Movzx(
                Place::Register(Register::Sub(reg.clone(), Part::Doubleword)),
                Value::Register(Register::Sub(reg.clone(), Part::Byte)),
            ));
            b.emit(AsmX32::Mov(
                map.get(id.unwrap()),
                Value::Register(Register::Sub(reg, Part::Doubleword)),
            ));

            b += unspill;
        }
        // LESS
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Relational(tac::RelationalOp::Less),
            tac::Value::ID(lhs),
            tac::Value::ID(rhs),
        )) => {
            let (reg, spill, unspill) = match map.get(id.unwrap()) {
                Place::Register(Register::Register(reg))
                | Place::Register(Register::Sub(reg, ..)) => {
                    (reg, asm::Block::new(), asm::Block::new())
                }
                _ => get_register(line, map),
            };

            b += spill;

            b += checked_cmp(line, &mut map, lhs, rhs);
            b.emit(AsmX32::Setl(Place::Register(Register::Sub(
                reg.clone(),
                Part::Byte,
            ))));
            b.emit(AsmX32::And(
                Place::Register(Register::Sub(reg.clone(), Part::Byte)),
                Value::Const(1),
            ));
            b.emit(AsmX32::Movzx(
                Place::Register(Register::Sub(reg.clone(), Part::Doubleword)),
                Value::Register(Register::Sub(reg.clone(), Part::Byte)),
            ));
            b.emit(AsmX32::Mov(
                map.get(id.unwrap()),
                Value::Register(Register::Sub(reg, Part::Doubleword)),
            ));

            b += unspill;
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Relational(tac::RelationalOp::Less),
            tac::Value::ID(lhs),
            tac::Value::Const(tac::Const::Int(rhs)),
        )) => {
            let (reg, spill, unspill) = match map.get(id.unwrap()) {
                Place::Register(Register::Register(reg))
                | Place::Register(Register::Sub(reg, ..)) => {
                    (reg, asm::Block::new(), asm::Block::new())
                }
                _ => get_register(line, map),
            };

            b += spill;

            b.emit(AsmX32::Cmp(map.get(lhs), Value::Const(rhs)));
            b.emit(AsmX32::Setl(Place::Register(Register::Sub(
                reg.clone(),
                Part::Byte,
            ))));
            b.emit(AsmX32::And(
                Place::Register(Register::Sub(reg.clone(), Part::Byte)),
                Value::Const(1),
            ));
            b.emit(AsmX32::Movzx(
                Place::Register(Register::Sub(reg.clone(), Part::Doubleword)),
                Value::Register(Register::Sub(reg.clone(), Part::Byte)),
            ));
            b.emit(AsmX32::Mov(
                map.get(id.unwrap()),
                Value::Register(Register::Sub(reg, Part::Doubleword)),
            ));

            b += unspill;
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Relational(tac::RelationalOp::Less),
            tac::Value::Const(tac::Const::Int(lhs)),
            tac::Value::ID(rhs),
        )) => {
            let (reg, spill, unspill) = match map.get(id.unwrap()) {
                Place::Register(Register::Register(reg))
                | Place::Register(Register::Sub(reg, ..)) => {
                    (reg, asm::Block::new(), asm::Block::new())
                }
                _ => get_register(line, map),
            };

            b += spill;

            b.emit(AsmX32::Mov(
                Place::Register(Register::Sub(reg.clone(), Part::Doubleword)),
                Value::Const(lhs),
            ));
            b.emit(AsmX32::Cmp(
                Place::Register(Register::Sub(reg.clone(), Part::Doubleword)),
                map.get(rhs).into(),
            ));
            b.emit(AsmX32::Setl(Place::Register(Register::Sub(
                reg.clone(),
                Part::Byte,
            ))));
            b.emit(AsmX32::And(
                Place::Register(Register::Sub(reg.clone(), Part::Byte)),
                Value::Const(1),
            ));
            b.emit(AsmX32::Movzx(
                Place::Register(Register::Sub(reg.clone(), Part::Doubleword)),
                Value::Register(Register::Sub(reg.clone(), Part::Byte)),
            ));
            b.emit(AsmX32::Mov(
                map.get(id.unwrap()),
                Value::Register(Register::Sub(reg, Part::Doubleword)),
            ));

            b += unspill;
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Relational(tac::RelationalOp::Less),
            tac::Value::Const(tac::Const::Int(lhs)),
            tac::Value::Const(tac::Const::Int(rhs)),
        )) => {
            let (reg, spill, unspill) = match map.get(id.unwrap()) {
                Place::Register(Register::Register(reg))
                | Place::Register(Register::Sub(reg, ..)) => {
                    (reg, asm::Block::new(), asm::Block::new())
                }
                _ => get_register(line, map),
            };

            let offset = map.alloc_stack();
            let tmp = Place::Indirect(Indirect::new(
                Register::Register(RegisterX64::RBP),
                offset,
                Size::Doubleword,
            ));

            b += spill;

            b.emit(AsmX32::Mov(tmp.clone(), Value::Const(lhs)));
            b.emit(AsmX32::Cmp(tmp.clone(), Value::Const(rhs).into()));
            b.emit(AsmX32::Setl(Place::Register(Register::Sub(
                reg.clone(),
                Part::Byte,
            ))));
            b.emit(AsmX32::And(
                Place::Register(Register::Sub(reg.clone(), Part::Byte)),
                Value::Const(1),
            ));
            b.emit(AsmX32::Movzx(
                Place::Register(Register::Sub(reg.clone(), Part::Doubleword)),
                Value::Register(Register::Sub(reg.clone(), Part::Byte)),
            ));
            b.emit(AsmX32::Mov(
                map.get(id.unwrap()),
                Value::Register(Register::Sub(reg, Part::Doubleword)),
            ));

            b += unspill;
        }
        // LESS_OR_EQUAL
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Relational(tac::RelationalOp::LessOrEq),
            tac::Value::ID(lhs),
            tac::Value::ID(rhs),
        )) => {
            let (reg, spill, unspill) = match map.get(id.unwrap()) {
                Place::Register(Register::Register(reg))
                | Place::Register(Register::Sub(reg, ..)) => {
                    (reg, asm::Block::new(), asm::Block::new())
                }
                _ => get_register(line, map),
            };

            b += spill;

            b += checked_cmp(line, &mut map, lhs, rhs);
            b.emit(AsmX32::Setle(Place::Register(Register::Sub(
                reg.clone(),
                Part::Byte,
            ))));
            b.emit(AsmX32::And(
                Place::Register(Register::Sub(reg.clone(), Part::Byte)),
                Value::Const(1),
            ));
            b.emit(AsmX32::Movzx(
                Place::Register(Register::Sub(reg.clone(), Part::Doubleword)),
                Value::Register(Register::Sub(reg.clone(), Part::Byte)),
            ));
            b.emit(AsmX32::Mov(
                map.get(id.unwrap()),
                Value::Register(Register::Sub(reg, Part::Doubleword)),
            ));

            b += unspill;
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Relational(tac::RelationalOp::LessOrEq),
            tac::Value::ID(lhs),
            tac::Value::Const(tac::Const::Int(rhs)),
        )) => {
            let (reg, spill, unspill) = match map.get(id.unwrap()) {
                Place::Register(Register::Register(reg))
                | Place::Register(Register::Sub(reg, ..)) => {
                    (reg, asm::Block::new(), asm::Block::new())
                }
                _ => get_register(line, map),
            };

            b += spill;

            b.emit(AsmX32::Cmp(map.get(lhs), Value::Const(rhs)));
            b.emit(AsmX32::Setle(Place::Register(Register::Sub(
                reg.clone(),
                Part::Byte,
            ))));
            b.emit(AsmX32::And(
                Place::Register(Register::Sub(reg.clone(), Part::Byte)),
                Value::Const(1),
            ));
            b.emit(AsmX32::Movzx(
                Place::Register(Register::Sub(reg.clone(), Part::Doubleword)),
                Value::Register(Register::Sub(reg.clone(), Part::Byte)),
            ));
            b.emit(AsmX32::Mov(
                map.get(id.unwrap()),
                Value::Register(Register::Sub(reg, Part::Doubleword)),
            ));

            b += unspill;
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Relational(tac::RelationalOp::LessOrEq),
            tac::Value::Const(tac::Const::Int(lhs)),
            tac::Value::ID(rhs),
        )) => {
            let (reg, spill, unspill) = match map.get(id.unwrap()) {
                Place::Register(Register::Register(reg))
                | Place::Register(Register::Sub(reg, ..)) => {
                    (reg, asm::Block::new(), asm::Block::new())
                }
                _ => get_register(line, map),
            };

            b += spill;

            b.emit(AsmX32::Mov(
                Place::Register(Register::Sub(reg.clone(), Part::Doubleword)),
                Value::Const(lhs),
            ));
            b.emit(AsmX32::Cmp(
                Place::Register(Register::Sub(reg.clone(), Part::Doubleword)),
                map.get(rhs).into(),
            ));
            b.emit(AsmX32::Setle(Place::Register(Register::Sub(
                reg.clone(),
                Part::Byte,
            ))));
            b.emit(AsmX32::And(
                Place::Register(Register::Sub(reg.clone(), Part::Byte)),
                Value::Const(1),
            ));
            b.emit(AsmX32::Movzx(
                Place::Register(Register::Sub(reg.clone(), Part::Doubleword)),
                Value::Register(Register::Sub(reg.clone(), Part::Byte)),
            ));
            b.emit(AsmX32::Mov(
                map.get(id.unwrap()),
                Value::Register(Register::Sub(reg, Part::Doubleword)),
            ));

            b += unspill;
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Relational(tac::RelationalOp::LessOrEq),
            tac::Value::Const(tac::Const::Int(lhs)),
            tac::Value::Const(tac::Const::Int(rhs)),
        )) => {
            let (reg, spill, unspill) = match map.get(id.unwrap()) {
                Place::Register(Register::Register(reg))
                | Place::Register(Register::Sub(reg, ..)) => {
                    (reg, asm::Block::new(), asm::Block::new())
                }
                _ => get_register(line, map),
            };

            let offset = map.alloc_stack();
            let tmp = Place::Indirect(Indirect::new(
                Register::Register(RegisterX64::RBP),
                offset,
                Size::Doubleword,
            ));

            b += spill;

            b.emit(AsmX32::Mov(tmp.clone(), Value::Const(lhs)));
            b.emit(AsmX32::Cmp(tmp.clone(), Value::Const(rhs).into()));
            b.emit(AsmX32::Setle(Place::Register(Register::Sub(
                reg.clone(),
                Part::Byte,
            ))));
            b.emit(AsmX32::And(
                Place::Register(Register::Sub(reg.clone(), Part::Byte)),
                Value::Const(1),
            ));
            b.emit(AsmX32::Movzx(
                Place::Register(Register::Sub(reg.clone(), Part::Doubleword)),
                Value::Register(Register::Sub(reg.clone(), Part::Byte)),
            ));
            b.emit(AsmX32::Mov(
                map.get(id.unwrap()),
                Value::Register(Register::Sub(reg, Part::Doubleword)),
            ));

            b += unspill;
        }
        // GREATER
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Relational(tac::RelationalOp::Greater),
            tac::Value::ID(lhs),
            tac::Value::ID(rhs),
        )) => {
            let (reg, spill, unspill) = match map.get(id.unwrap()) {
                Place::Register(Register::Register(reg))
                | Place::Register(Register::Sub(reg, ..)) => {
                    (reg, asm::Block::new(), asm::Block::new())
                }
                _ => get_register(line, map),
            };

            b += spill;

            b += checked_cmp(line, &mut map, lhs, rhs);
            b.emit(AsmX32::Setg(Place::Register(Register::Sub(
                reg.clone(),
                Part::Byte,
            ))));
            b.emit(AsmX32::And(
                Place::Register(Register::Sub(reg.clone(), Part::Byte)),
                Value::Const(1),
            ));
            b.emit(AsmX32::Movzx(
                Place::Register(Register::Sub(reg.clone(), Part::Doubleword)),
                Value::Register(Register::Sub(reg.clone(), Part::Byte)),
            ));
            b.emit(AsmX32::Mov(
                map.get(id.unwrap()),
                Value::Register(Register::Sub(reg, Part::Doubleword)),
            ));

            b += unspill;
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Relational(tac::RelationalOp::Greater),
            tac::Value::ID(lhs),
            tac::Value::Const(tac::Const::Int(rhs)),
        )) => {
            let (reg, spill, unspill) = match map.get(id.unwrap()) {
                Place::Register(Register::Register(reg))
                | Place::Register(Register::Sub(reg, ..)) => {
                    (reg, asm::Block::new(), asm::Block::new())
                }
                _ => get_register(line, map),
            };

            b += spill;

            b.emit(AsmX32::Cmp(map.get(lhs), Value::Const(rhs)));
            b.emit(AsmX32::Setg(Place::Register(Register::Sub(
                reg.clone(),
                Part::Byte,
            ))));
            b.emit(AsmX32::And(
                Place::Register(Register::Sub(reg.clone(), Part::Byte)),
                Value::Const(1),
            ));
            b.emit(AsmX32::Movzx(
                Place::Register(Register::Sub(reg.clone(), Part::Doubleword)),
                Value::Register(Register::Sub(reg.clone(), Part::Byte)),
            ));
            b.emit(AsmX32::Mov(
                map.get(id.unwrap()),
                Value::Register(Register::Sub(reg, Part::Doubleword)),
            ));

            b += unspill;
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Relational(tac::RelationalOp::Greater),
            tac::Value::Const(tac::Const::Int(lhs)),
            tac::Value::ID(rhs),
        )) => {
            let (reg, spill, unspill) = match map.get(id.unwrap()) {
                Place::Register(Register::Register(reg))
                | Place::Register(Register::Sub(reg, ..)) => {
                    (reg, asm::Block::new(), asm::Block::new())
                }
                _ => get_register(line, map),
            };

            b += spill;

            b.emit(AsmX32::Mov(
                Place::Register(Register::Sub(reg.clone(), Part::Doubleword)),
                Value::Const(lhs),
            ));
            b.emit(AsmX32::Cmp(
                Place::Register(Register::Sub(reg.clone(), Part::Doubleword)),
                map.get(rhs).into(),
            ));
            b.emit(AsmX32::Setg(Place::Register(Register::Sub(
                reg.clone(),
                Part::Byte,
            ))));
            b.emit(AsmX32::And(
                Place::Register(Register::Sub(reg.clone(), Part::Byte)),
                Value::Const(1),
            ));
            b.emit(AsmX32::Movzx(
                Place::Register(Register::Sub(reg.clone(), Part::Doubleword)),
                Value::Register(Register::Sub(reg.clone(), Part::Byte)),
            ));
            b.emit(AsmX32::Mov(
                map.get(id.unwrap()),
                Value::Register(Register::Sub(reg, Part::Doubleword)),
            ));

            b += unspill;
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Relational(tac::RelationalOp::Greater),
            tac::Value::Const(tac::Const::Int(lhs)),
            tac::Value::Const(tac::Const::Int(rhs)),
        )) => {
            let (reg, spill, unspill) = match map.get(id.unwrap()) {
                Place::Register(Register::Register(reg))
                | Place::Register(Register::Sub(reg, ..)) => {
                    (reg, asm::Block::new(), asm::Block::new())
                }
                _ => get_register(line, map),
            };

            let offset = map.alloc_stack();
            let tmp = Place::Indirect(Indirect::new(
                Register::Register(RegisterX64::RBP),
                offset,
                Size::Doubleword,
            ));

            b += spill;

            b.emit(AsmX32::Mov(tmp.clone(), Value::Const(lhs)));
            b.emit(AsmX32::Cmp(tmp.clone(), Value::Const(rhs).into()));
            b.emit(AsmX32::Setg(Place::Register(Register::Sub(
                reg.clone(),
                Part::Byte,
            ))));
            b.emit(AsmX32::And(
                Place::Register(Register::Sub(reg.clone(), Part::Byte)),
                Value::Const(1),
            ));
            b.emit(AsmX32::Movzx(
                Place::Register(Register::Sub(reg.clone(), Part::Doubleword)),
                Value::Register(Register::Sub(reg.clone(), Part::Byte)),
            ));
            b.emit(AsmX32::Mov(
                map.get(id.unwrap()),
                Value::Register(Register::Sub(reg, Part::Doubleword)),
            ));

            b += unspill;
        }
        // GREATER_OR_EQUAL
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Relational(tac::RelationalOp::GreaterOrEq),
            tac::Value::ID(lhs),
            tac::Value::ID(rhs),
        )) => {
            let (reg, spill, unspill) = match map.get(id.unwrap()) {
                Place::Register(Register::Register(reg))
                | Place::Register(Register::Sub(reg, ..)) => {
                    (reg, asm::Block::new(), asm::Block::new())
                }
                _ => get_register(line, map),
            };

            b += spill;

            b += checked_cmp(line, &mut map, lhs, rhs);
            b.emit(AsmX32::Setge(Place::Register(Register::Sub(
                reg.clone(),
                Part::Byte,
            ))));
            b.emit(AsmX32::And(
                Place::Register(Register::Sub(reg.clone(), Part::Byte)),
                Value::Const(1),
            ));
            b.emit(AsmX32::Movzx(
                Place::Register(Register::Sub(reg.clone(), Part::Doubleword)),
                Value::Register(Register::Sub(reg.clone(), Part::Byte)),
            ));
            b.emit(AsmX32::Mov(
                map.get(id.unwrap()),
                Value::Register(Register::Sub(reg, Part::Doubleword)),
            ));

            b += unspill;
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Relational(tac::RelationalOp::GreaterOrEq),
            tac::Value::ID(lhs),
            tac::Value::Const(tac::Const::Int(rhs)),
        )) => {
            let (reg, spill, unspill) = match map.get(id.unwrap()) {
                Place::Register(Register::Register(reg))
                | Place::Register(Register::Sub(reg, ..)) => {
                    (reg, asm::Block::new(), asm::Block::new())
                }
                _ => get_register(line, map),
            };

            b += spill;

            b.emit(AsmX32::Cmp(map.get(lhs), Value::Const(rhs)));
            b.emit(AsmX32::Setge(Place::Register(Register::Sub(
                reg.clone(),
                Part::Byte,
            ))));
            b.emit(AsmX32::And(
                Place::Register(Register::Sub(reg.clone(), Part::Byte)),
                Value::Const(1),
            ));
            b.emit(AsmX32::Movzx(
                Place::Register(Register::Sub(reg.clone(), Part::Doubleword)),
                Value::Register(Register::Sub(reg.clone(), Part::Byte)),
            ));
            b.emit(AsmX32::Mov(
                map.get(id.unwrap()),
                Value::Register(Register::Sub(reg, Part::Doubleword)),
            ));

            b += unspill;
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Relational(tac::RelationalOp::GreaterOrEq),
            tac::Value::Const(tac::Const::Int(lhs)),
            tac::Value::ID(rhs),
        )) => {
            let (reg, spill, unspill) = match map.get(id.unwrap()) {
                Place::Register(Register::Register(reg))
                | Place::Register(Register::Sub(reg, ..)) => {
                    (reg, asm::Block::new(), asm::Block::new())
                }
                _ => get_register(line, map),
            };

            b += spill;

            b.emit(AsmX32::Mov(
                Place::Register(Register::Sub(reg.clone(), Part::Doubleword)),
                Value::Const(lhs),
            ));
            b.emit(AsmX32::Cmp(
                Place::Register(Register::Sub(reg.clone(), Part::Doubleword)),
                map.get(rhs).into(),
            ));
            b.emit(AsmX32::Setge(Place::Register(Register::Sub(
                reg.clone(),
                Part::Byte,
            ))));
            b.emit(AsmX32::And(
                Place::Register(Register::Sub(reg.clone(), Part::Byte)),
                Value::Const(1),
            ));
            b.emit(AsmX32::Movzx(
                Place::Register(Register::Sub(reg.clone(), Part::Doubleword)),
                Value::Register(Register::Sub(reg.clone(), Part::Byte)),
            ));
            b.emit(AsmX32::Mov(
                map.get(id.unwrap()),
                Value::Register(Register::Sub(reg, Part::Doubleword)),
            ));

            b += unspill;
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Relational(tac::RelationalOp::GreaterOrEq),
            tac::Value::Const(tac::Const::Int(lhs)),
            tac::Value::Const(tac::Const::Int(rhs)),
        )) => {
            let (reg, spill, unspill) = match map.get(id.unwrap()) {
                Place::Register(Register::Register(reg))
                | Place::Register(Register::Sub(reg, ..)) => {
                    (reg, asm::Block::new(), asm::Block::new())
                }
                _ => get_register(line, map),
            };

            let offset = map.alloc_stack();
            let tmp = Place::Indirect(Indirect::new(
                Register::Register(RegisterX64::RBP),
                offset,
                Size::Doubleword,
            ));

            b += spill;

            b.emit(AsmX32::Mov(tmp.clone(), Value::Const(lhs)));
            b.emit(AsmX32::Cmp(tmp.clone(), Value::Const(rhs).into()));
            b.emit(AsmX32::Setge(Place::Register(Register::Sub(
                reg.clone(),
                Part::Byte,
            ))));
            b.emit(AsmX32::And(
                Place::Register(Register::Sub(reg.clone(), Part::Byte)),
                Value::Const(1),
            ));
            b.emit(AsmX32::Movzx(
                Place::Register(Register::Sub(reg.clone(), Part::Doubleword)),
                Value::Register(Register::Sub(reg.clone(), Part::Byte)),
            ));
            b.emit(AsmX32::Mov(
                map.get(id.unwrap()),
                Value::Register(Register::Sub(reg, Part::Doubleword)),
            ));

            b += unspill;
        }
        // OR
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Bit(tac::BitwiseOp::And),
            tac::Value::ID(_),
            tac::Value::ID(_),
        )) => unimplemented!(),
        // // ALLOC
        tac::Instruction::Alloc(tac::Value::Const(tac::Const::Int(v))) => {
            b.emit(AsmX32::Mov(map.get(id.unwrap()), Value::Const(v)));
        }
        tac::Instruction::Alloc(tac::Value::ID(v)) => {
            b += checked_mov(line, &mut map, v, id.unwrap());
        }
        // ASSIGN
        tac::Instruction::Assignment(id, tac::Value::Const(tac::Const::Int(v))) => {
            b.emit(AsmX32::Mov(map.get(id), Value::Const(v)));
        }
        tac::Instruction::Assignment(id, tac::Value::ID(v)) => {
            b += checked_mov(line, &mut map, v, id);
        }
        // RETURN
        tac::Instruction::ControlOp(tac::ControlOp::Return(tac::Value::ID(id))) => {
            b.emit(AsmX32::Mov(
                Place::Register(Register::Sub(RegisterX64::RAX, Part::Doubleword)),
                map.get(id).into(),
            ));
        }
        tac::Instruction::ControlOp(tac::ControlOp::Return(tac::Value::Const(
            tac::Const::Int(v),
        ))) => {
            b.emit(AsmX32::Mov(
                Place::Register(Register::Sub(RegisterX64::RAX, Part::Doubleword)),
                Value::Const(v),
            ));
        }
        // LABEL
        tac::Instruction::ControlOp(tac::ControlOp::Label(label)) => {
            b.emit_label(&format!("_L{}", label));
        }
        // GOTO
        tac::Instruction::ControlOp(tac::ControlOp::Branch(tac::Branch::GOTO(label))) => {
            b.emit(AsmX32::Jmp(format!("_L{}", label)));
        }
        // IfGOTO
        tac::Instruction::ControlOp(tac::ControlOp::Branch(tac::Branch::IfGOTO(
            tac::Value::ID(v),
            label,
        ))) => {
            b.emit(AsmX32::Cmp(map.get(v), Value::Const(0)));
            b.emit(AsmX32::Je(format!("_L{}", label)));
        }
        tac::Instruction::ControlOp(tac::ControlOp::Branch(tac::Branch::IfGOTO(
            tac::Value::Const(tac::Const::Int(c)),
            label,
        ))) => {
            let offset = map.alloc_stack();
            let tmp = Place::Indirect(Indirect::new(
                Register::Register(RegisterX64::RBP),
                offset,
                Size::Doubleword,
            ));
            b.emit(AsmX32::Mov(tmp.clone(), Value::Const(c).into()));
            b.emit(AsmX32::Cmp(tmp, Value::Const(0)));
            b.emit(AsmX32::Je(format!("_L{}", label)));
        }
        tac::Instruction::Call(tac::Call { name, params, .. }) => {
            let mut unspills = Vec::new();

            use RegisterX64::*;
            let regs = [RDI, RSI, RDX, RCX, R8, R9];
            for (p, reg) in params.iter().zip(&regs) {
                let p = match p {
                    tac::Value::ID(p) => map.get(*p).into(),
                    tac::Value::Const(tac::Const::Int(p)) => Value::Const(*p),
                };

                if map.live_at(line).contains(&Place::Register(Register::Sub(
                    reg.clone(),
                    Part::Doubleword,
                ))) && {
                    match map.get(id.unwrap()) {
                        Place::Register(Register::Register(reg))
                        | Place::Register(Register::Sub(reg, ..)) => !regs.contains(&reg),
                        _ => true,
                    }
                } {
                    let offset = map.alloc_stack();
                    let tmp = Place::Indirect(Indirect::new(
                        Register::Register(RegisterX64::RBP),
                        offset,
                        Size::Doubleword,
                    ));
                    let mut spill = asm::Block::new();
                    spill.emit(AsmX32::Mov(
                        tmp.clone(),
                        Value::Register(Register::Sub(reg.clone(), Part::Doubleword)),
                    ));

                    b += spill;
                    b.emit(AsmX32::Mov(
                        Place::Register(Register::Sub(reg.clone(), Part::Doubleword)),
                        p,
                    ));

                    let mut unspill = asm::Block::new();
                    unspill.emit(AsmX32::Mov(
                        Place::Register(Register::Sub(reg.clone(), Part::Doubleword)),
                        tmp.into(),
                    ));

                    unspills.push(unspill);
                } else {
                    b.emit(AsmX32::Mov(
                        Place::Register(Register::Sub(reg.clone(), Part::Doubleword)),
                        p,
                    ));
                }
            }

            let mut stack_reserved = 0;
            if params.len() > regs.len() {
                params
                    .iter()
                    .rev()
                    .take(params.len() - regs.len())
                    .for_each(|p| {
                        let p = match p {
                            tac::Value::ID(p) => map.get(*p).into(),
                            tac::Value::Const(tac::Const::Int(p)) => Value::Const(*p),
                        };

                        const PLATFORM_WORD_SIZE: usize = 8;
                        stack_reserved += PLATFORM_WORD_SIZE;

                        b.emit(AsmX32::Push(p));
                    });
            };

            if map.live_at(line).contains(&Place::Register(Register::Sub(
                RegisterX64::RAX,
                Part::Doubleword,
            ))) && map.get(id.unwrap())
                != Place::Register(Register::Sub(RegisterX64::RAX, Part::Doubleword))
            {
                let offset = map.alloc_stack();
                let tmp = Place::Indirect(Indirect::new(
                    Register::Register(RegisterX64::RBP),
                    offset,
                    Size::Doubleword,
                ));
                let mut spill = asm::Block::new();
                spill.emit(AsmX32::Mov(
                    tmp.clone(),
                    Value::Register(Register::Sub(RegisterX64::RAX, Part::Doubleword)),
                ));

                b += spill;

                let mut unspill = asm::Block::new();
                unspill.emit(AsmX32::Mov(
                    Place::Register(Register::Sub(RegisterX64::RAX, Part::Doubleword)),
                    tmp.into(),
                ));

                unspills.push(unspill);
            }

            b.emit(AsmX32::Call(name.to_owned()));

            if map.get(id.unwrap())
                != Place::Register(Register::Sub(RegisterX64::RAX, Part::Doubleword))
            {
                b.emit(AsmX32::Mov(
                    map.get(id.unwrap()),
                    Value::Register(Register::Sub(RegisterX64::RAX, Part::Doubleword)),
                ));
            }

            if stack_reserved != 0 {
                b.emit(AsmX32::Sub(
                    Place::Register(Register::Register(RegisterX64::RSP)),
                    Value::Const(stack_reserved as i32),
                ));
            }

            for u in unspills {
                b += u;
            }
        }
        i => {
            panic!("Unhandled instruction: {:?}", i);
        }
    }
    b
}
