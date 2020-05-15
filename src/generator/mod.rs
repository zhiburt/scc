mod allocator;
mod asm;
mod syntax;

use asm::Instruction;

use super::il::tac::{self, FuncDef, InstructionLine};

pub fn gen(code: Vec<FuncDef>) -> String {
    let g = Generator::new(code);
    let mut asm = g.gen();
    // allocator::alloc(&mut asm);

    asm.code()
}

struct Generator {
    ir: Vec<FuncDef>,
    code: asm::Assembly,
}

impl Generator {
    fn new(code: Vec<FuncDef>) -> Self {
        Self {
            ir: code,
            code: asm::Assembly::new(),
        }
    }

    fn gen_function(&mut self, func: tac::FuncDef) {
        let mut memory_map = allocator::Allocator::new(&func);
        let mut code = Vec::new();

        for (line, i) in func.instructions.into_iter().enumerate() {
            code.push(translate(line, &mut memory_map, i));
        }

        let header = {
            let mut header = asm::Block::new();
            header.emit_directive(&format!(".globl {}", func.name));
            header.emit_label(&func.name);
            header
        };

        let (prologue, epilogue) = {
            let mut prologue = asm::Block::new();
            prologue.emit(Instruction::new(
                "pushq",
                vec![asm::Register::machine("rbp", asm::Size::Quadword).into()],
            ));
            prologue.emit(Instruction::new(
                "movq",
                vec![
                    asm::Register::machine("rsp", asm::Size::Quadword).into(),
                    asm::Register::machine("rbp", asm::Size::Quadword).into(),
                ],
            ));

            let mut epilogue = asm::Block::new();
            if func.has_function_call {
                // todo: stack alignment
                // comment: now it's always allocated by 4 bytes so its got to be ok
                let stack_size = memory_map.stack_size;
                prologue.emit(Instruction::new(
                    "subq",
                    vec![
                        asm::Const(stack_size as i32).into(),
                        asm::Register::machine("rsp", asm::Size::Quadword).into(),
                    ],
                ));

                epilogue.emit(Instruction::new(
                    "addq",
                    vec![
                        asm::Const(stack_size as i32).into(),
                        asm::Register::machine("rsp", asm::Size::Quadword).into(),
                    ],
                ));
                epilogue.emit(Instruction::new(
                    "movq",
                    vec![
                        asm::Register::machine("rbp", asm::Size::Quadword).into(),
                        asm::Register::machine("rsp", asm::Size::Quadword).into(),
                    ],
                ));
                epilogue.emit(Instruction::new(
                    "popq",
                    vec![asm::Register::machine("rbp", asm::Size::Quadword).into()],
                ));
                epilogue.emit(Instruction::new("ret", vec![]));
            } else {
                epilogue.emit(Instruction::new(
                    "popq",
                    vec![asm::Register::machine("rbp", asm::Size::Quadword).into()],
                ));
                epilogue.emit(Instruction::new("ret", vec![]));
            }

            (prologue, epilogue)
        };

        let mut c = vec![header];
        c.push(prologue);
        c.extend(code);
        c.push(epilogue);

        self.code.emit_function(&func.name, c);
    }

    fn gen(mut self) -> asm::Assembly {
        let ir = std::mem::replace(&mut self.ir, Vec::new());
        for func in ir {
            self.gen_function(func);
        }

        self.code
    }
}

fn checked_mov(
    line: usize,
    al: &mut allocator::Allocator,
    from: tac::ID,
    to: tac::ID,
) -> asm::Block {
    let mut b = asm::Block::new();
    if matches!(al.get(from).rg, asm::RegisterBackend::StackOffset(..))
        && matches!(al.get(to).rg, asm::RegisterBackend::StackOffset(..))
    {
        match al.find_free_at(line) {
            Some(reg) => {
                b.emit(Instruction::new(
                    "movl",
                    vec![
                        al.get(from).into(),
                        asm::Register::machine(reg, asm::Size::Doubleword).into(),
                    ],
                ));
                b.emit(Instruction::new(
                    "movl",
                    vec![
                        asm::Register::machine(reg, asm::Size::Doubleword).into(),
                        al.get(to).into(),
                    ],
                ));
            }
            None => {
                let reg = al.live_at(line).first().unwrap().clone();
                let offset = al.alloc_stack();
                b.emit(Instruction::new(
                    "movl",
                    vec![
                        reg.into(),
                        asm::Register::new(
                            asm::RegisterBackend::StackOffset(offset),
                            asm::Size::Doubleword,
                        )
                        .into(),
                    ],
                ));

                b.emit(Instruction::new(
                    "movl",
                    vec![al.get(from).into(), reg.into()],
                ));

                b.emit(Instruction::new(
                    "movl",
                    vec![reg.into(), al.get(to).into()],
                ));

                b.emit(Instruction::new(
                    "movl",
                    vec![
                        asm::Register::new(
                            asm::RegisterBackend::StackOffset(offset),
                            asm::Size::Doubleword,
                        )
                        .into(),
                        reg.into(),
                    ],
                ));
            }
        }
    } else {
        b.emit(Instruction::new(
            "movl",
            vec![al.get(from).into(), al.get(to).into()],
        ));
    }
    b
}

fn checked_add(
    line: usize,
    al: &mut allocator::Allocator,
    from: tac::ID,
    to: tac::ID,
) -> asm::Block {
    let mut b = asm::Block::new();
    if matches!(al.get(from).rg, asm::RegisterBackend::StackOffset(..))
        && matches!(al.get(to).rg, asm::RegisterBackend::StackOffset(..))
    {
        match al.find_free_at(line) {
            Some(reg) => {
                b.emit(Instruction::new(
                    "movl",
                    vec![
                        al.get(from).into(),
                        asm::Register::machine(reg, asm::Size::Doubleword).into(),
                    ],
                ));
                b.emit(Instruction::new(
                    "addl",
                    vec![
                        asm::Register::machine(reg, asm::Size::Doubleword).into(),
                        al.get(to).into(),
                    ],
                ));
            }
            None => {
                let reg = al.live_at(line).first().unwrap().clone();
                let offset = al.alloc_stack();
                b.emit(Instruction::new(
                    "movl",
                    vec![
                        reg.into(),
                        asm::Register::new(
                            asm::RegisterBackend::StackOffset(offset),
                            asm::Size::Doubleword,
                        )
                        .into(),
                    ],
                ));

                b.emit(Instruction::new(
                    "movl",
                    vec![al.get(from).into(), reg.into()],
                ));

                b.emit(Instruction::new(
                    "addl",
                    vec![reg.into(), al.get(to).into()],
                ));

                b.emit(Instruction::new(
                    "movl",
                    vec![
                        asm::Register::new(
                            asm::RegisterBackend::StackOffset(offset),
                            asm::Size::Doubleword,
                        )
                        .into(),
                        reg.into(),
                    ],
                ));
            }
        }
    } else {
        b.emit(Instruction::new(
            "addl",
            vec![al.get(from).into(), al.get(to).into()],
        ));
    }
    b
}

fn checked(
    instr: &'static str,
    line: usize,
    al: &mut allocator::Allocator,
    from: tac::ID,
    to: tac::ID,
) -> asm::Block {
    let mut b = asm::Block::new();
    if matches!(al.get(from).rg, asm::RegisterBackend::StackOffset(..))
        && matches!(al.get(to).rg, asm::RegisterBackend::StackOffset(..))
    {
        match al.find_free_at(line) {
            Some(reg) => {
                b.emit(Instruction::new(
                    "movl",
                    vec![
                        al.get(from).into(),
                        asm::Register::machine(reg, asm::Size::Doubleword).into(),
                    ],
                ));
                b.emit(Instruction::new(
                    instr,
                    vec![
                        asm::Register::machine(reg, asm::Size::Doubleword).into(),
                        al.get(to).into(),
                    ],
                ));
            }
            None => {
                let reg = al.live_at(line).first().unwrap().clone();
                let offset = al.alloc_stack();
                b.emit(Instruction::new(
                    "movl",
                    vec![
                        reg.into(),
                        asm::Register::new(
                            asm::RegisterBackend::StackOffset(offset),
                            asm::Size::Doubleword,
                        )
                        .into(),
                    ],
                ));

                b.emit(Instruction::new(
                    "movl",
                    vec![al.get(from).into(), reg.into()],
                ));

                b.emit(Instruction::new(instr, vec![reg.into(), al.get(to).into()]));

                b.emit(Instruction::new(
                    "movl",
                    vec![
                        asm::Register::new(
                            asm::RegisterBackend::StackOffset(offset),
                            asm::Size::Doubleword,
                        )
                        .into(),
                        reg.into(),
                    ],
                ));
            }
        }
    } else {
        b.emit(Instruction::new(
            instr,
            vec![al.get(from).into(), al.get(to).into()],
        ));
    }
    b
}

fn get_register(
    line: usize,
    al: &mut allocator::Allocator,
) -> (asm::MachineRegister, asm::Block, asm::Block) {
    match al.find_free_at(line) {
        Some(reg) => (reg, asm::Block::new(), asm::Block::new()),
        None => {
            let reg = al.live_at(line).first().unwrap().clone();
            let offset = al.alloc_stack();

            let mut spill = asm::Block::new();
            spill.emit(Instruction::new(
                "movl",
                vec![
                    reg.into(),
                    asm::Register::new(
                        asm::RegisterBackend::StackOffset(offset),
                        asm::Size::Doubleword,
                    )
                    .into(),
                ],
            ));

            let mut unspill = asm::Block::new();
            unspill.emit(Instruction::new(
                "movl",
                vec![
                    asm::Register::new(
                        asm::RegisterBackend::StackOffset(offset),
                        asm::Size::Doubleword,
                    )
                    .into(),
                    reg.into(),
                ],
            ));

            (reg.as_machine(), spill, unspill)
        }
    }
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
            b.emit(Instruction::new(
                "addl",
                vec![asm::Const(rhs).into(), map.get(id.unwrap()).into()],
            ));
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Arithmetic(tac::ArithmeticOp::Add),
            tac::Value::Const(tac::Const::Int(lhs)),
            tac::Value::ID(rhs),
        )) => {
            b.emit(Instruction::new(
                "movl",
                vec![map.get(rhs).into(), map.get(id.unwrap()).into()],
            ));
            b.emit(Instruction::new(
                "addl",
                vec![asm::Const(lhs).into(), map.get(id.unwrap()).into()],
            ));
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Arithmetic(tac::ArithmeticOp::Add),
            tac::Value::Const(tac::Const::Int(lhs)),
            tac::Value::Const(tac::Const::Int(rhs)),
        )) => {
            b.emit(Instruction::new(
                "movl",
                vec![asm::Const(lhs).into(), map.get(id.unwrap()).into()],
            ));
            b.emit(Instruction::new(
                "addl",
                vec![asm::Const(rhs).into(), map.get(id.unwrap()).into()],
            ));
        }
        // SUB
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Arithmetic(tac::ArithmeticOp::Sub),
            tac::Value::ID(lhs),
            tac::Value::ID(rhs),
        )) => {
            b += checked_mov(line, &mut map, lhs, id.unwrap());
            b.emit(Instruction::new(
                "subl",
                vec![map.get(rhs).into(), map.get(id.unwrap()).into()],
            ));
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Arithmetic(tac::ArithmeticOp::Sub),
            tac::Value::ID(lhs),
            tac::Value::Const(tac::Const::Int(rhs)),
        )) => {
            b += checked_mov(line, &mut map, lhs, id.unwrap());
            b.emit(Instruction::new(
                "subl",
                vec![asm::Const(rhs).into(), map.get(id.unwrap()).into()],
            ));
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Arithmetic(tac::ArithmeticOp::Sub),
            tac::Value::Const(tac::Const::Int(lhs)),
            tac::Value::ID(rhs),
        )) => {
            b.emit(Instruction::new(
                "movl",
                vec![map.get(rhs).into(), map.get(id.unwrap()).into()],
            ));
            b.emit(Instruction::new(
                "subl",
                vec![asm::Const(lhs).into(), map.get(id.unwrap()).into()],
            ));
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Arithmetic(tac::ArithmeticOp::Sub),
            tac::Value::Const(tac::Const::Int(lhs)),
            tac::Value::Const(tac::Const::Int(rhs)),
        )) => {
            b.emit(Instruction::new(
                "movl",
                vec![asm::Const(lhs).into(), map.get(id.unwrap()).into()],
            ));
            b.emit(Instruction::new(
                "subl",
                vec![asm::Const(rhs).into(), map.get(id.unwrap()).into()],
            ));
        }
        // MUL
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Arithmetic(tac::ArithmeticOp::Mul),
            tac::Value::ID(lhs),
            tac::Value::ID(rhs),
        )) => {
            b += checked_mov(line, &mut map, lhs, id.unwrap());
            b.emit(Instruction::new(
                "imul",
                vec![map.get(rhs).into(), map.get(id.unwrap()).into()],
            ));
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Arithmetic(tac::ArithmeticOp::Mul),
            tac::Value::ID(lhs),
            tac::Value::Const(tac::Const::Int(rhs)),
        )) => {
            if matches!(map.get(id.unwrap()).rg, asm::RegisterBackend::Machine(..)) {
                b.emit(Instruction::new(
                    "imul",
                    vec![
                        asm::Const(rhs).into(),
                        map.get(lhs).into(),
                        map.get(id.unwrap()).into(),
                    ],
                ));
            } else {
                match map.find_free_at(line) {
                    Some(reg) => {
                        b.emit(Instruction::new(
                            "movl",
                            vec![
                                map.get(id.unwrap()).into(),
                                asm::Register::machine(reg, asm::Size::Doubleword).into(),
                            ],
                        ));
                        b.emit(Instruction::new(
                            "imul",
                            vec![
                                asm::Const(rhs).into(),
                                map.get(lhs).into(),
                                asm::Register::machine(reg, asm::Size::Doubleword).into(),
                            ],
                        ));
                        b.emit(Instruction::new(
                            "movl",
                            vec![
                                asm::Register::machine(reg, asm::Size::Doubleword).into(),
                                map.get(id.unwrap()).into(),
                            ],
                        ));
                    }
                    None => {
                        let reg = map.live_at(line).first().unwrap().clone();
                        let offset = map.alloc_stack();

                        b.emit(Instruction::new(
                            "movl",
                            vec![
                                reg.into(),
                                asm::Register::new(
                                    asm::RegisterBackend::StackOffset(offset),
                                    asm::Size::Doubleword,
                                )
                                .into(),
                            ],
                        ));

                        b.emit(Instruction::new(
                            "movl",
                            vec![map.get(id.unwrap()).into(), reg.into()],
                        ));
                        b.emit(Instruction::new(
                            "imul",
                            vec![asm::Const(rhs).into(), map.get(lhs).into(), reg.into()],
                        ));
                        b.emit(Instruction::new(
                            "movl",
                            vec![reg.into(), map.get(id.unwrap()).into()],
                        ));

                        b.emit(Instruction::new(
                            "movl",
                            vec![
                                asm::Register::new(
                                    asm::RegisterBackend::StackOffset(offset),
                                    asm::Size::Doubleword,
                                )
                                .into(),
                                reg.into(),
                            ],
                        ));
                        b.emit(Instruction::new(
                            "movl",
                            vec![reg.into(), map.get(id.unwrap()).into()],
                        ));
                    }
                }
            }
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Arithmetic(tac::ArithmeticOp::Mul),
            tac::Value::Const(tac::Const::Int(lhs)),
            tac::Value::ID(rhs),
        )) => {
            if matches!(map.get(id.unwrap()).rg, asm::RegisterBackend::Machine(..)) {
                b.emit(Instruction::new(
                    "imul",
                    vec![
                        asm::Const(lhs).into(),
                        map.get(rhs).into(),
                        map.get(id.unwrap()).into(),
                    ],
                ));
            } else {
                match map.find_free_at(line) {
                    Some(reg) => {
                        b.emit(Instruction::new(
                            "movl",
                            vec![
                                map.get(id.unwrap()).into(),
                                asm::Register::machine(reg, asm::Size::Doubleword).into(),
                            ],
                        ));
                        b.emit(Instruction::new(
                            "imul",
                            vec![
                                asm::Const(lhs).into(),
                                map.get(rhs).into(),
                                asm::Register::machine(reg, asm::Size::Doubleword).into(),
                            ],
                        ));
                        b.emit(Instruction::new(
                            "movl",
                            vec![
                                asm::Register::machine(reg, asm::Size::Doubleword).into(),
                                map.get(id.unwrap()).into(),
                            ],
                        ));
                    }
                    None => {
                        let reg = map.live_at(line).first().unwrap().clone();
                        let offset = map.alloc_stack();

                        b.emit(Instruction::new(
                            "movl",
                            vec![
                                reg.into(),
                                asm::Register::new(
                                    asm::RegisterBackend::StackOffset(offset),
                                    asm::Size::Doubleword,
                                )
                                .into(),
                            ],
                        ));

                        b.emit(Instruction::new(
                            "movl",
                            vec![map.get(id.unwrap()).into(), reg.into()],
                        ));
                        b.emit(Instruction::new(
                            "imul",
                            vec![asm::Const(lhs).into(), map.get(rhs).into(), reg.into()],
                        ));
                        b.emit(Instruction::new(
                            "movl",
                            vec![reg.into(), map.get(id.unwrap()).into()],
                        ));

                        b.emit(Instruction::new(
                            "movl",
                            vec![
                                asm::Register::new(
                                    asm::RegisterBackend::StackOffset(offset),
                                    asm::Size::Doubleword,
                                )
                                .into(),
                                reg.into(),
                            ],
                        ));
                        b.emit(Instruction::new(
                            "movl",
                            vec![reg.into(), map.get(id.unwrap()).into()],
                        ));
                    }
                }
            }
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Arithmetic(tac::ArithmeticOp::Mul),
            tac::Value::Const(tac::Const::Int(lhs)),
            tac::Value::Const(tac::Const::Int(rhs)),
        )) => {
            b.emit(Instruction::new(
                "movl",
                vec![asm::Const(lhs).into(), map.get(id.unwrap()).into()],
            ));
            b.emit(Instruction::new(
                "imul",
                vec![asm::Const(rhs).into(), map.get(id.unwrap()).into()],
            ));
        }
        // DIV
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Arithmetic(tac::ArithmeticOp::Div),
            tac::Value::ID(lhs),
            tac::Value::ID(rhs),
        )) => {
            let (eax_spill, eax_un_spill) = {
                if matches!(map.get(lhs).rg, asm::RegisterBackend::Machine("eax")) == false {
                    if map.free_at(line).contains(&"eax") {
                        let mut spill = asm::Block::new();
                        spill.emit(Instruction::new(
                            "movl",
                            vec![
                                map.get(lhs).into(),
                                asm::Register::machine("eax", asm::Size::Doubleword).into(),
                            ],
                        ));

                        (spill, asm::Block::new())
                    } else {
                        let offset = map.alloc_stack();
                        let mut spill = asm::Block::new();
                        spill.emit(Instruction::new(
                            "movl",
                            vec![
                                asm::Register::machine("eax", asm::Size::Doubleword).into(),
                                asm::Register::new(
                                    asm::RegisterBackend::StackOffset(offset),
                                    asm::Size::Doubleword,
                                )
                                .into(),
                            ],
                        ));

                        spill.emit(Instruction::new(
                            "movl",
                            vec![
                                map.get(lhs).into(),
                                asm::Register::machine("eax", asm::Size::Doubleword).into(),
                            ],
                        ));

                        let mut unspill = asm::Block::new();
                        unspill.emit(Instruction::new(
                            "movl",
                            vec![
                                asm::Register::new(
                                    asm::RegisterBackend::StackOffset(offset),
                                    asm::Size::Doubleword,
                                )
                                .into(),
                                asm::Register::machine("eax", asm::Size::Doubleword).into(),
                            ],
                        ));

                        (spill, unspill)
                    }
                } else {
                    (asm::Block::new(), asm::Block::new())
                }
            };

            let (divisor, spill_edx, un_spill_edx) = {
                if matches!(map.get(rhs).rg, asm::RegisterBackend::Machine("edx")) {
                    if let Some(reg) = map.free_at(line).iter().find(|&&reg| reg != "eax") {
                        let mut spill = asm::Block::new();
                        spill.emit(Instruction::new(
                            "movl",
                            vec![
                                map.get(rhs).into(),
                                asm::Register::machine(reg, asm::Size::Doubleword).into(),
                            ],
                        ));

                        (
                            asm::Register::machine(reg, asm::Size::Doubleword).into(),
                            spill,
                            asm::Block::new(),
                        )
                    } else {
                        let offset = map.alloc_stack();
                        let mut spill = asm::Block::new();
                        spill.emit(Instruction::new(
                            "movl",
                            vec![
                                asm::Register::machine("edx", asm::Size::Doubleword).into(),
                                asm::Register::new(
                                    asm::RegisterBackend::StackOffset(offset),
                                    asm::Size::Doubleword,
                                )
                                .into(),
                            ],
                        ));

                        let mut unspill = asm::Block::new();
                        unspill.emit(Instruction::new(
                            "movl",
                            vec![
                                asm::Register::new(
                                    asm::RegisterBackend::StackOffset(offset),
                                    asm::Size::Doubleword,
                                )
                                .into(),
                                asm::Register::machine("edx", asm::Size::Doubleword).into(),
                            ],
                        ));

                        (
                            asm::Register::new(
                                asm::RegisterBackend::StackOffset(offset),
                                asm::Size::Doubleword,
                            ),
                            spill,
                            unspill,
                        )
                    }
                } else {
                    if map
                        .live_at(line)
                        .contains(&asm::Register::machine("edx", asm::Size::Doubleword))
                    {
                        let offset = map.alloc_stack();
                        let mut spill = asm::Block::new();
                        spill.emit(Instruction::new(
                            "movl",
                            vec![
                                asm::Register::machine("edx", asm::Size::Doubleword).into(),
                                asm::Register::new(
                                    asm::RegisterBackend::StackOffset(offset),
                                    asm::Size::Doubleword,
                                )
                                .into(),
                            ],
                        ));

                        let mut unspill = asm::Block::new();
                        unspill.emit(Instruction::new(
                            "movl",
                            vec![
                                asm::Register::new(
                                    asm::RegisterBackend::StackOffset(offset),
                                    asm::Size::Doubleword,
                                )
                                .into(),
                                asm::Register::machine("edx", asm::Size::Doubleword).into(),
                            ],
                        ));

                        (map.get(rhs).into(), spill, unspill)
                    } else {
                        (map.get(rhs).into(), asm::Block::new(), asm::Block::new())
                    }
                }
            };

            b += eax_spill;
            b += spill_edx;

            b.emit(Instruction::new("cltd", vec![]));
            b.emit(Instruction::new("idivl", vec![divisor.into()]));
            b.emit(Instruction::new(
                "movl",
                vec![
                    asm::Register::machine("eax", asm::Size::Doubleword).into(),
                    map.get(id.unwrap()).into(),
                ],
            ));

            b += eax_un_spill;
            b += un_spill_edx;
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Arithmetic(tac::ArithmeticOp::Div),
            tac::Value::ID(lhs),
            tac::Value::Const(tac::Const::Int(rhs)),
        )) => {
            let (eax_spill, eax_un_spill) = {
                if matches!(map.get(lhs).rg, asm::RegisterBackend::Machine("eax")) == false {
                    if map.free_at(line).contains(&"eax") {
                        let mut spill = asm::Block::new();
                        spill.emit(Instruction::new(
                            "movl",
                            vec![
                                map.get(lhs).into(),
                                asm::Register::machine("eax", asm::Size::Doubleword).into(),
                            ],
                        ));

                        (spill, asm::Block::new())
                    } else {
                        let offset = map.alloc_stack();
                        let mut spill = asm::Block::new();
                        spill.emit(Instruction::new(
                            "movl",
                            vec![
                                asm::Register::machine("eax", asm::Size::Doubleword).into(),
                                asm::Register::new(
                                    asm::RegisterBackend::StackOffset(offset),
                                    asm::Size::Doubleword,
                                )
                                .into(),
                            ],
                        ));

                        spill.emit(Instruction::new(
                            "movl",
                            vec![
                                map.get(lhs).into(),
                                asm::Register::machine("eax", asm::Size::Doubleword).into(),
                            ],
                        ));

                        let mut unspill = asm::Block::new();
                        unspill.emit(Instruction::new(
                            "movl",
                            vec![
                                asm::Register::new(
                                    asm::RegisterBackend::StackOffset(offset),
                                    asm::Size::Doubleword,
                                )
                                .into(),
                                asm::Register::machine("eax", asm::Size::Doubleword).into(),
                            ],
                        ));

                        (spill, unspill)
                    }
                } else {
                    (asm::Block::new(), asm::Block::new())
                }
            };

            let (divisor, spill_edx, un_spill_edx) = {
                if let Some(reg) = map
                    .free_at(line)
                    .iter()
                    .find(|&&reg| reg != "eax" && reg != "edx")
                {
                    let mut spill = asm::Block::new();
                    spill.emit(Instruction::new(
                        "movl",
                        vec![
                            asm::Const(rhs).into(),
                            asm::Register::machine(reg, asm::Size::Doubleword).into(),
                        ],
                    ));

                    let unspill = if map
                        .live_at(line)
                        .contains(&asm::Register::machine("edx", asm::Size::Doubleword))
                    {
                        let offset = map.alloc_stack();
                        spill.emit(Instruction::new(
                            "movl",
                            vec![
                                asm::Register::machine("edx", asm::Size::Doubleword).into(),
                                asm::Register::new(
                                    asm::RegisterBackend::StackOffset(offset),
                                    asm::Size::Doubleword,
                                )
                                .into(),
                            ],
                        ));

                        let mut unspill = asm::Block::new();
                        unspill.emit(Instruction::new(
                            "movl",
                            vec![
                                asm::Register::new(
                                    asm::RegisterBackend::StackOffset(offset),
                                    asm::Size::Doubleword,
                                )
                                .into(),
                                asm::Register::machine("edx", asm::Size::Doubleword).into(),
                            ],
                        ));

                        unspill
                    } else {
                        asm::Block::new()
                    };

                    (
                        asm::Register::machine(reg, asm::Size::Doubleword).into(),
                        spill,
                        unspill,
                    )
                } else {
                    let offset = map.alloc_stack();
                    let mut spill = asm::Block::new();
                    spill.emit(Instruction::new(
                        "movl",
                        vec![
                            asm::Const(rhs).into(),
                            asm::Register::new(
                                asm::RegisterBackend::StackOffset(offset),
                                asm::Size::Doubleword,
                            )
                            .into(),
                        ],
                    ));

                    let unspill = if map
                        .live_at(line)
                        .contains(&asm::Register::machine("edx", asm::Size::Doubleword))
                    {
                        let offset = map.alloc_stack();
                        spill.emit(Instruction::new(
                            "movl",
                            vec![
                                asm::Register::machine("edx", asm::Size::Doubleword).into(),
                                asm::Register::new(
                                    asm::RegisterBackend::StackOffset(offset),
                                    asm::Size::Doubleword,
                                )
                                .into(),
                            ],
                        ));

                        let mut unspill = asm::Block::new();
                        unspill.emit(Instruction::new(
                            "movl",
                            vec![
                                asm::Register::new(
                                    asm::RegisterBackend::StackOffset(offset),
                                    asm::Size::Doubleword,
                                )
                                .into(),
                                asm::Register::machine("edx", asm::Size::Doubleword).into(),
                            ],
                        ));

                        unspill
                    } else {
                        asm::Block::new()
                    };

                    (
                        asm::Register::new(
                            asm::RegisterBackend::StackOffset(offset),
                            asm::Size::Doubleword,
                        ),
                        spill,
                        unspill,
                    )
                }
            };

            b += eax_spill;
            b += spill_edx;

            b.emit(Instruction::new("cltd", vec![]));
            b.emit(Instruction::new("idivl", vec![divisor.into()]));
            b.emit(Instruction::new(
                "movl",
                vec![
                    asm::Register::machine("eax", asm::Size::Doubleword).into(),
                    map.get(id.unwrap()).into(),
                ],
            ));

            b += eax_un_spill;
            b += un_spill_edx;
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Arithmetic(tac::ArithmeticOp::Div),
            tac::Value::Const(tac::Const::Int(lhs)),
            tac::Value::ID(rhs),
        )) => {
            let (eax_spill, eax_un_spill) = {
                if map.free_at(line).contains(&"eax") {
                    let mut spill = asm::Block::new();
                    spill.emit(Instruction::new(
                        "movl",
                        vec![
                            asm::Const(lhs).into(),
                            asm::Register::machine("eax", asm::Size::Doubleword).into(),
                        ],
                    ));

                    (spill, asm::Block::new())
                } else {
                    let offset = map.alloc_stack();
                    let mut spill = asm::Block::new();
                    spill.emit(Instruction::new(
                        "movl",
                        vec![
                            asm::Register::machine("eax", asm::Size::Doubleword).into(),
                            asm::Register::new(
                                asm::RegisterBackend::StackOffset(offset),
                                asm::Size::Doubleword,
                            )
                            .into(),
                        ],
                    ));

                    spill.emit(Instruction::new(
                        "movl",
                        vec![
                            asm::Const(lhs).into(),
                            asm::Register::machine("eax", asm::Size::Doubleword).into(),
                        ],
                    ));

                    let mut unspill = asm::Block::new();
                    unspill.emit(Instruction::new(
                        "movl",
                        vec![
                            asm::Register::new(
                                asm::RegisterBackend::StackOffset(offset),
                                asm::Size::Doubleword,
                            )
                            .into(),
                            asm::Register::machine("eax", asm::Size::Doubleword).into(),
                        ],
                    ));

                    (spill, unspill)
                }
            };

            let (divisor, spill_edx, un_spill_edx) = {
                if matches!(map.get(rhs).rg, asm::RegisterBackend::Machine("edx")) {
                    if let Some(reg) = map.free_at(line).iter().find(|&&reg| reg != "eax") {
                        let mut spill = asm::Block::new();
                        spill.emit(Instruction::new(
                            "movl",
                            vec![
                                map.get(rhs).into(),
                                asm::Register::machine(reg, asm::Size::Doubleword).into(),
                            ],
                        ));

                        (
                            asm::Register::machine(reg, asm::Size::Doubleword).into(),
                            spill,
                            asm::Block::new(),
                        )
                    } else {
                        let offset = map.alloc_stack();
                        let mut spill = asm::Block::new();
                        spill.emit(Instruction::new(
                            "movl",
                            vec![
                                asm::Register::machine("edx", asm::Size::Doubleword).into(),
                                asm::Register::new(
                                    asm::RegisterBackend::StackOffset(offset),
                                    asm::Size::Doubleword,
                                )
                                .into(),
                            ],
                        ));

                        let mut unspill = asm::Block::new();
                        if matches!(
                            map.get(id.unwrap()).rg,
                            asm::RegisterBackend::Machine("edx")
                        ) == false
                        {
                            unspill.emit(Instruction::new(
                                "movl",
                                vec![
                                    asm::Register::new(
                                        asm::RegisterBackend::StackOffset(offset),
                                        asm::Size::Doubleword,
                                    )
                                    .into(),
                                    asm::Register::machine("edx", asm::Size::Doubleword).into(),
                                ],
                            ));
                        }

                        (
                            asm::Register::new(
                                asm::RegisterBackend::StackOffset(offset),
                                asm::Size::Doubleword,
                            ),
                            spill,
                            unspill,
                        )
                    }
                } else {
                    if map
                        .live_at(line)
                        .contains(&asm::Register::machine("edx", asm::Size::Doubleword))
                    {
                        let offset = map.alloc_stack();
                        let mut spill = asm::Block::new();
                        spill.emit(Instruction::new(
                            "movl",
                            vec![
                                asm::Register::machine("edx", asm::Size::Doubleword).into(),
                                asm::Register::new(
                                    asm::RegisterBackend::StackOffset(offset),
                                    asm::Size::Doubleword,
                                )
                                .into(),
                            ],
                        ));

                        let mut unspill = asm::Block::new();
                        if matches!(
                            map.get(id.unwrap()).rg,
                            asm::RegisterBackend::Machine("edx")
                        ) == false
                        {
                            unspill.emit(Instruction::new(
                                "movl",
                                vec![
                                    asm::Register::new(
                                        asm::RegisterBackend::StackOffset(offset),
                                        asm::Size::Doubleword,
                                    )
                                    .into(),
                                    asm::Register::machine("edx", asm::Size::Doubleword).into(),
                                ],
                            ));
                        }

                        (map.get(rhs).into(), spill, unspill)
                    } else {
                        (map.get(rhs).into(), asm::Block::new(), asm::Block::new())
                    }
                }
            };

            b += eax_spill;
            b += spill_edx;

            b.emit(Instruction::new("cltd", vec![]));
            b.emit(Instruction::new("idivl", vec![divisor.into()]));
            b.emit(Instruction::new(
                "movl",
                vec![
                    asm::Register::machine("eax", asm::Size::Doubleword).into(),
                    map.get(id.unwrap()).into(),
                ],
            ));

            b += eax_un_spill;
            b += un_spill_edx;
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Arithmetic(tac::ArithmeticOp::Div),
            tac::Value::Const(tac::Const::Int(lhs)),
            tac::Value::Const(tac::Const::Int(rhs)),
        )) => {
            let (eax_spill, eax_un_spill) = {
                if map.free_at(line).contains(&"eax") {
                    let mut spill = asm::Block::new();
                    spill.emit(Instruction::new(
                        "movl",
                        vec![
                            asm::Const(lhs).into(),
                            asm::Register::machine("eax", asm::Size::Doubleword).into(),
                        ],
                    ));

                    (spill, asm::Block::new())
                } else {
                    let offset = map.alloc_stack();
                    let mut spill = asm::Block::new();
                    spill.emit(Instruction::new(
                        "movl",
                        vec![
                            asm::Register::machine("eax", asm::Size::Doubleword).into(),
                            asm::Register::new(
                                asm::RegisterBackend::StackOffset(offset),
                                asm::Size::Doubleword,
                            )
                            .into(),
                        ],
                    ));

                    spill.emit(Instruction::new(
                        "movl",
                        vec![
                            asm::Const(lhs).into(),
                            asm::Register::machine("eax", asm::Size::Doubleword).into(),
                        ],
                    ));

                    let mut unspill = asm::Block::new();
                    unspill.emit(Instruction::new(
                        "movl",
                        vec![
                            asm::Register::new(
                                asm::RegisterBackend::StackOffset(offset),
                                asm::Size::Doubleword,
                            )
                            .into(),
                            asm::Register::machine("eax", asm::Size::Doubleword).into(),
                        ],
                    ));

                    (spill, unspill)
                }
            };

            let (divisor, spill_edx, un_spill_edx) = {
                if let Some(reg) = map
                    .free_at(line)
                    .iter()
                    .find(|&&reg| reg != "eax" && reg != "edx")
                {
                    let mut spill = asm::Block::new();
                    spill.emit(Instruction::new(
                        "movl",
                        vec![
                            asm::Const(rhs).into(),
                            asm::Register::machine(reg, asm::Size::Doubleword).into(),
                        ],
                    ));

                    let unspill = if map
                        .live_at(line)
                        .contains(&asm::Register::machine("edx", asm::Size::Doubleword))
                    {
                        let offset = map.alloc_stack();
                        spill.emit(Instruction::new(
                            "movl",
                            vec![
                                asm::Register::machine("edx", asm::Size::Doubleword).into(),
                                asm::Register::new(
                                    asm::RegisterBackend::StackOffset(offset),
                                    asm::Size::Doubleword,
                                )
                                .into(),
                            ],
                        ));

                        let mut unspill = asm::Block::new();
                        if matches!(
                            map.get(id.unwrap()).rg,
                            asm::RegisterBackend::Machine("edx")
                        ) == false
                        {
                            unspill.emit(Instruction::new(
                                "movl",
                                vec![
                                    asm::Register::new(
                                        asm::RegisterBackend::StackOffset(offset),
                                        asm::Size::Doubleword,
                                    )
                                    .into(),
                                    asm::Register::machine("edx", asm::Size::Doubleword).into(),
                                ],
                            ));
                        }

                        unspill
                    } else {
                        asm::Block::new()
                    };

                    (
                        asm::Register::machine(reg, asm::Size::Doubleword).into(),
                        spill,
                        unspill,
                    )
                } else {
                    let offset = map.alloc_stack();
                    let mut spill = asm::Block::new();
                    spill.emit(Instruction::new(
                        "movl",
                        vec![
                            asm::Const(rhs).into(),
                            asm::Register::new(
                                asm::RegisterBackend::StackOffset(offset),
                                asm::Size::Doubleword,
                            )
                            .into(),
                        ],
                    ));

                    let unspill = if map
                        .live_at(line)
                        .contains(&asm::Register::machine("edx", asm::Size::Doubleword))
                    {
                        let offset = map.alloc_stack();
                        spill.emit(Instruction::new(
                            "movl",
                            vec![
                                asm::Register::machine("edx", asm::Size::Doubleword).into(),
                                asm::Register::new(
                                    asm::RegisterBackend::StackOffset(offset),
                                    asm::Size::Doubleword,
                                )
                                .into(),
                            ],
                        ));

                        let mut unspill = asm::Block::new();
                        if matches!(
                            map.get(id.unwrap()).rg,
                            asm::RegisterBackend::Machine("edx")
                        ) == false
                        {
                            unspill.emit(Instruction::new(
                                "movl",
                                vec![
                                    asm::Register::new(
                                        asm::RegisterBackend::StackOffset(offset),
                                        asm::Size::Doubleword,
                                    )
                                    .into(),
                                    asm::Register::machine("edx", asm::Size::Doubleword).into(),
                                ],
                            ));
                        }

                        unspill
                    } else {
                        asm::Block::new()
                    };

                    (
                        asm::Register::new(
                            asm::RegisterBackend::StackOffset(offset),
                            asm::Size::Doubleword,
                        ),
                        spill,
                        unspill,
                    )
                }
            };

            b += eax_spill;
            b += spill_edx;

            b.emit(Instruction::new("cltd", vec![]));
            b.emit(Instruction::new("idivl", vec![divisor.into()]));
            b.emit(Instruction::new(
                "movl",
                vec![
                    asm::Register::machine("eax", asm::Size::Doubleword).into(),
                    map.get(id.unwrap()).into(),
                ],
            ));

            b += eax_un_spill;
            b += un_spill_edx;
        }
        // MOD
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Arithmetic(tac::ArithmeticOp::Mod),
            tac::Value::ID(lhs),
            tac::Value::ID(rhs),
        )) => {
            let (eax_spill, eax_un_spill) = {
                if matches!(map.get(lhs).rg, asm::RegisterBackend::Machine("eax")) == false {
                    if map.free_at(line).contains(&"eax") {
                        let mut spill = asm::Block::new();
                        spill.emit(Instruction::new(
                            "movl",
                            vec![
                                map.get(lhs).into(),
                                asm::Register::machine("eax", asm::Size::Doubleword).into(),
                            ],
                        ));

                        (spill, asm::Block::new())
                    } else {
                        let offset = map.alloc_stack();
                        let mut spill = asm::Block::new();
                        spill.emit(Instruction::new(
                            "movl",
                            vec![
                                asm::Register::machine("eax", asm::Size::Doubleword).into(),
                                asm::Register::new(
                                    asm::RegisterBackend::StackOffset(offset),
                                    asm::Size::Doubleword,
                                )
                                .into(),
                            ],
                        ));

                        spill.emit(Instruction::new(
                            "movl",
                            vec![
                                map.get(lhs).into(),
                                asm::Register::machine("eax", asm::Size::Doubleword).into(),
                            ],
                        ));

                        let mut unspill = asm::Block::new();
                        unspill.emit(Instruction::new(
                            "movl",
                            vec![
                                asm::Register::new(
                                    asm::RegisterBackend::StackOffset(offset),
                                    asm::Size::Doubleword,
                                )
                                .into(),
                                asm::Register::machine("eax", asm::Size::Doubleword).into(),
                            ],
                        ));

                        (spill, unspill)
                    }
                } else {
                    (asm::Block::new(), asm::Block::new())
                }
            };

            let (divisor, spill_edx, un_spill_edx) = {
                if matches!(map.get(rhs).rg, asm::RegisterBackend::Machine("edx")) {
                    if let Some(reg) = map.free_at(line).iter().find(|&&reg| reg != "eax") {
                        let mut spill = asm::Block::new();
                        spill.emit(Instruction::new(
                            "movl",
                            vec![
                                map.get(rhs).into(),
                                asm::Register::machine(reg, asm::Size::Doubleword).into(),
                            ],
                        ));

                        (
                            asm::Register::machine(reg, asm::Size::Doubleword).into(),
                            spill,
                            asm::Block::new(),
                        )
                    } else {
                        let offset = map.alloc_stack();
                        let mut spill = asm::Block::new();
                        spill.emit(Instruction::new(
                            "movl",
                            vec![
                                asm::Register::machine("edx", asm::Size::Doubleword).into(),
                                asm::Register::new(
                                    asm::RegisterBackend::StackOffset(offset),
                                    asm::Size::Doubleword,
                                )
                                .into(),
                            ],
                        ));

                        let mut unspill = asm::Block::new();
                        unspill.emit(Instruction::new(
                            "movl",
                            vec![
                                asm::Register::new(
                                    asm::RegisterBackend::StackOffset(offset),
                                    asm::Size::Doubleword,
                                )
                                .into(),
                                asm::Register::machine("edx", asm::Size::Doubleword).into(),
                            ],
                        ));

                        (
                            asm::Register::new(
                                asm::RegisterBackend::StackOffset(offset),
                                asm::Size::Doubleword,
                            ),
                            spill,
                            unspill,
                        )
                    }
                } else {
                    if map
                        .live_at(line)
                        .contains(&asm::Register::machine("edx", asm::Size::Doubleword))
                    {
                        let offset = map.alloc_stack();
                        let mut spill = asm::Block::new();
                        spill.emit(Instruction::new(
                            "movl",
                            vec![
                                asm::Register::machine("edx", asm::Size::Doubleword).into(),
                                asm::Register::new(
                                    asm::RegisterBackend::StackOffset(offset),
                                    asm::Size::Doubleword,
                                )
                                .into(),
                            ],
                        ));

                        let mut unspill = asm::Block::new();
                        unspill.emit(Instruction::new(
                            "movl",
                            vec![
                                asm::Register::new(
                                    asm::RegisterBackend::StackOffset(offset),
                                    asm::Size::Doubleword,
                                )
                                .into(),
                                asm::Register::machine("edx", asm::Size::Doubleword).into(),
                            ],
                        ));

                        (map.get(rhs).into(), spill, unspill)
                    } else {
                        (map.get(rhs).into(), asm::Block::new(), asm::Block::new())
                    }
                }
            };

            b += eax_spill;
            b += spill_edx;

            b.emit(Instruction::new("cltd", vec![]));
            b.emit(Instruction::new("idivl", vec![divisor.into()]));
            b.emit(Instruction::new(
                "movl",
                vec![
                    asm::Register::machine("edx", asm::Size::Doubleword).into(),
                    map.get(id.unwrap()).into(),
                ],
            ));

            b += eax_un_spill;
            b += un_spill_edx;
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Arithmetic(tac::ArithmeticOp::Mod),
            tac::Value::ID(lhs),
            tac::Value::Const(tac::Const::Int(rhs)),
        )) => {
            let (eax_spill, eax_un_spill) = {
                if matches!(map.get(lhs).rg, asm::RegisterBackend::Machine("eax")) == false {
                    if map.free_at(line).contains(&"eax") {
                        let mut spill = asm::Block::new();
                        spill.emit(Instruction::new(
                            "movl",
                            vec![
                                map.get(lhs).into(),
                                asm::Register::machine("eax", asm::Size::Doubleword).into(),
                            ],
                        ));

                        (spill, asm::Block::new())
                    } else {
                        let offset = map.alloc_stack();
                        let mut spill = asm::Block::new();
                        spill.emit(Instruction::new(
                            "movl",
                            vec![
                                asm::Register::machine("eax", asm::Size::Doubleword).into(),
                                asm::Register::new(
                                    asm::RegisterBackend::StackOffset(offset),
                                    asm::Size::Doubleword,
                                )
                                .into(),
                            ],
                        ));

                        spill.emit(Instruction::new(
                            "movl",
                            vec![
                                map.get(lhs).into(),
                                asm::Register::machine("eax", asm::Size::Doubleword).into(),
                            ],
                        ));

                        let mut unspill = asm::Block::new();
                        unspill.emit(Instruction::new(
                            "movl",
                            vec![
                                asm::Register::new(
                                    asm::RegisterBackend::StackOffset(offset),
                                    asm::Size::Doubleword,
                                )
                                .into(),
                                asm::Register::machine("eax", asm::Size::Doubleword).into(),
                            ],
                        ));

                        (spill, unspill)
                    }
                } else {
                    (asm::Block::new(), asm::Block::new())
                }
            };

            let (divisor, spill_edx, un_spill_edx) = {
                if let Some(reg) = map
                    .free_at(line)
                    .iter()
                    .find(|&&reg| reg != "eax" && reg != "edx")
                {
                    let mut spill = asm::Block::new();
                    spill.emit(Instruction::new(
                        "movl",
                        vec![
                            asm::Const(rhs).into(),
                            asm::Register::machine(reg, asm::Size::Doubleword).into(),
                        ],
                    ));

                    let unspill = if map
                        .live_at(line)
                        .contains(&asm::Register::machine("edx", asm::Size::Doubleword))
                    {
                        let offset = map.alloc_stack();
                        spill.emit(Instruction::new(
                            "movl",
                            vec![
                                asm::Register::machine("edx", asm::Size::Doubleword).into(),
                                asm::Register::new(
                                    asm::RegisterBackend::StackOffset(offset),
                                    asm::Size::Doubleword,
                                )
                                .into(),
                            ],
                        ));

                        let mut unspill = asm::Block::new();
                        if matches!(
                            map.get(id.unwrap()).rg,
                            asm::RegisterBackend::Machine("edx")
                        ) == false
                        {
                            unspill.emit(Instruction::new(
                                "movl",
                                vec![
                                    asm::Register::new(
                                        asm::RegisterBackend::StackOffset(offset),
                                        asm::Size::Doubleword,
                                    )
                                    .into(),
                                    asm::Register::machine("edx", asm::Size::Doubleword).into(),
                                ],
                            ));
                        }
                        unspill
                    } else {
                        asm::Block::new()
                    };

                    (
                        asm::Register::machine(reg, asm::Size::Doubleword).into(),
                        spill,
                        unspill,
                    )
                } else {
                    let offset = map.alloc_stack();
                    let mut spill = asm::Block::new();
                    spill.emit(Instruction::new(
                        "movl",
                        vec![
                            asm::Const(rhs).into(),
                            asm::Register::new(
                                asm::RegisterBackend::StackOffset(offset),
                                asm::Size::Doubleword,
                            )
                            .into(),
                        ],
                    ));

                    let unspill = if map
                        .live_at(line)
                        .contains(&asm::Register::machine("edx", asm::Size::Doubleword))
                    {
                        let offset = map.alloc_stack();
                        spill.emit(Instruction::new(
                            "movl",
                            vec![
                                asm::Register::machine("edx", asm::Size::Doubleword).into(),
                                asm::Register::new(
                                    asm::RegisterBackend::StackOffset(offset),
                                    asm::Size::Doubleword,
                                )
                                .into(),
                            ],
                        ));

                        let mut unspill = asm::Block::new();
                        if matches!(
                            map.get(id.unwrap()).rg,
                            asm::RegisterBackend::Machine("edx")
                        ) == false
                        {
                            unspill.emit(Instruction::new(
                                "movl",
                                vec![
                                    asm::Register::new(
                                        asm::RegisterBackend::StackOffset(offset),
                                        asm::Size::Doubleword,
                                    )
                                    .into(),
                                    asm::Register::machine("edx", asm::Size::Doubleword).into(),
                                ],
                            ));
                        }

                        unspill
                    } else {
                        asm::Block::new()
                    };

                    (
                        asm::Register::new(
                            asm::RegisterBackend::StackOffset(offset),
                            asm::Size::Doubleword,
                        ),
                        spill,
                        unspill,
                    )
                }
            };

            b += eax_spill;
            b += spill_edx;

            b.emit(Instruction::new("cltd", vec![]));
            b.emit(Instruction::new("idivl", vec![divisor.into()]));
            b.emit(Instruction::new(
                "movl",
                vec![
                    asm::Register::machine("edx", asm::Size::Doubleword).into(),
                    map.get(id.unwrap()).into(),
                ],
            ));

            b += eax_un_spill;
            b += un_spill_edx;
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Arithmetic(tac::ArithmeticOp::Mod),
            tac::Value::Const(tac::Const::Int(lhs)),
            tac::Value::ID(rhs),
        )) => {
            let (eax_spill, eax_un_spill) = {
                if map.free_at(line).contains(&"eax") {
                    let mut spill = asm::Block::new();
                    spill.emit(Instruction::new(
                        "movl",
                        vec![
                            asm::Const(lhs).into(),
                            asm::Register::machine("eax", asm::Size::Doubleword).into(),
                        ],
                    ));

                    (spill, asm::Block::new())
                } else {
                    let offset = map.alloc_stack();
                    let mut spill = asm::Block::new();
                    spill.emit(Instruction::new(
                        "movl",
                        vec![
                            asm::Register::machine("eax", asm::Size::Doubleword).into(),
                            asm::Register::new(
                                asm::RegisterBackend::StackOffset(offset),
                                asm::Size::Doubleword,
                            )
                            .into(),
                        ],
                    ));

                    spill.emit(Instruction::new(
                        "movl",
                        vec![
                            asm::Const(lhs).into(),
                            asm::Register::machine("eax", asm::Size::Doubleword).into(),
                        ],
                    ));

                    let mut unspill = asm::Block::new();
                    unspill.emit(Instruction::new(
                        "movl",
                        vec![
                            asm::Register::new(
                                asm::RegisterBackend::StackOffset(offset),
                                asm::Size::Doubleword,
                            )
                            .into(),
                            asm::Register::machine("eax", asm::Size::Doubleword).into(),
                        ],
                    ));

                    (spill, unspill)
                }
            };

            let (divisor, spill_edx, un_spill_edx) = {
                if matches!(map.get(rhs).rg, asm::RegisterBackend::Machine("edx")) {
                    if let Some(reg) = map.free_at(line).iter().find(|&&reg| reg != "eax") {
                        let mut spill = asm::Block::new();
                        spill.emit(Instruction::new(
                            "movl",
                            vec![
                                map.get(rhs).into(),
                                asm::Register::machine(reg, asm::Size::Doubleword).into(),
                            ],
                        ));

                        (
                            asm::Register::machine(reg, asm::Size::Doubleword).into(),
                            spill,
                            asm::Block::new(),
                        )
                    } else {
                        let offset = map.alloc_stack();
                        let mut spill = asm::Block::new();
                        spill.emit(Instruction::new(
                            "movl",
                            vec![
                                asm::Register::machine("edx", asm::Size::Doubleword).into(),
                                asm::Register::new(
                                    asm::RegisterBackend::StackOffset(offset),
                                    asm::Size::Doubleword,
                                )
                                .into(),
                            ],
                        ));

                        let mut unspill = asm::Block::new();
                        unspill.emit(Instruction::new(
                            "movl",
                            vec![
                                asm::Register::new(
                                    asm::RegisterBackend::StackOffset(offset),
                                    asm::Size::Doubleword,
                                )
                                .into(),
                                asm::Register::machine("edx", asm::Size::Doubleword).into(),
                            ],
                        ));

                        (
                            asm::Register::new(
                                asm::RegisterBackend::StackOffset(offset),
                                asm::Size::Doubleword,
                            ),
                            spill,
                            unspill,
                        )
                    }
                } else {
                    if map
                        .live_at(line)
                        .contains(&asm::Register::machine("edx", asm::Size::Doubleword))
                    {
                        let offset = map.alloc_stack();
                        let mut spill = asm::Block::new();
                        spill.emit(Instruction::new(
                            "movl",
                            vec![
                                asm::Register::machine("edx", asm::Size::Doubleword).into(),
                                asm::Register::new(
                                    asm::RegisterBackend::StackOffset(offset),
                                    asm::Size::Doubleword,
                                )
                                .into(),
                            ],
                        ));

                        let mut unspill = asm::Block::new();
                        unspill.emit(Instruction::new(
                            "movl",
                            vec![
                                asm::Register::new(
                                    asm::RegisterBackend::StackOffset(offset),
                                    asm::Size::Doubleword,
                                )
                                .into(),
                                asm::Register::machine("edx", asm::Size::Doubleword).into(),
                            ],
                        ));

                        (map.get(rhs).into(), spill, unspill)
                    } else {
                        (map.get(rhs).into(), asm::Block::new(), asm::Block::new())
                    }
                }
            };

            b += eax_spill;
            b += spill_edx;

            b.emit(Instruction::new("cltd", vec![]));
            b.emit(Instruction::new("idivl", vec![divisor.into()]));
            b.emit(Instruction::new(
                "movl",
                vec![
                    asm::Register::machine("edx", asm::Size::Doubleword).into(),
                    map.get(id.unwrap()).into(),
                ],
            ));

            b += eax_un_spill;
            b += un_spill_edx;
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Arithmetic(tac::ArithmeticOp::Mod),
            tac::Value::Const(tac::Const::Int(lhs)),
            tac::Value::Const(tac::Const::Int(rhs)),
        )) => {
            let (eax_spill, eax_un_spill) = {
                if map.free_at(line).contains(&"eax") {
                    let mut spill = asm::Block::new();
                    spill.emit(Instruction::new(
                        "movl",
                        vec![
                            asm::Const(lhs).into(),
                            asm::Register::machine("eax", asm::Size::Doubleword).into(),
                        ],
                    ));

                    (spill, asm::Block::new())
                } else {
                    let offset = map.alloc_stack();
                    let mut spill = asm::Block::new();
                    spill.emit(Instruction::new(
                        "movl",
                        vec![
                            asm::Register::machine("eax", asm::Size::Doubleword).into(),
                            asm::Register::new(
                                asm::RegisterBackend::StackOffset(offset),
                                asm::Size::Doubleword,
                            )
                            .into(),
                        ],
                    ));

                    spill.emit(Instruction::new(
                        "movl",
                        vec![
                            asm::Const(lhs).into(),
                            asm::Register::machine("eax", asm::Size::Doubleword).into(),
                        ],
                    ));

                    let mut unspill = asm::Block::new();
                    unspill.emit(Instruction::new(
                        "movl",
                        vec![
                            asm::Register::new(
                                asm::RegisterBackend::StackOffset(offset),
                                asm::Size::Doubleword,
                            )
                            .into(),
                            asm::Register::machine("eax", asm::Size::Doubleword).into(),
                        ],
                    ));

                    (spill, unspill)
                }
            };

            let (divisor, spill_edx, un_spill_edx) = {
                if let Some(reg) = map
                    .free_at(line)
                    .iter()
                    .find(|&&reg| reg != "eax" && reg != "edx")
                {
                    let mut spill = asm::Block::new();
                    spill.emit(Instruction::new(
                        "movl",
                        vec![
                            asm::Const(rhs).into(),
                            asm::Register::machine(reg, asm::Size::Doubleword).into(),
                        ],
                    ));

                    let unspill = if map
                        .live_at(line)
                        .contains(&asm::Register::machine("edx", asm::Size::Doubleword))
                    {
                        let offset = map.alloc_stack();
                        spill.emit(Instruction::new(
                            "movl",
                            vec![
                                asm::Register::machine("edx", asm::Size::Doubleword).into(),
                                asm::Register::new(
                                    asm::RegisterBackend::StackOffset(offset),
                                    asm::Size::Doubleword,
                                )
                                .into(),
                            ],
                        ));

                        let mut unspill = asm::Block::new();
                        if matches!(
                            map.get(id.unwrap()).rg,
                            asm::RegisterBackend::Machine("edx")
                        ) == false
                        {
                            unspill.emit(Instruction::new(
                                "movl",
                                vec![
                                    asm::Register::new(
                                        asm::RegisterBackend::StackOffset(offset),
                                        asm::Size::Doubleword,
                                    )
                                    .into(),
                                    asm::Register::machine("edx", asm::Size::Doubleword).into(),
                                ],
                            ));
                        }

                        unspill
                    } else {
                        asm::Block::new()
                    };

                    (
                        asm::Register::machine(reg, asm::Size::Doubleword).into(),
                        spill,
                        unspill,
                    )
                } else {
                    let offset = map.alloc_stack();
                    let mut spill = asm::Block::new();
                    spill.emit(Instruction::new(
                        "movl",
                        vec![
                            asm::Const(rhs).into(),
                            asm::Register::new(
                                asm::RegisterBackend::StackOffset(offset),
                                asm::Size::Doubleword,
                            )
                            .into(),
                        ],
                    ));

                    let unspill = if map
                        .live_at(line)
                        .contains(&asm::Register::machine("edx", asm::Size::Doubleword))
                    {
                        let offset = map.alloc_stack();
                        spill.emit(Instruction::new(
                            "movl",
                            vec![
                                asm::Register::machine("edx", asm::Size::Doubleword).into(),
                                asm::Register::new(
                                    asm::RegisterBackend::StackOffset(offset),
                                    asm::Size::Doubleword,
                                )
                                .into(),
                            ],
                        ));

                        let mut unspill = asm::Block::new();
                        if matches!(
                            map.get(id.unwrap()).rg,
                            asm::RegisterBackend::Machine("edx")
                        ) == false
                        {
                            unspill.emit(Instruction::new(
                                "movl",
                                vec![
                                    asm::Register::new(
                                        asm::RegisterBackend::StackOffset(offset),
                                        asm::Size::Doubleword,
                                    )
                                    .into(),
                                    asm::Register::machine("edx", asm::Size::Doubleword).into(),
                                ],
                            ));
                        }

                        unspill
                    } else {
                        asm::Block::new()
                    };

                    (
                        asm::Register::new(
                            asm::RegisterBackend::StackOffset(offset),
                            asm::Size::Doubleword,
                        ),
                        spill,
                        unspill,
                    )
                }
            };

            b += eax_spill;
            b += spill_edx;

            b.emit(Instruction::new("cltd", vec![]));
            b.emit(Instruction::new("idivl", vec![divisor.into()]));
            b.emit(Instruction::new(
                "movl",
                vec![
                    asm::Register::machine("edx", asm::Size::Doubleword).into(),
                    map.get(id.unwrap()).into(),
                ],
            ));

            b += eax_un_spill;
            b += un_spill_edx;
        }
        // NEG
        tac::Instruction::Op(tac::Op::Unary(tac::UnOp::Neg, tac::Value::ID(v))) => {
            // comment: clang approach with 1 line instruction(sub) require to have
            // prepared register(equal 0)
            b.emit(Instruction::new(
                "xorl",
                vec![map.get(id.unwrap()).into(), map.get(id.unwrap()).into()],
            ));
            b.emit(Instruction::new(
                "subl",
                vec![map.get(v).into(), map.get(id.unwrap()).into()],
            ));
        }
        tac::Instruction::Op(tac::Op::Unary(
            tac::UnOp::Neg,
            tac::Value::Const(tac::Const::Int(v)),
        )) => {
            panic!("it's impossible as our lexer distinguish '-1' lexemes as just negative values")
        }
        // Bitwise
        tac::Instruction::Op(tac::Op::Unary(tac::UnOp::BitComplement, tac::Value::ID(v))) => {
            b.emit(Instruction::new(
                "movl",
                vec![map.get(v).into(), map.get(id.unwrap()).into()],
            ));
            b.emit(Instruction::new(
                "xorl",
                vec![asm::Const(-1).into(), map.get(id.unwrap()).into()],
            ));
        }
        tac::Instruction::Op(tac::Op::Unary(
            tac::UnOp::BitComplement,
            tac::Value::Const(tac::Const::Int(v)),
        )) => {
            b.emit(Instruction::new(
                "movl",
                vec![asm::Const(v).into(), map.get(id.unwrap()).into()],
            ));
            b.emit(Instruction::new(
                "xorl",
                vec![asm::Const(-1).into(), map.get(id.unwrap()).into()],
            ));
        }
        // Logicneg
        tac::Instruction::Op(tac::Op::Unary(tac::UnOp::LogicNeg, tac::Value::ID(v))) => {
            if matches!(
                map.get(id.unwrap()).rg,
                asm::RegisterBackend::StackOffset(..)
            ) {
                let (reg, spill, unspill) = match map.find_free_at(line) {
                    Some(reg) => (
                        asm::Register::machine(reg, asm::Size::Doubleword),
                        asm::Block::new(),
                        asm::Block::new(),
                    ),
                    None => {
                        let reg = map.live_at(line).first().unwrap().clone();
                        let offset = map.alloc_stack();

                        let mut spill = asm::Block::new();

                        spill.emit(Instruction::new(
                            "movl",
                            vec![
                                reg.into(),
                                asm::Register::new(
                                    asm::RegisterBackend::StackOffset(offset),
                                    asm::Size::Doubleword,
                                )
                                .into(),
                            ],
                        ));

                        let mut unspill = asm::Block::new();

                        unspill.emit(Instruction::new(
                            "movl",
                            vec![
                                asm::Register::new(
                                    asm::RegisterBackend::StackOffset(offset),
                                    asm::Size::Doubleword,
                                )
                                .into(),
                                reg.into(),
                            ],
                        ));

                        (reg, spill, unspill)
                    }
                };

                b += spill;

                b.emit(Instruction::new(
                    "cmpl",
                    vec![asm::Const(0).into(), map.get(v).into()],
                ));
                b.emit(Instruction::new("setne", vec![reg.as_byte().into()]));
                b.emit(Instruction::new(
                    "xorb",
                    vec![asm::Const(-1).into(), reg.as_byte().into()],
                ));

                b.emit(Instruction::new(
                    "andb",
                    vec![asm::Const(1).into(), reg.as_byte().into()],
                ));

                b.emit(Instruction::new(
                    "movzbl",
                    vec![reg.as_byte().into(), reg.into()],
                ));

                b.emit(Instruction::new(
                    "movl",
                    vec![reg.into(), map.get(id.unwrap()).into()],
                ));

                b += unspill;
            } else {
                let reg = map.get(id.unwrap());

                b.emit(Instruction::new(
                    "cmpl",
                    vec![asm::Const(0).into(), map.get(v).into()],
                ));
                b.emit(Instruction::new("setne", vec![reg.as_byte().into()]));
                b.emit(Instruction::new(
                    "xorb",
                    vec![asm::Const(-1).into(), reg.as_byte().into()],
                ));

                b.emit(Instruction::new(
                    "andb",
                    vec![asm::Const(1).into(), reg.as_byte().into()],
                ));

                b.emit(Instruction::new(
                    "movzbl",
                    vec![reg.as_byte().into(), reg.into()],
                ));
            }
        }
        tac::Instruction::Op(tac::Op::Unary(
            tac::UnOp::LogicNeg,
            tac::Value::Const(tac::Const::Int(v)),
        )) => {
            if matches!(
                map.get(id.unwrap()).rg,
                asm::RegisterBackend::StackOffset(..)
            ) {
                let (reg, spill, unspill) = match map.find_free_at(line) {
                    Some(reg) => (
                        asm::Register::machine(reg, asm::Size::Doubleword),
                        asm::Block::new(),
                        asm::Block::new(),
                    ),
                    None => {
                        let reg = map.live_at(line).first().unwrap().clone();
                        let offset = map.alloc_stack();

                        let mut spill = asm::Block::new();

                        spill.emit(Instruction::new(
                            "movl",
                            vec![
                                reg.into(),
                                asm::Register::new(
                                    asm::RegisterBackend::StackOffset(offset),
                                    asm::Size::Doubleword,
                                )
                                .into(),
                            ],
                        ));

                        let mut unspill = asm::Block::new();

                        unspill.emit(Instruction::new(
                            "movl",
                            vec![
                                asm::Register::new(
                                    asm::RegisterBackend::StackOffset(offset),
                                    asm::Size::Doubleword,
                                )
                                .into(),
                                reg.into(),
                            ],
                        ));

                        (reg, spill, unspill)
                    }
                };

                b += spill;

                b.emit(Instruction::new(
                    "movl",
                    vec![asm::Const(v).into(), reg.into()],
                ));

                b.emit(Instruction::new(
                    "cmpl",
                    vec![asm::Const(0).into(), reg.into()],
                ));
                b.emit(Instruction::new("setne", vec![reg.as_byte().into()]));
                b.emit(Instruction::new(
                    "xorb",
                    vec![asm::Const(-1).into(), reg.as_byte().into()],
                ));

                b.emit(Instruction::new(
                    "andb",
                    vec![asm::Const(1).into(), reg.as_byte().into()],
                ));

                b.emit(Instruction::new(
                    "movzbl",
                    vec![reg.as_byte().into(), reg.into()],
                ));

                b.emit(Instruction::new(
                    "movl",
                    vec![reg.into(), map.get(id.unwrap()).into()],
                ));

                b += unspill;
            } else {
                let reg = map.get(id.unwrap());

                b.emit(Instruction::new(
                    "movl",
                    vec![asm::Const(v).into(), reg.into()],
                ));

                b.emit(Instruction::new(
                    "cmpl",
                    vec![asm::Const(0).into(), reg.into()],
                ));
                b.emit(Instruction::new("setne", vec![reg.as_byte().into()]));
                b.emit(Instruction::new(
                    "xorb",
                    vec![asm::Const(-1).into(), reg.as_byte().into()],
                ));

                b.emit(Instruction::new(
                    "andb",
                    vec![asm::Const(1).into(), reg.as_byte().into()],
                ));

                b.emit(Instruction::new(
                    "movzbl",
                    vec![reg.as_byte().into(), reg.into()],
                ));
            }
        }
        // EQ
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Equality(tac::EqualityOp::Equal),
            tac::Value::ID(lhs),
            tac::Value::ID(rhs),
        )) => {
            if matches!(
                map.get(id.unwrap()).rg,
                asm::RegisterBackend::StackOffset(..)
            ) {
                let (reg, spill, unspill) = get_register(line, map);
                let reg = asm::Register::machine(reg, asm::Size::Doubleword);
                b += spill;
                b += checked("cmpl", line, &mut map, lhs, rhs);
                b.emit(Instruction::new("sete", vec![reg.as_byte().into()]));
                b.emit(Instruction::new(
                    "andb",
                    vec![asm::Const(1).into(), reg.as_byte().into()],
                ));

                b.emit(Instruction::new(
                    "movzbl",
                    vec![reg.as_byte().into(), reg.into()],
                ));

                b.emit(Instruction::new(
                    "movl",
                    vec![reg.into(), map.get(id.unwrap()).into()],
                ));

                b += unspill;
            } else {
                b += checked("cmpl", line, &mut map, lhs, rhs);
                b.emit(Instruction::new(
                    "sete",
                    vec![map.get(id.unwrap()).as_byte().into()],
                ));
                b.emit(Instruction::new(
                    "andb",
                    vec![asm::Const(1).into(), map.get(id.unwrap()).as_byte().into()],
                ));

                b.emit(Instruction::new(
                    "movzbl",
                    vec![
                        map.get(id.unwrap()).as_byte().into(),
                        map.get(id.unwrap()).into(),
                    ],
                ));
            }
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Equality(tac::EqualityOp::Equal),
            tac::Value::ID(lhs),
            tac::Value::Const(tac::Const::Int(rhs)),
        )) => {
            if matches!(
                map.get(id.unwrap()).rg,
                asm::RegisterBackend::StackOffset(..)
            ) {
                let (reg, spill, unspill) = get_register(line, map);
                let reg = asm::Register::machine(reg, asm::Size::Doubleword);
                b += spill;

                b.emit(Instruction::new(
                    "cmpl",
                    vec![asm::Const(rhs).into(), map.get(lhs).into()],
                ));
                b.emit(Instruction::new("sete", vec![reg.as_byte().into()]));
                b.emit(Instruction::new(
                    "andb",
                    vec![asm::Const(1).into(), reg.as_byte().into()],
                ));

                b.emit(Instruction::new(
                    "movzbl",
                    vec![reg.as_byte().into(), reg.into()],
                ));

                b.emit(Instruction::new(
                    "movl",
                    vec![reg.into(), map.get(id.unwrap()).into()],
                ));

                b += unspill;
            } else {
                b.emit(Instruction::new(
                    "cmpl",
                    vec![asm::Const(rhs).into(), map.get(lhs).into()],
                ));
                b.emit(Instruction::new(
                    "sete",
                    vec![map.get(id.unwrap()).as_byte().into()],
                ));
                b.emit(Instruction::new(
                    "andb",
                    vec![asm::Const(1).into(), map.get(id.unwrap()).as_byte().into()],
                ));

                b.emit(Instruction::new(
                    "movzbl",
                    vec![
                        map.get(id.unwrap()).as_byte().into(),
                        map.get(id.unwrap()).into(),
                    ],
                ));
            }
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Equality(tac::EqualityOp::Equal),
            tac::Value::Const(tac::Const::Int(lhs)),
            tac::Value::ID(rhs),
        )) => {
            if matches!(
                map.get(id.unwrap()).rg,
                asm::RegisterBackend::StackOffset(..)
            ) {
                let (reg, spill, unspill) = get_register(line, map);
                let reg = asm::Register::machine(reg, asm::Size::Doubleword);
                b += spill;

                b.emit(Instruction::new(
                    "cmpl",
                    vec![asm::Const(lhs).into(), map.get(rhs).into()],
                ));
                b.emit(Instruction::new("sete", vec![reg.as_byte().into()]));
                b.emit(Instruction::new(
                    "andb",
                    vec![asm::Const(1).into(), reg.as_byte().into()],
                ));

                b.emit(Instruction::new(
                    "movzbl",
                    vec![reg.as_byte().into(), reg.into()],
                ));

                b.emit(Instruction::new(
                    "movl",
                    vec![reg.into(), map.get(id.unwrap()).into()],
                ));

                b += unspill;
            } else {
                b.emit(Instruction::new(
                    "cmpl",
                    vec![asm::Const(lhs).into(), map.get(rhs).into()],
                ));
                b.emit(Instruction::new(
                    "sete",
                    vec![map.get(id.unwrap()).as_byte().into()],
                ));
                b.emit(Instruction::new(
                    "andb",
                    vec![asm::Const(1).into(), map.get(id.unwrap()).as_byte().into()],
                ));

                b.emit(Instruction::new(
                    "movzbl",
                    vec![
                        map.get(id.unwrap()).as_byte().into(),
                        map.get(id.unwrap()).into(),
                    ],
                ));
            }
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Equality(tac::EqualityOp::Equal),
            tac::Value::Const(tac::Const::Int(lhs)),
            tac::Value::Const(tac::Const::Int(rhs)),
        )) => {
            let offset = map.alloc_stack();
            b.emit(Instruction::new(
                "movl",
                vec![
                    asm::Const(rhs).into(),
                    asm::Register::new(
                        asm::RegisterBackend::StackOffset(offset),
                        asm::Size::Doubleword,
                    )
                    .into(),
                ],
            ));

            b.emit(Instruction::new(
                "cmpl",
                vec![
                    asm::Const(lhs).into(),
                    asm::Register::new(
                        asm::RegisterBackend::StackOffset(offset),
                        asm::Size::Doubleword,
                    )
                    .into(),
                ],
            ));
            b.emit(Instruction::new(
                "sete",
                vec![map.get(id.unwrap()).as_byte().into()],
            ));
            b.emit(Instruction::new(
                "andb",
                vec![asm::Const(1).into(), map.get(id.unwrap()).as_byte().into()],
            ));

            b.emit(Instruction::new(
                "movzbl",
                vec![
                    map.get(id.unwrap()).as_byte().into(),
                    map.get(id.unwrap()).into(),
                ],
            ));
        }
        // NOTEQ
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Equality(tac::EqualityOp::NotEq),
            tac::Value::ID(lhs),
            tac::Value::ID(rhs),
        )) => {
            if matches!(
                map.get(id.unwrap()).rg,
                asm::RegisterBackend::StackOffset(..)
            ) {
                let (reg, spill, unspill) = get_register(line, map);
                let reg = asm::Register::machine(reg, asm::Size::Doubleword);
                b += spill;
                b += checked("cmpl", line, &mut map, lhs, rhs);
                b.emit(Instruction::new("setne", vec![reg.as_byte().into()]));
                b.emit(Instruction::new(
                    "andb",
                    vec![asm::Const(1).into(), reg.as_byte().into()],
                ));

                b.emit(Instruction::new(
                    "movzbl",
                    vec![reg.as_byte().into(), reg.into()],
                ));

                b.emit(Instruction::new(
                    "movl",
                    vec![reg.into(), map.get(id.unwrap()).into()],
                ));

                b += unspill;
            } else {
                b += checked("cmpl", line, &mut map, lhs, rhs);
                b.emit(Instruction::new(
                    "setne",
                    vec![map.get(id.unwrap()).as_byte().into()],
                ));
                b.emit(Instruction::new(
                    "andb",
                    vec![asm::Const(1).into(), map.get(id.unwrap()).as_byte().into()],
                ));

                b.emit(Instruction::new(
                    "movzbl",
                    vec![
                        map.get(id.unwrap()).as_byte().into(),
                        map.get(id.unwrap()).into(),
                    ],
                ));
            }
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Equality(tac::EqualityOp::NotEq),
            tac::Value::ID(lhs),
            tac::Value::Const(tac::Const::Int(rhs)),
        )) => {
            if matches!(
                map.get(id.unwrap()).rg,
                asm::RegisterBackend::StackOffset(..)
            ) {
                let (reg, spill, unspill) = get_register(line, map);
                let reg = asm::Register::machine(reg, asm::Size::Doubleword);
                b += spill;

                b.emit(Instruction::new(
                    "cmpl",
                    vec![asm::Const(rhs).into(), map.get(lhs).into()],
                ));
                b.emit(Instruction::new("setne", vec![reg.as_byte().into()]));
                b.emit(Instruction::new(
                    "andb",
                    vec![asm::Const(1).into(), reg.as_byte().into()],
                ));

                b.emit(Instruction::new(
                    "movzbl",
                    vec![reg.as_byte().into(), reg.into()],
                ));

                b.emit(Instruction::new(
                    "movl",
                    vec![reg.into(), map.get(id.unwrap()).into()],
                ));

                b += unspill;
            } else {
                b.emit(Instruction::new(
                    "cmpl",
                    vec![asm::Const(rhs).into(), map.get(lhs).into()],
                ));
                b.emit(Instruction::new(
                    "setne",
                    vec![map.get(id.unwrap()).as_byte().into()],
                ));
                b.emit(Instruction::new(
                    "andb",
                    vec![asm::Const(1).into(), map.get(id.unwrap()).as_byte().into()],
                ));

                b.emit(Instruction::new(
                    "movzbl",
                    vec![
                        map.get(id.unwrap()).as_byte().into(),
                        map.get(id.unwrap()).into(),
                    ],
                ));
            }
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Equality(tac::EqualityOp::NotEq),
            tac::Value::Const(tac::Const::Int(lhs)),
            tac::Value::ID(rhs),
        )) => {
            if matches!(
                map.get(id.unwrap()).rg,
                asm::RegisterBackend::StackOffset(..)
            ) {
                let (reg, spill, unspill) = get_register(line, map);
                let reg = asm::Register::machine(reg, asm::Size::Doubleword);
                b += spill;

                b.emit(Instruction::new(
                    "cmpl",
                    vec![asm::Const(lhs).into(), map.get(rhs).into()],
                ));
                b.emit(Instruction::new("setne", vec![reg.as_byte().into()]));
                b.emit(Instruction::new(
                    "andb",
                    vec![asm::Const(1).into(), reg.as_byte().into()],
                ));

                b.emit(Instruction::new(
                    "movzbl",
                    vec![reg.as_byte().into(), reg.into()],
                ));

                b.emit(Instruction::new(
                    "movl",
                    vec![reg.into(), map.get(id.unwrap()).into()],
                ));

                b += unspill;
            } else {
                b.emit(Instruction::new(
                    "cmpl",
                    vec![asm::Const(lhs).into(), map.get(rhs).into()],
                ));
                b.emit(Instruction::new(
                    "setne",
                    vec![map.get(id.unwrap()).as_byte().into()],
                ));
                b.emit(Instruction::new(
                    "andb",
                    vec![asm::Const(1).into(), map.get(id.unwrap()).as_byte().into()],
                ));

                b.emit(Instruction::new(
                    "movzbl",
                    vec![
                        map.get(id.unwrap()).as_byte().into(),
                        map.get(id.unwrap()).into(),
                    ],
                ));
            }
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Equality(tac::EqualityOp::NotEq),
            tac::Value::Const(tac::Const::Int(lhs)),
            tac::Value::Const(tac::Const::Int(rhs)),
        )) => {
            let offset = map.alloc_stack();
            b.emit(Instruction::new(
                "movl",
                vec![
                    asm::Const(rhs).into(),
                    asm::Register::new(
                        asm::RegisterBackend::StackOffset(offset),
                        asm::Size::Doubleword,
                    )
                    .into(),
                ],
            ));

            b.emit(Instruction::new(
                "cmpl",
                vec![
                    asm::Const(lhs).into(),
                    asm::Register::new(
                        asm::RegisterBackend::StackOffset(offset),
                        asm::Size::Doubleword,
                    )
                    .into(),
                ],
            ));
            b.emit(Instruction::new(
                "setne",
                vec![map.get(id.unwrap()).as_byte().into()],
            ));
            b.emit(Instruction::new(
                "andb",
                vec![asm::Const(1).into(), map.get(id.unwrap()).as_byte().into()],
            ));

            b.emit(Instruction::new(
                "movzbl",
                vec![
                    map.get(id.unwrap()).as_byte().into(),
                    map.get(id.unwrap()).into(),
                ],
            ));
        }
        // LESS
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Relational(tac::RelationalOp::Less),
            tac::Value::ID(lhs),
            tac::Value::ID(rhs),
        )) => {
            if matches!(
                map.get(id.unwrap()).rg,
                asm::RegisterBackend::StackOffset(..)
            ) {
                let (reg, spill, unspill) = get_register(line, map);
                let reg = asm::Register::machine(reg, asm::Size::Doubleword);
                b += spill;
                b += checked("cmpl", line, &mut map, rhs, lhs);
                b.emit(Instruction::new("setl", vec![reg.as_byte().into()]));
                b.emit(Instruction::new(
                    "andb",
                    vec![asm::Const(1).into(), reg.as_byte().into()],
                ));

                b.emit(Instruction::new(
                    "movzbl",
                    vec![reg.as_byte().into(), reg.into()],
                ));

                b.emit(Instruction::new(
                    "movl",
                    vec![reg.into(), map.get(id.unwrap()).into()],
                ));

                b += unspill;
            } else {
                b += checked("cmpl", line, &mut map, rhs, lhs);
                b.emit(Instruction::new(
                    "setl",
                    vec![map.get(id.unwrap()).as_byte().into()],
                ));
                b.emit(Instruction::new(
                    "andb",
                    vec![asm::Const(1).into(), map.get(id.unwrap()).as_byte().into()],
                ));

                b.emit(Instruction::new(
                    "movzbl",
                    vec![
                        map.get(id.unwrap()).as_byte().into(),
                        map.get(id.unwrap()).into(),
                    ],
                ));
            }
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Relational(tac::RelationalOp::Less),
            tac::Value::ID(lhs),
            tac::Value::Const(tac::Const::Int(rhs)),
        )) => {
            if matches!(
                map.get(id.unwrap()).rg,
                asm::RegisterBackend::StackOffset(..)
            ) {
                let (reg, spill, unspill) = get_register(line, map);
                let reg = asm::Register::machine(reg, asm::Size::Doubleword);
                b += spill;

                b.emit(Instruction::new(
                    "cmpl",
                    vec![asm::Const(rhs).into(), map.get(lhs).into()],
                ));
                b.emit(Instruction::new("setl", vec![reg.as_byte().into()]));
                b.emit(Instruction::new(
                    "andb",
                    vec![asm::Const(1).into(), reg.as_byte().into()],
                ));

                b.emit(Instruction::new(
                    "movzbl",
                    vec![reg.as_byte().into(), reg.into()],
                ));

                b.emit(Instruction::new(
                    "movl",
                    vec![reg.into(), map.get(id.unwrap()).into()],
                ));

                b += unspill;
            } else {
                b.emit(Instruction::new(
                    "cmpl",
                    vec![asm::Const(rhs).into(), map.get(lhs).into()],
                ));
                b.emit(Instruction::new(
                    "setl",
                    vec![map.get(id.unwrap()).as_byte().into()],
                ));
                b.emit(Instruction::new(
                    "andb",
                    vec![asm::Const(1).into(), map.get(id.unwrap()).as_byte().into()],
                ));

                b.emit(Instruction::new(
                    "movzbl",
                    vec![
                        map.get(id.unwrap()).as_byte().into(),
                        map.get(id.unwrap()).into(),
                    ],
                ));
            }
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Relational(tac::RelationalOp::Less),
            tac::Value::Const(tac::Const::Int(lhs)),
            tac::Value::ID(rhs),
        )) => {
            if matches!(
                map.get(id.unwrap()).rg,
                asm::RegisterBackend::StackOffset(..)
            ) {
                let (reg, spill, unspill) = get_register(line, map);
                let reg = asm::Register::machine(reg, asm::Size::Doubleword);
                b += spill;

                b.emit(Instruction::new(
                    "movl",
                    vec![asm::Const(lhs).into(), map.get(id.unwrap()).into()],
                ));
                b.emit(Instruction::new(
                    "cmpl",
                    vec![map.get(rhs).into(), map.get(id.unwrap()).into()],
                ));
                b.emit(Instruction::new("setl", vec![reg.as_byte().into()]));
                b.emit(Instruction::new(
                    "andb",
                    vec![asm::Const(1).into(), reg.as_byte().into()],
                ));

                b.emit(Instruction::new(
                    "movzbl",
                    vec![reg.as_byte().into(), reg.into()],
                ));

                b.emit(Instruction::new(
                    "movl",
                    vec![reg.into(), map.get(id.unwrap()).into()],
                ));

                b += unspill;
            } else {
                b.emit(Instruction::new(
                    "movl",
                    vec![asm::Const(lhs).into(), map.get(id.unwrap()).into()],
                ));
                b.emit(Instruction::new(
                    "cmpl",
                    vec![map.get(rhs).into(), map.get(id.unwrap()).into()],
                ));
                b.emit(Instruction::new(
                    "setl",
                    vec![map.get(id.unwrap()).as_byte().into()],
                ));
                b.emit(Instruction::new(
                    "andb",
                    vec![asm::Const(1).into(), map.get(id.unwrap()).as_byte().into()],
                ));

                b.emit(Instruction::new(
                    "movzbl",
                    vec![
                        map.get(id.unwrap()).as_byte().into(),
                        map.get(id.unwrap()).into(),
                    ],
                ));
            }
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Relational(tac::RelationalOp::Less),
            tac::Value::Const(tac::Const::Int(lhs)),
            tac::Value::Const(tac::Const::Int(rhs)),
        )) => {
            b.emit(Instruction::new(
                "movl",
                vec![asm::Const(lhs).into(), map.get(id.unwrap()).into()],
            ));
            b.emit(Instruction::new(
                "cmpl",
                vec![asm::Const(rhs).into(), map.get(id.unwrap()).into()],
            ));
            b.emit(Instruction::new(
                "setl",
                vec![map.get(id.unwrap()).as_byte().into()],
            ));
            b.emit(Instruction::new(
                "andb",
                vec![asm::Const(1).into(), map.get(id.unwrap()).as_byte().into()],
            ));

            b.emit(Instruction::new(
                "movzbl",
                vec![
                    map.get(id.unwrap()).as_byte().into(),
                    map.get(id.unwrap()).into(),
                ],
            ));
        }
        // LESS_OR_EQUAL
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Relational(tac::RelationalOp::LessOrEq),
            tac::Value::ID(lhs),
            tac::Value::ID(rhs),
        )) => {
            if matches!(
                map.get(id.unwrap()).rg,
                asm::RegisterBackend::StackOffset(..)
            ) {
                let (reg, spill, unspill) = get_register(line, map);
                let reg = asm::Register::machine(reg, asm::Size::Doubleword);
                b += spill;
                b += checked("cmpl", line, &mut map, rhs, lhs);
                b.emit(Instruction::new("setle", vec![reg.as_byte().into()]));
                b.emit(Instruction::new(
                    "andb",
                    vec![asm::Const(1).into(), reg.as_byte().into()],
                ));

                b.emit(Instruction::new(
                    "movzbl",
                    vec![reg.as_byte().into(), reg.into()],
                ));

                b.emit(Instruction::new(
                    "movl",
                    vec![reg.into(), map.get(id.unwrap()).into()],
                ));

                b += unspill;
            } else {
                b += checked("cmpl", line, &mut map, rhs, lhs);
                b.emit(Instruction::new(
                    "setle",
                    vec![map.get(id.unwrap()).as_byte().into()],
                ));
                b.emit(Instruction::new(
                    "andb",
                    vec![asm::Const(1).into(), map.get(id.unwrap()).as_byte().into()],
                ));

                b.emit(Instruction::new(
                    "movzbl",
                    vec![
                        map.get(id.unwrap()).as_byte().into(),
                        map.get(id.unwrap()).into(),
                    ],
                ));
            }
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Relational(tac::RelationalOp::LessOrEq),
            tac::Value::ID(lhs),
            tac::Value::Const(tac::Const::Int(rhs)),
        )) => {
            if matches!(
                map.get(id.unwrap()).rg,
                asm::RegisterBackend::StackOffset(..)
            ) {
                let (reg, spill, unspill) = get_register(line, map);
                let reg = asm::Register::machine(reg, asm::Size::Doubleword);
                b += spill;

                b.emit(Instruction::new(
                    "cmpl",
                    vec![asm::Const(rhs).into(), map.get(lhs).into()],
                ));
                b.emit(Instruction::new("setle", vec![reg.as_byte().into()]));
                b.emit(Instruction::new(
                    "andb",
                    vec![asm::Const(1).into(), reg.as_byte().into()],
                ));

                b.emit(Instruction::new(
                    "movzbl",
                    vec![reg.as_byte().into(), reg.into()],
                ));

                b.emit(Instruction::new(
                    "movl",
                    vec![reg.into(), map.get(id.unwrap()).into()],
                ));

                b += unspill;
            } else {
                b.emit(Instruction::new(
                    "cmpl",
                    vec![asm::Const(rhs).into(), map.get(lhs).into()],
                ));
                b.emit(Instruction::new(
                    "setle",
                    vec![map.get(id.unwrap()).as_byte().into()],
                ));
                b.emit(Instruction::new(
                    "andb",
                    vec![asm::Const(1).into(), map.get(id.unwrap()).as_byte().into()],
                ));

                b.emit(Instruction::new(
                    "movzbl",
                    vec![
                        map.get(id.unwrap()).as_byte().into(),
                        map.get(id.unwrap()).into(),
                    ],
                ));
            }
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Relational(tac::RelationalOp::LessOrEq),
            tac::Value::Const(tac::Const::Int(lhs)),
            tac::Value::ID(rhs),
        )) => {
            if matches!(
                map.get(id.unwrap()).rg,
                asm::RegisterBackend::StackOffset(..)
            ) {
                let (reg, spill, unspill) = get_register(line, map);
                let reg = asm::Register::machine(reg, asm::Size::Doubleword);
                b += spill;

                b.emit(Instruction::new(
                    "movl",
                    vec![asm::Const(lhs).into(), map.get(id.unwrap()).into()],
                ));
                b.emit(Instruction::new(
                    "cmpl",
                    vec![map.get(rhs).into(), map.get(id.unwrap()).into()],
                ));
                b.emit(Instruction::new("setle", vec![reg.as_byte().into()]));
                b.emit(Instruction::new(
                    "andb",
                    vec![asm::Const(1).into(), reg.as_byte().into()],
                ));

                b.emit(Instruction::new(
                    "movzbl",
                    vec![reg.as_byte().into(), reg.into()],
                ));

                b.emit(Instruction::new(
                    "movl",
                    vec![reg.into(), map.get(id.unwrap()).into()],
                ));

                b += unspill;
            } else {
                b.emit(Instruction::new(
                    "movl",
                    vec![asm::Const(lhs).into(), map.get(id.unwrap()).into()],
                ));
                b.emit(Instruction::new(
                    "cmpl",
                    vec![map.get(rhs).into(), map.get(id.unwrap()).into()],
                ));
                b.emit(Instruction::new(
                    "setle",
                    vec![map.get(id.unwrap()).as_byte().into()],
                ));
                b.emit(Instruction::new(
                    "andb",
                    vec![asm::Const(1).into(), map.get(id.unwrap()).as_byte().into()],
                ));

                b.emit(Instruction::new(
                    "movzbl",
                    vec![
                        map.get(id.unwrap()).as_byte().into(),
                        map.get(id.unwrap()).into(),
                    ],
                ));
            }
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Relational(tac::RelationalOp::LessOrEq),
            tac::Value::Const(tac::Const::Int(lhs)),
            tac::Value::Const(tac::Const::Int(rhs)),
        )) => {
            b.emit(Instruction::new(
                "movl",
                vec![asm::Const(lhs).into(), map.get(id.unwrap()).into()],
            ));
            b.emit(Instruction::new(
                "cmpl",
                vec![asm::Const(rhs).into(), map.get(id.unwrap()).into()],
            ));
            b.emit(Instruction::new(
                "setle",
                vec![map.get(id.unwrap()).as_byte().into()],
            ));
            b.emit(Instruction::new(
                "andb",
                vec![asm::Const(1).into(), map.get(id.unwrap()).as_byte().into()],
            ));

            b.emit(Instruction::new(
                "movzbl",
                vec![
                    map.get(id.unwrap()).as_byte().into(),
                    map.get(id.unwrap()).into(),
                ],
            ));
        }
        // GREATER
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Relational(tac::RelationalOp::Greater),
            tac::Value::ID(lhs),
            tac::Value::ID(rhs),
        )) => {
            if matches!(
                map.get(id.unwrap()).rg,
                asm::RegisterBackend::StackOffset(..)
            ) {
                let (reg, spill, unspill) = get_register(line, map);
                let reg = asm::Register::machine(reg, asm::Size::Doubleword);
                b += spill;
                b += checked("cmpl", line, &mut map, rhs, lhs);
                b.emit(Instruction::new("setg", vec![reg.as_byte().into()]));
                b.emit(Instruction::new(
                    "andb",
                    vec![asm::Const(1).into(), reg.as_byte().into()],
                ));

                b.emit(Instruction::new(
                    "movzbl",
                    vec![reg.as_byte().into(), reg.into()],
                ));

                b.emit(Instruction::new(
                    "movl",
                    vec![reg.into(), map.get(id.unwrap()).into()],
                ));

                b += unspill;
            } else {
                b += checked("cmpl", line, &mut map, rhs, lhs);
                b.emit(Instruction::new(
                    "setg",
                    vec![map.get(id.unwrap()).as_byte().into()],
                ));
                b.emit(Instruction::new(
                    "andb",
                    vec![asm::Const(1).into(), map.get(id.unwrap()).as_byte().into()],
                ));

                b.emit(Instruction::new(
                    "movzbl",
                    vec![
                        map.get(id.unwrap()).as_byte().into(),
                        map.get(id.unwrap()).into(),
                    ],
                ));
            }
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Relational(tac::RelationalOp::Greater),
            tac::Value::ID(lhs),
            tac::Value::Const(tac::Const::Int(rhs)),
        )) => {
            if matches!(
                map.get(id.unwrap()).rg,
                asm::RegisterBackend::StackOffset(..)
            ) {
                let (reg, spill, unspill) = get_register(line, map);
                let reg = asm::Register::machine(reg, asm::Size::Doubleword);
                b += spill;

                b.emit(Instruction::new(
                    "cmpl",
                    vec![asm::Const(rhs).into(), map.get(lhs).into()],
                ));
                b.emit(Instruction::new("setg", vec![reg.as_byte().into()]));
                b.emit(Instruction::new(
                    "andb",
                    vec![asm::Const(1).into(), reg.as_byte().into()],
                ));

                b.emit(Instruction::new(
                    "movzbl",
                    vec![reg.as_byte().into(), reg.into()],
                ));

                b.emit(Instruction::new(
                    "movl",
                    vec![reg.into(), map.get(id.unwrap()).into()],
                ));

                b += unspill;
            } else {
                b.emit(Instruction::new(
                    "cmpl",
                    vec![asm::Const(rhs).into(), map.get(lhs).into()],
                ));
                b.emit(Instruction::new(
                    "setg",
                    vec![map.get(id.unwrap()).as_byte().into()],
                ));
                b.emit(Instruction::new(
                    "andb",
                    vec![asm::Const(1).into(), map.get(id.unwrap()).as_byte().into()],
                ));

                b.emit(Instruction::new(
                    "movzbl",
                    vec![
                        map.get(id.unwrap()).as_byte().into(),
                        map.get(id.unwrap()).into(),
                    ],
                ));
            }
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Relational(tac::RelationalOp::Greater),
            tac::Value::Const(tac::Const::Int(lhs)),
            tac::Value::ID(rhs),
        )) => {
            if matches!(
                map.get(id.unwrap()).rg,
                asm::RegisterBackend::StackOffset(..)
            ) {
                let (reg, spill, unspill) = get_register(line, map);
                let reg = asm::Register::machine(reg, asm::Size::Doubleword);
                b += spill;

                b.emit(Instruction::new(
                    "movl",
                    vec![asm::Const(lhs).into(), map.get(id.unwrap()).into()],
                ));

                b.emit(Instruction::new(
                    "cmpl",
                    vec![asm::Const(lhs).into(), map.get(id.unwrap()).into()],
                ));
                b.emit(Instruction::new("setg", vec![reg.as_byte().into()]));
                b.emit(Instruction::new(
                    "andb",
                    vec![asm::Const(1).into(), reg.as_byte().into()],
                ));

                b.emit(Instruction::new(
                    "movzbl",
                    vec![reg.as_byte().into(), reg.into()],
                ));

                b.emit(Instruction::new(
                    "movl",
                    vec![reg.into(), map.get(id.unwrap()).into()],
                ));

                b += unspill;
            } else {
                b.emit(Instruction::new(
                    "movl",
                    vec![asm::Const(lhs).into(), map.get(id.unwrap()).into()],
                ));
                b.emit(Instruction::new(
                    "cmpl",
                    vec![map.get(rhs).into(), map.get(id.unwrap()).into()],
                ));
                b.emit(Instruction::new(
                    "setg",
                    vec![map.get(id.unwrap()).as_byte().into()],
                ));
                b.emit(Instruction::new(
                    "andb",
                    vec![asm::Const(1).into(), map.get(id.unwrap()).as_byte().into()],
                ));

                b.emit(Instruction::new(
                    "movzbl",
                    vec![
                        map.get(id.unwrap()).as_byte().into(),
                        map.get(id.unwrap()).into(),
                    ],
                ));
            }
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Relational(tac::RelationalOp::Greater),
            tac::Value::Const(tac::Const::Int(lhs)),
            tac::Value::Const(tac::Const::Int(rhs)),
        )) => {
            b.emit(Instruction::new(
                "movl",
                vec![asm::Const(lhs).into(), map.get(id.unwrap()).into()],
            ));
            b.emit(Instruction::new(
                "cmpl",
                vec![asm::Const(rhs).into(), map.get(id.unwrap()).into()],
            ));
            b.emit(Instruction::new(
                "setg",
                vec![map.get(id.unwrap()).as_byte().into()],
            ));
            b.emit(Instruction::new(
                "andb",
                vec![asm::Const(1).into(), map.get(id.unwrap()).as_byte().into()],
            ));

            b.emit(Instruction::new(
                "movzbl",
                vec![
                    map.get(id.unwrap()).as_byte().into(),
                    map.get(id.unwrap()).into(),
                ],
            ));
        }
        // GREATER_OR_EQUAL
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Relational(tac::RelationalOp::GreaterOrEq),
            tac::Value::ID(lhs),
            tac::Value::ID(rhs),
        )) => {
            if matches!(
                map.get(id.unwrap()).rg,
                asm::RegisterBackend::StackOffset(..)
            ) {
                let (reg, spill, unspill) = get_register(line, map);
                let reg = asm::Register::machine(reg, asm::Size::Doubleword);
                b += spill;
                b += checked("cmpl", line, &mut map, rhs, lhs);
                b.emit(Instruction::new("setge", vec![reg.as_byte().into()]));
                b.emit(Instruction::new(
                    "andb",
                    vec![asm::Const(1).into(), reg.as_byte().into()],
                ));

                b.emit(Instruction::new(
                    "movzbl",
                    vec![reg.as_byte().into(), reg.into()],
                ));

                b.emit(Instruction::new(
                    "movl",
                    vec![reg.into(), map.get(id.unwrap()).into()],
                ));

                b += unspill;
            } else {
                b += checked("cmpl", line, &mut map, rhs, lhs);
                b.emit(Instruction::new(
                    "setge",
                    vec![map.get(id.unwrap()).as_byte().into()],
                ));
                b.emit(Instruction::new(
                    "andb",
                    vec![asm::Const(1).into(), map.get(id.unwrap()).as_byte().into()],
                ));

                b.emit(Instruction::new(
                    "movzbl",
                    vec![
                        map.get(id.unwrap()).as_byte().into(),
                        map.get(id.unwrap()).into(),
                    ],
                ));
            }
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Relational(tac::RelationalOp::GreaterOrEq),
            tac::Value::ID(lhs),
            tac::Value::Const(tac::Const::Int(rhs)),
        )) => {
            if matches!(
                map.get(id.unwrap()).rg,
                asm::RegisterBackend::StackOffset(..)
            ) {
                let (reg, spill, unspill) = get_register(line, map);
                let reg = asm::Register::machine(reg, asm::Size::Doubleword);
                b += spill;

                b.emit(Instruction::new(
                    "cmpl",
                    vec![asm::Const(rhs).into(), map.get(lhs).into()],
                ));
                b.emit(Instruction::new("setge", vec![reg.as_byte().into()]));
                b.emit(Instruction::new(
                    "andb",
                    vec![asm::Const(1).into(), reg.as_byte().into()],
                ));

                b.emit(Instruction::new(
                    "movzbl",
                    vec![reg.as_byte().into(), reg.into()],
                ));

                b.emit(Instruction::new(
                    "movl",
                    vec![reg.into(), map.get(id.unwrap()).into()],
                ));

                b += unspill;
            } else {
                b.emit(Instruction::new(
                    "cmpl",
                    vec![asm::Const(rhs).into(), map.get(lhs).into()],
                ));
                b.emit(Instruction::new(
                    "setge",
                    vec![map.get(id.unwrap()).as_byte().into()],
                ));
                b.emit(Instruction::new(
                    "andb",
                    vec![asm::Const(1).into(), map.get(id.unwrap()).as_byte().into()],
                ));

                b.emit(Instruction::new(
                    "movzbl",
                    vec![
                        map.get(id.unwrap()).as_byte().into(),
                        map.get(id.unwrap()).into(),
                    ],
                ));
            }
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Relational(tac::RelationalOp::GreaterOrEq),
            tac::Value::Const(tac::Const::Int(lhs)),
            tac::Value::ID(rhs),
        )) => {
            if matches!(
                map.get(id.unwrap()).rg,
                asm::RegisterBackend::StackOffset(..)
            ) {
                let (reg, spill, unspill) = get_register(line, map);
                let reg = asm::Register::machine(reg, asm::Size::Doubleword);
                b += spill;

                b.emit(Instruction::new(
                    "movl",
                    vec![asm::Const(lhs).into(), map.get(id.unwrap()).into()],
                ));

                b.emit(Instruction::new(
                    "cmpl",
                    vec![map.get(rhs).into(), map.get(id.unwrap()).into()],
                ));
                b.emit(Instruction::new("setge", vec![reg.as_byte().into()]));
                b.emit(Instruction::new(
                    "andb",
                    vec![asm::Const(1).into(), reg.as_byte().into()],
                ));

                b.emit(Instruction::new(
                    "movzbl",
                    vec![reg.as_byte().into(), reg.into()],
                ));

                b.emit(Instruction::new(
                    "movl",
                    vec![reg.into(), map.get(id.unwrap()).into()],
                ));

                b += unspill;
            } else {
                b.emit(Instruction::new(
                    "movl",
                    vec![asm::Const(lhs).into(), map.get(id.unwrap()).into()],
                ));

                b.emit(Instruction::new(
                    "cmpl",
                    vec![map.get(rhs).into(), map.get(id.unwrap()).into()],
                ));
                b.emit(Instruction::new(
                    "setge",
                    vec![map.get(id.unwrap()).as_byte().into()],
                ));
                b.emit(Instruction::new(
                    "andb",
                    vec![asm::Const(1).into(), map.get(id.unwrap()).as_byte().into()],
                ));

                b.emit(Instruction::new(
                    "movzbl",
                    vec![
                        map.get(id.unwrap()).as_byte().into(),
                        map.get(id.unwrap()).into(),
                    ],
                ));
            }
        }
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Relational(tac::RelationalOp::GreaterOrEq),
            tac::Value::Const(tac::Const::Int(lhs)),
            tac::Value::Const(tac::Const::Int(rhs)),
        )) => {
            b.emit(Instruction::new(
                "movl",
                vec![asm::Const(lhs).into(), map.get(id.unwrap()).into()],
            ));
            b.emit(Instruction::new(
                "cmpl",
                vec![asm::Const(rhs).into(), map.get(id.unwrap()).into()],
            ));
            b.emit(Instruction::new(
                "setge",
                vec![map.get(id.unwrap()).as_byte().into()],
            ));
            b.emit(Instruction::new(
                "andb",
                vec![asm::Const(1).into(), map.get(id.unwrap()).as_byte().into()],
            ));

            b.emit(Instruction::new(
                "movzbl",
                vec![
                    map.get(id.unwrap()).as_byte().into(),
                    map.get(id.unwrap()).into(),
                ],
            ));
        }
        // OR
        tac::Instruction::Op(tac::Op::Op(
            tac::TypeOp::Bit(tac::BitwiseOp::And),
            tac::Value::ID(lhs),
            tac::Value::ID(rhs),
        )) => {}
        // ALLOC
        tac::Instruction::Alloc(tac::Value::Const(tac::Const::Int(v))) => {
            b.emit(Instruction::new(
                "movl",
                vec![asm::Const(v).into(), map.get(id.unwrap()).into()],
            ));
        }
        tac::Instruction::Alloc(tac::Value::ID(v)) => {
            b += checked_mov(line, &mut map, v, id.unwrap());
        }
        // ASSIGN
        tac::Instruction::Assignment(id, tac::Value::Const(tac::Const::Int(v))) => {
            b.emit(Instruction::new(
                "movl",
                vec![asm::Const(v).into(), map.get(id).into()],
            ));
        }
        tac::Instruction::Assignment(id, tac::Value::ID(v)) => {
            b += checked_mov(line, &mut map, v, id);
        }
        // RETURN
        tac::Instruction::ControlOp(tac::ControlOp::Return(tac::Value::ID(id))) => {
            b.emit(Instruction::new(
                "movl",
                vec![
                    map.get(id).into(),
                    asm::Register::machine("eax", asm::Size::Doubleword).into(),
                ],
            ));
        }
        tac::Instruction::ControlOp(tac::ControlOp::Return(tac::Value::Const(
            tac::Const::Int(v),
        ))) => {
            b.emit(Instruction::new(
                "movl",
                vec![
                    asm::Const(v).into(),
                    asm::Register::machine("eax", asm::Size::Doubleword).into(),
                ],
            ));
        }
        // LABEL
        tac::Instruction::ControlOp(tac::ControlOp::Label(label)) => {
            b.emit_label(&format!("_L{}", label));
        }
        // GOTO
        tac::Instruction::ControlOp(tac::ControlOp::Branch(tac::Branch::GOTO(label))) => {
            b.emit(Instruction::new(
                "jmp",
                vec![asm::Arg::Label(format!("_L{}", label))],
            ));
        }
        // IfGOTO
        tac::Instruction::ControlOp(tac::ControlOp::Branch(tac::Branch::IfGOTO(
            tac::Value::ID(v),
            label,
        ))) => {
            b.emit(Instruction::new(
                "cmpl",
                vec![asm::Const(0).into(), map.get(v).into()],
            ));
            b.emit(Instruction::new(
                "je",
                vec![asm::Arg::Label(format!("_L{}", label))],
            ));
        }
        tac::Instruction::ControlOp(tac::ControlOp::Branch(tac::Branch::IfGOTO(
            tac::Value::Const(tac::Const::Int(c)),
            label,
        ))) => {
            let offset = map.alloc_stack();

            b.emit(Instruction::new(
                "movl",
                vec![
                    asm::Const(c).into(),
                    asm::Register::new(
                        asm::RegisterBackend::StackOffset(offset),
                        asm::Size::Doubleword,
                    )
                    .into(),
                ],
            ));

            b.emit(Instruction::new(
                "cmpl",
                vec![
                    asm::Const(0).into(),
                    asm::Register::new(
                        asm::RegisterBackend::StackOffset(offset),
                        asm::Size::Doubleword,
                    )
                    .into(),
                ],
            ));
            b.emit(Instruction::new(
                "je",
                vec![asm::Arg::Label(format!("_L{}", label))],
            ));
        }
        tac::Instruction::Call(tac::Call { name, params, .. }) => {
            let mut unspills = Vec::new();
            let regs = ["edi", "esi", "edx", "ecx", "r8d", "r9d"];
            for (p, reg) in params.iter().zip(&regs) {
                let p = match p {
                    tac::Value::ID(p) => map.get(*p).into(),
                    tac::Value::Const(tac::Const::Int(p)) => asm::Const(*p).into(),
                };

                if map
                    .live_at(line)
                    .contains(&asm::Register::machine(reg, asm::Size::Doubleword))
                    && {
                        match map.get(id.unwrap()).rg {
                            asm::RegisterBackend::Machine(reg) => !regs.contains(&reg),
                            _ => true,
                        }
                    }
                {
                    let offset = map.alloc_stack();
                    let mut spill = asm::Block::new();
                    spill.emit(Instruction::new(
                        "movl",
                        vec![
                            asm::Register::machine(reg, asm::Size::Doubleword).into(),
                            asm::Register::new(
                                asm::RegisterBackend::StackOffset(offset),
                                asm::Size::Doubleword,
                            )
                            .into(),
                        ],
                    ));

                    b += spill;
                    b.emit(Instruction::new(
                        "movl",
                        vec![p, asm::Register::machine(reg, asm::Size::Doubleword).into()],
                    ));

                    let mut unspill = asm::Block::new();
                    unspill.emit(Instruction::new(
                        "movl",
                        vec![
                            asm::Register::new(
                                asm::RegisterBackend::StackOffset(offset),
                                asm::Size::Doubleword,
                            )
                            .into(),
                            asm::Register::machine(reg, asm::Size::Doubleword).into(),
                        ],
                    ));

                    unspills.push(unspill);
                } else {
                    b.emit(Instruction::new(
                        "movl",
                        vec![p, asm::Register::machine(reg, asm::Size::Doubleword).into()],
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
                            tac::Value::Const(tac::Const::Int(p)) => asm::Const(*p).into(),
                        };

                        const PLATFORM_WORD_SIZE: usize = 8;
                        stack_reserved += PLATFORM_WORD_SIZE;

                        b.emit(Instruction::new("pushq", vec![p]));
                    });
            };

            if map
                .live_at(line)
                .contains(&asm::Register::machine("eax", asm::Size::Doubleword))
                && map.get(id.unwrap()) != asm::Register::machine("eax", asm::Size::Doubleword)
            {
                let offset = map.alloc_stack();
                let mut spill = asm::Block::new();
                spill.emit(Instruction::new(
                    "movl",
                    vec![
                        asm::Register::machine("eax", asm::Size::Doubleword).into(),
                        asm::Register::new(
                            asm::RegisterBackend::StackOffset(offset),
                            asm::Size::Doubleword,
                        )
                        .into(),
                    ],
                ));

                b += spill;

                let mut unspill = asm::Block::new();
                unspill.emit(Instruction::new(
                    "movl",
                    vec![
                        asm::Register::new(
                            asm::RegisterBackend::StackOffset(offset),
                            asm::Size::Doubleword,
                        )
                        .into(),
                        asm::Register::machine("eax", asm::Size::Doubleword).into(),
                    ],
                ));

                unspills.push(unspill);
            }

            b.emit(Instruction::new(
                "call",
                vec![asm::Arg::Label(name.to_owned())],
            ));

            if map.get(id.unwrap()) != asm::Register::machine("eax", asm::Size::Doubleword) {
                b.emit(Instruction::new(
                    "movl",
                    vec![
                        asm::Register::machine("eax", asm::Size::Doubleword).into(),
                        map.get(id.unwrap()).into(),
                    ],
                ));
            }

            if stack_reserved != 0 {
                b.emit(Instruction::new(
                    "subq",
                    vec![
                        asm::Const(stack_reserved as i32).into(),
                        asm::Register::machine("rsp", asm::Size::Quadword).into(),
                    ],
                ));
            }

            for u in unspills {
                b += u;
            }
        }
        _ => unimplemented!(),
    }
    b
}
