use super::asm::{Indirect, Offset, Part, Place, Register, RegisterX64, Size, Block, AsmX32};
use crate::il::lifeinterval;
use crate::il::tac;
use std::collections::HashMap;

pub struct Allocator {
    m: HashMap<tac::ID, Place>,
    intervals: lifeinterval::LiveIntervals,
    pub stack_size: usize,
    REGISTERS: &'static [RegisterX64],
}

impl Allocator {
    pub fn new(ir: &tac::File, f: &tac::FuncDef) -> (Self, Block) {
        use RegisterX64::*;
        use Size::*;

        let REGISTERS: &'static [RegisterX64] = {
            if f.name == "main" {
                &[RAX, RBX, RCX, RDX]
            } else {
                // let params_regs = ["edi", "esi", "edx", "ecx", "r8d", "r9d"];
                match f.parameters.len() {
                    6..=std::usize::MAX => &[RDI, RSI, RDX, RCX, R8, R9, RBX],
                    5 => &[RDI, RSI, RDX, RCX, R8, RBX, RAX],
                    4 => &[RDI, RSI, RDX, RCX, RAX],
                    3 => &[RDI, RSI, RDX, RAX],
                    2 => &[RDI, RSI, RAX],
                    1 => &[RDI, RAX],
                    0 => &[RAX],
                    _ => unreachable!(),
                }
            }
        };

        let intervals = lifeinterval::LiveIntervals::new(&f.instructions);
        let (mut s, mut stack_start) = Self::recognize_params(&f.parameters);

        let mut params = Block::new();
        for (param, place) in s.iter_mut() {
            stack_start += 4;
            let stack = Place::Indirect(Indirect::new(
                Register::Register(RegisterX64::RBP),
                stack_start,
                Size::Doubleword,
            ));
            params.emit(AsmX32::Mov(
                stack.clone(),
                place.clone().into(),
            ));

            *place = stack;
        }

        for (id, ..) in &ir.global_data {
            s.insert(
                *id,
                Place::Indirect(Indirect {
                    reg: Register::Register(RIP),
                    offset: Offset::Label(format!("_var_{}", id)),
                    size: Size::Doubleword,
                }),
            );
        }

        let mut free = REGISTERS.to_vec();
        let mut allocated: HashMap<RegisterX64, tac::ID> = HashMap::new();
        let used_registers = free.clone();
        let mut stack_ptr = stack_start;
        for (index, tac::InstructionLine(i, id)) in f.instructions.iter().enumerate() {
            if matches!(i, tac::Instruction::Alloc(..)) && f.ctx.is_variable(id.unwrap()) {
                stack_ptr += 4;
                s.insert(
                    id.unwrap(),
                    Place::Indirect(Indirect::new(
                        Register::Register(RBP),
                        stack_ptr,
                        Doubleword,
                    )),
                );
            } else if let Some(id) = id {
                allocated.retain(|reg, id| {
                    if index > intervals.get(*id).end {
                        free.push(reg.clone());
                        false
                    } else {
                        true
                    }
                });

                if free.is_empty() {
                    let reg = used_registers.first().unwrap();
                    let id = allocated.remove(reg).unwrap();
                    free.push(reg.clone());

                    stack_ptr += 4;
                    *s.get_mut(&id).unwrap() = Place::Indirect(Indirect::new(
                        Register::Register(RBP),
                        stack_ptr,
                        Doubleword,
                    ));
                }

                let reg = free.pop().unwrap();
                allocated.insert(reg.clone(), *id);
                s.entry(*id)
                    .or_insert(Place::Register(Register::Sub(reg, Part::Doubleword)));
            }
        }

        (Allocator {
            m: s,
            stack_size: stack_ptr,
            intervals,
            REGISTERS,
        }, params)
    }

    pub fn get(&self, id: usize) -> Place {
        self.m[&id].clone()
    }

    pub fn find_free_at(&self, index: usize) -> Option<RegisterX64> {
        let free = self.free_at(index);
        free.first().cloned()
    }

    // alive_at is a better name
    pub fn live_at(&self, index: usize) -> Vec<Place> {
        self.intervals
            .live_at(index)
            .iter()
            .map(|id| self.m[id].clone())
            .collect()
    }

    pub fn free_at(&self, index: usize) -> Vec<RegisterX64> {
        let live_at = self.intervals.live_at(index);
        let occupied = live_at
            .iter()
            .map(|id| self.m[id].clone())
            .flat_map(|reg| {
                if let Place::Register(reg) = reg {
                    Some(reg)
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        let mut regs = self.REGISTERS.to_vec();
        regs.retain(|reg| {
            occupied.contains(&Register::Register(reg.clone()))
                || occupied.contains(&Register::Sub(reg.clone(), Part::Doubleword)) == false
        });
        regs
    }

    pub fn alloc_stack(&mut self) -> usize {
        self.stack_size += 4;
        self.stack_size
    }

    fn recognize_params(params: &[tac::ID]) -> (HashMap<tac::ID, Place>, usize) {
        use RegisterX64::*;
        let regs = [RDI, RSI, RDX, RCX, R8, R9];
        let mut p = params
            .iter()
            .take(regs.len())
            .enumerate()
            .map(|(i, id)| {
                (
                    *id,
                    Place::Register(Register::Sub(regs[i].clone(), Part::Doubleword)),
                )
            })
            .collect::<HashMap<tac::ID, Place>>();

        if params.len() > regs.len() {
            const PLATFORM_WORD_SIZE: usize = 8;
            let mut param_offset = PLATFORM_WORD_SIZE * 2;
            p.extend(
                params
                    .iter()
                    .rev()
                    .take(params.len() - regs.len())
                    .rev()
                    .map(|id| {
                        let reg = Place::Indirect(Indirect::new(
                            Register::Register(RBP),
                            param_offset,
                            Size::Doubleword,
                        ));
                        param_offset += PLATFORM_WORD_SIZE;

                        (*id, reg)
                    })
                    .collect::<HashMap<tac::ID, Place>>(),
            );

            (p, param_offset)
        } else {
            (p, 0)
        }
    }
}
