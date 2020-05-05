use super::asm;
use crate::il::lifeinterval;
use crate::il::tac;
use std::collections::HashMap;

pub struct Allocator {
    m: HashMap<tac::ID, asm::Register>,
    intervals: lifeinterval::LiveIntervals,
    pub stack_size: usize,
    REGISTERS: &'static [asm::MachineRegister],
}

impl Allocator {
    pub fn new(is_main: bool, params: &[tac::ID], instructions: &[tac::InstructionLine]) -> Self {
        let REGISTERS: &'static [asm::MachineRegister] = {
            if is_main {
                &["eax", "ebx", "ecx", "edx"]
            } else {
                // let params_regs = ["edi", "esi", "edx", "ecx", "r8d", "r9d"];
                match params.len() {
                    6..=std::usize::MAX => &["edi", "esi", "edx", "ecx", "r8d", "r9d", "ebx"],
                    5 => &["edi", "esi", "edx", "ecx", "r8d", "ebx", "eax"],
                    4 => &["edi", "esi", "edx", "ecx", "eax"],
                    3 => &["edi", "esi", "edx", "eax"],
                    2 => &["edi", "esi", "eax"],
                    1 => &["edi", "eax"],
                    0 => &["eax"],
                    _ => unreachable!(),
                }
            }
        };

        let intervals = lifeinterval::LiveIntervals::new(instructions);
        let (mut s, stack_start) = Self::recognize_params(params);

        let mut free = REGISTERS.to_vec();
        let mut allocated: HashMap<&str, tac::ID> = HashMap::new();

        for (id, reg) in s.iter() {
            let reg = match reg.rg {
                asm::RegisterBackend::Machine(reg) => reg,
                _ => continue,
            };
            if free.contains(&reg) {
                free.remove(free.iter().position(|r| r == &reg).unwrap());
            }
        }

        let mut used_registers = free.clone();

        let mut stack_ptr = stack_start;
        for (index, tac::InstructionLine(i, id)) in instructions.iter().enumerate() {
            if let tac::Instruction::Alloc(..) = i {
                stack_ptr += 4;
                s.insert(
                    id.unwrap(),
                    asm::Register::new(
                        asm::RegisterBackend::StackOffset(stack_ptr),
                        asm::Size::Doubleword,
                    ),
                );
            } else if let Some(id) = id {
                allocated.retain(|reg, id| {
                    if index > intervals.get(*id).end {
                        free.push(reg);
                        false
                    } else {
                        true
                    }
                });

                if free.is_empty() {
                    let reg = used_registers.first().unwrap();
                    let id = allocated.remove(reg).unwrap();
                    free.push(reg);

                    stack_ptr += 4;
                    *s.get_mut(&id).unwrap() = asm::Register::new(
                        asm::RegisterBackend::StackOffset(stack_ptr),
                        asm::Size::Doubleword,
                    );
                }

                let reg = free.pop().unwrap();
                allocated.insert(reg, *id);
                s.entry(*id).or_insert(asm::Register::new(
                    asm::RegisterBackend::Machine(reg),
                    asm::Size::Doubleword,
                ));
            }
        }

        Allocator {
            m: s,
            stack_size: stack_ptr,
            intervals,
            REGISTERS,
        }
    }

    pub fn get(&self, id: usize) -> asm::Register {
        self.m[&id]
    }

    pub fn find_free_at(&self, index: usize) -> Option<asm::MachineRegister> {
        let free = self.free_at(index);
        free.first().cloned()
    }

    // alive_at is a better name
    pub fn live_at(&self, index: usize) -> Vec<asm::Register> {
        self.intervals
            .live_at(index)
            .iter()
            .map(|id| self.m[id].clone())
            .collect()
    }

    pub fn free_at(&self, index: usize) -> Vec<asm::MachineRegister> {
        let live_at = self.intervals.live_at(index);
        let occupied = live_at
            .iter()
            .map(|id| self.m[id].clone())
            .flat_map(|reg| {
                if let asm::RegisterBackend::Machine(reg) = reg.rg {
                    Some(reg)
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        let mut regs = self.REGISTERS.to_vec();
        regs.retain(|reg| !occupied.contains(reg));
        regs
    }

    pub fn alloc_stack(&mut self) -> usize {
        self.stack_size += 4;
        self.stack_size
    }

    fn recognize_params(params: &[tac::ID]) -> (HashMap<tac::ID, asm::Register>, usize) {
        let regs = ["edi", "esi", "edx", "ecx", "r8d", "r9d"];
        let mut p = params
            .iter()
            .take(regs.len())
            .enumerate()
            .map(|(i, id)| (*id, asm::Register::machine(regs[i], asm::Size::Doubleword)))
            .collect::<HashMap<tac::ID, asm::Register>>();

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
                        let reg = asm::Register::new(
                            asm::RegisterBackend::StackOffset(param_offset),
                            asm::Size::Doubleword,
                        );
                        param_offset += PLATFORM_WORD_SIZE;

                        (*id, reg)
                    })
                    .collect::<HashMap<tac::ID, asm::Register>>(),
            );

            (p, param_offset)
        } else {
            (p, 0)
        }
    }
}
