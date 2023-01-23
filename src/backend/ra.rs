use hashbrown::{HashMap, HashSet};
use log::{debug, error, info, trace, warn};
use std::fmt;
use std::ops::Deref;
use vector_map::set::VecSet;
use vector_map::VecMap;

use crate::backend::asmgen::ARGS_IN_REGISTERS;
use crate::backend::ir::{self, CallingConvention, EndType, IrFunction};
use crate::frontend::semantic_analysis::ast::Ident;

use super::asmgen::{params_registers, Frame, QUADWORD_SIZE};
use super::ir::{
    BasicBlock, BasicBlockIdx, BinOpType, InPlaceUnOpType, Instant, Quadruple, RelOpType,
    StringLiteral, UnOpType, Value, Var, CFG,
};

impl fmt::Debug for Reg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <Self as fmt::Display>::fmt(&self, f)
    }
}

const CALLER_SAVE_REGS: &[Reg] = &[RAX, RCX, RDX, R8, R9, R10, R11];
const CALLEE_SAVE_REGS: &[Reg] = &[RBX, RBP, R12, R13, R14, R15];

#[derive(PartialEq, Eq, Clone, Copy)]
pub(crate) enum Reg {
    CallerSave(CallerSaveReg),
    CalleeSave(CalleeSaveReg),
}
pub(crate) const RAX: Reg = Reg::CallerSave(CallerSaveReg::Rax);
pub(crate) const RBX: Reg = Reg::CallerSave(CallerSaveReg::Rax);
pub(crate) const RCX: Reg = Reg::CallerSave(CallerSaveReg::Rcx);
pub(crate) const RDX: Reg = Reg::CallerSave(CallerSaveReg::Rdx);
pub(crate) const RDI: Reg = Reg::CallerSave(CallerSaveReg::Rdi);
pub(crate) const RSI: Reg = Reg::CallerSave(CallerSaveReg::Rsi);
pub(crate) const R8: Reg = Reg::CallerSave(CallerSaveReg::R8);
pub(crate) const R9: Reg = Reg::CallerSave(CallerSaveReg::R9);
pub(crate) const R10: Reg = Reg::CallerSave(CallerSaveReg::R10);
pub(crate) const R11: Reg = Reg::CallerSave(CallerSaveReg::R11);
pub(crate) const R12: Reg = Reg::CalleeSave(CalleeSaveReg::R12);
pub(crate) const R13: Reg = Reg::CalleeSave(CalleeSaveReg::R13);
pub(crate) const R14: Reg = Reg::CalleeSave(CalleeSaveReg::R14);
pub(crate) const R15: Reg = Reg::CalleeSave(CalleeSaveReg::R15);
pub(crate) const RSP: Reg = Reg::CalleeSave(CalleeSaveReg::Rsp);
pub(crate) const RBP: Reg = Reg::CalleeSave(CalleeSaveReg::Rbp);

#[derive(PartialEq, Eq, Clone, Copy)]
pub(crate) enum CallerSaveReg {
    Rax,
    Rcx,
    Rdx,
    Rsi,
    Rdi,
    R8,
    R9,
    R10,
    R11,
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub(crate) enum CalleeSaveReg {
    Rbx,
    Rsp,
    Rbp,
    R12,
    R13,
    R14,
    R15,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Label {
    Num(usize),
    Named(Ident),
    Str(StringLiteral),
}

#[derive(Debug)]
pub(crate) enum Loc<I: InstrLevel> {
    Reg(Reg),
    Mem(I::Mem),
}

#[derive(Debug)]
pub(crate) enum Val<I: InstrLevel> {
    Reg(Reg),
    Instant(Instant),
    Mem(I::Mem),
}

impl<I: InstrLevel> From<Loc<I>> for Val<I> {
    fn from(loc: Loc<I>) -> Self {
        match loc {
            Loc::Reg(reg) => Val::Reg(reg),
            Loc::Mem(mem) => Val::Mem(mem),
        }
    }
}

#[derive(Debug)]
pub(crate) struct RaLevel;
impl InstrLevel for RaLevel {
    type Mem = RaMem;
}

pub(crate) type RaInstr = Instr<RaLevel>;

#[derive(Debug)]
pub(crate) enum RaMem {
    Stack { frame_offset: FrameOffset },
    Heap { base: Reg, displacement: isize },
}

pub(crate) trait InstrLevel {
    type Mem;
}

#[derive(Debug)]
#[must_use = "You probably want to emit some code"]
pub(crate) enum Instr<I: InstrLevel> {
    Label(Label),

    Jmp(Label),
    Jz(Label),
    Jnz(Label),
    Jg(Label),
    Jge(Label),
    Jl(Label),
    Jle(Label),

    LoadString(Reg, StringLiteral),
    MovToReg(Reg, Val<I>),
    MovToMem(I::Mem, Reg),

    Xchg(Reg, Loc<I>),

    Test(Reg),
    Cmp(Reg, Val<I>),
    Add(Reg, Val<I>),
    Sub(Reg, Val<I>),
    IMul(Reg, Val<I>),
    IDiv(Loc<I>), // RDX:RAX divided by Reg/Mem, quotient in RAX, remainder in RDX.
    Cqo,          // Prepared RDX as empty for IDiv
    Inc(Loc<I>),
    Dec(Loc<I>),
    Neg(Loc<I>),
    Not(Loc<I>),
    Sal(Reg, Instant),
    Sar(Reg, Instant),

    Lea(Reg, I::Mem),
    LeaLabel(Reg, String),

    Push(Val<I>),

    Ret,
    Call(Label),
    CallReg(Reg),

    // quasi-instructions
    Empty,
    Newline,
    AdvanceRSPForCall(usize), // (len in bytes)
    AdjustRSPForStackParam,
    ResetRSP,
}

impl RelOpType {
    fn instrs(&self) -> (fn(Label) -> RaInstr, fn(Label) -> RaInstr) {
        // (then, else)
        match self {
            RelOpType::Gt => (RaInstr::Jg, RaInstr::Jle),
            RelOpType::Ge => (RaInstr::Jge, RaInstr::Jl),
            RelOpType::Lt => (RaInstr::Jl, RaInstr::Jge),
            RelOpType::Le => (RaInstr::Jle, RaInstr::Jg),
            RelOpType::Eq => (RaInstr::Jz, RaInstr::Jnz),
            RelOpType::NEq => (RaInstr::Jnz, RaInstr::Jz),
        }
    }
}

struct RaGenState {
    block_labels: Vec<Option<Label>>,
    next_label_idx: usize,
}

impl RaGenState {
    fn new(blocks: &[BasicBlock]) -> Self {
        Self {
            block_labels: vec![None; blocks.len()],
            next_label_idx: 0,
        }
    }

    fn get_block_label(&mut self, block: BasicBlockIdx) -> Label {
        self.block_labels
            .get(*block.deref())
            .unwrap()
            .clone()
            .unwrap_or_else(|| {
                let l = self.gen_label();
                *self.block_labels.get_mut(*block.deref()).unwrap() = Some(l.clone());
                l
            })
    }

    fn gen_label(&mut self) -> Label {
        let label = Label::Num(self.next_label_idx);
        self.next_label_idx += 1;
        label
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub(crate) struct FrameOffset(isize);
impl From<FrameOffset> for isize {
    fn from(offset: FrameOffset) -> Self {
        offset.0
    }
}

struct VarLoc {
    regs: VecSet<Reg>,
    mem: VecSet<FrameOffset>,
}
impl VarLoc {
    fn empty() -> Self {
        Self {
            regs: VecSet::new(),
            mem: VecSet::new(),
        }
    }

    fn is_in_reg(&self) -> bool {
        !self.regs.is_empty()
    }
    fn any_reg(&self) -> Option<Reg> {
        self.regs.iter().next().copied()
    }
    fn any_mem(&self) -> Option<FrameOffset> {
        self.mem.iter().next().copied()
    }
    fn has_any_loc(&self) -> bool {
        self.any_reg().is_some() || self.any_mem().is_some()
    }
}

enum CallerSaveRegState {
    Free,
    Taken(Var),
}

enum CalleeSaveRegState {
    CallerTaken,
    Free,
    Taken(Var),
}

struct Description {
    reallocation_robin: usize,
    caller_save_regs: VecMap<Reg, CallerSaveRegState>,
    callee_save_regs: VecMap<Reg, CalleeSaveRegState>,

    next_frame_offset: isize,

    temporary_pool: VecMap<FrameOffset, Var>,

    var_locs: VecMap<Var, VarLoc>,
    persistent_vars: VecMap<Var, FrameOffset>,
}

impl Description {
    fn next_offset(&mut self) -> FrameOffset {
        let offset = self.next_frame_offset;
        self.next_frame_offset += 1;
        FrameOffset(offset)
    }

    fn new_func(vars: &VecSet<Var>) -> Self {
        Self {
            caller_save_regs: CALLER_SAVE_REGS
                .iter()
                .copied()
                .map(|reg| (reg, CallerSaveRegState::Free))
                .collect(),
            callee_save_regs: CALLEE_SAVE_REGS
                .iter()
                .copied()
                .map(|reg| (reg, CalleeSaveRegState::CallerTaken))
                .collect(),

            next_frame_offset: 0,
            var_locs: vars
                .iter()
                .copied()
                .map(|var| (var, VarLoc::empty()))
                .collect(),
            persistent_vars: VecMap::new(),
            reallocation_robin: CALLER_SAVE_REGS.len(),
            temporary_pool: VecMap::new(),
        }
    }

    fn get_reg_or_allocate(
        &mut self,
        var: Var,
        live_atm: &VecSet<Var>,
        instructions: &mut Vec<RaInstr>,
    ) -> Reg {
        self.any_reg(var)
            .unwrap_or_else(|| self.allocate_caller_save_reg(var, live_atm, instructions))
    }

    fn allocate_caller_save_reg(
        &mut self,
        var: Var,
        live_atm: &VecSet<Var>,
        instructions: &mut Vec<RaInstr>,
    ) -> Reg {
        self.allocate_free_caller_save_any_reg(var, live_atm)
            .unwrap_or_else(|| {
                self.reallocate_taken_caller_save_robin_reg(var, live_atm, instructions)
            })
    }

    fn allocate_caller_save_provided_reg(
        &mut self,
        var: Option<Var>,
        reg: Reg,
        live_atm: &VecSet<Var>,
        instructions: &mut Vec<RaInstr>,
    ) -> Reg {
        self.allocate_free_caller_save_provided_reg(var, reg, live_atm)
            .unwrap_or_else(|| {
                self.reallocate_taken_caller_save_provided_reg(var, reg, live_atm, instructions)
            })
    }

    fn allocate_free_caller_save_provided_reg(
        &mut self,
        var: Option<Var>,
        reg: Reg,
        live_atm: &VecSet<Var>,
    ) -> Option<Reg> {
        let reg = match self.caller_save_regs.get_mut(&reg).unwrap() {
            state @ CallerSaveRegState::Free => {
                if let Some(var) = var {
                    trace!("Allocated free {} for var {:?}", reg, var);
                    *state = CallerSaveRegState::Taken(var);
                } else {
                    trace!("Deallocated for free {}", reg);
                }
                Some(reg)
            }
            CallerSaveRegState::Taken(prev_var) if live_atm.contains(prev_var) => None,
            CallerSaveRegState::Taken(prev_var) => {
                if let Some(var) = var {
                    trace!(
                        "Allocated {} (taken by dead var {:?}) for var {:?}",
                        reg,
                        prev_var,
                        var
                    );
                    self.var_locs.get_mut(prev_var).unwrap().regs.remove(&reg);
                    *prev_var = var;
                } else {
                    trace!("Deallocated {} (was taken by dead var {:?})", reg, prev_var,);
                }
                Some(reg)
            }
        };
        if let Some(reg) = reg {
            if let Some(var) = var {
                self.var_locs.get_mut(&var).unwrap().regs.insert(reg);
                // not sure if I should do it already here, but probably should.
            } else {
                *self.caller_save_regs.get_mut(&reg).unwrap() = CallerSaveRegState::Free;
            }
        }
        reg
    }

    fn allocate_free_caller_save_any_reg(
        &mut self,
        var: Var,
        live_atm: &VecSet<Var>,
    ) -> Option<Reg> {
        CALLER_SAVE_REGS
            .iter()
            .find(|&reg| {
                self.allocate_free_caller_save_provided_reg(Some(var), *reg, live_atm)
                    .is_some()
            })
            .copied()
    }

    fn reallocate_taken_caller_save_provided_reg(
        &mut self,
        var: Option<Var>,
        reg: Reg,
        live_atm: &VecSet<Var>,
        instructions: &mut Vec<RaInstr>,
    ) -> Reg {
        let state = self.caller_save_regs.get_mut(&reg).unwrap();
        match state {
            CallerSaveRegState::Free => {
                error!("Free reg during reallocation - this shouldn't happen.");
                if let Some(var) = var {
                    *state = CallerSaveRegState::Taken(var);
                }
            }
            CallerSaveRegState::Taken(prev_var) => {
                let prev_var = {
                    let prev = *prev_var;
                    if let Some(var) = var {
                        *prev_var = var;
                    }
                    prev
                };
                if let Some(var) = var {
                    trace!(
                        "Reallocated {} (previously taken by {:?}) for var {:?}",
                        reg,
                        prev_var,
                        var
                    );
                } else {
                    trace!(
                        "Deallocated {} (previously was taken by {:?})",
                        reg,
                        prev_var,
                    );
                }
                self.var_locs.get_mut(&prev_var).unwrap().regs.remove(&reg);

                if !self.var_locs.get_mut(&prev_var).unwrap().has_any_loc() {
                    let loc = self
                        .get_persistent_var(prev_var)
                        .unwrap_or_else(|| self.alloc_from_temporary_mem_pool(prev_var, live_atm));
                    let prev_locs = self.var_locs.get_mut(&prev_var).unwrap();
                    prev_locs.mem.insert(loc);
                    prev_locs.regs.remove(&reg);
                    instructions.push(Instr::MovToMem(RaMem::Stack { frame_offset: loc }, reg));
                }
            }
        };
        if let Some(var) = var {
            self.var_locs.get_mut(&var).unwrap().regs.insert(reg);
            // not sure if I should do it already here, but probably should.
        } else {
            *self.caller_save_regs.get_mut(&reg).unwrap() = CallerSaveRegState::Free;
        }
        reg
    }

    fn reallocate_taken_caller_save_robin_reg(
        &mut self,
        var: Var,
        live_atm: &VecSet<Var>,
        instructions: &mut Vec<RaInstr>,
    ) -> Reg {
        self.reallocation_robin = (self.reallocation_robin + 1) % CALLER_SAVE_REGS.len();
        let reg = self
            .caller_save_regs
            .keys()
            .skip(self.reallocation_robin)
            .next()
            .copied()
            .unwrap();
        self.reallocate_taken_caller_save_provided_reg(Some(var), reg, live_atm, instructions)
    }

    fn alloc_from_temporary_mem_pool(&mut self, var: Var, live_atm: &VecSet<Var>) -> FrameOffset {
        // first hope that some spilled var is now dead, so try to replace it
        for (&loc, spilled_var) in self.temporary_pool.iter_mut() {
            if !live_atm.contains(spilled_var) {
                // success!
                self.var_locs.get_mut(spilled_var).unwrap().mem.remove(&loc);
                trace!(
                    "Reallocated spilled {:?} (previously taken by {:?}) for var {:?}",
                    loc,
                    spilled_var,
                    var
                );
                *spilled_var = var;
                return loc;
            }
        }
        // unfortunately, we must enlarge our pool
        let new_spilled_loc = self.next_offset();
        self.temporary_pool.insert(new_spilled_loc, var);
        trace!(
            "Allocated new spilled {:?} for var {:?}",
            new_spilled_loc,
            var
        );

        new_spilled_loc
    }

    fn save_caller_save_regs(&mut self, live_atm: &VecSet<Var>, instructions: &mut Vec<RaInstr>) {
        trace!("Saving caller save regs before call.");
        for reg in CALLER_SAVE_REGS.iter().copied() {
            self.allocate_caller_save_provided_reg(None, reg, live_atm, instructions);
        }
    }

    fn local_variables_count(&self) -> usize {
        self.persistent_vars.len() + self.temporary_pool.len()
    }

    fn get_any_var_loc_preferring_regs(
        &self,
        var: Var,
        preferred_reg: Option<Reg>,
    ) -> Loc<RaLevel> {
        let locs = self.var_locs.get(&var).unwrap();

        if let Some(preferred_reg) =
            preferred_reg.and_then(|preferred| locs.regs.contains(&preferred).then_some(preferred))
        {
            Loc::Reg(preferred_reg)
        } else if let Some(reg) = locs.any_reg() {
            Loc::Reg(reg)
        } else {
            Loc::Mem(RaMem::Stack {
                frame_offset: locs.any_mem().unwrap(),
            })
        }
    }

    fn any_reg(&self, var: Var) -> Option<Reg> {
        self.var_locs.get(&var).unwrap().any_reg()
    }

    fn put_in_memory(&mut self, var: Var, loc: FrameOffset) {
        self.var_locs.get_mut(&var).unwrap().mem.insert(loc);
    }

    fn put_to_reg(&mut self, var: Var, reg: Reg) {
        self.var_locs.get_mut(&var).unwrap().regs.insert(reg);
    }

    fn variable_mutated(&mut self, var: Var, now_in_reg: Reg) {
        // it means that we have to remove variable from all its locs.
        let locs = self.var_locs.get_mut(&var).unwrap();
        locs.mem.clear();
        locs.regs.clear();
        locs.regs.insert(now_in_reg);
    }

    fn get_variable_mem(&mut self, var: Var) -> RaMem {
        RaMem::Stack {
            frame_offset: self
                .var_locs
                .get(&var)
                .unwrap()
                .mem
                .iter()
                .next()
                .copied()
                .unwrap_or_else(|| {
                    warn!("No mem loc found for var {:?}", var);
                    self.get_or_register_persistent_var(var)
                }),
        }
    }

    fn get_val(&mut self, val: Value) -> Val<RaLevel> {
        match val {
            Value::Instant(i) => Val::Instant(i),
            Value::Variable(var) => self
                .var_locs
                .get(&var)
                .unwrap()
                .any_reg()
                .map(|reg| Val::Reg(reg))
                .unwrap_or_else(|| Val::Mem(self.get_variable_mem(var))),
        }
    }

    fn get_or_register_persistent_var(&mut self, var: Var) -> FrameOffset {
        if let Some(offset) = self.persistent_vars.get(&var) {
            *offset
        } else {
            debug!("Registering persistent var {:?}", var);
            let offset = self.next_offset();
            self.persistent_vars.insert(var, offset);
            offset
        }
    }

    fn get_persistent_var(&self, var: Var) -> Option<FrameOffset> {
        self.persistent_vars.get(&var).copied()
    }

    fn enter_block(&mut self, cfg: &CFG, block: BasicBlockIdx) {
        for (_reg, state) in self.caller_save_regs.iter_mut() {
            *state = CallerSaveRegState::Free;
        }
        let live_in = cfg[block].flow_analysis.live_variables.first().unwrap();
        let live_out = cfg[block].flow_analysis.live_variables.last().unwrap();
        let live_in_out = live_in.union(live_out);
        for var in live_in_out.copied() {
            self.get_or_register_persistent_var(var);
        }
    }

    fn reserve_rax_and_rdx(
        &mut self,
        reg: Reg,
        op2: Value,
        instructions: &mut Vec<RaInstr>,
    ) -> (Reg, Val<RaLevel>) {
        let val = self.get_val(op2);
        match (reg, val) {
            (RAX, Val::Reg(RDX)) => todo!(),
            (RAX, Val::Reg(reg_op2)) => todo!(),
            (RDX, Val::Reg(RAX)) => todo!(),
            (RDX, Val::Reg(reg_op2)) => todo!(),
            (reg_op1, Val::Reg(RAX)) => todo!(),
            (reg_op1, Val::Reg(RDX)) => todo!(),
            (reg_op1, Val::Reg(reg_op2)) => todo!(),
            (RAX, Val::Instant(_)) => todo!(),
            (RDX, Val::Instant(_)) => todo!(),
            (reg_op1, Val::Instant(_)) => todo!(),
            (RAX, Val::Mem(_)) => todo!(),
            (RDX, Val::Mem(_)) => todo!(),
            (reg_op1, Val::Mem(_)) => todo!(),
        }
    }
}

impl CFG {
    /**
     * Returns:
     * Vec of functions' asm, together with funcs' names,
     * Mapping of funcs into their frames.
     **/
    pub(crate) fn asm_instructions(
        &self,
    ) -> (Vec<(Ident, Option<Vec<RaInstr>>)>, HashMap<Ident, Frame>) {
        let mut state = RaGenState::new(&self.blocks);
        let mut emitted = HashSet::new();

        info!("Building instructions and frames");
        let (frames, instructions): (HashMap<Ident, Frame>, Vec<(Ident, Option<Vec<RaInstr>>)>) =
            self.functions
                .iter()
                .map(
                    |(
                        func,
                        IrFunction {
                            params,
                            convention,
                            entry,
                            ..
                        },
                    )| {
                        let (maybe_instrs, maybe_description) =
                            if matches!(convention, CallingConvention::SimpleCdecl) {
                                let mut instructions = Vec::new();
                                let mut vars = self.variables_in_function(func);
                                vars.extend(&self.functions.get(func).unwrap().params);
                                let mut description = Description::new_func(&vars);

                                // for var in vars.iter().copied() {
                                //     description.get_or_register_persistent_var(var);
                                // }

                                info!("Emitting ra function: {}", func);

                                // Params in registers
                                for (var, reg) in params.iter().copied().zip(params_registers()) {
                                    instructions.push(RaInstr::MovToMem(
                                        description.get_variable_mem(var),
                                        reg,
                                    ));
                                    let param_loc = description.get_or_register_persistent_var(var);
                                    description.put_in_memory(var, param_loc);
                                }
                                // Params on the stack
                                for (no, var) in params
                                    .iter()
                                    .copied()
                                    .skip(params_registers().count())
                                    .enumerate()
                                {
                                    instructions.push(RaInstr::MovToReg(
                                        RAX,
                                        Val::Mem(RaMem::Stack {
                                            frame_offset: FrameOffset(-((no + 2) as isize)),
                                        }),
                                    ));
                                    instructions.push(RaInstr::MovToMem(
                                        description.get_variable_mem(var),
                                        RAX,
                                    ));
                                    let param_loc = description.get_or_register_persistent_var(var);
                                    description.put_in_memory(var, param_loc);
                                }

                                self.function_block_instructions(
                                    &mut description,
                                    &mut instructions,
                                    entry.unwrap(),
                                    &mut emitted,
                                    &mut state,
                                    None,
                                );
                                (Some(instructions), Some(description))
                            } else {
                                (None, None)
                            };

                        let frame = match convention {
                            CallingConvention::SimpleCdecl => Frame::new(
                                func.clone(),
                                isize::max(params.len() as isize - ARGS_IN_REGISTERS as isize, 0)
                                    as usize,
                                maybe_description.unwrap().local_variables_count(),
                            ),
                            CallingConvention::CdeclFFI => Frame::new_ffi(func.clone()),
                        };

                        ((func.clone(), frame), (func.clone(), maybe_instrs))
                    },
                )
                .unzip();

        // debug!("Built frames: {:#?}", &frames);

        (instructions, frames)
    }

    fn function_block_instructions(
        &self,
        description: &mut Description,
        instructions: &mut Vec<RaInstr>,
        func_block: BasicBlockIdx,
        emitted: &mut HashSet<BasicBlockIdx>,
        state: &mut RaGenState,
        next_l: Option<&Label>,
    ) {
        if emitted.contains(&func_block) {
            trace!(
                "Block: {:?} has already been emitted; returning.",
                func_block
            );
            return;
        } else {
            emitted.insert(func_block);
        }

        let block_label = state.get_block_label(func_block);
        debug!("Emitting ir block: {:?} as {}", func_block, block_label);
        instructions.push(RaInstr::Label(block_label));

        description.enter_block(self, func_block);
        self[func_block].instructions(self, func_block, description, instructions, state);

        // Upon end of the block, make sure to put all live vars to their correspoding places in mem.
        let live_out = self[func_block]
            .flow_analysis
            .live_variables
            .last()
            .unwrap();
        for var in live_out {
            let loc = description.get_persistent_var(*var).unwrap();
            let locs = description.var_locs.get_mut(var).unwrap();
            if !locs.mem.contains(&loc) {
                locs.mem.insert(loc);
                if let Some(reg) = locs.any_reg() {
                    instructions.push(Instr::MovToMem(description.get_variable_mem(*var), reg));
                } else {
                    unimplemented!("Unlikely situation, too little time...")
                }
            }
        }

        match &self[func_block].end_type {
            Some(EndType::Return(None)) | None => {
                instructions.push(RaInstr::Ret);
            }
            Some(EndType::Return(Some(val))) => {
                match val {
                    ir::Value::Instant(i) => {
                        instructions.push(RaInstr::MovToReg(RAX, Val::Instant(*i)))
                    }
                    ir::Value::Variable(var) => {
                        instructions.push(RaInstr::MovToReg(
                            RAX,
                            Val::Mem(description.get_variable_mem(*var)),
                        ));
                        let loc = description.get_any_var_loc_preferring_regs(*var, Some(RAX));
                        if matches!(loc, Loc::Reg(RAX)) {
                            // nothing to do
                        } else {
                            instructions.push(RaInstr::MovToReg(RAX, loc.into()))
                        }
                    }
                }
                instructions.push(RaInstr::Ret);
            }
            Some(EndType::Goto(block_idx)) => {
                // If not emitted yet, simply emit it below and save a jump
                if emitted.contains(block_idx) && next_l != Some(&state.get_block_label(*block_idx))
                {
                    instructions.push(RaInstr::Jmp(state.get_block_label(*block_idx).clone()));
                }
                self.function_block_instructions(
                    description,
                    instructions,
                    *block_idx,
                    emitted,
                    state,
                    next_l,
                );
            }
            Some(EndType::IfElse(_, _, _, then_block, else_block)) if then_block == else_block => {
                let block_idx = then_block;
                // Reduce to Goto
                // If not emitted yet, simply emit it below and save a jump
                if emitted.contains(block_idx) && next_l != Some(&state.get_block_label(*block_idx))
                {
                    instructions.push(RaInstr::Jmp(state.get_block_label(*block_idx).clone()));
                }
                self.function_block_instructions(
                    description,
                    instructions,
                    *block_idx,
                    emitted,
                    state,
                    next_l,
                );
            }
            Some(EndType::IfElse(a, rel, b, then_block, else_block)) => {
                let then_l = state.get_block_label(*then_block).clone();
                let else_l = state.get_block_label(*else_block).clone();

                // Cond
                let reg = description
                    .var_locs
                    .get(a)
                    .unwrap()
                    .any_reg()
                    .unwrap_or_else(|| {
                        let live_atm = self[func_block].flow_analysis.live_before_end_type();
                        let reg = description
                            .allocate_free_caller_save_any_reg(*a, live_atm)
                            .unwrap_or_else(|| {
                                description.reallocate_taken_caller_save_robin_reg(
                                    *a,
                                    live_atm,
                                    instructions,
                                )
                            });
                        instructions.push(RaInstr::MovToReg(
                            reg,
                            Val::Mem(description.get_variable_mem(*a)),
                        ));
                        reg
                    });
                instructions.push(RaInstr::Cmp(reg, description.get_val(*b)));

                let (then_instr, else_instr) = rel.instrs();

                if next_l == Some(&then_l) && emitted.contains(else_block) {
                    instructions.push(else_instr(else_l.clone()));
                } else if next_l == Some(&else_l) && emitted.contains(then_block) {
                    instructions.push(then_instr(then_l.clone()));
                } else {
                    instructions.push(else_instr(else_l.clone()));
                    if emitted.contains(then_block) {
                        instructions.push(RaInstr::Jmp(then_l));
                    }
                    // Then
                    self.function_block_instructions(
                        description,
                        instructions,
                        *then_block,
                        emitted,
                        state,
                        Some(&else_l),
                    );

                    // Else
                    self.function_block_instructions(
                        description,
                        instructions,
                        *else_block,
                        emitted,
                        state,
                        next_l,
                    );
                }
            }
        }
    }
}

impl BasicBlock {
    fn instructions(
        &self,
        cfg: &CFG,
        block_idx: BasicBlockIdx,
        description: &mut Description,
        instructions: &mut Vec<RaInstr>,
        state: &mut RaGenState,
    ) {
        for (quadruple_idx, quadruple) in self.quadruples.iter().enumerate() {
            quadruple.instructions(
                cfg,
                block_idx,
                description,
                instructions,
                state,
                quadruple_idx,
            );
        }
    }
}

impl Quadruple {
    fn instructions(
        &self,
        cfg: &CFG,
        block_idx: BasicBlockIdx,
        description: &mut Description,
        instructions: &mut Vec<RaInstr>,
        state: &mut RaGenState,
        quadruple_idx: usize,
    ) {
        let live_before = &cfg[block_idx].flow_analysis.live_variables[quadruple_idx];
        let live_after = &cfg[block_idx].flow_analysis.live_variables[quadruple_idx + 1];
        match self {
            Quadruple::BinOp(dst, op1, bin_op, op2) => {
                let op1_loc = description.get_any_var_loc_preferring_regs(*op1, Some(RAX)); // for Div/Mod
                let reg = description.any_reg(*dst).unwrap_or_else(|| match op1_loc {
                    Loc::Reg(op1_reg) if !live_after.contains(op1) => {
                        // we can reuse op1_reg
                        op1_reg
                    }
                    _ => {
                        let reg =
                            description.allocate_caller_save_reg(*op1, live_before, instructions);
                        description.put_to_reg(*op1, reg);
                        instructions.push(RaInstr::MovToReg(reg, op1_loc.into()));
                        reg
                    }
                });
                // At this moment, `op1` value resides in `reg`.

                match bin_op {
                    BinOpType::Add | BinOpType::Sub | BinOpType::Mul => {
                        let val = description.get_val(*op2);
                        match bin_op {
                            BinOpType::Add => instructions.push(RaInstr::Add(reg, val)),
                            BinOpType::Sub => instructions.push(RaInstr::Sub(reg, val)),
                            BinOpType::Mul => instructions.push(RaInstr::IMul(reg, val)),
                            _ => unreachable!(),
                        }
                        description.variable_mutated(*dst, reg);
                    }
                    BinOpType::Div | BinOpType::Mod => {
                        todo!();
                        // We need to have dividend in RAX=reg, divisor anywhere else than in RDX.
                        let (dividend_reg, divisor_val) =
                            description.reserve_rax_and_rdx(reg, *op2, instructions);
                        assert!(matches!(
                            description.caller_save_regs.get(&RDX).unwrap(),
                            CallerSaveRegState::Free
                        ));
                        instructions.push(RaInstr::Cqo);
                        match divisor_val {
                            Val::Reg(reg) => instructions.push(RaInstr::IDiv(Loc::Reg(reg))),
                            Val::Instant(_) => {
                                instructions.push(RaInstr::MovToReg(RCX, divisor_val));
                                instructions.push(RaInstr::IDiv(Loc::Reg(RCX)));
                            }
                            Val::Mem(mem) => instructions.push(RaInstr::IDiv(Loc::Mem(mem))),
                        }
                        match bin_op {
                            BinOpType::Div => description.variable_mutated(*dst, RAX),
                            BinOpType::Mod => description.variable_mutated(*dst, RDX),
                            _ => unreachable!(),
                        }
                    }
                };
            }
            Quadruple::UnOp(dst, un_op_type, op) => {
                let reg = description.get_reg_or_allocate(*dst, live_before, instructions);

                instructions.push(RaInstr::MovToReg(reg, description.get_val(*op)));
                match un_op_type {
                    UnOpType::Neg => instructions.push(RaInstr::Neg(Loc::Reg(reg))),
                }
                description.variable_mutated(*dst, reg);
            }
            Quadruple::Copy(dst, src) => {
                let reg = description
                    .var_locs
                    .get(dst)
                    .unwrap()
                    .any_reg()
                    .unwrap_or_else(|| {
                        description.allocate_caller_save_reg(*dst, live_before, instructions)
                    });

                description.variable_mutated(*dst, reg);
                instructions.push(RaInstr::MovToReg(
                    reg,
                    description
                        .get_any_var_loc_preferring_regs(*src, None)
                        .into(),
                ));
                description.variable_mutated(*dst, reg);
                // instructions.push(RaInstr::MovToMem(description.get_variable_mem(*dst), RAX));
            }
            Quadruple::Set(dst, i) => {
                let reg = description.get_reg_or_allocate(*dst, live_before, instructions);
                instructions.push(RaInstr::MovToReg(reg, Val::Instant(*i)));
                description.variable_mutated(*dst, reg);
            }
            Quadruple::GetStrLit(dst, str_idx) => {
                let reg = description.get_reg_or_allocate(*dst, live_before, instructions);
                instructions.push(RaInstr::LoadString(reg, *str_idx));
                description.variable_mutated(*dst, reg);
            }

            Quadruple::Call(dst, func, args) => {
                description.save_caller_save_regs(live_before, instructions);

                let cfg_function = cfg.functions.get(func).unwrap();
                let callee_convention = &cfg_function.convention;
                match callee_convention {
                    CallingConvention::SimpleCdecl => {
                        // Params in registers
                        for (reg, arg) in params_registers().zip(args.iter().copied()) {
                            instructions.push(RaInstr::MovToReg(reg, description.get_val(arg)));
                        }

                        let params_on_the_stack_num = usize::max(
                            (args.len() as isize - params_registers().count() as isize) as usize,
                            0,
                        );

                        // FIXME: asmgen part should fix this
                        let stack_alignment_growth = if params_on_the_stack_num % 2 != 0 {
                            QUADWORD_SIZE
                        } else {
                            0
                        }; // stack alignment

                        if stack_alignment_growth != 0 {
                            instructions.push(RaInstr::AdvanceRSPForCall(stack_alignment_growth))
                            // state
                            //     .advance_rsp(stack_alignment_growth as isize);
                        }

                        // Params on the stack
                        for arg in args.iter().copied().skip(params_registers().count()).rev() {
                            instructions.push(RaInstr::Push(description.get_val(arg)));
                            instructions.push(RaInstr::AdjustRSPForStackParam);
                        }

                        instructions.push(RaInstr::Call(Label::Named(func.clone())));

                        if stack_alignment_growth != 0 || params_on_the_stack_num > 0 {
                            instructions.push(RaInstr::ResetRSP);
                        }

                        // Save return value
                        description.variable_mutated(*dst, RAX);
                    }
                    CallingConvention::CdeclFFI => {
                        // place arguments in corresponding registers
                        for (arg, reg) in args.iter().copied().zip(params_registers()) {
                            instructions.push(RaInstr::MovToReg(reg, description.get_val(arg)));
                        }

                        instructions.push(RaInstr::Call(Label::Named(func.clone())));

                        // Save return value
                        description.variable_mutated(*dst, RAX);
                    }
                }
            }

            Quadruple::ArrLoad(_, _, _) => todo!(),
            Quadruple::ArrStore(_, _, _) => todo!(),

            Quadruple::DerefLoad(dst, ptr) => {
                todo!();
                instructions.push(RaInstr::MovToReg(
                    RAX,
                    Val::Mem(description.get_variable_mem(ptr.base)),
                ));
                instructions.push(RaInstr::MovToReg(
                    RAX,
                    Val::Mem(RaMem::Heap {
                        base: RAX,
                        displacement: ptr.offset as isize,
                    }),
                ));
                instructions.push(RaInstr::MovToMem(description.get_variable_mem(*dst), RAX));
            }

            Quadruple::DerefStore(src, ptr) => {
                todo!();
                // src in RAX, ptr in RDX
                instructions.push(RaInstr::MovToReg(RAX, description.get_val(*src)));
                instructions.push(RaInstr::MovToReg(
                    RDX,
                    Val::Mem(description.get_variable_mem(ptr.base)),
                ));
                let ptr_mem = RaMem::Heap {
                    base: RDX,
                    displacement: ptr.offset as isize,
                };
                instructions.push(RaInstr::MovToMem(ptr_mem, RAX));
            }

            Quadruple::InPlaceUnOp(op, ir_loc) => {
                let loc = {
                    let var = ir_loc.var();
                    let reg = description.get_reg_or_allocate(var, live_before, instructions);

                    match ir_loc {
                        ir::Loc::Var(_) => {
                            description.variable_mutated(var, reg);
                            Loc::Reg(reg)
                        }
                        ir::Loc::Mem(ir::Mem { offset, .. }) => {
                            instructions.push(RaInstr::MovToReg(
                                reg,
                                Val::Mem(description.get_variable_mem(var)),
                            ));
                            Loc::Mem(RaMem::Heap {
                                base: reg,
                                displacement: *offset as isize,
                            })
                        }
                    }
                };
                match op {
                    ir::InPlaceUnOpType::Inc => instructions.push(RaInstr::Inc(loc)),
                    ir::InPlaceUnOpType::Dec => instructions.push(RaInstr::Dec(loc)),
                }
            }

            Quadruple::VstStore(class_idx, mem) => {
                todo!();
                instructions.push(RaInstr::LeaLabel(R8, cfg.classes[class_idx.0].vst_name()));
                instructions.push(RaInstr::MovToReg(
                    R9,
                    Val::Mem(description.get_variable_mem(mem.base)),
                ));
                instructions.push(RaInstr::MovToMem(
                    RaMem::Heap {
                        base: R9,
                        displacement: 0,
                    },
                    R8,
                ));
            }

            Quadruple::VirtualCall(dst, object, method_idx, args) => {
                description.save_caller_save_regs(live_before, instructions);

                // Params in registers
                for (reg, arg) in params_registers().zip(args.iter().copied()) {
                    instructions.push(RaInstr::MovToReg(reg, description.get_val(arg)));
                }

                let params_on_the_stack_num = usize::max(
                    (args.len() as isize - params_registers().count() as isize) as usize,
                    0,
                );

                let stack_alignment_growth = if params_on_the_stack_num % 2 != 0 {
                    QUADWORD_SIZE
                } else {
                    0
                }; // stack alignment

                if stack_alignment_growth != 0 {
                    instructions.push(Instr::AdvanceRSPForCall(stack_alignment_growth))
                    // state
                    //     .advance_rsp(stack_alignment_growth as isize);
                }

                // Params on the stack
                for arg in args.iter().copied().skip(params_registers().count()).rev() {
                    instructions.push(RaInstr::Push(description.get_val(arg)));
                    instructions.push(RaInstr::AdjustRSPForStackParam);
                    // state.rsp_displacement += QUADWORD_SIZE as isize;
                }

                // Get the object
                instructions.push(RaInstr::MovToReg(
                    RAX,
                    Val::Mem(description.get_variable_mem(*object)),
                ));

                // Get VST ptr from the object
                instructions.push(RaInstr::MovToReg(
                    RAX,
                    Val::Mem(RaMem::Heap {
                        base: RAX,
                        displacement: 0,
                    }),
                ));

                // Virtual call: get method address from VST
                instructions.push(RaInstr::MovToReg(
                    RAX,
                    Val::Mem(RaMem::Heap {
                        base: RAX,
                        displacement: (*method_idx * QUADWORD_SIZE) as isize,
                    }),
                ));

                instructions.push(RaInstr::CallReg(RAX));

                if stack_alignment_growth != 0 || params_on_the_stack_num > 0 {
                    instructions.push(RaInstr::ResetRSP);
                }

                // Save return value
                description.variable_mutated(*dst, RAX);
            }
        };
    }
}
