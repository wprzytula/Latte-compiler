use hashbrown::{HashMap, HashSet};
use log::{debug, info, trace};
use std::fmt;
use std::ops::Deref;
use vector_map::set::VecSet;
use vector_map::VecMap;

use crate::backend::asmgen::ARGS_IN_REGISTERS;
use crate::backend::ir::{self, CallingConvention, EndType, IrFunction};
use crate::frontend::semantic_analysis::ast::Ident;

use super::asmgen::{params_registers, Frame, QUADWORD_SIZE};
use super::ir::liveness::FlowAnalysis;
use super::ir::{
    BasicBlock, BasicBlockIdx, BinOpType, InPlaceUnOpType, Instant, Quadruple, RelOpType,
    StringLiteral, UnOpType, Value, Var, CFG,
};

impl fmt::Debug for Reg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <Self as fmt::Display>::fmt(&self, f)
    }
}

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

impl Loc<RaLevel> {
    fn new_from_ir_loc(
        description: &mut Description,
        instructions: &mut Vec<RaInstr>,
        loc: &ir::Loc,
    ) -> Self {
        let var = loc.var();
        instructions.push(RaInstr::MovToReg(
            RAX,
            Val::Mem(description.get_variable_mem(var)),
        ));

        match loc {
            ir::Loc::Var(_) => Loc::Reg(RAX),
            ir::Loc::Mem(ir::Mem { offset, .. }) => Loc::Mem(RaMem::Heap {
                base: RAX,
                displacement: *offset as isize,
            }),
        }
    }
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

    Test(Reg),
    Cmp(Reg, Val<I>),
    Add(Reg, Val<I>),
    Sub(Reg, Val<I>),
    IMul(Reg, Val<I>),
    IDivReg(Reg),    // RDX:RAX divided by Reg, quotient in RAX, remainder in RDX.
    IDivMem(I::Mem), // RDX:RAX divided by Mem, quotient in RAX, remainder in RDX.
    Cqo,             // Prepared RDX as empty for IDiv
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
    caller_save_regs: VecMap<Reg, CallerSaveRegState>,
    callee_save_regs: VecMap<Reg, CalleeSaveRegState>,

    next_frame_offset: isize,

    var_locs: VecMap<Var, VarLoc>,
    persistent_vars: VecMap<Var, FrameOffset>,
}

impl Description {
    fn new_func(vars: &VecSet<Var>) -> Self {
        Self {
            caller_save_regs: [RAX, RCX, RDX, R8, R9, R10, R11]
                .into_iter()
                .map(|reg| (reg, CallerSaveRegState::Free))
                .collect(),
            callee_save_regs: [RBX, RBP, R12, R13, R14, R15]
                .into_iter()
                .map(|reg| (reg, CalleeSaveRegState::CallerTaken))
                .collect(),

            next_frame_offset: 0,
            var_locs: vars
                .iter()
                .copied()
                .map(|var| (var, VarLoc::empty()))
                .collect(),
            persistent_vars: VecMap::new(),
        }
    }

    fn allocate_free_caller_save_reg(&mut self, var: Var, live_atm: &VecSet<Var>) -> Option<Reg> {
        self.caller_save_regs
            .iter_mut()
            .find_map(|(reg, state)| match state {
                CallerSaveRegState::Free => {
                    *state = CallerSaveRegState::Taken(var);
                    Some(*reg)
                }
                CallerSaveRegState::Taken(prev_var) if live_atm.contains(prev_var) => None,
                CallerSaveRegState::Taken(prev_var) => {
                    self.var_locs.get_mut(prev_var).unwrap().regs.remove(reg);
                    *prev_var = var;
                    Some(*reg)
                }
            })
    }

    fn local_variables_count(&self) -> usize {
        self.persistent_vars.len()
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

    fn put_in_memory(&mut self, var: Var, loc: FrameOffset) {
        self.var_locs.get_mut(&var).unwrap().mem.insert(loc);
    }

    fn get_variable_mem(&mut self, var: Var) -> RaMem {
        RaMem::Stack {
            frame_offset: self.get_or_register_persistent_var(var),
        }
    }

    fn get_val(&mut self, val: Value) -> Val<RaLevel> {
        match val {
            Value::Instant(i) => Val::Instant(i),
            Value::Variable(var) => Val::Mem(self.get_variable_mem(var)),
        }
    }

    fn get_or_register_persistent_var(&mut self, var: Var) -> FrameOffset {
        if let Some(offset) = self.persistent_vars.get(&var) {
            *offset
        } else {
            let offset = FrameOffset(self.next_frame_offset);
            self.next_frame_offset += 1;
            self.persistent_vars.insert(var, offset);
            offset
        }
    }

    fn get_persistent_var(&mut self, var: Var) -> Option<FrameOffset> {
        self.persistent_vars.get(&var).copied()
    }

    fn enter_block(&mut self, cfg: &CFG, block: BasicBlockIdx) {
        let live_in = cfg[block].flow_analysis.live_variables.first().unwrap();
        let live_out = cfg[block].flow_analysis.live_variables.last().unwrap();
        let live_in_out = live_in.union(live_out);
        for var in live_in_out.copied() {
            self.get_or_register_persistent_var(var);
        }
    }

    fn next_quadruple(
        &mut self,
        flow_analysis: &FlowAnalysis,
        quadruple_idx: usize,
        quadruple: &Quadruple,
    ) {
        match quadruple {
            Quadruple::BinOp(dst, op1, bin_op, op2) => {
                let op1_survives = flow_analysis.live_variables[quadruple_idx + 1].contains(op1);
                if !op1_survives {
                    // ADD Loc, op2
                }

                // Instr::MovToReg(
                //     RAX,
                //     Val::Mem(description.get_variable_mem(*op1)),
                // )
                // );;
                // match bin_op {
                //     BinOpType::Add => {
                //         Instr::Add(RAX, description.get_val(*op2)));
                //     }
                //     BinOpType::Sub => {
                //         Instr::Sub(RAX, description.get_val(*op2)));
                //     }
                //     BinOpType::Mul => {
                //         Instr::IMul(RAX, description.get_val(*op2)));
                //     }
                //     BinOpType::Div => {
                //         Instr::Cqo);;
                //         let val = description.get_val(*op2);
                //         match val {
                //             Val::Reg(reg) => Instr::IDivReg(reg));,
                //             Val::Instant(_) => {
                //                 Instr::MovToReg(RCX, val));;
                //                 Instr::IDivReg(RCX));;
                //             }
                //             Val::Mem(mem) => Instr::IDivMem(mem));,
                //         }
                //     }
                //     BinOpType::Mod => {
                //         Instr::Cqo);;
                //         let val = description.get_val(*op2);
                //         match val {
                //             Val::Reg(reg) => Instr::IDivReg(reg));,
                //             Val::Instant(_) => {
                //                 Instr::MovToReg(RCX, val));;
                //                 Instr::IDivReg(RCX));;
                //             }
                //             Val::Mem(mem) => Instr::IDivMem(mem));,
                //         }
                //         // mov remainder from RDX to RAX:
                //         Instr::MovToReg(RAX, Val::Reg(RDX)));;
                //     }
                // };
                // Instr::MovToMem(description.get_variable_mem(*dst), RAX)
                //     );
            }
            _ => (),
            //     Quadruple::UnOp(dst, un_op_type, op) => {
            //         Instr::MovToReg(RAX, description.get_val(*op)));;
            //         match un_op_type {
            //             UnOpType::Not => Instr::Not(RAX));,
            //             UnOpType::Neg => Instr::Neg(Loc::Reg(RAX)));,
            //         }
            //         Instr::MovToMem(description.get_variable_mem(*dst), RAX)
            //             );;
            //     }
            //     Quadruple::Copy(dst, src) => {
            //         Instr::MovToReg(
            //             RAX,
            //             Val::Mem(description.get_variable_mem(*src)),
            //         )
            //         );;
            //         Instr::MovToMem(description.get_variable_mem(*dst), RAX)
            //             );;
            //     }
            //     Quadruple::Set(dst, i) => {
            //         Instr::MovToReg(RAX, Val::Instant(*i)));;
            //         Instr::MovToMem(description.get_variable_mem(*dst), RAX)
            //             );;
            //     }
            //     Quadruple::GetStrLit(dst, str_idx) => {
            //         Instr::LoadString(RAX, *str_idx));;
            //         Instr::MovToMem(description.get_variable_mem(*dst), RAX)
            //             );;
            //     }

            //     Quadruple::Call(dst, func, args) => {
            //         let cfg_function = cfg.functions.get(func).unwrap();
            //         let callee_convention = &cfg_function.convention;
            //         match callee_convention {
            //             CallingConvention::SimpleCdecl => {
            //                 // Params in registers
            //                 for (reg, arg) in params_registers().zip(args.iter().copied()) {
            //                     Instr::MovToReg(reg, description.get_val(arg))
            //                         );;
            //                 }

            //                 let params_on_the_stack_num = usize::max(
            //                     (args.len() as isize - params_registers().count() as isize) as usize,
            //                     0,
            //                 );

            //                 let stack_alignment_growth = if params_on_the_stack_num % 2 != 0 {
            //                     QUADWORD_SIZE
            //                 } else {
            //                     0
            //                 }; // stack alignment

            //                 if stack_alignment_growth != 0 {
            //                     state
            //                         .advance_rsp(stack_alignment_growth as isize)
            //                         );;
            //                 }

            //                 // Params on the stack
            //                 for arg in args.iter().copied().skip(params_registers().count()).rev() {
            //                     Instr::Push(description.get_val(arg)));;
            //                     state.rsp_displacement += QUADWORD_SIZE as isize;

            //                     /* Instr::MovToReg(RAX, description.get_val(arg))
            //                         );;

            //                     Instr::MovToMem(
            //                         Mem {
            //                             word_len: WordLen::Qword,
            //                             base: RSP,
            //                             index: None,
            //                             displacement: Some(
            //                                 -((QUADWORD_SIZE + no * QUADWORD_SIZE) as isize),
            //                             ),
            //                         },
            //                         RAX,
            //                     )
            //                     );; */
            //                 }

            //                 // // place arguments in corresponding callee's variables
            //                 // for (arg, param) in args.iter().copied().zip(callee_params.iter().copied())
            //                 // {
            //                 //     Instr::MovToReg(RAX, description.get_val(arg))
            //                 //         );;
            //                 //     Instr::MovToMem(
            //                 //         callee_description.get_variable_mem(param, -(RETADDR_SIZE as isize)),
            //                 //         RAX,
            //                 //     )
            //                 //     );;
            //                 // }

            //                 Instr::Call(Label::Named(func.clone())));;

            //                 if stack_alignment_growth != 0 || params_on_the_stack_num > 0 {
            //                     state.reset_rsp());;
            //                 }

            //                 // Save return value
            //                 Instr::MovToMem(description.get_variable_mem(*dst), RAX)
            //                     );;
            //             }
            //             CallingConvention::CdeclFFI => {
            //                 // place arguments in corresponding registers
            //                 for (arg, reg) in args.iter().copied().zip(params_registers()) {
            //                     Instr::MovToReg(reg, description.get_val(arg))
            //                         );;
            //                 }

            //                 // stack alignment, as no params on the stack
            //                 // Instr::Sub(RSP, Val::Instant(Instant(8))));;

            //                 // // sanity alignment check
            //                 // // before call, the stack must be aligned to 0 mod 16
            //                 // Instr::MovToReg(RAX, Val::Reg(RSP)));;
            //                 // Instr::Cqo);;
            //                 // Instr::MovToReg(RCX, Val::Instant(Instant(16))));;
            //                 // Instr::IDivReg(RCX));;
            //                 // // mov remainder from RDX to RAX:
            //                 // Instr::Test(RDX));;
            //                 // Instr::Jnz(Label::Func(Ident::from("_notaligned"))));;

            //                 Instr::Call(Label::Named(func.clone())));;

            //                 // Instr::Add(RSP, Val::Instant(Instant(8))));;

            //                 // Save return value
            //                 Instr::MovToMem(description.get_variable_mem(*dst), RAX)
            //                     );;
            //             }
            //         }
            //     }

            //     Quadruple::ArrLoad(_, _, _) => todo!(),
            //     Quadruple::ArrStore(_, _, _) => todo!(),

            //     Quadruple::DerefLoad(dst, ptr) => {
            //         Instr::MovToReg(
            //             RAX,
            //             Val::Mem(description.get_variable_mem(ptr.base)),
            //         )
            //         );;
            //         Instr::MovToReg(
            //             RAX,
            //             Val::Mem(Mem {
            //                 word_len: WordLen::Qword,
            //                 base: RAX,
            //                 index: None,
            //                 displacement: Some(ptr.offset as isize),
            //             }),
            //         )
            //         );;
            //         Instr::MovToMem(description.get_variable_mem(*dst), RAX)
            //             );;
            //     }

            //     Quadruple::DerefStore(src, ptr) => {
            //         // src in RAX, ptr in RDX
            //         Instr::MovToReg(RAX, description.get_val(*src)));;
            //         Instr::MovToReg(
            //             RDX,
            //             Val::Mem(description.get_variable_mem(ptr.base)),
            //         )
            //         );;
            //         let ptr_mem = Mem {
            //             word_len: WordLen::Qword,
            //             base: RDX,
            //             index: None,
            //             displacement: Some(ptr.offset as isize),
            //         };
            //         Instr::MovToMem(ptr_mem, RAX));;
            //     }

            //     Quadruple::InPlaceUnOp(op, ir_loc) => {
            //         let loc = Loc::new_from_ir_loc(out, frame, state, ir_loc)?;
            //         match op {
            //             InPlaceUnOpType::Inc => Instr::Inc(loc));,
            //             InPlaceUnOpType::Dec => Instr::Dec(loc));,
            //         }
            //         if let Loc::Var(var) = ir_loc {
            //             Instr::MovToMem(description.get_variable_mem(*var), RAX)
            //                 );;
            //         }
            //     }

            //     Quadruple::VstStore(class_idx, mem) => {
            //         Instr::LeaLabel(R8, cfg.classes[class_idx.0].vst_name()));;
            //         Instr::MovToReg(
            //             R9,
            //             Val::Mem(description.get_variable_mem(mem.base)),
            //         )
            //         );;
            //         Instr::MovToMem(
            //             Mem {
            //                 word_len: WordLen::Qword,
            //                 base: R9,
            //                 index: None,
            //                 displacement: None,
            //             },
            //             R8,
            //         )
            //         );;
            //     }

            //     Quadruple::VirtualCall(dst, object, method_idx, args) => {
            //         // Params in registers
            //         for (reg, arg) in params_registers().zip(args.iter().copied()) {
            //             Instr::MovToReg(reg, description.get_val(arg)));;
            //         }

            //         let params_on_the_stack_num = usize::max(
            //             (args.len() as isize - params_registers().count() as isize) as usize,
            //             0,
            //         );

            //         let stack_alignment_growth = if params_on_the_stack_num % 2 != 0 {
            //             QUADWORD_SIZE
            //         } else {
            //             0
            //         }; // stack alignment

            //         if stack_alignment_growth != 0 {
            //             state
            //                 .advance_rsp(stack_alignment_growth as isize)
            //                 );;
            //         }

            //         // Params on the stack
            //         for arg in args.iter().copied().skip(params_registers().count()).rev() {
            //             Instr::Push(description.get_val(arg)));;
            //             state.rsp_displacement += QUADWORD_SIZE as isize;
            //     }

            //         // Get the object
            //         Instr::MovToReg(
            //             RAX,
            //             Val::Mem(description.get_variable_mem(*object)),
            //         )
            //         );;

            //         // Get VST ptr from the object
            //         Instr::MovToReg(
            //             RAX,
            //             Val::Mem(Mem {
            //                 word_len: WordLen::Qword,
            //                 base: RAX,
            //                 index: None,
            //                 displacement: None,
            //             }),
            //         )
            //         );;

            //         // Virtual call: get method address from VST
            //         Instr::MovToReg(
            //             RAX,
            //             Val::Mem(Mem {
            //                 word_len: WordLen::Qword,
            //                 base: RAX,
            //                 index: None,
            //                 displacement: Some((*method_idx * QUADWORD_SIZE) as isize),
            //             }),
            //         )
            //         );;

            //         Instr::CallReg(RAX));;

            //         if stack_alignment_growth != 0 || params_on_the_stack_num > 0 {
            //             state.reset_rsp());;
            //         }

            //         // Save return value
            //         Instr::MovToMem(description.get_variable_mem(*dst), RAX)
            //             );;
        }
        // };
        // Ok(())
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

        debug!("Built frames: {:#?}", &frames);

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
        self[func_block].instructions(self, description, instructions, state);

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
                        /* let loc = description.get_any_var_loc_preferring_regs(*var, Some(RAX));
                        if matches!(loc, Loc::Reg(RAX)) {
                            // nothing to do
                        } else {
                            instructions.push(RaInstr::MovToReg(RAX, loc.into()))
                        } */
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
                instructions.push(RaInstr::MovToReg(
                    RAX,
                    Val::Mem(description.get_variable_mem(*a)),
                ));
                instructions.push(RaInstr::Cmp(RAX, description.get_val(*b)));

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
        description: &mut Description,
        instructions: &mut Vec<RaInstr>,
        state: &mut RaGenState,
    ) {
        for quadruple in self.quadruples.iter() {
            quadruple.instructions(cfg, description, instructions, state);
        }
    }
}

impl Quadruple {
    fn instructions(
        &self,
        cfg: &CFG,
        description: &mut Description,
        instructions: &mut Vec<RaInstr>,
        state: &mut RaGenState,
    ) {
        match self {
            Quadruple::BinOp(dst, op1, bin_op, op2) => {
                instructions.push(RaInstr::MovToReg(
                    RAX,
                    Val::Mem(description.get_variable_mem(*op1)),
                ));
                match bin_op {
                    BinOpType::Add => {
                        instructions.push(RaInstr::Add(RAX, description.get_val(*op2)));
                    }
                    BinOpType::Sub => {
                        instructions.push(RaInstr::Sub(RAX, description.get_val(*op2)));
                    }
                    BinOpType::Mul => {
                        instructions.push(RaInstr::IMul(RAX, description.get_val(*op2)));
                    }
                    BinOpType::Div => {
                        instructions.push(RaInstr::Cqo);
                        let val = description.get_val(*op2);
                        match val {
                            Val::Reg(reg) => instructions.push(RaInstr::IDivReg(reg)),
                            Val::Instant(_) => {
                                instructions.push(RaInstr::MovToReg(RCX, val));
                                instructions.push(RaInstr::IDivReg(RCX));
                            }
                            Val::Mem(mem) => instructions.push(RaInstr::IDivMem(mem)),
                        }
                    }
                    BinOpType::Mod => {
                        instructions.push(RaInstr::Cqo);
                        let val = description.get_val(*op2);
                        match val {
                            Val::Reg(reg) => instructions.push(RaInstr::IDivReg(reg)),
                            Val::Instant(_) => {
                                instructions.push(RaInstr::MovToReg(RCX, val));
                                instructions.push(RaInstr::IDivReg(RCX));
                            }
                            Val::Mem(mem) => instructions.push(RaInstr::IDivMem(mem)),
                        }
                        // mov remainder from RDX to RAX:
                        instructions.push(RaInstr::MovToReg(RAX, Val::Reg(RDX)));
                    }
                };
                instructions.push(RaInstr::MovToMem(description.get_variable_mem(*dst), RAX));
            }
            Quadruple::UnOp(dst, un_op_type, op) => {
                instructions.push(RaInstr::MovToReg(RAX, description.get_val(*op)));
                match un_op_type {
                    UnOpType::Neg => instructions.push(RaInstr::Neg(Loc::Reg(RAX))),
                }
                instructions.push(RaInstr::MovToMem(description.get_variable_mem(*dst), RAX));
            }
            Quadruple::Copy(dst, src) => {
                instructions.push(RaInstr::MovToReg(
                    RAX,
                    Val::Mem(description.get_variable_mem(*src)),
                ));
                instructions.push(RaInstr::MovToMem(description.get_variable_mem(*dst), RAX));
            }
            Quadruple::Set(dst, i) => {
                instructions.push(RaInstr::MovToReg(RAX, Val::Instant(*i)));
                instructions.push(RaInstr::MovToMem(description.get_variable_mem(*dst), RAX));
            }
            Quadruple::GetStrLit(dst, str_idx) => {
                instructions.push(RaInstr::LoadString(RAX, *str_idx));
                instructions.push(RaInstr::MovToMem(description.get_variable_mem(*dst), RAX));
            }

            Quadruple::Call(dst, func, args) => {
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
                        instructions
                            .push(RaInstr::MovToMem(description.get_variable_mem(*dst), RAX));
                    }
                    CallingConvention::CdeclFFI => {
                        // place arguments in corresponding registers
                        for (arg, reg) in args.iter().copied().zip(params_registers()) {
                            instructions.push(RaInstr::MovToReg(reg, description.get_val(arg)));
                        }

                        instructions.push(RaInstr::Call(Label::Named(func.clone())));

                        // Save return value
                        instructions
                            .push(RaInstr::MovToMem(description.get_variable_mem(*dst), RAX));
                    }
                }
            }

            Quadruple::ArrLoad(_, _, _) => todo!(),
            Quadruple::ArrStore(_, _, _) => todo!(),

            Quadruple::DerefLoad(dst, ptr) => {
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
                let loc = Loc::new_from_ir_loc(description, instructions, ir_loc);
                match op {
                    ir::InPlaceUnOpType::Inc => instructions.push(RaInstr::Inc(loc)),
                    ir::InPlaceUnOpType::Dec => instructions.push(RaInstr::Dec(loc)),
                }
                if let ir::Loc::Var(var) = ir_loc {
                    instructions.push(RaInstr::MovToMem(description.get_variable_mem(*var), RAX));
                }
            }

            Quadruple::VstStore(class_idx, mem) => {
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
                instructions.push(RaInstr::MovToMem(description.get_variable_mem(*dst), RAX));
            }
        };
    }
}
