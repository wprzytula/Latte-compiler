use std::{
    fmt::Display,
    io::{self, Write},
};

use hashbrown::HashMap;

use log::{debug, info, trace};

use crate::{
    backend::ra::RSP,
    frontend::semantic_analysis::{ast::Ident, INITIAL_FUNCS},
};

use super::{
    ir::{self, Class, Instant, Ir, Method, StringLiteral, CFG, CONCAT_STRINGS_FUNC, NEW_FUNC},
    ra::{
        CalleeSaveReg, CallerSaveReg, FrameOffset, Instr, InstrLevel, Label, Loc, RaLevel, RaMem,
        Reg, Val, R8, R9, RCX, RDI, RDX, RSI,
    },
};

pub const ARGS_IN_REGISTERS: usize = 6;
pub const QUADWORD_SIZE: usize = 8; // in bytes
const RETADDR_SIZE: usize = QUADWORD_SIZE;

pub(crate) fn params_registers() -> impl Iterator<Item = Reg> {
    [RDI, RSI, RDX, RCX, R8, R9].into_iter()
}

type AsmGenResult = io::Result<()>;

type AsmInstr = Instr<AsmLevel>;

struct AsmLevel;
impl InstrLevel for AsmLevel {
    type Mem = Mem;
}

impl RaMem {
    fn into_asm_level(&self, frame: &Frame, rsp_displacement: isize) -> Mem {
        match *self {
            RaMem::Stack {
                frame_offset: offset,
            } => Mem {
                word_len: WordLen::Qword,
                base: RSP,
                index: None,
                displacement: {
                    let displacement =
                        frame.get_variable_offset_relative_to_rsp(offset, rsp_displacement);
                    if displacement != 0 {
                        Some(displacement)
                    } else {
                        None
                    }
                },
            },
            RaMem::Heap { base, displacement } => Mem {
                word_len: WordLen::Qword,
                base,
                index: None,
                displacement: if displacement != 0 {
                    Some(displacement)
                } else {
                    None
                },
            },
        }
    }
}

impl Instr<AsmLevel> {
    // fn from_ra(: Vec<(Ident, Vec<Instr<RaLevel>>)>) -> Vec<Self> {
    //     todo!()
    // }
}

impl Display for Loc<AsmLevel> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Loc::Reg(reg) => write!(f, "{}", reg),
            Loc::Mem(mem) => write!(f, "{}", mem),
        }
    }
}

impl Loc<RaLevel> {
    fn into_asm_level(self, frame: &Frame, rsp_displacement: isize) -> Loc<AsmLevel> {
        // AsmInstr::MovToReg(
        //     RAX,
        //     Val::Mem(frame.get_variable_mem(var, state.rsp_displacement)),
        // )
        // .emit(out)?;

        match self {
            Loc::Reg(r) => Loc::Reg(r),
            Loc::Mem(mem) => Loc::Mem(mem.into_asm_level(frame, rsp_displacement)),
        }
    }
}

impl Display for Val<AsmLevel> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Val::Reg(reg) => write!(f, "{}", reg),
            Val::Instant(i) => write!(f, "{}", **i),
            Val::Mem(mem) => write!(f, "{}", mem),
        }
    }
}

enum MemScale {
    One,
    Two,
    Four,
    Eight,
}
impl Display for MemScale {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                MemScale::One => 1,
                MemScale::Two => 2,
                MemScale::Four => 4,
                MemScale::Eight => 8,
            }
        )
    }
}
struct MemIndex {
    index: Reg,
    scale: MemScale,
}
impl Display for MemIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}*{}", self.scale, self.index)
    }
}

enum WordLen {
    Byte,
    Qword,
}
impl Display for WordLen {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            WordLen::Byte => "BYTE",
            WordLen::Qword => "QWORD",
        })
    }
}
struct Mem {
    word_len: WordLen,
    base: Reg,
    index: Option<MemIndex>,
    displacement: Option<isize>,
}
impl Display for Mem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} [{}", self.word_len, self.base)?;
        if let Some(ref mem_index) = self.index {
            write!(f, " + {}", mem_index)?;
        }
        if let Some(displacement) = self.displacement {
            let abs = displacement.abs() as usize;
            let sg = if displacement < 0 { '-' } else { '+' };
            write!(f, " {} {}", sg, abs)?;
        }
        write!(f, "]")
    }
}

impl Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Reg::CallerSave(reg) => write!(f, "{}", reg),
            Reg::CalleeSave(reg) => write!(f, "{}", reg),
        }
    }
}

impl Display for CallerSaveReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CallerSaveReg::Rax => write!(f, "rax"),
            CallerSaveReg::Rcx => write!(f, "rcx"),
            CallerSaveReg::Rdx => write!(f, "rdx"),
            CallerSaveReg::Rsi => write!(f, "rsi"),
            CallerSaveReg::Rdi => write!(f, "rdi"),
            CallerSaveReg::R8 => write!(f, "r8"),
            CallerSaveReg::R9 => write!(f, "r9"),
            CallerSaveReg::R10 => write!(f, "r10"),
            CallerSaveReg::R11 => write!(f, "r11"),
        }
    }
}

impl Display for CalleeSaveReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CalleeSaveReg::Rbx => write!(f, "rbx"),
            CalleeSaveReg::Rsp => write!(f, "rsp"),
            CalleeSaveReg::Rbp => write!(f, "rbp"),
            CalleeSaveReg::R12 => write!(f, "r12"),
            CalleeSaveReg::R13 => write!(f, "r13"),
            CalleeSaveReg::R14 => write!(f, "r14"),
            CalleeSaveReg::R15 => write!(f, "r15"),
        }
    }
}

/// Contains variable's offset relative to frame, in bytes
#[derive(Debug, Clone, Copy)]
pub struct Var(isize);

impl Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Label::Num(n) => write!(f, ".__block_{}", n),
            Label::Named(id) => f.write_str(id),
            Label::Str(n) => write!(f, "__string_{}", n.0),
        }
    }
}
impl Label {
    fn emit(&self, out: &mut impl Write) -> AsmGenResult {
        writeln!(out, "{}:", self)
    }
}

struct AsmGenState {
    rsp_displacement: isize, // where is end of the frame relatively to current RSP
    calltime_rsp_position: isize, // rsp displacement at the moment of function call
}

impl AsmGenState {
    fn new() -> Self {
        Self {
            rsp_displacement: 0,
            calltime_rsp_position: 0,
        }
    }

    fn enter_frame(&mut self, frame_size: usize) {
        let calltime_rsp_position = -((frame_size - RETADDR_SIZE) as isize);
        self.calltime_rsp_position = calltime_rsp_position;
        self.rsp_displacement = calltime_rsp_position;
        trace!(
            "Entering frame, so RSP gets displacement {}",
            -calltime_rsp_position
        );
    }

    fn advance_rsp(&mut self, displacement: isize) -> AsmInstr {
        trace!("Advancing RSP with displacement {}", -displacement);
        self.rsp_displacement += displacement;
        AsmInstr::Sub(RSP, Val::Instant(Instant(displacement as i64)))
    }

    fn reset_rsp(&mut self) -> AsmInstr {
        let rsp_displacement = self.rsp_displacement;
        trace!("Resetting RSP with displacement {}", rsp_displacement);
        self.rsp_displacement = 0;
        AsmInstr::Add(RSP, Val::Instant(Instant(rsp_displacement as i64)))
    }

    fn leave(&mut self) -> AsmInstr {
        assert_eq!(self.rsp_displacement, 0);
        // self.rsp_displacement = self.calltime_rsp_position; // this should not happen not to affect other code paths
        AsmInstr::Sub(
            RSP,
            Val::Instant(Instant(self.calltime_rsp_position as i64)),
        )
    }
}

impl AsmInstr {
    fn emit(&self, out: &mut impl Write) -> AsmGenResult {
        if let AsmInstr::Label(l) = self {
            l.emit(out)?;
            return Ok(());
        }
        write!(out, "\t")?;
        match self {
            AsmInstr::Newline => writeln!(out, ""),
            AsmInstr::Empty => Ok(()),
            AsmInstr::AdjustRSPForStackParam => unreachable!(),
            AsmInstr::AdvanceRSPForCall(_) => unreachable!(),
            AsmInstr::ResetRSP => unreachable!(),

            AsmInstr::Label(label) => label.emit(out),
            AsmInstr::Jmp(label) => writeln!(out, "jmp {}", label),
            AsmInstr::Jz(label) => writeln!(out, "jz {}", label),
            AsmInstr::Jnz(label) => writeln!(out, "jnz {}", label),
            AsmInstr::Jg(label) => writeln!(out, "jg {}", label),
            AsmInstr::Jge(label) => writeln!(out, "jge {}", label),
            AsmInstr::Jl(label) => writeln!(out, "jl {}", label),
            AsmInstr::Jle(label) => writeln!(out, "jle {}", label),

            AsmInstr::MovToReg(reg, val) => writeln!(out, "mov {}, {}", reg, val),
            AsmInstr::MovToMem(mem, reg) => writeln!(out, "mov {}, {}", mem, reg),

            AsmInstr::Test(reg) => writeln!(out, "test {}, {}", reg, reg),
            AsmInstr::Cmp(reg, val) => writeln!(out, "cmp {}, {}", reg, val),
            AsmInstr::Add(reg, val) => writeln!(out, "add {}, {}", reg, val),
            AsmInstr::Sub(reg, val) => writeln!(out, "sub {}, {}", reg, val),
            AsmInstr::IMul(reg, val) => writeln!(out, "imul {}, {}", reg, val),
            AsmInstr::IDivReg(reg) => writeln!(out, "idiv {}", reg),
            AsmInstr::IDivMem(mem) => writeln!(out, "idiv {}", mem),
            AsmInstr::Cqo => writeln!(out, "cqo"),
            AsmInstr::Inc(loc) => writeln!(out, "inc {}", loc),
            AsmInstr::Dec(loc) => writeln!(out, "dec {}", loc),
            AsmInstr::Neg(loc) => writeln!(out, "neg {}", loc),
            AsmInstr::Not(loc) => writeln!(out, "not {}", loc),
            AsmInstr::Sal(reg, i) => writeln!(out, "sal {}, {}", reg, **i),
            AsmInstr::Sar(reg, i) => writeln!(out, "sar {}, {}", reg, **i),

            AsmInstr::Lea(reg, mem) => writeln!(out, "lea {}, {}", reg, mem),
            AsmInstr::LeaLabel(reg, label) => writeln!(out, "lea {}, [rel {}]", reg, label),

            AsmInstr::Push(val) => writeln!(out, "push {}", val),

            AsmInstr::Ret => writeln!(out, "ret"),
            AsmInstr::Call(label) => writeln!(out, "call {}", label),
            AsmInstr::CallReg(reg) => writeln!(out, "call {}", reg),

            AsmInstr::LoadString(reg, idx) => {
                writeln!(out, "lea {}, [rel {}]", reg, Label::Str(*idx))
            }
        }
    }
}

/**
 * Calling convention - x86_64
 * base_retaddr [base_frame end] | base_var1 | ... | base_varn | [child_frame begin] retaddr | var1 | var2 | ... | varn [child_frame_end]
 * upon call, the caller simply calls (pushes retaddr and jumps to the callee's label).
 * the callee makes place for all variables, and cleans them up afterwards.
 * after call, the caller has no clean up to do.
 *
 * frame_size: in bytes, including return address, variables, and stack alignment padding (Constant, stored in Frame)
 * rsp_displacement: in bytes, signifies current rsp movement relative to current function's return address (Mutable, stored in State)
 */

#[derive(Debug)]
pub(crate) struct Frame {
    pub(crate) name: Ident,
    pub(crate) local_variables_count: usize,
    // variables_mapping: VecMap<ir::Var, Var>, // var -> offset wrt RBP (frame)
    pub(crate) frame_size: usize, // in bytes, must be divisible by 16
}

impl Frame {
    pub(crate) fn new(
        name: Ident,
        _stack_parameters_count: usize,
        local_variables_count: usize,
        // variables: HashSet<ir::Var>,
    ) -> Self {
        debug!("Creating frame for {} variables", local_variables_count);
        // let variables_mapping = variables
        //     .into_iter()
        //     .enumerate()
        //     .map(|(k, var)| (var, Var(k as isize * -8)))
        //     .collect::<VecMap<_, _>>();
        // trace!("Variables got mappings: {:?}", &variables_mapping);

        Self {
            name,
            local_variables_count,
            frame_size: ((local_variables_count + 1/*retaddr*/) * QUADWORD_SIZE + QUADWORD_SIZE)
                / 16
                * 16, /*stack alignment*/
        }
    }

    pub(crate) fn new_ffi(name: Ident) -> Self {
        Self {
            name,
            local_variables_count: 0,
            frame_size: RETADDR_SIZE,
        }
    }

    fn get_variable_offset_relative_to_frame_end(&self, offset: FrameOffset) -> isize {
        //                  by frame   by rsp
        // RET_ADDR         - +8    -  +24
        // a                -  0    -  +16
        // b                - -8    -  +8
        // (alignment)      - -16   -   0
        // CALLEE_RET_ADDR  - -24   -  -8
        // frame_size = 32
        // b <- frame + -8

        //                  by frame   by rsp
        // RET_ADDR         - +8    -  +40
        // a                -  0    -  +32
        // b                - -8    -  +24
        // c                - -16   -  +16
        // d                - -24   -  +8
        // (alignment)      - -32   -   0
        // CALLEE_RET_ADDR  - -40   -  -8
        // frame_size = 48
        // b <- frame + -8
        let res = -(isize::from(offset) * QUADWORD_SIZE as isize) + self.frame_size as isize
            - RETADDR_SIZE as isize
            - QUADWORD_SIZE as isize;
        // trace!("Queried {:?} retaddr offset, got {}.", variable, res);
        res
    }

    fn get_variable_offset_relative_to_rsp(
        &self,
        offset: FrameOffset,
        rsp_displacement: isize,
    ) -> isize {
        let res = self.get_variable_offset_relative_to_frame_end(offset) + rsp_displacement;
        // trace!(
        //     "Queried {:?} offset relative to rsp, got {}.",
        //     variable, res
        // );
        res
    }

    fn get_variable_mem(&self, offset: FrameOffset, rsp_displacement: isize) -> Mem {
        Mem {
            word_len: WordLen::Qword,
            base: RSP,
            index: None,
            displacement: Some(self.get_variable_offset_relative_to_rsp(offset, rsp_displacement)),
        }
    }

    fn get_val(&self, val: Val<RaLevel>, rsp_displacement: isize) -> Val<AsmLevel> {
        match val {
            Val::Instant(i) => Val::Instant(i),
            Val::Reg(reg) => Val::Reg(reg),
            Val::Mem(ra_mem) => Val::Mem(ra_mem.into_asm_level(self, rsp_displacement)),
        }
    }
}

impl Ir {
    pub fn emit_assembly(&self, out: &mut impl Write) -> AsmGenResult {
        info!("---- COMPILING TO ASSEMBLY INSTRUCTIONS ----");
        let (instructions, frames) = self.cfg.asm_instructions();
        // trace!("Assembly immediate code: {:#?}", &instructions);

        info!("---- ASSEMBLY EMITTING ----");
        self.cfg
            .emit_assembly(out, &self.string_literals, instructions, frames)
    }
}

impl CFG {
    pub fn emit_assembly(
        &self,
        out: &mut impl Write,
        string_literals: &Vec<String>,
        instructions: Vec<(Ident, Option<Vec<Instr<RaLevel>>>)>,
        frames: HashMap<Ident, Frame>,
    ) -> AsmGenResult {
        writeln!(out, "section .data")?;
        emit_string_literals(out, string_literals)?;

        let mut state = AsmGenState::new();

        writeln!(out, "\nsection .text")?;
        emit_vsts(out, &self.classes)?;

        emit_header(out)?;

        for (func, instructions) in instructions
            .into_iter()
            .filter_map(|(func, maybe_instrs)| maybe_instrs.map(|instrs| (func, instrs)))
        {
            info!("Emitting asm function: {}", func);
            writeln!(out, "")?;
            Label::Named(func.clone()).emit(out)?;
            let frame = frames.get(&func).unwrap();

            state.enter_frame(frame.frame_size);
            let stack_growth = frame.frame_size - RETADDR_SIZE;
            if stack_growth != 0 {
                state.reset_rsp().emit(out)?;
            }

            for instr in instructions {
                let instr = instr.convert(frame, &mut state);
                if matches!(instr, Instr::Ret) {
                    let stack_growth = frame.frame_size - RETADDR_SIZE;
                    if stack_growth != 0 {
                        state.leave().emit(out)?;
                    }
                }
                instr.emit(out)?;
            }
        }
        Ok(())
    }

    /* fn emit_function_block(
        &self,
        out: &mut impl Write,
        func_block: BasicBlockIdx,
        emitted: &mut HashSet<BasicBlockIdx>,
        frames: &HashMap<Ident, Frame>,
        frame: &Frame,
        state: &mut AsmGenState,
        next_l: Option<&Label>,
    ) -> AsmGenResult {
        if emitted.contains(&func_block) {
            trace!(
                "Block: {:?} has already been emitted; returning.",
                func_block
            );
            return Ok(());
        } else {
            emitted.insert(func_block);
        }

        let block_label = state.get_block_label(func_block);
        block_label.emit(out)?;
        debug!("Emitting ir block: {:?} as {}", func_block, block_label);

        self[func_block].emit(self, out, frame, state)?;

        match &self[func_block].end_type {
            Some(EndType::Return(None)) | None => {
                let stack_growth = frame.frame_size - RETADDR_SIZE;
                if stack_growth != 0 {
                    state.leave().emit(out)?;
                }
                AsmInstr::Ret.emit(out)?;
            }
            Some(EndType::Return(Some(val))) => {
                match val {
                    ir::Value::Instant(i) => AsmInstr::MovToReg(RAX, Val::Instant(*i)).emit(out)?,
                    ir::Value::Variable(var) => AsmInstr::MovToReg(
                        RAX,
                        Val::Mem(frame.get_variable_mem(*var, state.rsp_displacement)),
                    )
                    .emit(out)?,
                }
                let stack_growth = frame.frame_size - RETADDR_SIZE;
                if stack_growth != 0 {
                    state.leave().emit(out)?;
                }
                AsmInstr::Ret.emit(out)?;
            }
            Some(EndType::Goto(block_idx)) => {
                // If not emitted yet, simply emit it below and save a jump
                if emitted.contains(block_idx) && next_l != Some(&state.get_block_label(*block_idx))
                {
                    AsmInstr::Jmp(state.get_block_label(*block_idx).clone()).emit(out)?;
                }
                self.emit_function_block(out, *block_idx, emitted, frames, frame, state, next_l)?;
            }
            Some(EndType::IfElse(_, _, _, then_block, else_block)) if then_block == else_block => {
                let block_idx = then_block;
                // Reduce to Goto
                // If not emitted yet, simply emit it below and save a jump
                if emitted.contains(block_idx) && next_l != Some(&state.get_block_label(*block_idx))
                {
                    AsmInstr::Jmp(state.get_block_label(*block_idx).clone()).emit(out)?;
                }
                self.emit_function_block(out, *block_idx, emitted, frames, frame, state, next_l)?;
            }
            Some(EndType::IfElse(a, rel, b, then_block, else_block)) => {
                let then_l = state.get_block_label(*then_block).clone();
                let else_l = state.get_block_label(*else_block).clone();

                // Cond
                AsmInstr::MovToReg(
                    RAX,
                    Val::Mem(frame.get_variable_mem(*a, state.rsp_displacement)),
                )
                .emit(out)?;
                AsmInstr::Cmp(RAX, frame.get_val(*b, state.rsp_displacement)).emit(out)?;

                let (then_instr, else_instr) = rel.instrs();

                if next_l == Some(&then_l) && emitted.contains(else_block) {
                    else_instr(else_l.clone()).emit(out)?;
                } else if next_l == Some(&else_l) && emitted.contains(then_block) {
                    then_instr(then_l.clone()).emit(out)?;
                } else {
                    else_instr(else_l.clone()).emit(out)?;
                    if emitted.contains(then_block) {
                        AsmInstr::Jmp(then_l).emit(out)?;
                    }
                    // Then
                    self.emit_function_block(
                        out,
                        *then_block,
                        emitted,
                        frames,
                        frame,
                        state,
                        Some(&else_l),
                    )?;

                    // Else
                    self.emit_function_block(
                        out,
                        *else_block,
                        emitted,
                        frames,
                        frame,
                        state,
                        next_l,
                    )?;
                }
            }
        }
        Ok(())
    } */
}

/* impl BasicBlock {
    fn emit(
        &self,
        cfg: &CFG,
        out: &mut impl Write,
        frame: &Frame,
        state: &mut AsmGenState,
    ) -> AsmGenResult {
        for quadruple in self.quadruples.iter() {
            quadruple.emit(cfg, out, frame, state)?;
        }

        Ok(())
    }
} */

/* impl Quadruple {
    fn emit(
        &self,
        cfg: &CFG,
        out: &mut impl Write,
        frame: &Frame,
        state: &mut AsmGenState,
    ) -> AsmGenResult {
        match self {
            Quadruple::BinOp(dst, op1, bin_op, op2) => {
                AsmInstr::MovToReg(
                    RAX,
                    Val::Mem(frame.get_variable_mem(*op1, state.rsp_displacement)),
                )
                .emit(out)?;
                match bin_op {
                    BinOpType::Add => {
                        AsmInstr::Add(RAX, frame.get_val(*op2, state.rsp_displacement)).emit(out)?
                    }
                    BinOpType::Sub => {
                        AsmInstr::Sub(RAX, frame.get_val(*op2, state.rsp_displacement)).emit(out)?
                    }
                    BinOpType::Mul => {
                        AsmInstr::IMul(RAX, frame.get_val(*op2, state.rsp_displacement))
                            .emit(out)?
                    }
                    BinOpType::Div => {
                        AsmInstr::Cqo.emit(out)?;
                        let val = frame.get_val(*op2, state.rsp_displacement);
                        match val {
                            Val::Reg(reg) => AsmInstr::IDivReg(reg).emit(out)?,
                            Val::Instant(_) => {
                                AsmInstr::MovToReg(RCX, val).emit(out)?;
                                AsmInstr::IDivReg(RCX).emit(out)?;
                            }
                            Val::Mem(mem) => AsmInstr::IDivMem(mem).emit(out)?,
                        }
                    }
                    BinOpType::Mod => {
                        AsmInstr::Cqo.emit(out)?;
                        let val = frame.get_val(*op2, state.rsp_displacement);
                        match val {
                            Val::Reg(reg) => AsmInstr::IDivReg(reg).emit(out)?,
                            Val::Instant(_) => {
                                AsmInstr::MovToReg(RCX, val).emit(out)?;
                                AsmInstr::IDivReg(RCX).emit(out)?;
                            }
                            Val::Mem(mem) => AsmInstr::IDivMem(mem).emit(out)?,
                        }
                        // mov remainder from RDX to RAX:
                        AsmInstr::MovToReg(RAX, Val::Reg(RDX)).emit(out)?;
                    }
                };
                AsmInstr::MovToMem(frame.get_variable_mem(*dst, state.rsp_displacement), RAX)
                    .emit(out)?
            }
            Quadruple::UnOp(dst, un_op_type, op) => {
                AsmInstr::MovToReg(RAX, frame.get_val(*op, state.rsp_displacement)).emit(out)?;
                match un_op_type {
                    UnOpType::Neg => AsmInstr::Neg(Loc::Reg(RAX)).emit(out)?,
                }
                AsmInstr::MovToMem(frame.get_variable_mem(*dst, state.rsp_displacement), RAX)
                    .emit(out)?;
            }
            Quadruple::Copy(dst, src) => {
                AsmInstr::MovToReg(
                    RAX,
                    Val::Mem(frame.get_variable_mem(*src, state.rsp_displacement)),
                )
                .emit(out)?;
                AsmInstr::MovToMem(frame.get_variable_mem(*dst, state.rsp_displacement), RAX)
                    .emit(out)?;
            }
            Quadruple::Set(dst, i) => {
                AsmInstr::MovToReg(RAX, Val::Instant(*i)).emit(out)?;
                AsmInstr::MovToMem(frame.get_variable_mem(*dst, state.rsp_displacement), RAX)
                    .emit(out)?;
            }
            Quadruple::GetStrLit(dst, str_idx) => {
                AsmInstr::LoadString(RAX, *str_idx).emit(out)?;
                AsmInstr::MovToMem(frame.get_variable_mem(*dst, state.rsp_displacement), RAX)
                    .emit(out)?;
            }

            Quadruple::Call(dst, func, args) => {
                let cfg_function = cfg.functions.get(func).unwrap();
                let callee_convention = &cfg_function.convention;
                match callee_convention {
                    CallingConvention::SimpleCdecl => {
                        // Params in registers
                        for (reg, arg) in params_registers().zip(args.iter().copied()) {
                            AsmInstr::MovToReg(reg, frame.get_val(arg, state.rsp_displacement))
                                .emit(out)?;
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
                            state
                                .advance_rsp(stack_alignment_growth as isize)
                                .emit(out)?;
                        }

                        // Params on the stack
                        for arg in args.iter().copied().skip(params_registers().count()).rev() {
                            AsmInstr::Push(frame.get_val(arg, state.rsp_displacement)).emit(out)?;
                            state.rsp_displacement += QUADWORD_SIZE as isize;
                        }

                        AsmInstr::Call(Label::Named(func.clone())).emit(out)?;

                        if stack_alignment_growth != 0 || params_on_the_stack_num > 0 {
                            state.reset_rsp().emit(out)?;
                        }

                        // Save return value
                        AsmInstr::MovToMem(
                            frame.get_variable_mem(*dst, state.rsp_displacement),
                            RAX,
                        )
                        .emit(out)?;
                    }
                    CallingConvention::CdeclFFI => {
                        // place arguments in corresponding registers
                        for (arg, reg) in args.iter().copied().zip(params_registers()) {
                            AsmInstr::MovToReg(reg, frame.get_val(arg, state.rsp_displacement))
                                .emit(out)?;
                        }

                        AsmInstr::Call(Label::Named(func.clone())).emit(out)?;

                        // Save return value
                        AsmInstr::MovToMem(
                            frame.get_variable_mem(*dst, state.rsp_displacement),
                            RAX,
                        )
                        .emit(out)?;
                    }
                }
            }

            Quadruple::ArrLoad(_, _, _) => todo!(),
            Quadruple::ArrStore(_, _, _) => todo!(),

            Quadruple::DerefLoad(dst, ptr) => {
                AsmInstr::MovToReg(
                    RAX,
                    Val::Mem(frame.get_variable_mem(ptr.base, state.rsp_displacement)),
                )
                .emit(out)?;
                AsmInstr::MovToReg(
                    RAX,
                    Val::Mem(Mem {
                        word_len: WordLen::Qword,
                        base: RAX,
                        index: None,
                        displacement: Some(ptr.offset as isize),
                    }),
                )
                .emit(out)?;
                AsmInstr::MovToMem(frame.get_variable_mem(*dst, state.rsp_displacement), RAX)
                    .emit(out)?;
            }

            Quadruple::DerefStore(src, ptr) => {
                // src in RAX, ptr in RDX
                AsmInstr::MovToReg(RAX, frame.get_val(*src, state.rsp_displacement)).emit(out)?;
                AsmInstr::MovToReg(
                    RDX,
                    Val::Mem(frame.get_variable_mem(ptr.base, state.rsp_displacement)),
                )
                .emit(out)?;
                let ptr_mem = Mem {
                    word_len: WordLen::Qword,
                    base: RDX,
                    index: None,
                    displacement: Some(ptr.offset as isize),
                };
                AsmInstr::MovToMem(ptr_mem, RAX).emit(out)?;
            }

            Quadruple::InPlaceUnOp(op, ir_loc) => {
                let loc = Loc::new_from_ir_loc(out, frame, state, ir_loc)?;
                match op {
                    ir::InPlaceUnOpType::Inc => AsmInstr::Inc(loc).emit(out)?,
                    ir::InPlaceUnOpType::Dec => AsmInstr::Dec(loc).emit(out)?,
                }
                if let ir::Loc::Var(var) = ir_loc {
                    AsmInstr::MovToMem(frame.get_variable_mem(*var, state.rsp_displacement), RAX)
                        .emit(out)?;
                }
            }

            Quadruple::VstStore(class_idx, mem) => {
                AsmInstr::LeaLabel(R8, cfg.classes[class_idx.0].vst_name()).emit(out)?;
                AsmInstr::MovToReg(
                    R9,
                    Val::Mem(frame.get_variable_mem(mem.base, state.rsp_displacement)),
                )
                .emit(out)?;
                AsmInstr::MovToMem(
                    Mem {
                        word_len: WordLen::Qword,
                        base: R9,
                        index: None,
                        displacement: None,
                    },
                    R8,
                )
                .emit(out)?;
            }

            Quadruple::VirtualCall(dst, object, method_idx, args) => {
                // Params in registers
                for (reg, arg) in params_registers().zip(args.iter().copied()) {
                    AsmInstr::MovToReg(reg, frame.get_val(arg, state.rsp_displacement))
                        .emit(out)?;
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
                    state
                        .advance_rsp(stack_alignment_growth as isize)
                        .emit(out)?;
                }

                // Params on the stack
                for arg in args.iter().copied().skip(params_registers().count()).rev() {
                    AsmInstr::Push(frame.get_val(arg, state.rsp_displacement)).emit(out)?;
                    state.rsp_displacement += QUADWORD_SIZE as isize;
                }

                // Get the object
                AsmInstr::MovToReg(
                    RAX,
                    Val::Mem(frame.get_variable_mem(*object, state.rsp_displacement)),
                )
                .emit(out)?;

                // Get VST ptr from the object
                AsmInstr::MovToReg(
                    RAX,
                    Val::Mem(Mem {
                        word_len: WordLen::Qword,
                        base: RAX,
                        index: None,
                        displacement: None,
                    }),
                )
                .emit(out)?;

                // Virtual call: get method address from VST
                AsmInstr::MovToReg(
                    RAX,
                    Val::Mem(Mem {
                        word_len: WordLen::Qword,
                        base: RAX,
                        index: None,
                        displacement: Some((*method_idx * QUADWORD_SIZE) as isize),
                    }),
                )
                .emit(out)?;

                AsmInstr::CallReg(RAX).emit(out)?;

                if stack_alignment_growth != 0 || params_on_the_stack_num > 0 {
                    state.reset_rsp().emit(out)?;
                }

                // Save return value
                AsmInstr::MovToMem(frame.get_variable_mem(*dst, state.rsp_displacement), RAX)
                    .emit(out)?;
            }
        };
        Ok(())
    }
} */

impl Instr<RaLevel> {
    fn convert(self, frame: &Frame, state: &mut AsmGenState) -> AsmInstr {
        match self {
            Instr::Jmp(l) => Instr::Jmp(l),
            Instr::Jz(l) => Instr::Jz(l),
            Instr::Jnz(l) => Instr::Jnz(l),
            Instr::Jg(l) => Instr::Jg(l),
            Instr::Jge(l) => Instr::Jge(l),
            Instr::Jl(l) => Instr::Jl(l),
            Instr::Jle(l) => Instr::Jle(l),
            Instr::Newline => Instr::Newline,
            Instr::Label(l) => Instr::Label(l),
            Instr::Test(reg) => Instr::Test(reg),
            Instr::LoadString(reg, lit) => Instr::LoadString(reg, lit),
            Instr::Cqo => Instr::Cqo,
            Instr::Sal(reg, i) => Instr::Sal(reg, i),
            Instr::Sar(reg, i) => Instr::Sar(reg, i),
            Instr::Ret => Instr::Ret,
            Instr::LeaLabel(reg, l) => Instr::LeaLabel(reg, l),
            Instr::Call(l) => Instr::Call(l),
            Instr::CallReg(reg) => Instr::CallReg(reg),
            Instr::IDivReg(reg) => Instr::IDivReg(reg),

            Instr::MovToReg(reg, val) => {
                Instr::MovToReg(reg, frame.get_val(val, state.rsp_displacement))
            }
            Instr::MovToMem(mem, reg) => {
                Instr::MovToMem(mem.into_asm_level(frame, state.rsp_displacement), reg)
            }
            Instr::Cmp(reg, val) => Instr::Cmp(reg, frame.get_val(val, state.rsp_displacement)),
            Instr::Add(reg, val) => Instr::Add(reg, frame.get_val(val, state.rsp_displacement)),
            Instr::Sub(reg, val) => Instr::Sub(reg, frame.get_val(val, state.rsp_displacement)),
            Instr::IMul(reg, val) => Instr::IMul(reg, frame.get_val(val, state.rsp_displacement)),
            Instr::IDivMem(mem) => {
                Instr::IDivMem(mem.into_asm_level(frame, state.rsp_displacement))
            }
            Instr::Inc(loc) => Instr::Inc(loc.into_asm_level(frame, state.rsp_displacement)),
            Instr::Dec(loc) => Instr::Dec(loc.into_asm_level(frame, state.rsp_displacement)),
            Instr::Neg(loc) => Instr::Neg(loc.into_asm_level(frame, state.rsp_displacement)),
            Instr::Not(loc) => Instr::Not(loc.into_asm_level(frame, state.rsp_displacement)),
            Instr::Lea(reg, mem) => {
                Instr::Lea(reg, mem.into_asm_level(frame, state.rsp_displacement))
            }
            Instr::Push(val) => Instr::Push(frame.get_val(val, state.rsp_displacement)),

            Instr::Empty => Instr::Empty,
            Instr::AdvanceRSPForCall(advance) => state.advance_rsp(advance as isize),
            Instr::AdjustRSPForStackParam => {
                state.rsp_displacement += QUADWORD_SIZE as isize;
                Instr::Empty
            }
            Instr::ResetRSP => state.reset_rsp(),
        }
    }
}

fn emit_header(out: &mut impl Write) -> AsmGenResult {
    for (func, _) in INITIAL_FUNCS.iter() {
        writeln!(out, "extern {}", func)?;
    }
    writeln!(out, "extern {}", CONCAT_STRINGS_FUNC)?;
    writeln!(out, "extern {}", NEW_FUNC)?;

    writeln!(out, "\nglobal main\n")?;
    Ok(())
}

fn emit_string_literals(out: &mut impl Write, string_literals: &Vec<String>) -> AsmGenResult {
    for (idx, string_literal) in string_literals.iter().enumerate() {
        Label::Str(StringLiteral(idx)).emit(out)?;
        writeln!(
            out,
            "\tdq {}",
            string_literal.len() - 2 /* quotes are included */
        )?;
        writeln!(
            out,
            "\tdb `{}`",
            &string_literal[1..string_literal.len() - 1]
        )?;
    }
    Ok(())
}

fn emit_vsts(out: &mut impl Write, classes: &[Class]) -> AsmGenResult {
    for class in classes {
        Label::Named(class.vst_name()).emit(out)?;
        for mangled_name in methods_sorted_by_idx(class.methods.values()) {
            writeln!(out, "\tdq {}", mangled_name)?;
        }
    }
    writeln!(out, "")?;
    Ok(())
}

fn methods_sorted_by_idx<'a>(
    methods: impl Iterator<Item = &'a Method>,
) -> impl Iterator<Item = &'a String> {
    let mut vec = methods
        .map(
            |ir::Method {
                 mangled_name: name,
                 idx,
                 ..
             }| (name, idx),
        )
        .collect::<Vec<_>>();
    vec.sort_unstable_by_key(|(_, &idx)| idx);
    vec.into_iter().map(|(name, _)| name)
}
