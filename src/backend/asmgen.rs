use std::{
    fmt::Display,
    io::{self, Write},
};

use hashbrown::{HashMap, HashSet};

use log::{debug, info, trace};
use vector_map::VecMap;

use crate::frontend::semantic_analysis::{ast::Ident, INITIAL_FUNCS};

use super::ir::{
    self, BasicBlock, BasicBlockIdx, BinOpType, CallingConvention, Class, EndType, Instant, Ir,
    IrFunction, Method, Quadruple, RelOpType, StringLiteral, UnOpType, CFG, CONCAT_STRINGS_FUNC,
    NEW_FUNC,
};

const ARGS_IN_REGISTERS: usize = 6;
pub const QUADWORD_SIZE: usize = 8; // in bytes
const RETADDR_SIZE: usize = QUADWORD_SIZE;

pub(crate) fn params_registers() -> impl Iterator<Item = Reg> {
    [RDI, RSI, RDX, RCX, R8, R9].into_iter()
}

type AsmGenResult = io::Result<()>;

enum Loc {
    Reg(Reg),
    Mem(Mem),
}
impl Display for Loc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Loc::Reg(reg) => write!(f, "{}", reg),
            Loc::Mem(mem) => write!(f, "{}", mem),
        }
    }
}

impl Loc {
    fn new_from_ir_loc(
        out: &mut impl Write,
        frame: &Frame,
        state: &AsmGenState,
        loc: &ir::Loc,
    ) -> io::Result<Self> {
        let var = loc.var();
        Instr::MovToReg(
            RAX,
            Val::Mem(frame.get_variable_mem(var, state.rsp_displacement)),
        )
        .emit(out)?;

        Ok(match loc {
            ir::Loc::Var(_) => Loc::Reg(RAX),
            ir::Loc::Mem(ir::Mem { offset, .. }) => Loc::Mem(Mem {
                word_len: WordLen::Qword,
                base: RAX,
                index: None,
                displacement: Some(*offset as isize),
            }),
        })
    }
}

enum Val {
    Reg(Reg),
    Instant(Instant),
    Mem(Mem),
}
impl Display for Val {
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

#[derive(PartialEq, Eq, Clone, Copy)]
pub(crate) enum Reg {
    CallerSave(CallerSaveReg),
    CalleeSave(CalleeSaveReg),
}
pub(crate) const RAX: Reg = Reg::CallerSave(CallerSaveReg::Rax);
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
impl Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Reg::CallerSave(reg) => write!(f, "{}", reg),
            Reg::CalleeSave(reg) => write!(f, "{}", reg),
        }
    }
}

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

#[derive(Clone, PartialEq, Eq)]
pub enum Label {
    Num(usize),
    Named(Ident),
    Str(StringLiteral),
}
impl Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Label::Num(n) => write!(f, "__block_{}", n),
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
    block_labels: HashMap<BasicBlockIdx, Label>,
    next_label_idx: usize,
    rsp_displacement: isize, // where is end of the frame relatively to current RSP
    calltime_rsp_position: isize, // rsp displacement at the moment of function call
}

impl AsmGenState {
    fn new() -> Self {
        Self {
            block_labels: HashMap::new(),
            next_label_idx: 0,
            rsp_displacement: 0,
            calltime_rsp_position: 0,
        }
    }

    fn get_block_label(&mut self, block: BasicBlockIdx) -> Label {
        self.block_labels.get(&block).cloned().unwrap_or_else(|| {
            let l = self.gen_label();
            self.block_labels.insert(block, l.clone());
            l
        })
    }

    fn gen_label(&mut self) -> Label {
        let label = Label::Num(self.next_label_idx);
        self.next_label_idx += 1;
        label
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

    fn advance_rsp(&mut self, displacement: isize) -> Instr {
        trace!("Advancing RSP with displacement {}", -displacement);
        self.rsp_displacement += displacement;
        Instr::Sub(RSP, Val::Instant(Instant(displacement as i64)))
    }

    fn reset_rsp(&mut self) -> Instr {
        let rsp_displacement = self.rsp_displacement;
        trace!("Resetting RSP with displacement {}", rsp_displacement);
        self.rsp_displacement = 0;
        Instr::Add(RSP, Val::Instant(Instant(rsp_displacement as i64)))
    }

    fn leave(&mut self) -> Instr {
        assert_eq!(self.rsp_displacement, 0);
        // self.rsp_displacement = self.calltime_rsp_position; // this should not happen not to affect other code paths
        Instr::Sub(
            RSP,
            Val::Instant(Instant(self.calltime_rsp_position as i64)),
        )
    }
}

#[must_use]
enum Instr {
    Jmp(Label),
    Jz(Label),
    Jnz(Label),
    Jg(Label),
    Jge(Label),
    Jl(Label),
    Jle(Label),

    LoadString(Reg, StringLiteral),
    MovToReg(Reg, Val),
    MovToMem(Mem, Reg),

    Test(Reg),
    Cmp(Reg, Val),
    Add(Reg, Val),
    Sub(Reg, Val),
    IMul(Reg, Val),
    IDivReg(Reg), // RDX:RAX divided by Reg, quotient in RAX, remainder in RDX.
    IDivMem(Mem), // RDX:RAX divided by Mem, quotient in RAX, remainder in RDX.
    Cqo,          // Prepared RDX as empty for IDiv
    Inc(Loc),
    Dec(Loc),
    Neg(Loc),
    Not(Reg),
    Sal(Reg, Instant),
    Sar(Reg, Instant),

    Lea(Reg, Mem),
    LeaLabel(Reg, String),

    Push(Val),

    Ret,
    Call(Label),
    CallReg(Reg),
}

impl Instr {
    fn emit(&self, out: &mut impl Write) -> AsmGenResult {
        write!(out, "\t")?;
        match self {
            Instr::Jmp(label) => writeln!(out, "jmp {}", label),
            Instr::Jz(label) => writeln!(out, "jz {}", label),
            Instr::Jnz(label) => writeln!(out, "jnz {}", label),
            Instr::Jg(label) => writeln!(out, "jg {}", label),
            Instr::Jge(label) => writeln!(out, "jge {}", label),
            Instr::Jl(label) => writeln!(out, "jl {}", label),
            Instr::Jle(label) => writeln!(out, "jle {}", label),

            Instr::MovToReg(reg, val) => writeln!(out, "mov {}, {}", reg, val),
            Instr::MovToMem(mem, reg) => writeln!(out, "mov {}, {}", mem, reg),

            Instr::Test(reg) => writeln!(out, "test {}, {}", reg, reg),
            Instr::Cmp(reg, val) => writeln!(out, "cmp {}, {}", reg, val),
            Instr::Add(reg, val) => writeln!(out, "add {}, {}", reg, val),
            Instr::Sub(reg, val) => writeln!(out, "sub {}, {}", reg, val),
            Instr::IMul(reg, val) => writeln!(out, "imul {}, {}", reg, val),
            Instr::IDivReg(reg) => writeln!(out, "idiv {}", reg),
            Instr::IDivMem(mem) => writeln!(out, "idiv {}", mem),
            Instr::Cqo => writeln!(out, "cqo"),
            Instr::Inc(reg) => writeln!(out, "inc {}", reg),
            Instr::Dec(reg) => writeln!(out, "dec {}", reg),
            Instr::Neg(reg) => writeln!(out, "neg {}", reg),
            Instr::Not(reg) => writeln!(out, "not {}", reg),
            Instr::Sal(reg, i) => writeln!(out, "sal {}, {}", reg, **i),
            Instr::Sar(reg, i) => writeln!(out, "sar {}, {}", reg, **i),

            Instr::Lea(reg, mem) => writeln!(out, "lea {}, {}", reg, mem),
            Instr::LeaLabel(reg, label) => writeln!(out, "lea {}, [rel {}]", reg, label),

            Instr::Push(val) => writeln!(out, "push {}", val),

            Instr::Ret => writeln!(out, "ret"),
            Instr::Call(label) => writeln!(out, "call {}", label),
            Instr::CallReg(reg) => writeln!(out, "call {}", reg),

            Instr::LoadString(reg, idx) => writeln!(out, "lea {}, [rel {}]", reg, Label::Str(*idx)),
        }
    }
}

impl RelOpType {
    fn instrs(&self) -> (fn(Label) -> Instr, fn(Label) -> Instr) {
        // (then, else)
        match self {
            RelOpType::Gt => (Instr::Jg, Instr::Jle),
            RelOpType::Ge => (Instr::Jge, Instr::Jl),
            RelOpType::Lt => (Instr::Jl, Instr::Jge),
            RelOpType::Le => (Instr::Jle, Instr::Jg),
            RelOpType::Eq => (Instr::Jz, Instr::Jnz),
            RelOpType::NEq => (Instr::Jnz, Instr::Jz),
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
struct Frame {
    name: Ident,
    // convention: CallingConvention,
    params: Vec<ir::Var>,
    local_variables_count: usize,
    variables_mapping: VecMap<ir::Var, Var>, // var -> offset wrt RBP (frame)
    frame_size: usize,                       // in bytes, must be divisible by 16
}

impl Frame {
    fn new(
        name: Ident,
        _stack_parameters_count: usize,
        params: Vec<ir::Var>,
        variables: HashSet<ir::Var>,
    ) -> Self {
        debug!("Creating frame with variables: {:?}", variables);
        let variables_mapping = variables
            .into_iter()
            .enumerate()
            .map(|(k, var)| (var, Var(k as isize * -8)))
            .collect::<VecMap<_, _>>();
        trace!("Variables got mappings: {:?}", &variables_mapping);

        Self {
            name,
            // convention: CallingConvention::StackVars,
            local_variables_count: variables_mapping.len(),
            frame_size: ((variables_mapping.len() + 1/*retaddr*/) * QUADWORD_SIZE + QUADWORD_SIZE)
                / 16
                * 16, /*stack alignment*/
            variables_mapping,
            params,
        }
    }

    fn new_ffi(name: Ident, params: Vec<ir::Var>) -> Self {
        Self {
            name,
            // convention: CallingConvention::Cdecl,
            local_variables_count: 0,
            variables_mapping: VecMap::new(),
            frame_size: RETADDR_SIZE,
            params,
        }
    }

    fn get_variable_offset_relative_to_frame(&self, variable: ir::Var) -> isize {
        let res = self
            .variables_mapping
            .get(&variable)
            .unwrap_or_else(|| panic!("{:?} not registered in frame: {:#?}.", variable, self))
            .0;
        // trace!("Queried {:?} frame offset, got {}.", variable, res);
        res
    }

    fn get_variable_offset_relative_to_frame_end(&self, variable: ir::Var) -> isize {
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
        let res = self.get_variable_offset_relative_to_frame(variable) + self.frame_size as isize
            - RETADDR_SIZE as isize
            - QUADWORD_SIZE as isize;
        // trace!("Queried {:?} retaddr offset, got {}.", variable, res);
        res
    }

    fn get_variable_offset_relative_to_rsp(
        &self,
        variable: ir::Var,
        rsp_displacement: isize,
    ) -> isize {
        let res = self.get_variable_offset_relative_to_frame_end(variable) + rsp_displacement;
        // trace!(
        //     "Queried {:?} offset relative to rsp, got {}.",
        //     variable, res
        // );
        res
    }

    fn get_variable_mem(&self, variable: ir::Var, rsp_displacement: isize) -> Mem {
        Mem {
            word_len: WordLen::Qword,
            base: RSP,
            index: None,
            displacement: Some(
                self.get_variable_offset_relative_to_rsp(variable, rsp_displacement),
            ),
        }
    }

    fn get_val(&self, val: ir::Value, rsp_displacement: isize) -> Val {
        match val {
            ir::Value::Instant(i) => Val::Instant(i),
            ir::Value::Variable(var) => Val::Mem(self.get_variable_mem(var, rsp_displacement)),
        }
    }
}

impl Ir {
    pub fn emit_assembly(&self, out: &mut impl Write) -> AsmGenResult {
        info!("---- ASSEMBLY EMITTING ----");
        self.cfg.emit_assembly(out, &self.string_literals)
    }
}

impl CFG {
    pub fn emit_assembly(
        &self,
        out: &mut impl Write,
        string_literals: &Vec<String>,
    ) -> AsmGenResult {
        writeln!(out, "section .data")?;
        emit_string_literals(out, string_literals)?;

        let mut state = AsmGenState::new();

        info!("Building frames");
        let frames = self
            .functions
            .iter()
            .map(
                |(
                    func,
                    IrFunction {
                        params, convention, ..
                    },
                )| {
                    (
                        func.clone(),
                        match convention {
                            CallingConvention::SimpleCdecl => {
                                let mut variables = self.variables_in_function(func);
                                variables.extend(params);
                                Frame::new(
                                    func.clone(),
                                    isize::max(
                                        params.len() as isize - ARGS_IN_REGISTERS as isize,
                                        0,
                                    ) as usize,
                                    params.clone(),
                                    variables,
                                )
                            }
                            CallingConvention::CdeclFFI => {
                                Frame::new_ffi(func.clone(), params.clone())
                            }
                        },
                    )
                },
            )
            .collect::<HashMap<_, _>>();

        debug!("Built frames: {:#?}", &frames);

        writeln!(out, "\nsection .text")?;
        emit_vsts(out, &self.classes)?;

        emit_header(out)?;

        let mut emitted = HashSet::new();

        for (
            func,
            IrFunction {
                entry, convention, ..
            },
        ) in self.functions.iter()
        {
            let frame = frames.get(func).unwrap();
            if matches!(convention, CallingConvention::SimpleCdecl) {
                // assert_eq!(state.rsp_displacement, 0); // RSP initially set to ret addr
                let func_label = Label::Named(func.clone());

                info!("Emitting function: {}", func);
                writeln!(out, "")?;
                func_label.emit(out)?;

                state.enter_frame(frame.frame_size);
                let stack_growth = frame.frame_size - RETADDR_SIZE;
                if stack_growth != 0 {
                    state.reset_rsp().emit(out)?;
                }

                // Params in registers
                for (var, reg) in frame.params.iter().copied().zip(params_registers()) {
                    Instr::MovToMem(frame.get_variable_mem(var, state.rsp_displacement), reg)
                        .emit(out)?;
                }
                // Params on the stack
                for (no, var) in frame
                    .params
                    .iter()
                    .copied()
                    .skip(params_registers().count())
                    .enumerate()
                {
                    Instr::MovToReg(
                        RAX,
                        Val::Mem(Mem {
                            word_len: WordLen::Qword,
                            base: RSP,
                            index: None,
                            displacement: Some((frame.frame_size + no * QUADWORD_SIZE) as isize),
                        }),
                    )
                    .emit(out)?;
                    Instr::MovToMem(frame.get_variable_mem(var, state.rsp_displacement), RAX)
                        .emit(out)?;
                }

                self.emit_function_block(
                    out,
                    entry.unwrap(),
                    &mut emitted,
                    &frames,
                    frame,
                    &mut state,
                    None,
                )?;
            }
        }

        Ok(())
    }

    fn emit_function_block(
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
                Instr::Ret.emit(out)?;
            }
            Some(EndType::Return(Some(val))) => {
                match val {
                    ir::Value::Instant(i) => Instr::MovToReg(RAX, Val::Instant(*i)).emit(out)?,
                    ir::Value::Variable(var) => Instr::MovToReg(
                        RAX,
                        Val::Mem(frame.get_variable_mem(*var, state.rsp_displacement)),
                    )
                    .emit(out)?,
                }
                let stack_growth = frame.frame_size - RETADDR_SIZE;
                if stack_growth != 0 {
                    state.leave().emit(out)?;
                }
                Instr::Ret.emit(out)?;
            }
            Some(EndType::Goto(block_idx)) => {
                // If not emitted yet, simply emit it below and save a jump
                if emitted.contains(block_idx) && next_l != Some(&state.get_block_label(*block_idx))
                {
                    Instr::Jmp(state.get_block_label(*block_idx).clone()).emit(out)?;
                }
                self.emit_function_block(out, *block_idx, emitted, frames, frame, state, next_l)?;
            }
            Some(EndType::IfElse(_, _, _, then_block, else_block)) if then_block == else_block => {
                let block_idx = then_block;
                // Reduce to Goto
                // If not emitted yet, simply emit it below and save a jump
                if emitted.contains(block_idx) && next_l != Some(&state.get_block_label(*block_idx))
                {
                    Instr::Jmp(state.get_block_label(*block_idx).clone()).emit(out)?;
                }
                self.emit_function_block(out, *block_idx, emitted, frames, frame, state, next_l)?;
            }
            Some(EndType::IfElse(a, rel, b, then_block, else_block)) => {
                let then_l = state.get_block_label(*then_block).clone();
                let else_l = state.get_block_label(*else_block).clone();

                // Cond
                Instr::MovToReg(
                    RAX,
                    Val::Mem(frame.get_variable_mem(*a, state.rsp_displacement)),
                )
                .emit(out)?;
                Instr::Cmp(RAX, frame.get_val(*b, state.rsp_displacement)).emit(out)?;

                let (then_instr, else_instr) = rel.instrs();

                if next_l == Some(&then_l) && emitted.contains(else_block) {
                    else_instr(else_l.clone()).emit(out)?;
                } else if next_l == Some(&else_l) && emitted.contains(then_block) {
                    then_instr(then_l.clone()).emit(out)?;
                } else {
                    else_instr(else_l.clone()).emit(out)?;
                    if emitted.contains(then_block) {
                        Instr::Jmp(then_l).emit(out)?;
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
    }
}

impl BasicBlock {
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
}

impl Quadruple {
    fn emit(
        &self,
        cfg: &CFG,
        out: &mut impl Write,
        frame: &Frame,
        state: &mut AsmGenState,
    ) -> AsmGenResult {
        match self {
            Quadruple::BinOp(dst, op1, bin_op, op2) => {
                Instr::MovToReg(
                    RAX,
                    Val::Mem(frame.get_variable_mem(*op1, state.rsp_displacement)),
                )
                .emit(out)?;
                match bin_op {
                    BinOpType::Add => {
                        Instr::Add(RAX, frame.get_val(*op2, state.rsp_displacement)).emit(out)?
                    }
                    BinOpType::Sub => {
                        Instr::Sub(RAX, frame.get_val(*op2, state.rsp_displacement)).emit(out)?
                    }
                    BinOpType::Mul => {
                        Instr::IMul(RAX, frame.get_val(*op2, state.rsp_displacement)).emit(out)?
                    }
                    BinOpType::Div => {
                        Instr::Cqo.emit(out)?;
                        let val = frame.get_val(*op2, state.rsp_displacement);
                        match val {
                            Val::Reg(reg) => Instr::IDivReg(reg).emit(out)?,
                            Val::Instant(_) => {
                                Instr::MovToReg(RCX, val).emit(out)?;
                                Instr::IDivReg(RCX).emit(out)?;
                            }
                            Val::Mem(mem) => Instr::IDivMem(mem).emit(out)?,
                        }
                    }
                    BinOpType::Mod => {
                        Instr::Cqo.emit(out)?;
                        let val = frame.get_val(*op2, state.rsp_displacement);
                        match val {
                            Val::Reg(reg) => Instr::IDivReg(reg).emit(out)?,
                            Val::Instant(_) => {
                                Instr::MovToReg(RCX, val).emit(out)?;
                                Instr::IDivReg(RCX).emit(out)?;
                            }
                            Val::Mem(mem) => Instr::IDivMem(mem).emit(out)?,
                        }
                        // mov remainder from RDX to RAX:
                        Instr::MovToReg(RAX, Val::Reg(RDX)).emit(out)?;
                    }
                };
                Instr::MovToMem(frame.get_variable_mem(*dst, state.rsp_displacement), RAX)
                    .emit(out)?
            }
            Quadruple::UnOp(dst, un_op_type, op) => {
                Instr::MovToReg(RAX, frame.get_val(*op, state.rsp_displacement)).emit(out)?;
                match un_op_type {
                    UnOpType::Not => Instr::Not(RAX).emit(out)?,
                    UnOpType::Neg => Instr::Neg(Loc::Reg(RAX)).emit(out)?,
                }
                Instr::MovToMem(frame.get_variable_mem(*dst, state.rsp_displacement), RAX)
                    .emit(out)?;
            }
            Quadruple::Copy(dst, src) => {
                Instr::MovToReg(
                    RAX,
                    Val::Mem(frame.get_variable_mem(*src, state.rsp_displacement)),
                )
                .emit(out)?;
                Instr::MovToMem(frame.get_variable_mem(*dst, state.rsp_displacement), RAX)
                    .emit(out)?;
            }
            Quadruple::Set(dst, i) => {
                Instr::MovToReg(RAX, Val::Instant(*i)).emit(out)?;
                Instr::MovToMem(frame.get_variable_mem(*dst, state.rsp_displacement), RAX)
                    .emit(out)?;
            }
            Quadruple::GetStrLit(dst, str_idx) => {
                Instr::LoadString(RAX, *str_idx).emit(out)?;
                Instr::MovToMem(frame.get_variable_mem(*dst, state.rsp_displacement), RAX)
                    .emit(out)?;
            }

            Quadruple::Call(dst, func, args) => {
                let cfg_function = cfg.functions.get(func).unwrap();
                let callee_convention = &cfg_function.convention;
                match callee_convention {
                    CallingConvention::SimpleCdecl => {
                        // Params in registers
                        for (reg, arg) in params_registers().zip(args.iter().copied()) {
                            Instr::MovToReg(reg, frame.get_val(arg, state.rsp_displacement))
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
                            Instr::Push(frame.get_val(arg, state.rsp_displacement)).emit(out)?;
                            state.rsp_displacement += QUADWORD_SIZE as isize;
                        }

                        // // place arguments in corresponding callee's variables
                        // for (arg, param) in args.iter().copied().zip(callee_params.iter().copied())
                        // {
                        //     Instr::MovToReg(RAX, frame.get_val(arg, state.rsp_displacement))
                        //         .emit(out)?;
                        //     Instr::MovToMem(
                        //         callee_frame.get_variable_mem(param, -(RETADDR_SIZE as isize)),
                        //         RAX,
                        //     )
                        //     .emit(out)?;
                        // }

                        Instr::Call(Label::Named(func.clone())).emit(out)?;

                        if stack_alignment_growth != 0 || params_on_the_stack_num > 0 {
                            state.reset_rsp().emit(out)?;
                        }

                        // Save return value
                        Instr::MovToMem(frame.get_variable_mem(*dst, state.rsp_displacement), RAX)
                            .emit(out)?;
                    }
                    CallingConvention::CdeclFFI => {
                        // place arguments in corresponding registers
                        for (arg, reg) in args.iter().copied().zip(params_registers()) {
                            Instr::MovToReg(reg, frame.get_val(arg, state.rsp_displacement))
                                .emit(out)?;
                        }

                        // stack alignment, as no params on the stack
                        // Instr::Sub(RSP, Val::Instant(Instant(8))).emit(out)?;

                        // // sanity alignment check
                        // // before call, the stack must be aligned to 0 mod 16
                        // Instr::MovToReg(RAX, Val::Reg(RSP)).emit(out)?;
                        // Instr::Cqo.emit(out)?;
                        // Instr::MovToReg(RCX, Val::Instant(Instant(16))).emit(out)?;
                        // Instr::IDivReg(RCX).emit(out)?;
                        // // mov remainder from RDX to RAX:
                        // Instr::Test(RDX).emit(out)?;
                        // Instr::Jnz(Label::Func(Ident::from("_notaligned"))).emit(out)?;

                        Instr::Call(Label::Named(func.clone())).emit(out)?;

                        // Instr::Add(RSP, Val::Instant(Instant(8))).emit(out)?;

                        // Save return value
                        Instr::MovToMem(frame.get_variable_mem(*dst, state.rsp_displacement), RAX)
                            .emit(out)?;
                    }
                }
            }

            Quadruple::ArrLoad(_, _, _) => todo!(),
            Quadruple::ArrStore(_, _, _) => todo!(),

            Quadruple::DerefLoad(dst, ptr) => {
                Instr::MovToReg(
                    RAX,
                    Val::Mem(frame.get_variable_mem(ptr.base, state.rsp_displacement)),
                )
                .emit(out)?;
                Instr::MovToReg(
                    RAX,
                    Val::Mem(Mem {
                        word_len: WordLen::Qword,
                        base: RAX,
                        index: None,
                        displacement: Some(ptr.offset as isize),
                    }),
                )
                .emit(out)?;
                Instr::MovToMem(frame.get_variable_mem(*dst, state.rsp_displacement), RAX)
                    .emit(out)?;
            }

            Quadruple::DerefStore(src, ptr) => {
                // src in RAX, ptr in RDX
                Instr::MovToReg(RAX, frame.get_val(*src, state.rsp_displacement)).emit(out)?;
                Instr::MovToReg(
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
                Instr::MovToMem(ptr_mem, RAX).emit(out)?;
            }

            Quadruple::InPlaceUnOp(op, ir_loc) => {
                let loc = Loc::new_from_ir_loc(out, frame, state, ir_loc)?;
                match op {
                    ir::InPlaceUnOpType::Inc => Instr::Inc(loc).emit(out)?,
                    ir::InPlaceUnOpType::Dec => Instr::Dec(loc).emit(out)?,
                }
                if let ir::Loc::Var(var) = ir_loc {
                    Instr::MovToMem(frame.get_variable_mem(*var, state.rsp_displacement), RAX)
                        .emit(out)?;
                }
            }

            Quadruple::VstStore(class_idx, mem) => {
                Instr::LeaLabel(R8, cfg.classes[class_idx.0].vst_name()).emit(out)?;
                Instr::MovToReg(
                    R9,
                    Val::Mem(frame.get_variable_mem(mem.base, state.rsp_displacement)),
                )
                .emit(out)?;
                Instr::MovToMem(
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
                    Instr::MovToReg(reg, frame.get_val(arg, state.rsp_displacement)).emit(out)?;
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
                    Instr::Push(frame.get_val(arg, state.rsp_displacement)).emit(out)?;
                    state.rsp_displacement += QUADWORD_SIZE as isize;
                }

                // Get the object
                Instr::MovToReg(
                    RAX,
                    Val::Mem(frame.get_variable_mem(*object, state.rsp_displacement)),
                )
                .emit(out)?;

                // Get VST ptr from the object
                Instr::MovToReg(
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
                Instr::MovToReg(
                    RAX,
                    Val::Mem(Mem {
                        word_len: WordLen::Qword,
                        base: RAX,
                        index: None,
                        displacement: Some((*method_idx * QUADWORD_SIZE) as isize),
                    }),
                )
                .emit(out)?;

                Instr::CallReg(RAX).emit(out)?;

                if stack_alignment_growth != 0 || params_on_the_stack_num > 0 {
                    state.reset_rsp().emit(out)?;
                }

                // Save return value
                Instr::MovToMem(frame.get_variable_mem(*dst, state.rsp_displacement), RAX)
                    .emit(out)?;
            }
        };
        Ok(())
    }
}

fn emit_header(out: &mut impl Write) -> AsmGenResult {
    for (func, _) in INITIAL_FUNCS.iter() {
        writeln!(out, "extern {}", func)?;
    }
    writeln!(out, "extern {}", CONCAT_STRINGS_FUNC)?;
    writeln!(out, "extern {}", NEW_FUNC)?;

    writeln!(out, "\nglobal main\n")?;

    // Label::Named(Ident::from("main".to_string())).emit(out)?;

    // let stack_growth = main_frame.frame_size - RETADDR_SIZE;
    // if stack_growth != 0 {
    //     state.advance_rsp(stack_growth as isize).emit(out)?;
    // }

    // Instr::Call(Label::Named(Ident::from(REAL_MAIN.to_string()))).emit(out)?;

    // if stack_growth != 0 {
    //     state.reset_rsp().emit(out)?;
    // }

    // Instr::Ret.emit(out)?;

    // Label::Func(Ident::from("_notaligned")).emit(out)?;
    // Instr::MovToReg(RDI, Val::Instant(Instant(66))).emit(out)?;
    // Instr::MovToReg(RAX, Val::Instant(SYS_EXIT)).emit(out)?;
    // Instr::Syscall.emit(out)?;

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
