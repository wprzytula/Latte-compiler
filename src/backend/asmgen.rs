use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    io::{self, Write},
};

use vector_map::VecMap;

use crate::frontend::semantic_analysis::ast::Ident;

use super::ir::{
    self, BasicBlock, BasicBlockIdx, BinOpType, CfgFunction, EndType, Instant, Quadruple,
    RelOpType, UnOpType, CFG,
};

const ARGS_IN_REGISTERS: usize = 6;
const QUADWORD_SIZE: usize = 8; // in bytes
const RETADDR_SIZE: usize = QUADWORD_SIZE;

const SYS_EXIT: Instant = Instant(60);

type AsmGenResult = io::Result<()>;

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

enum Reg {
    CallerSave(CallerSaveReg),
    CalleeSave(CalleeSaveReg),
    Rip,
}
const RAX: Reg = Reg::CallerSave(CallerSaveReg::Rax);
const RCX: Reg = Reg::CallerSave(CallerSaveReg::Rcx);
const RDX: Reg = Reg::CallerSave(CallerSaveReg::Rdx);
const RDI: Reg = Reg::CallerSave(CallerSaveReg::Rdi);
const RSI: Reg = Reg::CallerSave(CallerSaveReg::Rsi);
const R8: Reg = Reg::CallerSave(CallerSaveReg::R8);
const R9: Reg = Reg::CallerSave(CallerSaveReg::R9);
const R10: Reg = Reg::CallerSave(CallerSaveReg::R10);
const R11: Reg = Reg::CallerSave(CallerSaveReg::R11);

const R12: Reg = Reg::CalleeSave(CalleeSaveReg::R12);
const R13: Reg = Reg::CalleeSave(CalleeSaveReg::R13);
const R14: Reg = Reg::CalleeSave(CalleeSaveReg::R14);
const R15: Reg = Reg::CalleeSave(CalleeSaveReg::R15);
const RSP: Reg = Reg::CalleeSave(CalleeSaveReg::Rsp);
const RBP: Reg = Reg::CalleeSave(CalleeSaveReg::Rbp);

impl Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Reg::CallerSave(reg) => write!(f, "{}", reg),
            Reg::CalleeSave(reg) => write!(f, "{}", reg),
            Reg::Rip => write!(f, "rip"),
        }
    }
}

enum CallerSaveReg {
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

enum CalleeSaveReg {
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
    Func(Ident),
}
impl Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Label::Num(n) => write!(f, "label_{}", n),
            Label::Func(id) => f.write_str(id),
        }
    }
}
impl Label {
    fn emit(&self, out: &mut impl Write) -> AsmGenResult {
        writeln!(out, "{}:", self)
    }
}

struct AsmGenState /* <'a> */ {
    // vars: HashMap<&'a str, Var>,
    // next_var_idx: usize,
    block_labels: HashMap<BasicBlockIdx, Label>,
    next_label_idx: usize,
    rsp_displacement: isize, // where is ret addr relatively to current RSP
}

impl AsmGenState /* <'a> */ {
    fn new() -> Self {
        Self {
            // vars: HashMap::new(),
            // next_var_idx: 0,
            block_labels: HashMap::new(),
            next_label_idx: 0,
            rsp_displacement: 0,
        }
    }

    fn get_block_label(&mut self, block: BasicBlockIdx) -> Label {
        self.block_labels.get(&block).cloned().unwrap_or_else(|| {
            let l = self.gen_label();
            self.block_labels.insert(block, l.clone());
            l
        })
    }

    /* fn declare_variable(&mut self, var: &'a str) {
        if !self.vars.contains_key(var) {
            self.vars.insert(var, Var(self.next_var_idx));
            self.next_var_idx += 1;
        }
    }

    fn get_var_idx(&self, var: &'a str) -> Var {
        *self.vars.get(var).unwrap()
    } */

    fn gen_label(&mut self) -> Label {
        let label = Label::Num(self.next_label_idx);
        self.next_label_idx += 1;
        label
    }

    fn advance_rsp(&mut self, displacement: isize) -> Instr {
        self.rsp_displacement = displacement;
        Instr::Sub(RSP, Val::Instant(Instant(displacement as i64)))
    }

    fn reset_rsp(&mut self) -> Instr {
        let rsp_displacement = self.rsp_displacement;
        self.rsp_displacement = 0;
        Instr::Add(RSP, Val::Instant(Instant(rsp_displacement as i64)))
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

    Sete,
    Setne,
    Setl,
    Setle,
    Setg,
    Setge,

    MovToReg(Reg, Val),
    MovToMem(Mem, Reg),

    Test(Reg),
    Cmp(Reg, Val),
    Add(Reg, Val),
    Sub(Reg, Val),
    IMul(Reg, Val),
    IDiv(Val), // RDX:RAX divided by Val, quotient in RAX, remainder in RDX.
    Cqo,       // Prepared RDX as empty for IDiv
    Inc(Reg),
    Dec(Reg),
    Neg(Reg),
    Not(Reg),
    And(Reg, Val),
    Or(Reg, Val),
    Xor(Reg, Val),
    Sal(Reg, Instant),
    Sar(Reg, Instant),

    Lea(Reg, Mem),

    Push(Val),
    Pop(Reg),

    Ret,
    Call(Label),
    Syscall,
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

            Instr::Sete => writeln!(out, "sete cl"),
            Instr::Setne => writeln!(out, "setne cl"),
            Instr::Setl => writeln!(out, "setl cl"),
            Instr::Setle => writeln!(out, "setle cl"),
            Instr::Setg => writeln!(out, "setg cl"),
            Instr::Setge => writeln!(out, "setge cl"),

            Instr::MovToReg(reg, val) => writeln!(out, "mov {}, {}", reg, val),
            Instr::MovToMem(mem, reg) => writeln!(out, "mov {}, {}", mem, reg),

            Instr::Test(reg) => writeln!(out, "test {}, {}", reg, reg),
            Instr::Cmp(reg, val) => writeln!(out, "cmp {}, {}", reg, val),
            Instr::Add(reg, val) => writeln!(out, "add {}, {}", reg, val),
            Instr::Sub(reg, val) => writeln!(out, "sub {}, {}", reg, val),
            Instr::IMul(reg, val) => writeln!(out, "imul {}, {}", reg, val),
            Instr::IDiv(val) => writeln!(out, "idiv {}", val),
            Instr::Cqo => writeln!(out, "cqo"),
            Instr::Inc(reg) => writeln!(out, "inc {}", reg),
            Instr::Dec(reg) => writeln!(out, "dec {}", reg),
            Instr::Neg(reg) => writeln!(out, "neg {}", reg),
            Instr::Not(reg) => writeln!(out, "not {}", reg),
            Instr::And(reg, val) => writeln!(out, "sub {}, {}", reg, val),
            Instr::Or(reg, val) => writeln!(out, "or {}, {}", reg, val),
            Instr::Xor(reg, val) => writeln!(out, "xor {}, {}", reg, val),
            Instr::Sal(reg, i) => writeln!(out, "sal {}, {}", reg, **i),
            Instr::Sar(reg, i) => writeln!(out, "sar {}, {}", reg, **i),

            Instr::Lea(reg, mem) => writeln!(out, "lea {}, {}", reg, mem),

            Instr::Push(val) => writeln!(out, "push {}", val),
            Instr::Pop(reg) => writeln!(out, "pop {}", reg),

            Instr::Ret => writeln!(out, "ret"),
            Instr::Call(label) => writeln!(out, "call {}", label),
            Instr::Syscall => writeln!(out, "syscall"),
        }
    }
}

/**
 * Calling convention
 * base_var1 | ... | base_varn | base_retcode [base_frame end] | [child_frame begin] var1 | var2 | ... | varn | retcode [child_frame_end]
 * upon call, the caller makes place for all callee's variables and calls (pushes retcode and jumps to the callee's label).
 * after call, the caller must clean up the callee's variables.
 *
 * frame_size: in bytes, including variables and return address (Constant, stored in Frame)
 * rsp_displacement: in bytes, signifies current rsp movement relative to current function's return address (Mutable, stored in State)
 */

#[derive(Debug)]
struct Frame {
    // stack_parameters_count: usize,
    local_variables_count: usize,
    variables_mapping: VecMap<ir::Var, Var>, // var -> offset wrt RBP (frame)
    frame_size: usize,                       // in bytes, must be divisible by 16
}

impl Frame {
    fn new(_stack_parameters_count: usize, variables: HashSet<ir::Var>) -> Self {
        eprintln!("Creating frame with variables: {:?}", variables);
        let variables_mapping = variables
            .into_iter()
            .enumerate()
            .map(|(k, var)| (var, Var(k as isize * -8)))
            .collect::<VecMap<_, _>>();
        eprintln!("Variables got mappings: {:?}", &variables_mapping);

        Self {
            local_variables_count: variables_mapping.len(),
            frame_size: ((variables_mapping.len() + 1/*retaddr*/) * QUADWORD_SIZE) / 16 * 16, /*stack alignment*/
            variables_mapping,
        }
    }

    fn get_variable_offset_relative_to_frame(&self, variable: ir::Var) -> isize {
        let res = self
            .variables_mapping
            .get(&variable)
            .unwrap_or_else(|| panic!("{:?} not registered in frame.", variable))
            .0;
        eprintln!("Queried {:?} frame offset, got {}.", variable, res);
        res
    }

    fn get_variable_offset_relative_to_retaddr(&self, variable: ir::Var) -> isize {
        //              by frame   by rsp
        // BASE_RET_ADDR - +8    -  +24
        // a             -  0    -  +16
        // b             - -8    -  +8
        // RET_ADDR      - -16   -   0
        // frame_size = 24
        // b <- frame - -8
        let res = self.get_variable_offset_relative_to_frame(variable) + self.frame_size as isize
            - RETADDR_SIZE as isize;
        eprintln!("Queried {:?} retaddr offset, got {}.", variable, res);
        res
    }

    fn get_variable_offset_relative_to_rsp(
        &self,
        variable: ir::Var,
        rsp_displacement: isize,
    ) -> isize {
        let res = self.get_variable_offset_relative_to_retaddr(variable) + rsp_displacement;
        eprintln!(
            "Queried {:?} offset relative to rsp, got {}.",
            variable, res
        );
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

impl CFG {
    pub fn emit_assembly(&self, out: &mut impl Write) -> AsmGenResult {
        let mut state = AsmGenState::new();

        eprintln!("Building frames");
        let frames = self
            .functions
            .iter()
            .map(|(func, CfgFunction { params, .. })| {
                let mut variables = self.variables_in_function(func);
                variables.extend(params);
                (
                    func.clone(),
                    Frame::new(
                        isize::max(params.len() as isize - ARGS_IN_REGISTERS as isize, 0) as usize,
                        variables,
                    ),
                )
            })
            .collect::<HashMap<_, _>>();

        eprintln!("Built frames: {:#?}\n\n", &frames);

        emit_start(out, &mut state, frames.get(&"main".to_string()).unwrap())?;

        let mut emitted = HashSet::new();

        for (func, CfgFunction { entry, .. }) in self.functions.iter() {
            assert_eq!(state.rsp_displacement, 0); // RSP initially set to ret addr
            let func_label = Label::Func(func.clone());

            eprintln!("\nEmitting function: {}", func);
            writeln!(out, "")?;
            func_label.emit(out)?;

            self.emit_function_block(
                out,
                *entry,
                &mut emitted,
                &frames,
                frames.get(func).unwrap(),
                &mut state,
                None,
            )?;
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
            eprintln!(
                "Block: {:?} has already been emitted; returning.",
                func_block
            );
            return Ok(());
        } else {
            emitted.insert(func_block);
        }

        eprintln!("Emitting block: {:?}", func_block);
        state.get_block_label(func_block).emit(out)?;
        self[func_block].emit(self, out, frames, frame, state)?;

        match &self[func_block].end_type {
            Some(EndType::Return(None)) | None => {
                // state.reset_rsp().emit(out)?;
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
                // state.reset_rsp().emit(out)?;
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
            Some(EndType::IfElse(ir_var, then_block, else_block)) => {
                let then_l = state.get_block_label(*then_block).clone();
                let else_l = state.get_block_label(*else_block).clone();

                // Cond
                Instr::MovToReg(
                    RAX,
                    Val::Mem(frame.get_variable_mem(*ir_var, state.rsp_displacement)),
                )
                .emit(out)?;
                Instr::Test(RAX).emit(out)?;
                if next_l == Some(&then_l) {
                    Instr::Jnz(else_l.clone()).emit(out)?;
                    // self.emit_function_block(out, *then_block, emitted, frames, frame, state, Some(&else_l))?;

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
                } else if next_l == Some(&else_l) {
                    Instr::Jz(then_l.clone()).emit(out)?;
                    // Else already emitted
                    // self.emit_function_block(out, *else_block, emitted, frames, frame, state, &then_l)?;

                    // Then
                    self.emit_function_block(
                        out,
                        *then_block,
                        emitted,
                        frames,
                        frame,
                        state,
                        next_l,
                    )?;
                } else {
                    Instr::Jnz(else_l.clone()).emit(out)?;
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
        frames: &HashMap<Ident, Frame>,
        frame: &Frame,
        state: &mut AsmGenState,
    ) -> AsmGenResult {
        for quadruple in self.quadruples.iter() {
            quadruple.emit(cfg, out, frames, frame, state)?;
        }
        Ok(())
    }
}

impl Quadruple {
    fn emit(
        &self,
        cfg: &CFG,
        out: &mut impl Write,
        frames: &HashMap<Ident, Frame>,
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
                        Instr::IDiv(frame.get_val(*op2, state.rsp_displacement)).emit(out)?;
                    }
                    BinOpType::Mod => {
                        Instr::Cqo.emit(out)?;
                        Instr::IDiv(frame.get_val(*op2, state.rsp_displacement)).emit(out)?;
                        // mov remainder from RDX to RAX:
                        Instr::MovToReg(RAX, Val::Reg(RDX)).emit(out)?;
                    }
                    BinOpType::And => {
                        Instr::And(RAX, frame.get_val(*op2, state.rsp_displacement)).emit(out)?
                    }
                    BinOpType::Or => {
                        Instr::Or(RAX, frame.get_val(*op2, state.rsp_displacement)).emit(out)?
                    }
                    BinOpType::Xor => {
                        Instr::Xor(RAX, frame.get_val(*op2, state.rsp_displacement)).emit(out)?
                    }
                };
                Instr::MovToMem(frame.get_variable_mem(*dst, state.rsp_displacement), RAX)
                    .emit(out)?
            }
            Quadruple::RelOp(dst, op1, rel_op, op2) => {
                Instr::MovToReg(
                    RAX,
                    Val::Mem(frame.get_variable_mem(*op1, state.rsp_displacement)),
                )
                .emit(out)?;
                let op2 = frame.get_val(*op2, state.rsp_displacement);
                Instr::Xor(RCX, Val::Reg(RCX)).emit(out)?;
                Instr::Cmp(RAX, op2).emit(out)?;
                match rel_op {
                    RelOpType::Gt => Instr::Setle.emit(out)?,
                    RelOpType::Ge => Instr::Setl.emit(out)?,
                    RelOpType::Lt => Instr::Setge.emit(out)?,
                    RelOpType::Le => Instr::Setg.emit(out)?,
                    RelOpType::Eq => Instr::Sete.emit(out)?,
                    RelOpType::NEq => Instr::Setne.emit(out)?,
                }
                Instr::MovToMem(frame.get_variable_mem(*dst, state.rsp_displacement), RCX)
                    .emit(out)?;
            }
            Quadruple::UnOp(dst, un_op_type, op) => {
                Instr::MovToReg(RAX, frame.get_val(*op, state.rsp_displacement)).emit(out)?;
                match un_op_type {
                    UnOpType::Not => Instr::Not(RAX).emit(out)?,
                    UnOpType::Neg => Instr::Neg(RAX).emit(out)?,
                    UnOpType::Inc => Instr::Inc(RAX).emit(out)?,
                    UnOpType::Dec => Instr::Dec(RAX).emit(out)?,
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
            Quadruple::Call(dst, func, args) => {
                let callee_params = &cfg.functions.get(func).unwrap().params;
                let callee_frame = frames.get(func).unwrap();
                let stack_growth = callee_frame.frame_size - RETADDR_SIZE;

                if stack_growth != 0 {
                    state.advance_rsp(stack_growth as isize).emit(out)?;
                }

                // place arguments in corresponding callee's variables
                for (arg, param) in args.iter().copied().zip(callee_params.iter().copied()) {
                    Instr::MovToReg(RAX, frame.get_val(arg, state.rsp_displacement)).emit(out)?;
                    Instr::MovToMem(
                        callee_frame.get_variable_mem(param, -(RETADDR_SIZE as isize)),
                        RAX,
                    )
                    .emit(out)?;
                }
                Instr::Call(Label::Func(func.clone())).emit(out)?;

                if stack_growth != 0 {
                    state.reset_rsp().emit(out)?;
                }

                // Save return value
                Instr::MovToMem(frame.get_variable_mem(*dst, state.rsp_displacement), RAX)
                    .emit(out)?;
            }

            Quadruple::ArrLoad(_, _, _) => todo!(),
            Quadruple::ArrStore(_, _, _) => todo!(),
            Quadruple::DerefLoad(_, _) => todo!(),
            Quadruple::DerefStore(_, _) => todo!(),
        };
        Ok(())
    }
}

fn emit_start(out: &mut impl Write, state: &mut AsmGenState, main_frame: &Frame) -> AsmGenResult {
    writeln!(out, "global _start")?;
    Label::Func(Ident::from("_start".to_string())).emit(out)?;

    let stack_growth = main_frame.frame_size - RETADDR_SIZE;
    if stack_growth != 0 {
        state.advance_rsp(stack_growth as isize).emit(out)?;
    }

    Instr::Call(Label::Func(Ident::from("main".to_string()))).emit(out)?;

    if stack_growth != 0 {
        state.reset_rsp().emit(out)?;
    }

    Instr::MovToReg(RDI, Val::Reg(RAX)).emit(out)?;
    Instr::MovToReg(RAX, Val::Instant(SYS_EXIT)).emit(out)?;
    Instr::Syscall.emit(out)?;

    Ok(())
}
