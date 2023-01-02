use std::{fmt::{Display, write}, io::{Write, self}, collections::HashMap};

use super::ir::{Instant, CFG};

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
        write!(f, "{}", match self {
            MemScale::One => 1,
            MemScale::Two => 2,
            MemScale::Four => 4,
            MemScale::Eight => 8,
        })
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
    displacement: Option<usize>,
}
impl Display for Mem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} [{}", self.word_len, self.base)?;
        if let Some(ref mem_index) = self.index {
            write!(f, ", {}", mem_index)?;
        }
        if let Some(displacement) = self.displacement {
            write!(f, ", {}", displacement)?;
        }
        write!(f, "]")
    }
}

enum Reg {
    CallerSave(CallerSaveReg),
    CalleeSave(CalleeSaveReg),
    Rip,
}
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

#[derive(Clone, Copy)]
pub struct Var(usize);

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Label(usize);
impl Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "label_{}", self.0)
    }
}
impl Label {
    fn emit(&self, out: &mut impl Write) -> io::Result<()> {
        writeln!(out, "{}:", self)
    }
}

struct State<'a> {
    vars: HashMap<&'a str, Var>,
    next_var_idx: usize,
    next_label_idx: usize,
}

impl<'a> State<'a> {
    fn new() -> Self {
        Self{ vars: HashMap::new(), next_var_idx: 0, next_label_idx: 0}
    }

    fn declare_variable(&mut self, var: &'a str) {
        if !self.vars.contains_key(var) {
            self.vars.insert(var, Var(self.next_var_idx));
            self.next_var_idx += 1;
        }
    }

    fn get_var_idx(&self, var: &'a str) -> Var {
        *self.vars.get(var).unwrap()
    }

    fn gen_label(&mut self) -> Label {
        let label = Label(self.next_label_idx);
        self.next_label_idx += 1;
        label
    }
}

enum Instr {
    Comm(usize),

    Jmp(Label),
    Jz(Label),
    Jnz(Label),
    Jg(Label),
    Jge(Label),
    Jl(Label),
    Jle(Label),

    MovToReg(Reg, Val),
    MovToMem(Mem, Reg),

    Test(Reg),
    Cmp(Reg, Val),
    Add(Reg, Val),
    Sub(Reg, Val),
    IMul(Reg, Val),
    IDiv(Val), // RDX:RAX divided by Val, quotient in RAX, remainder in RDX. 
    Cqo, // Prepared RDX as empty for IDiv
    Inc(Reg),
    Dec(Reg),
    Neg(Reg),
    And(Reg, Val),
    Or(Reg, Val),
    Xor(Reg, Val),
    Sal(Reg, Instant),
    Sar(Reg, Instant),

    Lea(Reg, Mem),

    Store(Var, Reg),
    Load(Reg, Var),
    
    Push(Val),
    Pop(Reg),

    Ret,
    Call(Label),
}

impl Instr {
    fn emit(&self, out: &mut impl Write) -> io::Result<()> {
        write!(out, "\t")?;
        match self {
            Instr::Comm(i) => writeln!(out, ".comm vars, {}, 32", i),

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
            Instr::IDiv(val) => writeln!(out, "idiv {}", val),
            Instr::Cqo => writeln!(out, "cqo"),
            Instr::Inc(reg) => writeln!(out, "inc {}", reg),
            Instr::Dec(reg) => writeln!(out, "dec {}", reg),
            Instr::Neg(reg) => writeln!(out, "neg {}", reg),
            Instr::And(reg, val) => writeln!(out, "sub {}, {}", reg, val),
            Instr::Or(reg, val) => writeln!(out, "or {}, {}", reg, val),
            Instr::Xor(reg, val) => writeln!(out, "xor {}, {}", reg, val),
            Instr::Sal(reg, i) => writeln!(out, "sal {}, {}", reg, **i),
            Instr::Sar(reg, i) => writeln!(out, "sar {}, {}", reg, **i),

            Instr::Lea(reg, mem) => writeln!(out, "lea {}, {}", reg, mem),

            Instr::Store(var, reg) => writeln!(out, "mov QWORD [vars + {}], {}", var.0, reg),
            Instr::Load(reg, var) => writeln!(out, "mov {}, [vars + {}]", reg, var.0),

            Instr::Push(val) => writeln!(out, "push {}", val),
            Instr::Pop(reg) => writeln!(out, "pop {}", reg),

            Instr::Ret => writeln!(out, "ret"),
            Instr::Call(label) => writeln!(out, "call {}", label),
        }
    }
}


impl CFG {
    pub fn emit_assembly(out: &mut impl Write) -> io::Result<()> {



        Ok(())
    }
}