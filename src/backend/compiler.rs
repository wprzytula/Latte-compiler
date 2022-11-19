use std::fmt::{self, Display};

use crate::frontend::{semantic_analysis::SymbolTable as SymbolTableGeneric, Exp, Prog as AST};
use generator::{CodeDisplay, Generator};
type SymbolTable = SymbolTableGeneric<Register>;

use self::generator::{
    generate_llvm_code_for_allocas, generate_llvm_code_for_stmt, RegNumAssigner,
};

#[derive(Clone, Copy)]
pub struct Register(usize);
impl Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%r{}", self.0)
    }
}

pub enum LlvmOperand {
    Immediate(i32),
    Register(Register),
}

impl fmt::Display for LlvmOperand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LlvmOperand::Immediate(i) => write!(f, "{}", i),
            LlvmOperand::Register(reg) => write!(f, "{}", reg),
        }
    }
}

pub struct LlvmInstruction {
    kind: LlvmInstructionKind,
    register: Option<Register>,
}

impl fmt::Display for LlvmInstruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(reg) = &self.register {
            write!(f, "{} = ", reg)?;
        }
        match &self.kind {
            LlvmInstructionKind::Br => todo!(),
            LlvmInstructionKind::Ret(i) => writeln!(f, "ret i32 {}", i),
            LlvmInstructionKind::Op(op, l, r) => writeln!(f, "{} i32 {}, {}", op, l, r),
            LlvmInstructionKind::Alloca => writeln!(f, "alloca i32"),
            LlvmInstructionKind::Store(val, ptr) => writeln!(f, "store i32 {}, i32* {}", val, ptr),
            LlvmInstructionKind::Load(ptr) => writeln!(f, "load i32, i32* {}", ptr),
            LlvmInstructionKind::CallPrint(reg) => writeln!(f, "call void @printInt(i32 {})", reg),
        }
    }
}

pub enum LlvmInstructionKind {
    Br,
    Ret(LlvmOperand),
    Op(LlvmOp, LlvmOperand, LlvmOperand),
    Alloca,
    Store(LlvmOperand, Register), // value, ptr
    Load(Register),               // ptr
    CallPrint(LlvmOperand),
}

impl LlvmInstructionKind {}

pub enum LlvmOp {
    Add,
    Mul,
    Sub,
    Div,
}

impl fmt::Display for LlvmOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            LlvmOp::Add => "add",
            LlvmOp::Mul => "mul",
            LlvmOp::Sub => "sub",
            LlvmOp::Div => "sdiv",
        })
    }
}

#[allow(dead_code)]
pub struct LlvmBlock<It: Iterator<Item = LlvmInstruction>> {
    reg: Register,
    instructions: It,
    terminator: LlvmInstruction,
}

mod generator {
    use std::{fmt, iter};

    use crate::{
        backend::compiler::{LlvmOp, LlvmOperand},
        frontend::semantic_analysis::VarIdx,
    };
    use either::Either;

    use crate::frontend::{Exp, Stmt};

    use super::{LlvmInstruction, LlvmInstructionKind, Register, SymbolTable};

    pub enum CodeDisplay {
        Str(&'static str),
        Instr(LlvmInstruction),
    }

    impl fmt::Display for CodeDisplay {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                CodeDisplay::Str(s) => s.fmt(f),
                CodeDisplay::Instr(instr) => instr.fmt(f),
            }
        }
    }

    impl From<&'static str> for CodeDisplay {
        fn from(s: &'static str) -> Self {
            Self::Str(s)
        }
    }

    impl From<LlvmInstruction> for CodeDisplay {
        fn from(instr: LlvmInstruction) -> Self {
            Self::Instr(instr)
        }
    }

    pub struct Generator<Instructions>
    where
        Instructions: Iterator<Item = LlvmInstruction>,
    {
        state: GeneratorNext,
        instructions: Instructions,
    }

    impl<Instructions> Generator<Instructions>
    where
        Instructions: Iterator<Item = LlvmInstruction>,
    {
        pub(crate) fn new(instructions: Instructions) -> Self {
            Self {
                state: GeneratorNext::FactoryNew,
                instructions,
            }
        }
    }

    enum GeneratorNext {
        FactoryNew,
        Instruction,
        MainEnd,
    }

    impl<Instructions> Iterator for Generator<Instructions>
    where
        Instructions: Iterator<Item = LlvmInstruction>,
    {
        type Item = CodeDisplay;

        fn next(&mut self) -> Option<Self::Item> {
            match self.state {
                GeneratorNext::FactoryNew => {
                    self.state = GeneratorNext::Instruction;
                    Some(
                        "
declare void @printInt(i32)
define i32 @main() {
"
                        .into(),
                    )
                }
                GeneratorNext::Instruction => {
                    if let Some(instr) = self.instructions.next() {
                        Some(instr.into())
                    } else {
                        self.state = GeneratorNext::MainEnd;
                        Some(
                            "ret i32 0
}
"
                            .into(),
                        )
                    }
                }
                GeneratorNext::MainEnd => None,
            }
        }
    }

    trait ImmediateExp {
        fn immediate(&self) -> Option<i32>;
    }
    impl ImmediateExp for Exp {
        fn immediate(&self) -> Option<i32> {
            match self {
                Exp::Lit(i) => Some(*i),
                _ => None,
            }
        }
    }

    pub struct RegNumAssigner(usize);
    impl RegNumAssigner {
        pub fn next_num(&mut self) -> Register {
            let num = self.0;
            self.0 = num + 1;
            Register(num)
        }
        pub fn new(initial: usize) -> Self {
            Self(initial)
        }
    }

    impl VarIdx for Register {
        fn next_free_idx(&mut self) -> Self {
            let num = self.0;
            self.0 = num + 1;
            Register(num)
        }

        fn default() -> Self {
            Self(0)
        }
    }

    pub fn generate_llvm_code_for_stmt(
        symbol_table: &SymbolTable,
        reg_assigner: &mut RegNumAssigner,
        stmt: Stmt<Exp>,
        line: usize,
    ) -> impl Iterator<Item = LlvmInstruction> {
        match stmt {
            Stmt::Print(exp) => Either::Left({
                let exp_yield = generate_llvm_code_for_expr(symbol_table, reg_assigner, exp, line);
                match exp_yield {
                    ExprYield::Immediate(i) => Either::Left(iter::once(LlvmInstruction {
                        register: None,
                        kind: LlvmInstructionKind::CallPrint(LlvmOperand::Immediate(i)),
                    })),
                    ExprYield::Code(reg, code) => {
                        Either::Right(code.chain(iter::once(LlvmInstruction {
                            register: None,
                            kind: LlvmInstructionKind::CallPrint(LlvmOperand::Register(reg)),
                        })))
                    }
                }
                // print the result
            }),
            Stmt::Ass(var, exp) => Either::Right({
                let (var_id, _) = symbol_table.retrieve_var_id(&var).unwrap();
                let exp_yield = generate_llvm_code_for_expr(symbol_table, reg_assigner, exp, line);
                match exp_yield {
                    ExprYield::Immediate(i) => Either::Left(iter::once(LlvmInstruction {
                        register: None,
                        kind: LlvmInstructionKind::Store(LlvmOperand::Immediate(i), var_id),
                    })),
                    ExprYield::Code(reg, code) => {
                        Either::Right(code.chain(iter::once(LlvmInstruction {
                            register: None,
                            kind: LlvmInstructionKind::Store(LlvmOperand::Register(reg), var_id),
                        })))
                    }
                }
            }),
        }
    }

    enum ExpCodeIterator<LitIter, AddIter, MulIter, SubIter, DivIter>
    where
        LitIter: Iterator<Item = LlvmInstruction>,
        AddIter: Iterator<Item = LlvmInstruction>,
        MulIter: Iterator<Item = LlvmInstruction>,
        SubIter: Iterator<Item = LlvmInstruction>,
        DivIter: Iterator<Item = LlvmInstruction>,
    {
        Lit(LitIter),
        Add(AddIter),
        Mul(MulIter),
        Sub(SubIter),
        Div(DivIter),
    }
    impl<LitIter, AddIter, MulIter, SubIter, DivIter> Iterator
        for ExpCodeIterator<LitIter, AddIter, MulIter, SubIter, DivIter>
    where
        LitIter: Iterator<Item = LlvmInstruction>,
        AddIter: Iterator<Item = LlvmInstruction>,
        MulIter: Iterator<Item = LlvmInstruction>,
        SubIter: Iterator<Item = LlvmInstruction>,
        DivIter: Iterator<Item = LlvmInstruction>,
    {
        type Item = LlvmInstruction;
        fn next(&mut self) -> Option<Self::Item> {
            match self {
                ExpCodeIterator::Lit(it) => it.next(),
                ExpCodeIterator::Add(it) => it.next(),
                ExpCodeIterator::Mul(it) => it.next(),
                ExpCodeIterator::Sub(it) => it.next(),
                ExpCodeIterator::Div(it) => it.next(),
            }
        }
    }

    enum ExprYield<ExprIt: Iterator<Item = LlvmInstruction>> {
        Immediate(i32),
        Code(Register, ExprIt),
    }

    impl<ExprIt: Iterator<Item = LlvmInstruction>> ExprYield<ExprIt> {
        fn operand(&self) -> LlvmOperand {
            match self {
                ExprYield::Immediate(i) => LlvmOperand::Immediate(*i),
                ExprYield::Code(reg, _) => LlvmOperand::Register(*reg),
            }
        }
        fn code(self) -> impl Iterator<Item = LlvmInstruction> {
            match self {
                ExprYield::Immediate(_) => Either::Left(iter::empty()),
                ExprYield::Code(_, code) => Either::Right(code),
            }
        }
    }

    fn generate_llvm_code_for_expr(
        symbol_table: &SymbolTable,
        reg_assigner: &mut RegNumAssigner,
        exp: Exp,
        line: usize,
    ) -> ExprYield<impl Iterator<Item = LlvmInstruction>> {
        fn generate_for_exprs_with_op(
            symbol_table: &SymbolTable,
            reg_assigner: &mut RegNumAssigner,
            l: Exp,
            _is_commutative: bool,
            op: LlvmOp,
            r: Exp,
            line: usize,
        ) -> (Register, Box<dyn Iterator<Item = LlvmInstruction>>) {
            let l_yield = generate_llvm_code_for_expr(symbol_table, reg_assigner, l, line);
            let r_yield = generate_llvm_code_for_expr(symbol_table, reg_assigner, r, line);
            let l_operand = l_yield.operand();
            let r_operand = r_yield.operand();
            let reg = reg_assigner.next_num();

            (
                reg,
                Box::new(
                    l_yield
                        .code()
                        .chain(r_yield.code())
                        .chain(iter::once(LlvmInstruction {
                            register: Some(reg),
                            kind: LlvmInstructionKind::Op(op, l_operand, r_operand),
                        })),
                ),
            )
        }

        match exp {
            Exp::Lit(i) => ExprYield::Immediate(i),
            Exp::VarRef(var) => {
                let new_reg = reg_assigner.next_num();
                ExprYield::Code(
                    new_reg,
                    ExpCodeIterator::Lit(iter::once({
                        let (reg, assigned_line) = symbol_table.retrieve_var_id(&var).unwrap();
                        assert!(assigned_line < line, "Reference to an unassigned variable!");
                        LlvmInstruction {
                            register: Some(new_reg),
                            kind: LlvmInstructionKind::Load(reg),
                        }
                    })),
                )
            }
            Exp::Add(exp1, exp2) => {
                let (reg, code) = generate_for_exprs_with_op(
                    symbol_table,
                    reg_assigner,
                    *exp1,
                    true,
                    LlvmOp::Add,
                    *exp2,
                    line,
                );
                ExprYield::Code(reg, ExpCodeIterator::Add(code))
            }
            Exp::Mul(exp1, exp2) => {
                let (reg, code) = generate_for_exprs_with_op(
                    symbol_table,
                    reg_assigner,
                    *exp1,
                    true,
                    LlvmOp::Mul,
                    *exp2,
                    line,
                );
                ExprYield::Code(reg, ExpCodeIterator::Mul(code))
            }
            Exp::Sub(exp1, exp2) => {
                let (reg, code) = generate_for_exprs_with_op(
                    symbol_table,
                    reg_assigner,
                    *exp1,
                    false,
                    LlvmOp::Sub,
                    *exp2,
                    line,
                );
                ExprYield::Code(reg, ExpCodeIterator::Sub(code))
            }
            Exp::Div(exp1, exp2) => {
                let (reg, code) = generate_for_exprs_with_op(
                    symbol_table,
                    reg_assigner,
                    *exp1,
                    false,
                    LlvmOp::Div,
                    *exp2,
                    line,
                );
                ExprYield::Code(reg, ExpCodeIterator::Div(code))
            }
        }
    }

    pub fn generate_llvm_code_for_allocas(
        symbol_table: &SymbolTable,
    ) -> impl Iterator<Item = LlvmInstruction> {
        (0..symbol_table.variable_mapping.len()).map(|i| LlvmInstruction {
            kind: LlvmInstructionKind::Alloca,
            register: Some(Register(i)),
        })
    }
}

pub fn compile(ast: AST<Exp>) -> impl Iterator<Item = CodeDisplay> {
    let symbol_table = SymbolTable::build(&ast);
    let mut reg_assigner = RegNumAssigner::new(symbol_table.variable_mapping.len());

    let allocas = generate_llvm_code_for_allocas(&symbol_table);

    let instructions = ast
        .0
        .into_iter()
        .enumerate()
        .map(move |(line, stmt)| {
            generate_llvm_code_for_stmt(&symbol_table, &mut reg_assigner, stmt, line)
        })
        .flatten();

    Generator::new(allocas.chain(instructions))
}
