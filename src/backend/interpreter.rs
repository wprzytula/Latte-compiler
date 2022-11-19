use std::collections::HashMap;

use crate::frontend::{Exp, Stmt, VarName};
type State = HashMap<VarName, i32>;

fn eval(state: &State, e: &Exp) -> i32 {
    match e {
        Exp::Lit(i) => *i,
        Exp::VarRef(var) => state.get(var).copied().unwrap_or(0),
        Exp::Mul(l, r) => eval(state, l) * eval(state, r),
        Exp::Sub(l, r) => eval(state, l) - eval(state, r),
        Exp::Add(l, r) => eval(state, l) + eval(state, r),
        Exp::Div(l, r) => eval(state, l) / eval(state, r),
    }
}

pub fn interpret(prog: impl Iterator<Item = Stmt<Exp>>) {
    let mut vars = State::new();
    for stmt in prog {
        match stmt {
            Stmt::Print(e) => {
                println!("{}", eval(&vars, &e));
            }
            Stmt::Ass(var, e) => {
                let val = eval(&vars, &e);
                vars.insert(var, val);
            }
        }
    }
}
