use hashbrown::HashSet;
use log::{debug, trace};
use vector_map::set::VecSet;

use crate::backend::ir::{BasicBlockIdx, Value};

use super::{EndType, Quadruple, Var, CFG};

#[derive(Debug)]
pub enum LiveVarsAnalysisState {
    NotYet,
    OngoingGlobal {
        block_kill: VecSet<Var>, // vars that the block defines and never uses before some definition
        block_use: VecSet<Var>,  // vars that the block uses before it ever defines them
    },
    DoneGlobal {
        live_in: VecSet<Var>,
        live_out: VecSet<Var>,
    },
    Finished,
}

#[derive(Debug)]
pub struct FlowAnalysis {
    /*before_nth_quadruple*/ live_variables: Vec<VecSet<Var>>,
    live_variables_analysis_state: LiveVarsAnalysisState,
    // TODO: reaching definitions?
}

impl Default for FlowAnalysis {
    fn default() -> Self {
        Self {
            live_variables: Default::default(),
            live_variables_analysis_state: LiveVarsAnalysisState::NotYet,
        }
    }
}

impl FlowAnalysis {
    fn init_and_perform_local_analysis(&mut self, quadruples: &[Quadruple], end_type: &EndType) {
        assert!(matches!(
            self.live_variables_analysis_state,
            LiveVarsAnalysisState::NotYet
        ));
        self.live_variables
            .resize(quadruples.len() + 1, Default::default());

        // Liveness
        // in[S] = (out [S] − kill [S]) ∪ use[S]
        // live before kth = (live after kth - lost during kth) ∪ read in kth
        let mut live = VecSet::<Var>::new();
        match *end_type {
            EndType::Goto(_) => (),
            EndType::Return(None) => (),
            EndType::Return(Some(Value::Instant(_))) => (),
            EndType::Return(Some(Value::Variable(var))) => {
                live.insert(var);
            }
            EndType::IfElse(var, _, val, _, _) => {
                live.insert(var);
                if let Value::Variable(var) = val {
                    live.insert(var);
                }
            }
        };
        let mut block_kill = VecSet::<Var>::new();
        self.live_variables[quadruples.len()] = live.clone();
        for (idx, quadruple) in quadruples.iter().enumerate() {
            // kill
            match quadruple {
                Quadruple::BinOp(var, _, _, _)
                | Quadruple::RelOp(var, _, _, _)
                | Quadruple::UnOp(var, _, _)
                | Quadruple::Copy(var, _)
                | Quadruple::Set(var, _)
                | Quadruple::GetStrLit(var, _)
                | Quadruple::Call(var, _, _)
                | Quadruple::VirtualCall(var, _, _, _)
                | Quadruple::DerefLoad(var, _) => {
                    live.remove(var);
                    block_kill.insert(*var);
                }
                Quadruple::DerefStore(_, _) => (),
                Quadruple::VstStore(_, _) => (),
                Quadruple::InPlaceUnOp(_, _) => (),
                Quadruple::ArrLoad(_, _, _) => todo!(),
                Quadruple::ArrStore(_, _, _) => todo!(),
            }

            // use
            match quadruple {
                Quadruple::BinOp(_, var2, _, val) | Quadruple::RelOp(_, var2, _, val) => {
                    live.insert(*var2);
                    block_kill.remove(var2);
                    if let Value::Variable(var) = val {
                        live.insert(*var);
                        block_kill.remove(var);
                    }
                }
                Quadruple::UnOp(_, _, val) => {
                    if let Value::Variable(var) = val {
                        live.insert(*var);
                        block_kill.remove(var);
                    }
                }
                Quadruple::Copy(_, var2) => {
                    live.insert(*var2);
                    block_kill.remove(var2);
                }
                Quadruple::GetStrLit(_, _) | Quadruple::Set(_, _) => (),

                Quadruple::ArrLoad(_, _, _) => todo!(),
                Quadruple::ArrStore(_, _, _) => todo!(),

                Quadruple::DerefLoad(_, mem) => {
                    live.insert(mem.base);
                    block_kill.remove(&mem.base);
                }
                Quadruple::DerefStore(val, mem) => {
                    live.insert(mem.base);
                    block_kill.remove(&mem.base);
                    if let Value::Variable(var) = val {
                        live.insert(*var);
                        block_kill.remove(var);
                    }
                }
                Quadruple::Call(_, _, args) => {
                    for arg in args {
                        if let Value::Variable(var) = arg {
                            live.insert(*var);
                            block_kill.remove(var);
                        }
                    }
                }
                Quadruple::InPlaceUnOp(_, loc) => {
                    live.insert(loc.var());
                    block_kill.remove(&loc.var());
                }
                Quadruple::VstStore(_, mem) => {
                    live.insert(mem.base);
                    block_kill.remove(&mem.base);
                }
                Quadruple::VirtualCall(_, obj_var, _, args) => {
                    live.insert(*obj_var);
                    block_kill.remove(obj_var);
                    for arg in args {
                        if let Value::Variable(var) = arg {
                            live.insert(*var);
                            block_kill.remove(var);
                        }
                    }
                }
            };

            self.live_variables[idx] = live.clone();
        }

        self.live_variables_analysis_state = LiveVarsAnalysisState::OngoingGlobal {
            block_kill,
            block_use: self.live_variables[0].clone(),
        };
    }
}

impl CFG {
    fn perform_global_analysis(&mut self) {
        let mut live_out = vec![VecSet::<Var>::new(); self.blocks.len()];
        let mut live_in = live_out.clone();

        fn dfs_liveness_iter(
            cfg: &CFG,
            visited: &mut HashSet<BasicBlockIdx>,
            live_in: &mut Vec<VecSet<Var>>,
            live_out: &mut Vec<VecSet<Var>>,
            current: BasicBlockIdx,
        ) -> bool /*Something changed*/ {
            if visited.contains(&current) {
                return false;
            } else {
                visited.insert(current);
            }

            trace!("dfs_liveness_iter(): entered {:?}", current);

            let change_in_successors = cfg[current]
                .successors
                .iter()
                .copied()
                .map(|succ| dfs_liveness_iter(cfg, visited, live_in, live_out, succ))
                .any(|i| i);

            let len_in_before = live_in[current.0].len();
            let len_out_before = live_out[current.0].len();

            if let LiveVarsAnalysisState::OngoingGlobal { block_kill, block_use } = &cfg[current].flow_analysis.live_variables_analysis_state {
                // in[n] := use[n] ∪ (out[n] \ kill[n]);
                live_in[current.0].extend(live_out[current.0].iter().copied());
                live_in[current.0].retain(|var| !block_kill.contains(var));
                live_in[current.0].extend(block_use.iter().copied());

                for succ in cfg[current].successors.iter().copied() {
                    // out[n] += s ∈ succ(n)
                    live_out[current.0].extend(live_in[succ.0].iter().copied())
                }
            } else {
                panic!("Local analysis not yet performed.");
            }

            let len_in_after = live_in[current.0].len();
            let len_out_after = live_out[current.0].len();

            change_in_successors || len_in_before < len_in_after || len_out_before < len_out_after
        }

        let mut visited = HashSet::<BasicBlockIdx>::new();
        for entry_idx in self.blocks.iter().filter_map(|b| b.entry.then_some(b._idx)) {
            let mut i = 0;
            while dfs_liveness_iter(self, &mut visited, &mut live_in, &mut live_out, entry_idx) {
                debug!("{} iteration over function {}", i, &self[entry_idx]._func);
                i += 1;
            }
        }

        debug!(
            "Live in: {:#?}",
            live_in.iter().enumerate().collect::<Vec<_>>()
        );
        debug!(
            "Live out: {:#?}",
            live_out.iter().enumerate().collect::<Vec<_>>()
        );

        for (block, (live_in, live_out)) in self.blocks.iter_mut().zip(
            live_in.into_iter().zip(live_out.into_iter())
        ) {
            block.flow_analysis.live_variables_analysis_state = LiveVarsAnalysisState::DoneGlobal { live_in, live_out };
        }
    }
}
