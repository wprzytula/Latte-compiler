use std::mem;

use hashbrown::HashSet;
use log::{debug, info, trace};
use vector_map::{set::VecSet, VecMap};

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
pub(crate) struct FlowAnalysis {
    /*before_nth_quadruple*/ pub live_variables: Vec<VecSet<Var>>,
    /*before_nth_quadruple*/ pub next_use: Vec<VecMap<Var, usize>>,
    live_variables_analysis_state: LiveVarsAnalysisState,
    // TODO: reaching definitions?
}

impl Default for FlowAnalysis {
    fn default() -> Self {
        Self {
            live_variables: Default::default(),
            live_variables_analysis_state: LiveVarsAnalysisState::NotYet,
            next_use: Default::default(),
        }
    }
}

impl FlowAnalysis {
    pub(crate) fn live_before_end_type(&self) -> &VecSet<Var> {
        assert!(matches!(
            self.live_variables_analysis_state,
            LiveVarsAnalysisState::Finished
        ));
        self.live_variables
            .get(self.live_variables.len() - 2)
            .unwrap()
    }

    fn init_and_perform_local_analysis(
        &mut self,
        quadruples: &[Quadruple],
        end_type: &Option<EndType>,
    ) {
        assert!(matches!(
            self.live_variables_analysis_state,
            LiveVarsAnalysisState::NotYet
        ));
        self.live_variables.resize(
            quadruples.len() + 2, /*before and after End*/
            Default::default(),
        );
        self.next_use.resize(
            quadruples.len() + 2, /*before and after End*/
            VecMap::new(),
        );

        // Liveness - with next use
        // in[S] = (out [S] − kill [S]) ∪ use[S]
        // live before kth = (live after kth - lost during kth) ∪ read in kth
        let mut next_use = VecMap::<Var, usize>::new();
        if let Some(end_type) = end_type {
            match *end_type {
                EndType::Goto(_) => (),
                EndType::Return(None) => (),
                EndType::Return(Some(Value::Instant(_))) => (),
                EndType::Return(Some(Value::Variable(var))) => {
                    next_use.insert(var, 0);
                }
                EndType::IfElse(var, _, val, _, _) => {
                    next_use.insert(var, 0);
                    if let Value::Variable(var) = val {
                        next_use.insert(var, 0);
                    }
                }
            }
        }
        self.live_variables[quadruples.len()] = next_use.keys().copied().collect();
        self.next_use[quadruples.len()] = next_use.clone();

        let mut block_kill = VecSet::<Var>::new();
        for (idx, quadruple) in quadruples.iter().enumerate().rev() {
            Self::update_kill_and_next_use(quadruple, &mut next_use, &mut block_kill);
            self.live_variables[idx] = next_use.keys().copied().collect();
            self.next_use[idx] = next_use.clone();
        }

        self.live_variables_analysis_state = LiveVarsAnalysisState::OngoingGlobal {
            block_kill,
            block_use: self.live_variables[0].clone(),
        };
    }

    fn update_kill_and_use(
        quadruple: &Quadruple,
        live: &mut VecSet<Var>,
        block_kill: &mut VecSet<Var>,
    ) {
        // kill
        match quadruple {
            Quadruple::BinOp(var, _, _, _)
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
            Quadruple::BinOp(_, var2, _, val) => {
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
    }

    fn update_kill_and_next_use(
        quadruple: &Quadruple,
        next_use: &mut VecMap<Var, usize>,
        block_kill: &mut VecSet<Var>,
    ) {
        // kill
        match quadruple {
            Quadruple::BinOp(var, _, _, _)
            | Quadruple::UnOp(var, _, _)
            | Quadruple::Copy(var, _)
            | Quadruple::Set(var, _)
            | Quadruple::GetStrLit(var, _)
            | Quadruple::Call(var, _, _)
            | Quadruple::VirtualCall(var, _, _, _)
            | Quadruple::DerefLoad(var, _) => {
                next_use.remove(var);
                block_kill.insert(*var);
            }
            Quadruple::DerefStore(_, _) => (),
            Quadruple::VstStore(_, _) => (),
            Quadruple::InPlaceUnOp(_, _) => (),
            Quadruple::ArrLoad(_, _, _) => todo!(),
            Quadruple::ArrStore(_, _, _) => todo!(),
        }

        // bump up next_use distance
        for (_, dist) in next_use.iter_mut() {
            *dist += 1;
        }

        // use
        match quadruple {
            Quadruple::BinOp(_, var2, _, val) => {
                next_use.insert(*var2, 0);
                block_kill.remove(var2);
                if let Value::Variable(var) = val {
                    next_use.insert(*var, 0);
                    block_kill.remove(var);
                }
            }
            Quadruple::UnOp(_, _, val) => {
                if let Value::Variable(var) = val {
                    next_use.insert(*var, 0);
                    block_kill.remove(var);
                }
            }
            Quadruple::Copy(_, var2) => {
                next_use.insert(*var2, 0);
                block_kill.remove(var2);
            }
            Quadruple::GetStrLit(_, _) | Quadruple::Set(_, _) => (),

            Quadruple::ArrLoad(_, _, _) => todo!(),
            Quadruple::ArrStore(_, _, _) => todo!(),

            Quadruple::DerefLoad(_, mem) => {
                next_use.insert(mem.base, 0);
                block_kill.remove(&mem.base);
            }
            Quadruple::DerefStore(val, mem) => {
                next_use.insert(mem.base, 0);
                block_kill.remove(&mem.base);
                if let Value::Variable(var) = val {
                    next_use.insert(*var, 0);
                    block_kill.remove(var);
                }
            }
            Quadruple::Call(_, _, args) => {
                for arg in args {
                    if let Value::Variable(var) = arg {
                        next_use.insert(*var, 0);
                        block_kill.remove(var);
                    }
                }
            }
            Quadruple::InPlaceUnOp(_, loc) => {
                next_use.insert(loc.var(), 0);
                block_kill.remove(&loc.var());
            }
            Quadruple::VstStore(_, mem) => {
                next_use.insert(mem.base, 0);
                block_kill.remove(&mem.base);
            }
            Quadruple::VirtualCall(_, obj_var, _, args) => {
                next_use.insert(*obj_var, 0);
                block_kill.remove(obj_var);
                for arg in args {
                    if let Value::Variable(var) = arg {
                        next_use.insert(*var, 0);
                        block_kill.remove(var);
                    }
                }
            }
        };
    }

    fn update_local_liveness_with_global_fixpoint(
        &mut self,
        quadruples: &[Quadruple],
        end_type: &Option<EndType>,
    ) {
        let (live_in, live_out) = if let LiveVarsAnalysisState::DoneGlobal { live_in, live_out } =
            mem::replace(
                &mut self.live_variables_analysis_state,
                LiveVarsAnalysisState::Finished,
            ) {
            (live_in, live_out)
        } else {
            panic!("Global liveness analysis not yet performed");
        };

        self.live_variables[quadruples.len() + 1] = live_out.clone();

        // Liveness
        // in[S] = (out [S] − kill [S]) ∪ use[S]
        // live before kth = (live after kth - lost during kth) ∪ read in kth
        let mut live = live_out;
        if let Some(end_type) = end_type {
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
            }
        }
        self.live_variables[quadruples.len()] = live.clone();

        let mut block_kill = VecSet::<Var>::new();

        for (idx, quadruple) in quadruples.iter().enumerate().rev() {
            Self::update_kill_and_use(quadruple, &mut live, &mut block_kill);
            self.live_variables[idx] = live.clone();
        }
        assert_eq!(&self.live_variables[0], &live_in);
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

            trace!(
                "dfs_liveness_iter(): entered {:?}:{:?}",
                current,
                &cfg[current].kind
            );

            let change_in_successors = cfg[current]
                .successors
                .iter()
                .copied()
                .map(|succ| dfs_liveness_iter(cfg, visited, live_in, live_out, succ))
                .any(|i| i);

            let len_in_before = live_in[current.0].len();
            let len_out_before = live_out[current.0].len();

            if let LiveVarsAnalysisState::OngoingGlobal {
                block_kill,
                block_use,
            } = &cfg[current].flow_analysis.live_variables_analysis_state
            {
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
            debug!("{} iteration over function {}", i, &self[entry_idx]._func);
            visited.clear();
            while dfs_liveness_iter(self, &mut visited, &mut live_in, &mut live_out, entry_idx) {
                visited.clear();
                i += 1;
                debug!("{} iteration over function {}", i, &self[entry_idx]._func);
            }
        }

        debug!(
            "Live in: {:?}",
            live_in.iter().enumerate().collect::<Vec<_>>()
        );
        debug!(
            "Live out: {:?}",
            live_out.iter().enumerate().collect::<Vec<_>>()
        );

        for (block, (live_in, live_out)) in self
            .blocks
            .iter_mut()
            .zip(live_in.into_iter().zip(live_out.into_iter()))
        {
            block.flow_analysis.live_variables_analysis_state =
                LiveVarsAnalysisState::DoneGlobal { live_in, live_out };
        }
    }

    pub(super) fn liveness_analysis(&mut self) {
        info!("----- BEGINNING LIVENESS ANALYSIS -------");
        for block in self.blocks.iter_mut() {
            block
                .flow_analysis
                .init_and_perform_local_analysis(&block.quadruples, &block.end_type);
        }
        self.perform_global_analysis();
        for block in self.blocks.iter_mut() {
            block
                .flow_analysis
                .update_local_liveness_with_global_fixpoint(&block.quadruples, &block.end_type);
        }
    }
}
