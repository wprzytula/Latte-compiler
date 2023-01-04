use vector_map::VecMap;

use super::{BasicBlockIdx, state::State, CFG, Var, Quadruple, Value, EndType};

fn rename_var(var: &mut Var, prev: Var, current: Var) {
    if *var == prev {
        *var = current;
        eprintln!("Renaming usage Var {} -> {}", prev.0, current.0);
    }
}
fn rename_val(val: &mut Value, prev: Var, current: Var) {
    if let Value::Variable(var) = val {
        rename_var(var, prev, current)
    }
}

impl Quadruple {
    fn rename_usages(&mut self, prev: Var, current: Var) {
        match self {
            Quadruple::BinOp(_, var1, _, val) | Quadruple::RelOp(_, var1, _, val) => {
                rename_var(var1, prev, current);
                rename_val(val, prev, current);
            }
            Quadruple::UnOp(_, _, val) => rename_val(val, prev, current),
            Quadruple::Copy(_, var) => rename_var(var, prev, current),
            Quadruple::Set(_, _) => (),
            Quadruple::Call(_, _, vals) => {
                for val in vals {
                    rename_val(val, prev, current);
                }
            }
            Quadruple::ArrLoad(_, _, _) => todo!(),
            Quadruple::ArrStore(_, _, _) => todo!(),
            Quadruple::DerefLoad(_, _) => todo!(),
            Quadruple::DerefStore(_, _) => todo!(),
        }
    }

    fn assigns_to_var(&self, var: Var) -> bool {
        match self {
            Quadruple::BinOp(ass, _, _, _)
            | Quadruple::RelOp(ass, _, _, _)
            | Quadruple::UnOp(ass, _, _)
            | Quadruple::Copy(ass, _)
            | Quadruple::Set(ass, _)
            | Quadruple::Call(ass, _, _) => *ass == var,
            Quadruple::ArrLoad(_, _, _) => todo!(),
            Quadruple::ArrStore(_, _, _) => todo!(),
            Quadruple::DerefLoad(_, _) => todo!(),
            Quadruple::DerefStore(_, _) => todo!(),
        }
    }

    fn rename_assignment(&mut self, prev: Var, current: Var) {
        eprintln!("Renaming assignment of Var {} -> {}", prev.0, current.0);
        assert!(self.assigns_to_var(prev));
        match self {
            Quadruple::BinOp(ass, _, _, _)
            | Quadruple::RelOp(ass, _, _, _)
            | Quadruple::UnOp(ass, _, _)
            | Quadruple::Copy(ass, _)
            | Quadruple::Set(ass, _)
            | Quadruple::Call(ass, _, _) => *ass = current,
            Quadruple::ArrLoad(_, _, _) => todo!(),
            Quadruple::ArrStore(_, _, _) => todo!(),
            Quadruple::DerefLoad(_, _) => todo!(),
            Quadruple::DerefStore(_, _) => todo!(),
        }
    }
}

impl EndType {
    fn rename_usages(&mut self, prev: Var, current: Var) {
        match self {
            EndType::IfElse(var, _, _) => rename_var(var, prev, current),
            EndType::Return(Some(val)) => rename_val(val, prev, current),
            EndType::Return(None) => (),
            EndType::Goto(_) => (),
        }
    }
}

impl CFG {
    fn make_ssa(&mut self, state: &mut State) {
        let mut phi_to_replace = vec![];

        for (idx, block) in self.blocks.iter_mut().enumerate() {
            let this = BasicBlockIdx(idx);
            let vars = block.all_variables();

            if block.predecessors.len() > 1 {
                // insert (possibly reduntant) phi nodes
                for var in vars.iter().copied() {
                    block.phi_nodes.insert(
                        var,
                        block.predecessors.iter().map(|pred| (*pred, var)).collect(),
                    );
                }
            }

            for var in vars.iter().copied() {
                let mut current;

                // proceed with renaming variables

                if block.predecessors.len() > 1 {
                    // in case of a phi node block, variables are defined in phi nodes.
                    let phi_node = block.phi_nodes.remove(&var).unwrap();
                    current = state.fresh_reg(None);
                    block.phi_nodes.insert(current, phi_node);
                } else {
                    // in case of a non-phi node block, variables are defined first in quadruples, so we need to rename them there.
                    // but we don't rename variable uses until the first definition of that variable. Hence we do noop rename here.
                    current = var;
                }

                for quadruple in block.quadruples.iter_mut() {
                    quadruple.rename_usages(var, current);
                    if quadruple.assigns_to_var(var) {
                        current = state.fresh_reg(None);
                        quadruple.rename_assignment(var, current);
                    }
                }

                if current != var {
                    // if we did rename some variable, let's rename end info as well.
                    if let Some(ref mut end_info) = block.end_type {
                        end_info.rename_usages(var, current)
                    }

                    // if we did rename some variable, let's propagate the change to our successors' phi nodes.
                    for succ in block.successors.iter().copied() {
                        phi_to_replace.push((succ, this, var, current));
                    }
                }
            }
        }

        // Actually update successors' phi nodes.
        for (succ, this, var, current) in phi_to_replace {
            for (_, phi_node) in self[succ].phi_nodes.iter_mut() {
                for used in phi_node
                    .iter_mut()
                    .filter_map(|(pred, then)| (*pred == this).then(|| then))
                    .filter(|used| **used == var)
                {
                    *used = current;
                }
            }
        }
    }

    /// Removes redundant variables and their corresponding phi nodes.
    fn optimise_ssa(&mut self) {
        println!("Removing redundant variables:");

        fn exactly_one_mapping(phi_node: &VecMap<BasicBlockIdx, Var>) -> Option<Var> {
            let mut one_mapping = None;
            for (_edge, mapping) in phi_node {
                match one_mapping {
                    Some(one_mapping) if one_mapping == *mapping => (),
                    Some(_) => return None,
                    None => one_mapping = Some(*mapping),
                }
            }
            one_mapping
        }

        let redundant_phi_nodes = self
            .blocks
            .iter()
            .map(|block| {
                block.phi_nodes.iter().filter_map(|(var, phi_node)| {
                    exactly_one_mapping(phi_node).map(|mapping| {
                        (*var, mapping) // (to_be_replaced, replaced_with)
                    })
                })
            })
            .flatten();

        let redundant_copied_vars = self
            .blocks
            .iter()
            .map(|block| {
                block.quadruples.iter().filter_map(|quadruple| {
                    if let Quadruple::Copy(to, from) = quadruple {
                        Some((*to, *from))
                    } else {
                        None
                    }
                })
            })
            .flatten();

        let redundant_vars = redundant_phi_nodes
            .chain(redundant_copied_vars)
            .collect::<Vec<_>>();

        for (redundant_var, replacement_var) in redundant_vars {
            println!(
                "Removing redundant var {}, replacing with {}.",
                redundant_var.0, replacement_var.0
            );
            for block in self.blocks.iter_mut() {
                block.phi_nodes.remove(&redundant_var);
                for (_, phi_node) in block.phi_nodes.iter_mut() {
                    for (_, var) in phi_node.iter_mut() {
                        rename_var(var, redundant_var, replacement_var)
                    }
                }
                for quadruple in block.quadruples.iter_mut() {
                    quadruple.rename_usages(redundant_var, replacement_var);
                }
                block.quadruples.retain(|quadruple| match *quadruple {
                    Quadruple::Copy(to, from) if to == redundant_var => {
                        assert_eq!(from, replacement_var);
                        false
                    }
                    _ => true,
                });
                if let Some(ref mut end_info) = block.end_type {
                    end_info.rename_usages(redundant_var, replacement_var);
                }
            }
        }
    }
}