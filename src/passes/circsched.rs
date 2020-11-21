use crate::arraydep::*;
use crate::ast::*;
use crate::helper::ast_qubit_iterator;
use bitvec::prelude::*;
use std::collections::BTreeMap;
// We perform ASAP schedule on prologue and epilogue.
//
pub fn asap_schedule(ast: AST, range: &Option<Range>) -> Vec<Vec<ASTNode>> {
    let mut place_ticks = Vec::new();
    place_ticks.resize(ast.len(), 0);
    for i in 0..ast.len() {
        let mut tick = 0;
        for j in 0..i {
            for qi in ast_qubit_iterator(Some(&ast[i]).into_iter()) {
                for qj in ast_qubit_iterator(Some(&ast[j]).into_iter()) {
                    if qi == qj || (qi.0 == qj.0 && dependency_check_in_loop(&qi.1, &qj.1, range)) {
                        tick = std::cmp::max(tick, place_ticks[j] + 1);
                    }
                }
            }
        }
        place_ticks[i]=tick;
    }
    let n = if place_ticks.len()==0{0}else{(place_ticks.iter().copied().into_iter().max().unwrap())+1};
    let mut time_slots = Vec::new();
    time_slots.resize(n, Vec::new());
    for (i, insn) in ast.into_iter().enumerate() {
        time_slots[place_ticks[i]].push(insn);
    }
    time_slots
}
