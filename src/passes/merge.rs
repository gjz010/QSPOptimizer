use crate::ast::*;
use bitvec::prelude::*;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use crate::arraydep::*;
use crate::helper::*;

use crate::ast::ASTNode::*;
use log::*;
enum MergeBehaviour {
    AppendAfter,
    MergeWith(usize),
    CancelWith(usize, bool),
}
use MergeBehaviour::*;
// Merging adjacent gates as possible as well as keeping order.
pub fn left_merge_adjacent_gates(ast: AST, range: Option<Range>) -> AST {
    macro_rules! greedy_merge {
        ([$name: ident, $name_ast: ident, $name_g1: ident, $name_g2: ident, $ret: ident] , $iter: tt, $merge_op: tt, $finalize: tt) => {
            fn $name(mut $name_ast: AST, range: Option<Range>) -> AST {
                trace!("{}", std::stringify!($name));
                //let mut new_instruction_list = LinkedList::new();
                let mut new_instruction_list=Vec::new();
                // CZ-buffer holds the latest added gates on the qubit: latest SQ and latest CZs.

                // Append a series of identities.
                for qubit in ast_qubit_iterator($name_ast.iter()){
                    new_instruction_list.push(SQ{qubit, gate: SQGate::new(), moved: false});
                }
                for insn in $name_ast.into_iter(){
                    let mut behav = AppendAfter;
                    match &insn{
                        VCZ{q1, q2, ..}=>{
                            for (c, p) in new_instruction_list.iter().enumerate(){
                                match p{
                                    VCZ{q1: q1_, q2: q2_, ..}=>{
                                        if (q1==q1_ && q2==q2_) || (q1==q2_ && q2==q1_){
                                            behav=CancelWith(c, q1==q1_ && q2==q2_);
                                        }
                                    }
                                    SQ{qubit, gate, ..}=>{
                                        match gate.get_type(){
                                            GateType::Diagonal=>{

                                            }
                                            GateType::AntiDiagonal=>{
                                                if q1==qubit || q2==qubit{
                                                    // no dependency
                                                }else{
                                                    // unknown appended-Z?
                                                    if (q1.0 == qubit.0 && dependency_check_in_loop(&q1.1, &qubit.1, &range)) ||
                                                    (q2.0 == qubit.0 && dependency_check_in_loop(&q2.1, &qubit.1, &range)){
                                                        behav=AppendAfter;
                                                    }
                                                }
                                            }
                                            GateType::General=>{
                                                if q1==qubit || q2==qubit || (q1.0 == qubit.0 && dependency_check_in_loop(&q1.1, &qubit.1, &range)) ||
                                                (q2.0 == qubit.0 && dependency_check_in_loop(&q2.1, &qubit.1, &range)){
                                                    behav=AppendAfter;
                                                }
                                            }
                                        }
                                    }
                                    _=>{unreachable!()}
                                }
                            }
                        }
                        SQ{qubit, gate, ..}=>{
                            let gate_type=gate.get_type();
                            for (c, p) in new_instruction_list.iter().enumerate(){
                                match p{
                                    VCZ{q1: q1, q2: q2, ..}=>{
                                        match gate_type{
                                            GateType::Diagonal=>{

                                            }
                                            GateType::AntiDiagonal=>{
                                                if q1==qubit || q2==qubit{
                                                    // no dependency
                                                }else{
                                                    // unknown appended-Z?
                                                    if (q1.0 == qubit.0 && dependency_check_in_loop(&q1.1, &qubit.1, &range)) ||
                                                    (q2.0 == qubit.0 && dependency_check_in_loop(&q2.1, &qubit.1, &range)){
                                                        behav=AppendAfter;
                                                    }
                                                }
                                            }
                                            GateType::General=>{
                                                if q1==qubit || q2==qubit || (q1.0 == qubit.0 && dependency_check_in_loop(&q1.1, &qubit.1, &range)) ||
                                                (q2.0 == qubit.0 && dependency_check_in_loop(&q2.1, &qubit.1, &range)){
                                                    behav=AppendAfter;
                                                }
                                            }
                                        }
                                    }
                                    SQ{qubit: qubit_, gate: gate_, ..}=>{
                                        if qubit==qubit_{
                                            behav=MergeWith(c);
                                        }else{
                                            if (qubit.0==qubit_.0 && dependency_check_in_loop(&qubit.1, &qubit_.1, &range) &&
                                                (gate.get_type()==GateType::General || gate_.get_type()==GateType::General)){
                                                behav=AppendAfter;
                                            }
                                        }
                                    }
                                    _=>{unreachable!()}
                                }
                            }
                        }
                    }
                    match behav{
                        MergeBehaviour::AppendAfter=>{
                            new_instruction_list.push(insn);
                        }
                        MergeBehaviour::MergeWith(cursor)=>{
                            if let (SQ{qubit: q1, gate: g1, moved: m1}, SQ{qubit: q2, gate: g2, moved: m2}) = (new_instruction_list.get_mut(cursor).unwrap(), insn){
                                let mut $name_g2=g2;
                                if g1.components.len()==0{
                                    *m1=m2;
                                }else{
                                    *m1=false;
                                }
                                let $name_g1=g1;
                                let t=$name_g2.get_type();
                                $merge_op
                                if t==GateType::AntiDiagonal{
                                    for i in (cursor+1..new_instruction_list.len()){
                                        match &mut new_instruction_list[i]{
                                            VCZ{q1: q1_, q2: q2_, cancelled, p1, p2, ..}=>{
                                                if !*cancelled{
                                                    if *q1_==q2 {*p1=!*p1}
                                                    if *q2_==q2 {*p2=!*p2}
                                                }
                                            }
                                            SQ{qubit, ..}=>{
                                                assert_eq!((*qubit == q2) || (qubit.0 == q2.0 && dependency_check_in_loop(&qubit.1, &q2.1, &range)), false, "Bad reordering while merging!");
                                            }
                                        }
                                        
                                    }
                                }

                            }else{
                                panic!("MergeWith happens, but not between two SQ gates.");
                            }
                        }
                        MergeBehaviour::CancelWith(cursor, _)=>{
                            if let VCZ{q1: q1_, q2: q2_, cancelled, p1, p2, moved} = &mut new_instruction_list[cursor]{
                                *cancelled=!*cancelled;
                                if *moved{
                                    trace!("Erasing rotation mark for CZ cancelling.");
                                }
                                *moved=false;
                            }else{
                                unreachable!();
                            }
                        }
                    }
                }

                let $ret=new_instruction_list;
                let tmp=$finalize;
                tmp.flat_map(|x| {
                    match x{
                        SQ{qubit, gate, moved}=>{
                            let o1=if gate.is_empty(){
                                if moved{
                                    trace!("Erasing rotation mark for SQ merging.");
                                }
                                None
                            }else{Some(SQ{gate, qubit, moved})};
                            let o2=None;
                            let o3=None;
                            o1.into_iter().chain(o2.into_iter()).chain(o3.into_iter())
                        }
                        VCZ{q1, q2, p1, p2, cancelled, moved}=>{
                            let o1=if cancelled{None} else {Some(ASTNode::VCZ{q1, q2, p1: false, p2: false, cancelled, moved})};
                            let o2=if p1 {
                                Some(SQ {
                                    gate: SQGate::singleton(Gate::known(GateSet::Z.matrix())),
                                    qubit: q2,moved: false
                                })
                            } else {
                                None
                            };
                            let o3=if p2 {
                                Some(SQ {
                                    gate: SQGate::singleton(Gate::known(GateSet::Z.matrix())),
                                    qubit: q1,moved: false
                                })
                            } else {
                                None
                            };
                            o1.into_iter().chain(o2.into_iter()).chain(o3.into_iter())
                        }
                        _=>unreachable!()
                    }.into_iter()
                }).collect()
            }
        };
    }
    greedy_merge!(
        [greedy_left_merge, ast, orig_gate, gate, ret],
        ast,
        { orig_gate.merge_sqgate(gate) },
        { ret.into_iter() }
    );
    greedy_merge!(
        [greedy_right_merge, ast, orig_gate, gate, ret],
        (ast.into_iter().rev()),
        { orig_gate.merge_sqgate_left(gate) },
        { ret.into_iter().rev() }
    );

    assert_eq!(ast.iter().any(|x| {
        match x{
            ASTNode::VCZ{p1, p2, ..}=>*p1 || *p2,
            _=>false
        }
    }), false);
    let ast = greedy_left_merge(
        greedy_left_merge(
            greedy_right_merge(greedy_right_merge(ast, range), range),
            range,
        ),
        range,
    );
    assert_eq!(ast.iter().any(|x| {
        match x{
            ASTNode::VCZ{p1, p2, ..}=>*p1 || *p2,
            _=>false
        }
    }), false);
    ast
}
