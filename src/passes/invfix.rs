use super::sp::*;
use crate::ast::*;

#[derive(Copy, Clone)]
enum Dep{
    OneDep(isize),
    TwoDep(isize, isize)
}
impl Dep{
    fn iter(&self)->impl Iterator<Item=isize>{
        match *self{
            OneDep(x)=>Some(x).into_iter().chain(None.into_iter()),
            TwoDep(a, b)=>Some(a).into_iter().chain(Some(b).into_iter())
        }
    }

}

use Dep::*;
pub struct FlattenedInstruction {
    pub node: ModuloSchedulingInstruction,
    pub appended_zs: Vec<(Qubit, isize)>,
}

impl FlattenedInstruction {
    pub fn round_iter<'a>(&'a self) -> impl Iterator<Item = isize> + 'a {
        Some(self.node.round.round as isize)
            .into_iter()
            .chain(self.appended_zs.iter().map(|x| x.1))
    }
}
pub fn fix_inversion_pairs(n: usize, inv: ModuloSchedulingTable) -> Vec<Vec<FlattenedInstruction>> {
    let n = n as isize;
    // the order. larger is later.
    macro_rules! absolute_order {
        ($x: expr) => {
            absolute_order!($x, 0)
        };
        ($x: expr, $dt: expr) => {
            (-($x.round.round as isize) + $dt) * n + $x.round.original_order as isize
        };
    }
    // we flattern instructions first. in this case all instructions have a fixed order.
    // moreover, a greedy scheduling approach will convert the ordering back.
    let mut flattened_instructions = inv
        .instructions
        .into_iter()
        .map(|x| {
            x.into_iter()
                .map(|node| FlattenedInstruction {
                    node,
                    appended_zs: Vec::new(),
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    let mut inloop_inversion = Vec::new();
    let mut acrossloop_inversion = Vec::new();

    // find inloop inversions

    for i in 0..flattened_instructions.len() {
        for j in (i + 1)..flattened_instructions.len() {
            for (insn_i_id, insn_i) in flattened_instructions[i].iter().enumerate() {
                for (insn_j_id, insn_j) in flattened_instructions[j].iter().enumerate() {
                    if absolute_order!(insn_i.node) > absolute_order!(insn_j.node) {
                        inloop_inversion.push(((i, insn_i_id), (j, insn_j_id)));
                        inloop_inversion.push(((j, insn_j_id), (i, insn_i_id)));
                    }
                }
            }
        }
    }
    // find acrossloop inversions
    for dt in 1.. {
        let mut flag = false;
        for i in 0..flattened_instructions.len() {
            for j in 0..flattened_instructions.len() {
                for (insn_i_id, insn_i) in flattened_instructions[i].iter().enumerate() {
                    for (insn_j_id, insn_j) in flattened_instructions[j].iter().enumerate() {
                        if absolute_order!(insn_i.node) > absolute_order!(insn_j.node, dt) {
                            acrossloop_inversion
                                .push((((i, insn_i_id), (j, insn_j_id)), dt as isize));
                            acrossloop_inversion
                                .push((((j, insn_j_id), (i, insn_i_id)), -(dt as isize)));
                            flag = true;
                        }
                    }
                }
            }
        }
        if !flag {
            break;
        }
    }

    for ((i, insn_i), (j, insn_j)) in inloop_inversion.into_iter() {
        match (
            &flattened_instructions[i][insn_i].node.node,
            &flattened_instructions[j][insn_j].node.node,
        ) {
            (ASTNode::VCZ { q1, q2, .. }, ASTNode::SQ { qubit, gate, .. }) => {
                let q1 = q1.incr_by(-(flattened_instructions[i][insn_i].node.round.round as i32));
                let q2 = q2.incr_by(-(flattened_instructions[i][insn_i].node.round.round as i32));
                let qubit = qubit.incr_by(-(flattened_instructions[j][insn_j].node.round.round as i32));
                if qubit == q1 || qubit == q2 {
                    match gate.get_type() {
                        GateType::Diagonal => {}
                        GateType::AntiDiagonal => {
                            let q = qubit;
                            //let hint = {if q==*q1{*q2_hint} else {*q1_hint}};
                            flattened_instructions[i][insn_i].appended_zs.push((q, 0));
                        }
                        GateType::General => {
                            unreachable!("Bad reordering found!");
                        }
                    }
                }
            }
            _ => {}
        }
    }

    for (((i, insn_i), (j, insn_j)), dt) in acrossloop_inversion.into_iter() {
        match (
            &flattened_instructions[i][insn_i].node.node,
            &flattened_instructions[j][insn_j].node.node,
        ) {
            (
                ASTNode::VCZ {
                    q1,
                    q2, /*, q1_hint, q2_hint*/
                    ..
                },
                ASTNode::SQ { qubit, gate, .. },
            ) => {
                let q1 = q1.incr_by(-(flattened_instructions[i][insn_i].node.round.round as i32));
                let q2 = q2.incr_by(-(flattened_instructions[i][insn_i].node.round.round as i32));
                let qubit = qubit.incr_by(-(flattened_instructions[j][insn_j].node.round.round as i32));
                let qubit = qubit.incr_by(dt as i32);
                if qubit == q1 || qubit == q2 {
                    match gate.get_type() {
                        GateType::Diagonal => {}
                        GateType::AntiDiagonal => {
                            /*let hint = {if qubit==*q1{*q2_hint} else {*q1_hint}};*/
                            flattened_instructions[i][insn_i]
                                .appended_zs
                                .push((qubit, dt));
                        }
                        GateType::General => {
                            unreachable!("Bad reordering found!");
                        }
                    }
                }
            }
            _ => {}
        }
    }

    flattened_instructions
}

fn instruction_conflict(a: &ASTNode, b: &ASTNode, range: Option<Range>) -> bool {
    use crate::arraydep::dependency_check_in_loop;
    use crate::helper::ast_qubit_iterator;
    macro_rules! check {
        ($a: expr, $b: expr) => {
            ($a == $b) || ($a.0 == $b.0) && (dependency_check_in_loop(&$a.1, &$b.1, &range))
        };
    }
    for p in ast_qubit_iterator(Some(a).into_iter()) {
        for q in ast_qubit_iterator(Some(b).into_iter()) {
            if check!(p, q) {
                return true;
            }
        }
    }
    false
}

/// Generate prologue, epilogue and kernel.
pub fn generate_code_unknown(
    all_instructions: Vec<Vec<FlattenedInstruction>>,
) -> (AST, Vec<AST>, AST, isize, isize, isize, isize) {
    if all_instructions.iter().flat_map(|x| x.iter()).count()==0{
        return (vec![], vec![], vec![], 0,0,0,0);
    }
    let mut minimal_offset = std::isize::MAX;
    let mut maximal_offset = std::isize::MIN;
    all_instructions
        .iter()
        .flat_map(|x| x.iter().flat_map(|y| y.round_iter()))
        .for_each(|x| {
            minimal_offset = std::cmp::min(minimal_offset, -x);
            maximal_offset = std::cmp::max(maximal_offset, -x);
        });
    use log::*;
    trace!("{} {} {}", all_instructions.len(), maximal_offset, minimal_offset);
    let prologue_epilogue_length = maximal_offset - minimal_offset;
    // Even though, the interesting thing is that no instructions with round>0 will appear.
    let first_stmt_i = -maximal_offset;
    let last_stmt_i = -minimal_offset;
    let first_kernel_i = first_stmt_i + prologue_epilogue_length;
    let last_kernel_i = last_stmt_i - prologue_epilogue_length;
    // putting lo in.
    let prologue_linear = first_stmt_i..=first_kernel_i - 1;
    // putting hi in.
    let epilogue_linear = last_kernel_i + 1..=last_stmt_i;

    let prologue_epilogue_range = (minimal_offset..maximal_offset).rev();

    let loop_body = flatten_instruction(all_instructions);
    let (prologue, epilogue) = prologue_epilogue_range.enumerate()
        .flat_map(|(i,x)| {
            loop_body
                .iter()
                .flat_map(|x| x.iter())
                .map(move |n| (
                    if n.1.iter().all(|y| y>x) {Some(n.0.clone().incr_by(i as i32))} else {None},
                    if n.1.iter().all(|y| y<=x) {Some(n.0.clone().incr_by(i as i32))} else {None}
                ))
        })
        .unzip::<_, _, Vec<_>, Vec<_>>();
    
    //let (prologue, epilogue): (Vec<_>, Vec<_>) = prologue_epilogue.into_iter().unzip();
    (
        prologue.into_iter().flat_map(|x| x.into_iter()).collect(),
        loop_body
            .into_iter()
            .map(|x| x.into_iter().map(|(a, _)| a).fold(Vec::new(), |mut v, mut x| {
                for i in v.iter_mut(){
                    match (i, &mut x){
                        (ASTNode::SQ{qubit, gate, ..}, ASTNode::SQ{qubit: qubit_, gate: gate_, ..})=>{
                            if qubit==qubit_{
                                let gate_=std::mem::replace(gate_, SQGate::new());
                                gate.merge_sqgate(gate_);
                                return v;
                            }
                        }
                        _=>{}
                    }
                }
                v.push(x);
                v
            }))
            .collect(),
        epilogue.into_iter().flat_map(|x| x.into_iter()).collect(),
        first_kernel_i,
        last_kernel_i,
        first_stmt_i,
        last_stmt_i
    )
}

fn flatten_instruction(insns: Vec<Vec<FlattenedInstruction>>) -> Vec<Vec<(ASTNode, Dep)>> {
    insns
        .into_iter()
        .map(|x| {
            let mut v=x.into_iter()
                .flat_map(|y| {
                    let loc=(-(y.node.round.round as isize), y.node.round.original_order as isize);
                    let r = -(y.node.round.round as isize);
                    Some((y.node.commit(), OneDep(r as isize), loc))
                        .into_iter()
                        .chain(y.appended_zs.into_iter().map(move |r| {
                            (
                                ASTNode::SQ {
                                    gate: SQGate::singleton(Gate::known(GateSet::Z.matrix())),
                                    qubit: r.0,
                                    moved: false,
                                },
                                TwoDep(loc.0, loc.0 + r.1),
                                loc
                            )
                        }))
                })
                .collect::<Vec<_>>();
            v.sort_unstable_by_key(|x| x.2); // We don't know whether SQs can be merged yet, but at least we can order them correctly.
            v.into_iter().map(|x| (x.0, x.1)).collect()
        })
        .collect()
}
