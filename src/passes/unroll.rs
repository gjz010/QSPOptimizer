use super::merge::*;
use crate::arraydep::*;
use crate::ast::*;
use crate::gcd::*;
use log::*;
/*
pub type AcrossLoopDependencies = Vec<(usize, usize, Bound)>;
// All modulus below are euclidean modulus.
// Note that (a-(a%b))%b=0.
type SQIndex = (QArray, Bound /*k*/, Bound /* b % k */);
type CZLineIndex = ((QArray, Bound), (QArray, Bound), Bound /*k1b2-k2b1*/);
type CZIndex = (
    CZLineIndex,
    Bound, /*(special solution - ref special solution for all) % gcd(k1, k2)*/
);

type CZSingleIndex = (
    (QArray, Bound /* constant bound */),
    (QArray, Bound /* b%k */),
);

fn split_qubit(q: Qubit) -> (SQIndex, Bound) {
    (
        (q.0, q.1.slope, q.1.intercept.rem_euclid(q.1.slope)),
        q.1.intercept.div_euclid(q.1.slope),
    )
}
fn split_cz<'a>(q1: &'a Qubit, q2: &'a Qubit) -> (CZLineIndex, &'a Qubit, &'a Qubit) {
    fn split_cz_ordered(q1: &Qubit, q2: &Qubit) -> CZLineIndex {
        (
            (q1.0, q1.1.slope),
            (q2.0, q2.1.slope),
            q1.1.slope * q2.1.intercept - q2.1.slope * q1.1.intercept,
        )
    }
    let (ia, _) = split_qubit(*q1);
    let (ib, _) = split_qubit(*q2);
    if ia < ib {
        (split_cz_ordered(q1, q2), q1, q2)
    } else {
        (split_cz_ordered(q2, q1), q2, q1)
    }
}

fn split_single_cz<'a>(
    q1: &'a Qubit,
    q2: &'a Qubit,
) -> Option<((CZSingleIndex, Bound), &'a Qubit, &'a Qubit)> {
    fn split_single_cz_ordered(q1: &Qubit, q2: &Qubit) -> (CZSingleIndex, Bound) {
        (
            (
                (q1.0, q1.1.intercept),
                (q2.0, q2.1.intercept.rem_euclid(q2.1.slope)),
            ),
            q2.1.intercept.div_euclid(q2.1.slope),
        )
    }
    if q1.1.slope == 0 && q2.1.slope != 0 {
        Some((split_single_cz_ordered(q1, q2), q1, q2))
    } else if q1.1.slope != 0 && q2.1.slope == 0 {
        Some((split_single_cz_ordered(q2, q1), q2, q1))
    } else {
        None
    }
}

// constant gates are not in this list.
pub fn find_across_loop_merging_potential(ast: &AST) -> Vec<(usize, usize, Bound)> {
    // Reference points for determining when the lines are the same but gcd(k1, k2)!=1.
    let mut sq_intercepts: BTreeMap<SQIndex, Vec<(usize, Bound)>> = BTreeMap::new();

    let mut cz_references: BTreeMap<
        CZLineIndex,
        (
            Bound,          /* GCD */
            (Bound, Bound), /* ref special solution*/
        ),
    > = BTreeMap::new();
    let mut cz_intercepts: BTreeMap<CZIndex, Vec<(usize, Bound)>> = BTreeMap::new();

    let mut cz_single_intercept: BTreeMap<CZSingleIndex, Vec<(usize, Bound)>> = BTreeMap::new();
    for (id, i) in ast.iter().enumerate() {
        match i {
            ASTNode::SQ { qubit, .. } => {
                if qubit.1.slope == 0 {
                    continue;
                }
                let (i, x) = split_qubit(*qubit);
                sq_intercepts.entry(i).or_insert(Vec::new()).push((id, x));
            }
            ASTNode::VCZ { q1, q2, .. } => {
                if q1.1.slope == 0 && q2.1.slope == 0 {
                    continue;
                }
                if let Some(((index, r), q1, q2)) = split_single_cz(q1, q2) {
                    cz_single_intercept
                        .entry(index)
                        .or_insert(Vec::new())
                        .push((id, r));
                    continue;
                }
                let (i1, q1, q2) = split_cz(q1, q2);
                let (g, (s1, s2)) = cz_references.entry(i1).or_insert((
                    gcd(q1.1.slope.abs(), q2.1.slope.abs()),
                    (q1.1.intercept, q2.1.intercept),
                ));
                let dk1 = q1.1.slope / *g;
                let dk2 = q2.1.slope / *g;
                assert!((q1.1.intercept - *s1) % dk1 == 0);
                assert!((q2.1.intercept - *s2) % dk2 == 0);
                let dstep1 = (q1.1.intercept - *s1) / dk1;
                let dstep2 = (q2.1.intercept - *s2) / dk2;
                assert!(dstep1 == dstep2);
                let dstep = dstep1;
                let dstep_mod = dstep.rem_euclid(*g);
                let dstep_quot = dstep.div_euclid(*g);
                cz_intercepts
                    .entry((i1, dstep_mod))
                    .or_insert(Vec::new())
                    .push((id, dstep_quot));
            }
            _ => unreachable!(),
        }
    }
    let mut ret = Vec::new();
    macro_rules! find_insn_dependency {
        ($v: ident) => {
            for (ij, j) in $v.iter() {
                for (ik, k) in $v.iter() {
                    if *k > *j {
                        let d = *k - *j;
                        // ``ij'' d-iterations later will collide with ``ik'' in this iteration.
                        // ik->ij dif=d.
                        ret.push((*ij, *ik, d));
                    }
                }
            }
        };
    }
    for i in sq_intercepts.values() {
        find_insn_dependency!(i);
    }
    for i in cz_intercepts.values() {
        find_insn_dependency!(i);
    }
    ret
}

*/
pub fn find_across_loop_merging_potential(ast: &AST) -> Vec<(usize, usize, Bound)> {
    let mut v = Vec::new();
    use ASTNode::*;
    for (i, insn_i) in ast.iter().enumerate(){
        for (j, insn_j) in ast.iter().enumerate(){
            match (insn_i, insn_j){
                (SQ{qubit: q1, ..}, SQ{qubit: q2, ..})=>{
                    if q1.0==q2.0 && q1.1.slope==q2.1.slope && q1.1.slope!=0{
                        let db = q2.1.intercept-q1.1.intercept;
                        let k=q1.1.slope;
                        if db%k==0{
                            let d = db/k;
                            if d>0{
                                v.push((i, j, d))
                            }
                        }
                    }
                }
                (VCZ{q1, q2, ..}, VCZ{q1: q1_, q2: q2_, ..})=>{
                    let (q1, q2) = (std::cmp::min(q1, q2), std::cmp::max(q1, q2));
                    let (q1_, q2_) = (std::cmp::min(q1_, q2_), std::cmp::max(q1_, q2_));
                    if q1.0 == q1_.0 && q2.0 == q2_.0{
                        if q1.1.slope == q1_.1.slope && q2.1.slope==q2_.1.slope{
                            macro_rules! delta {
                                ($q: expr, $q_: expr) => {
                                    if $q.1.slope==0 {
                                        Some(None)
                                    }else{
                                        let db = $q_.1.intercept-$q.1.intercept;
                                        let k = $q.1.slope;
                                        if db % k ==0{
                                            let d = db / k;
                                            if d>0{
                                                Some(Some(d))
                                            }else{
                                                None
                                            }
                                        }else{
                                            None
                                        }
                                        
                                    };
                                };
                            }
                            let p1 = delta!(q1, q1_);
                            let p2 = delta!(q2, q2_);
                            let d=match (p1, p2){
                                (Some(None), Some(Some(d)))=> Some(d),
                                (Some(Some(d)), Some(None))=> Some(d),
                                (Some(Some(d1)), Some(Some(d2)))=> if d1==d2 {Some(d1)} else {None}
                                _ => None
                            };
                            if let Some(d) = d{
                                v.push((i, j, d))
                            }
                        }
                    }
                }
                _=>{ /* do nothing */}
            }
        }
    }
    v
}
pub fn find_unroll_time(ast: &AST, max_unroll_iterations: Option<Bound>) -> usize {
    let mut unroll_time = 2; // TODO: when should this be 1?
    let deps = find_across_loop_merging_potential(ast);
    for (_, _, d) in deps.iter() {
        if let Some(m) = max_unroll_iterations {
            if *d > m {
                continue;
            }
        }
        unroll_time = std::cmp::max(*d as usize, unroll_time);
    }
    unroll_time
}

// This transforms the loop of
// for i in a..b do {}
// into
// for in in 0..((b-a+1)/T) do{} /*prologue*/
pub fn unroll_loop(ast: AST, unroll_time: usize, bound: Option<Range>) -> AST {
    let resolve_qoffset = |mut offset: QOffset, dt: usize| {
        offset.intercept =
            offset.slope * (bound.unwrap_or((0, 0)).0) + offset.intercept + dt as i32 * offset.slope as i32;
        offset.slope = offset.slope * unroll_time as i32;
        offset
    };
    let resolve_qubit = |mut qubit: Qubit, dt: usize| {
        qubit.1 = resolve_qoffset(qubit.1, dt);
        qubit
    };
    let resolve_gate = |mut gate: SQGate, dt: usize| {
        gate.components.iter_mut().for_each(|x| {
            if let Gate::Unknown(a, b) = x {
                *b = resolve_qoffset(*b, dt);
            }
        });
        gate
    };
    let unrolled_ast = (0..unroll_time)
        .flat_map(|dt| {
            ast.iter().map(move |x| match x {
                ASTNode::SQ { qubit, gate, .. } => ASTNode::SQ {
                    gate: resolve_gate(gate.clone(), dt),
                    qubit: resolve_qubit(*qubit, dt),
                    moved: false,
                },
                ASTNode::VCZ { q1, q2, .. } => ASTNode::VCZ {
                    q1: resolve_qubit(*q1, dt),
                    q2: resolve_qubit(*q2, dt),
                    cancelled: false,
                    p1: false,
                    p2: false,
                    moved: false,
                },
            })
        })
        .collect();
    unrolled_ast
}
macro_rules! mark_moved {
    ($insn: expr) => {
        match &mut $insn{
            ASTNode::SQ{moved, ..}=>{*moved=true;}
            ASTNode::VCZ{moved, ..}=>{*moved=true;}
        }
    };
}
macro_rules! clear_moved {
    ($insn: expr) => {
        match &mut $insn{
            ASTNode::SQ{moved, ..}=>{*moved=false;}
            ASTNode::VCZ{moved, ..}=>{*moved=false;}
        }
    };
}
// Don't forget the reason why we try to eliminate dif of weak dependencies to 1:
// we want wo eliminate (actual, not potential) across-loop merging as possible.
// So after eliminating all weak dependencies with dif>1, we can rotate the leftmost SQs to the right.
// After this step, there will be no across-loop merging even though there are weak dependencies.
// In thie way, our loop body is exactly the repeating pattern in the loop-unrolled version.
// That is where the name ``perfect pipelining'' comes from.

// Usage: three parts of AST are filled in.
// Prelude: fill the first iteration into variable x.
// Postlude: fill the last iteration into variable x.
// Kernel: decrease the hi-Bound by t.
// Also we only consider cases where slope>0.
// Cases where slope=0 are handled by self-merging gates detecting.

// The interesting fact: whether an operation can be rotated to meet another operation
// depends only on the qubits that the op is on.
// This allows us to rotate one left-most instruction at a time.
// If no leftmost-instructions are rotable, give up.

// If a known range is given, return the range after unrolling.
// Otherwise, the delta is outputed.

pub fn rotate_ast(mut ast: AST, mut range: Option<Range>) -> (AST, AST, AST, usize) {
    trace!("ast length = {}", ast.len());
    let mut prelude = Vec::new();
    let mut postlude = Vec::new();
    #[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Copy, Clone)]
    enum Index {
        SQIndex(Qubit),
        CZIndex(Qubit, Qubit),
    }
    impl Index {
        pub fn incr(self) -> Self {
            use Index::*;
            match self {
                SQIndex(a) => SQIndex(a.incr()),
                CZIndex(a, b) => CZIndex(a.incr(), b.incr()),
            }
        }
    }

    macro_rules! qindex {
        ($x: expr) => {
            match &$x {
                ASTNode::SQ { gate, qubit, .. } => Index::SQIndex(*qubit),
                ASTNode::VCZ { q1, q2, p1, p2, .. } => {
                    if q1 < q2 {
                        Index::CZIndex(*q1, *q2)
                    } else {
                        Index::CZIndex(*q2, *q1)
                    }
                }
            }
        };
    }
    let mut rotate_counter=0;
    'rotate_once: loop {
        rotate_counter+=1;
        trace!("Rotate once {}", rotate_counter);
        // We consider a rotation:
        // CZ can carry a single qubit gate on constant offset.
        // single qubit gates can rotate.
        let mut moving_instruction = None;
        let mut carried_instruction = None;
        'find_next_instruction: for (i, insn_i) in ast.iter().enumerate() {
            let mut carried_candidate = None;
            match insn_i {
                ASTNode::SQ { moved, qubit, gate } => {
                    if *moved {
                        continue 'find_next_instruction;
                    }
                }
                ASTNode::VCZ { q1, q2, moved, .. } => {
                    if *moved {
                        continue 'find_next_instruction;
                    }
                }
            }
            for (j, insn_j) in (0..i).into_iter().map(|x| (x, &ast[x])) {
                match (insn_j, insn_i) {
                    (
                        ASTNode::SQ { qubit, gate, .. },
                        ASTNode::SQ {
                            qubit: qubit_,
                            gate: gate_,
                            ..
                        },
                    ) => {
                        if may_conflict_in_loop(qubit, qubit_, &range) {
                            // This can't be moved.
                            continue 'find_next_instruction;
                        }
                    }
                    (ASTNode::SQ { qubit, gate, .. }, ASTNode::VCZ { q1, q2, .. }) => {
                        if (qubit == q1 || qubit == q2) && qubit.1.slope == 0 {
                            carried_candidate = Some(j);
                        }
                    }
                    (ASTNode::VCZ { q1, q2, .. }, ASTNode::SQ { qubit, gate, .. }) => {
                        if may_conflict_in_loop(q1, qubit, &range)
                            || may_conflict_in_loop(q2, qubit, &range)
                        {
                            // This can't be moved.
                            continue 'find_next_instruction;
                        }
                    }
                    (
                        ASTNode::VCZ { q1, q2, .. },
                        ASTNode::VCZ {
                            q1: q1_, q2: q2_, ..
                        },
                    ) => {}
                }
            }
            for (k, insn_k) in (i + 1..ast.len()).map(|x| (x, &ast[x])) {
                let index_i = qindex![insn_i];
                let index_k = qindex![insn_k];
                if index_i.incr() == index_k {
                    moving_instruction = Some(i);
                    carried_instruction = carried_candidate;
                    break 'find_next_instruction;
                }
            }
        }
        if let Some(m) = moving_instruction {

            mark_moved!(ast[m]);
            if let Some(i)=carried_instruction{
                mark_moved!(ast[i]);
            }
            let (prologue, epilogue) = ast
                .into_iter()
                .enumerate()
                .partition::<Vec<_>, _>(|(i, x)| Some(*i) == carried_instruction || *i == m);
            prelude.push(prologue.iter().map(|(a, b)| b).cloned().collect::<Vec<_>>());
            postlude.push(epilogue.iter().map(|(a, b)| b).cloned().collect::<Vec<_>>());
            ast = epilogue
                .into_iter()
                .chain(prologue.into_iter().map(|x| (x.0, x.1.incr_by(1))))
                .map(|(a, b)| b)
                .collect();
            let ast_=left_merge_adjacent_gates(ast, range);
            ast=ast_;
            // Range upper-bound decrese by 1, since maybe the rotation will reveal more chances for rotating.
            // Note that unrolling is not affected by bounds.
            range=range.map(|mut x| {x.1-=1; x});
        } else {
            break 'rotate_once;
        }
    }

    let bound = prelude.len();
    for mut insn in ast.iter_mut(){clear_moved!(insn);}
    (
        prelude.into_iter().flat_map(|x| x.into_iter().enumerate().map(|(i, y)| y.incr_by(i as i32))).map(|mut x| {clear_moved!(x);x}).collect(),
        ast,
        postlude
            .into_iter()
            .rev()
            .enumerate()
            .flat_map(|(i,x)| x.into_iter().map(move |y| y.incr_by(i as i32)))
            .map(|mut x| {clear_moved!(x);x})
            .collect(),
        bound,
    )
}
