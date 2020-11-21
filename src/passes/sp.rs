use super::qdg::*;
use crate::arraydep::*;
use crate::ast::*;
use crate::graph::*;
use bitvec::prelude::*;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::vec::*;
use log::*;
#[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd, std::fmt::Debug)]
pub struct Round {
    pub original_order: usize,
    pub round: usize,
    pub current_pos: usize,
}
impl Round{
    pub fn to_abs(self)->(isize, usize){
        (-(self.round as isize), self.original_order)
    }
}
//#[derive(std::fmt::Debug)]
pub struct ModuloSchedulingInstruction {
    pub node: ASTNode,
    pub round: Round,
}
impl std::fmt::Debug for ModuloSchedulingInstruction{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use crate::frontend::*;
        use crate::helper::*;
        let p=Printer::new().print_ast(&ast_to_last(liconst(0), &self.node));
        let w = format!("[o={} r={}] {}", self.round.original_order, self.round.round, p);
        f.write_str(&w)
    }
}
impl ModuloSchedulingInstruction {
    pub fn new(node: ASTNode, ii: usize, original_order: usize) -> Self {
        ModuloSchedulingInstruction {
            node,
            round: Round {
                original_order,
                round: 0,
                current_pos: 0,
            }
        }
    }
    pub fn offset_by(&mut self, ii: usize, dt: usize){
        let pos = dt + self.round.current_pos + self.round.round * ii;
        let step = pos / ii;
        let modulo = pos % ii;
        self.round.round = step;
        self.round.current_pos=modulo;
    }
    pub fn restore(self) -> ASTNode {
        self.node
    }
    pub fn commit(self) -> ASTNode{
        let mut node=self.node;
        match &mut node {
            ASTNode::SQ { qubit, .. } => {
                *qubit = qubit
                    .incr_by(-(self.round.round as i32));
            }
            ASTNode::VCZ { q1, q2, .. } => {
                *q1 = q1
                    .incr_by(-(self.round.round as i32));
                *q2 = q2
                    .incr_by(-(self.round.round as i32));
            }
            _ => unreachable!(),
        }
        node
    }
}


pub struct ModuloSchedulingTable {
    pub instructions: Vec<Vec<ModuloSchedulingInstruction>>,
    ii: usize,
    range: Option<Range>,
}

impl std::fmt::Debug for ModuloSchedulingTable{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use crate::frontend::*;
        use crate::helper::*;
        let p=self.instructions.iter().map(|x| format!("[{}]",x.iter().map(|x| format!("{:?}", x)).collect::<Vec<_>>().join(", "))).collect::<Vec<_>>().join(", \n");
        let w = format!("Table II={} \n range = {:?} [\n{}\n]", self.ii, self.range, p);
        f.write_str(&w)
    }
}

pub enum ConflictReason {
    AliasConflict, // Conflict caused by a fixed resource.
    QubitConflict, // Conflict caused by a dynamic resource.
}
enum ResourceConflict {
    ConflictA,
    ConflictQ,
    Accepted,
}
fn check_resource_conflict(
    a: &ModuloSchedulingInstruction,
    b: &ModuloSchedulingInstruction,
    ii: usize,
    dt: usize,
    range: Option<Range>,
) -> ResourceConflict {
    use ASTNode::*;
    use ResourceConflict::*;
    let r1 = a.round.round;
    let r2 = b.round.round;
    // fix dt
    let tb = dt + b.round.current_pos + r2 * ii;
    let r2 = tb / ii;
    let s2 = tb % ii;
    let eq = |a: &Qubit, b: &Qubit| {
        let a = a.incr_by(-(r1 as i32));
        let a = &a;
        let qb = b.incr_by(-(r2 as i32));
        a == &qb
    };
    let has_conflict = |a: &Qubit, b: &Qubit| {
        let a = a.incr_by(-(r1 as i32));
        let a = &a;
        let qb = b.incr_by(-(r2 as i32));
        let b = &qb;
        if a == b {
            true
        } else if a.0 != b.0 {
            false
        } else if a.1.slope== b.1.slope{
            false
        }else{
            dependency_check_in_loop(
                &a.1,
                &b.1,
                &range.map(|(p, q)| {
                    (
                        p + std::cmp::max(r1, r2) as i32,
                        q + std::cmp::min(r1, r2) as i32,
                    )
                }),
            )
        }
    };
    match (&a.node, &b.node) {
        (SQ { qubit, gate, .. }, VCZ { q1, q2, .. }) => {
            if eq(qubit, q1) || eq(qubit, q2) {
                if (eq(qubit, q1) && q1.1.slope == 0) || (eq(qubit, q2) && q2.1.slope == 0) {
                    trace!("ConflictA 1");
                    ConflictA
                } else {
                    ConflictQ
                }
            } else if has_conflict(qubit, q1) || has_conflict(qubit, q2) {
                trace!("ConflictA 2");
                ConflictA
            } else {
                Accepted
            }
        }
        (
            SQ { qubit, gate, .. },
            SQ {
                qubit: qubit_,
                gate: gate_,
                ..
            },
        ) => {
            if eq(qubit, qubit_) {
                Accepted
            } else {
                if has_conflict(qubit, qubit_) {
                    trace!("ConflictA 3");
                    ConflictA
                } else {
                    Accepted
                }
            }
        }
        (VCZ { q1, q2, .. }, SQ { qubit, gate, .. }) => {
            if eq(q1, qubit) || eq(q2, qubit) {
                if qubit.1.slope == 0 {
                    trace!("ConflictA 4");
                    ConflictA
                } else {
                    ConflictQ
                }
            } else if has_conflict(q1, qubit) || has_conflict(q2, qubit) {
                trace!("ConflictA 5");
                ConflictA
            } else {
                Accepted
            }
        }
        (
            VCZ {
                q1, q2, cancelled, ..
            },
            VCZ {
                q1: q1_, q2: q2_, ..
            },
        ) => {
            let (q1, q2) = (std::cmp::min(q1, q2), std::cmp::max(q1, q2));
            let (q1_, q2_) = (std::cmp::min(q1_, q2_), std::cmp::max(q1_, q2_));
            let rel = |a: &Qubit, b: &Qubit|{
                if eq(a, b) {
                    if a.1.slope ==0 {
                        ConflictA
                    }else{
                        ConflictQ
                    }
                }else{
                    if has_conflict(a, b){
                        ConflictA
                    }else{
                        Accepted
                    }
                }
            };
            match (rel(q1, q1_), rel(q1, q2_), rel(q2, q1_), rel(q2, q2_)){
                (ConflictA, _, _, _)=>ConflictA,
                (_, ConflictA, _, _)=>ConflictA,
                (_, _, ConflictA, _)=>ConflictA,
                (_, _, _, ConflictA)=>ConflictA,
                (Accepted, Accepted, Accepted, Accepted)=>Accepted,
                _=>ConflictQ
            }/*
            if eq(q1, q1_) && eq(q2, q2_) {
                ConflictQ
            } else if eq(q1, q1_) || eq(q2, q2_) && (q1.0 == q1_.0
                && q2.0 == q2_.0
                && q1.1.slope == q1_.1.slope
                && q2.1.slope == q2_.1.slope
                && (!(eq(q1, q1_)) || q1_.1.slope != 0) // not equal, or equal but has a chance to be resolved.
                && (!(eq(q2, q2_)) || q2_.1.slope != 0))
            {
                ConflictQ
            } else if has_conflict(q1, q1_)
                || has_conflict(q1, q2_)
                || has_conflict(q2, q1_)
                || has_conflict(q2, q2_)
            {
                println!("ConflictA 6 {:?} {:?} {:?}", (q1, q2), (q1_, q2_), (r1, r2));
                ConflictA
            } else {
                Accepted
            }
            */
        }
        _ => unreachable!(),
    }
}

fn merge_reason(a: ResourceConflict, b: ResourceConflict)->ResourceConflict{
    match (a,b){
        (ResourceConflict::ConflictA, _)=>ResourceConflict::ConflictA,
        (ResourceConflict::ConflictQ, _)=>ResourceConflict::ConflictQ,
        (ResourceConflict::Accepted, ResourceConflict::Accepted)=>ResourceConflict::Accepted,
        (a, b)=>merge_reason(b, a)

    }
}
impl ModuloSchedulingTable {
    pub fn new(ii: usize, range: Option<Range>) -> Self {
        ModuloSchedulingTable {
            instructions: {
                let mut v = Vec::with_capacity(ii);
                for _i in 0..ii {
                    v.push(Vec::new());
                }
                v
            },
            ii,
            range,
        }
    }
    pub fn insert_insn(
        &mut self,
        original_order: usize,
        instruction: &mut Option<ASTNode>,
        pos: usize,
    ) -> Result<Round, ConflictReason> {
        let raw_insn = std::mem::replace(instruction, None).unwrap();
        let mut insn = ModuloSchedulingInstruction::new(raw_insn, self.ii, original_order);
        macro_rules! abort_insertion {
            ($err: expr) => {
                *instruction=Some(insn.restore());
                return Err($err);
            };
        }
        let mut conflictq = false;
        //let a=pos/ii;
        let b=pos%self.ii;
        // Check resource conflict
        for insn_old in self.instructions[b].iter() {
            match check_resource_conflict(&insn_old, &insn, self.ii, pos, self.range) {
                ResourceConflict::Accepted => {
                    // continue
                }
                ResourceConflict::ConflictQ => {
                    conflictq=true;
                    
                }
                ResourceConflict::ConflictA => {
                    abort_insertion!(ConflictReason::AliasConflict);
                }
            }
        }
        if conflictq {
            abort_insertion!(ConflictReason::QubitConflict);
        }
        insn.offset_by(self.ii, pos);
        let r = insn.round;
        self.instructions[b].push(insn);
        Ok(r)
    }
    pub fn merge_schedule(&mut self, a: &mut Self, pos: usize) -> Result<(), ConflictReason> {
        assert_eq!(self.ii, a.ii);
        let mut err = None;
        
        'find_conflict: for (new_index, insns) in a.instructions.iter().enumerate() {
            let old_insns = &mut self.instructions[(new_index + pos) % self.ii];
            for insn in insns.iter() {
                for insn_old in old_insns.iter() {
                    match check_resource_conflict(&insn_old, &insn, self.ii, pos, self.range) {
                        ResourceConflict::Accepted => {
                            // continue
                        }
                        ResourceConflict::ConflictQ => {
                            err = Some(ConflictReason::QubitConflict);
                        }
                        ResourceConflict::ConflictA => {
                            err = Some(ConflictReason::AliasConflict);
                            break 'find_conflict;
                        }
                    }
                }
            }
        }
        match err {
            None => {
                let v = std::mem::replace(&mut a.instructions, Vec::new());
                for (new_index, insns) in v.into_iter().enumerate() {
                    let old_insns = &mut self.instructions[(new_index + pos) % self.ii];
                    for mut insn in insns {
                        insn.offset_by(self.ii, pos);
                        old_insns.push(insn);
                    }
                }
                Ok(())
            }
            Some(e) => Err(e),
        }
    }
}

pub fn try_modulo_scheduling_in_range(
    ast: &AST,
    qdg: UnassignedGraph,
    range: Option<Range>,
) -> Option<ModuloSchedulingTable> {
    if ast.len()==0{
        return Some(ModuloSchedulingTable::new(1, range));
    }
    let mut sccs = tarjan_qdg(ast, &qdg);
    let mut orig_edges = qdg.iter().map(|(k,v)| {(k.0,k.1,v.dif==0)}).collect::<Vec<_>>();
    orig_edges.sort_unstable();
    // Split edges into different edge maps
    let mut scc_index = Vec::new();
    let mut in_scc_index = Vec::new();

    let mut edge_maps = Vec::new();
    let mut rev_edge_maps = Vec::new();
    let mut cross_edge_maps = Vec::new();
    let mut pred_counts = Vec::new();
    let mut inloop_dep_subgraph_pred_counts = Vec::new(); // For inloop-dependency subgraph pred counts.
    edge_maps.resize(sccs.len(), Vec::new());
    rev_edge_maps.resize(sccs.len(), Vec::new());
    pred_counts.resize(ast.len(), 0usize);
    inloop_dep_subgraph_pred_counts.resize(ast.len(), 0usize);
    scc_index.resize(ast.len(), 0usize);
    in_scc_index.resize(ast.len(), 0usize);

    for (scc_id, scc) in sccs.iter_mut().enumerate() {
        scc.sort_unstable();
        edge_maps[scc_id].resize(scc.len(), Vec::new());
        rev_edge_maps[scc_id].resize(scc.len(), Vec::new());
        for (insn_id, insn) in scc.iter().enumerate() {
            scc_index[*insn] = scc_id;
            in_scc_index[*insn] = insn_id;
        }
    }

    for (a, b, is_inloop) in orig_edges.iter().copied() {
        let scc_id_a = scc_index[a];
        let scc_id_b = scc_index[b];
        if scc_id_a == scc_id_b {
            let scc_id = scc_id_a;
            edge_maps[scc_id][in_scc_index[a]].push(in_scc_index[b]);
            rev_edge_maps[scc_id][in_scc_index[b]].push(in_scc_index[a]);
            pred_counts[b] += 1;
            if is_inloop{
                inloop_dep_subgraph_pred_counts[b]+=1;
            }
        } else {
            cross_edge_maps.push((a, b));
        }
    }
    //println!("sccs: {:?}", sccs);
    // Topology sort every block
    let topology_order_in_sccs = sccs
        .iter()
        .enumerate()
        .map(|(scc_id, scc)| {
            let mut queue = Vec::new(); // by instruction id. In fact this is a ``stack'', not a ``queue''.
                                        // TODO: use a priority-queue.

            let mut topology_order = Vec::new(); // by instruction id
            let edge_map = &edge_maps[scc_id]; // by index in scc
            let rev_edge_map = &rev_edge_maps[scc_id]; // index in scc

            // filter out start points for topology sort.
            for (ii, i) in scc.iter().enumerate() {
                if inloop_dep_subgraph_pred_counts[*i] == 0 {
                    queue.push(*i);
                }
            }

            //do topology steps
            while queue.len() != 0 {
                let elm = queue.pop().unwrap();
                topology_order.push(elm);
                for target in edge_map[in_scc_index[elm]].iter().map(|x| scc[*x]) {
                    let pos = &mut inloop_dep_subgraph_pred_counts[target];
                    *pos -= 1;
                    if *pos == 0 {
                        queue.push(target);
                    }
                }
            }
            assert_eq!(topology_order.len(), scc.len());
            topology_order
        })
        .collect::<Vec<_>>();
    // Reduce the DDG

    let mut scc_preds: Vec<usize> = Vec::new();

    let mut scc_edges: Vec<BTreeSet<usize>> = Vec::new();
    scc_preds.resize(sccs.len(), 0);
    scc_edges.resize(sccs.len(), BTreeSet::new());
    for (u, v) in cross_edge_maps.iter().copied() {
        let scc_u = scc_index[u];
        let scc_v = scc_index[v];
        if !scc_edges[scc_u].contains(&scc_v){
            scc_preds[scc_v] += 1;
        }
        
        scc_edges[scc_u].insert(scc_v);
    }

    // Topology-order the big graph
    let mut scc_topology_order: Vec<usize> = Vec::new();
    use binary_heap_plus::BinaryHeap;
    let mut heap = BinaryHeap::new_by_key(|a: &usize| sccs[*a].len());
    for (scc, cnt) in scc_preds.iter().enumerate() {
        if *cnt == 0 {
            heap.push(scc);
        }
    }
    while let Some(scc) = heap.pop() {
        scc_topology_order.push(scc);
        for v in scc_edges[scc].iter().copied() {
            let cnt = &mut scc_preds[v];
            *cnt = *cnt - 1;
            if *cnt == 0 {
                heap.push(v);
            }
        }
    }
    assert_eq!(scc_topology_order.len(), sccs.len());
    // find minimum ii
    let mut lo = 1;
    let mut hi = ast.len();
    trace!("hi={} {:?}", ast.len(), ast);
    let mut current_sched = None;
    while lo != hi {
        let t = (lo + hi) / 2;
        
        if let Some(sched) = try_modulo_scheduling(
            ast,
            &qdg,
            t,
            range,
            &sccs,
            &scc_index,
            &in_scc_index,
            &topology_order_in_sccs,
            &edge_maps,
            &rev_edge_maps,
            &cross_edge_maps,
            pred_counts.clone(),
            &scc_topology_order,
            &scc_edges,
        ) {
            trace!("Trying {} success.", t);
            hi = t;
            current_sched = Some(sched);
        } else {
            trace!("Trying {} failed.", t);
            lo = t + 1;
        }
    }
    current_sched.map(|x| *x)
    //*(current_sched.unwrap())
}
pub fn try_modulo_scheduling(
    ast: &AST,
    qdg: &UnassignedGraph,
    ii: usize,
    range: Option<Range>,
    sccs: &Vec<Vec<usize>>,
    scc_index: &Vec<usize>,
    in_scc_index: &Vec<usize>,
    topology_order_in_sccs: &Vec<Vec<usize>>,
    edge_maps: &Vec<Vec<Vec<usize>>>,
    rev_edge_maps: &Vec<Vec<Vec<usize>>>,
    cross_edge_maps: &Vec<(usize, usize)>,
    mut pred_counts: Vec<usize>,
    scc_topology_order: &Vec<usize>,
    scc_edges: &Vec<BTreeSet<usize>>,
) -> Option<Box<ModuloSchedulingTable>> {
    trace!("sccs: {:?}\nscc_index: {:?}\ntopology_order_in_sccs: {:?}\nedge_maps: {:?}\nrev_edge_maps: {:?}\n", sccs, scc_index, topology_order_in_sccs, edge_maps, rev_edge_maps);
    let assigned_graph = assign(ast, qdg, ii as Bound);
    trace!("{}", assigned_graph.print());
    let cost_mat = assigned_graph.floyd()?;
    trace!("Floyd?");
    macro_rules! cost {
        ($u: expr, $v: expr) => {
            cost_mat[$u * ast.len() + $v].to_option_ref().copied()
        };
    }
    let mut ms = Box::new(ModuloSchedulingTable::new(ii, range));
    let mut sigma_low = (0..ast.len())
        .map(|v| {
            let mut ret = 0;
            for u in 0..ast.len() {
                if let Some(val) = cost![u, v] {
                    ret = std::cmp::max(ret, val);
                }
            }
            ret
        })
        .collect::<Vec<_>>(); // by instruction id
    let mut sigma_up = (0..ast.len()).map(|v| None).collect::<Vec<Option<isize>>>(); // by instruction id

    let mut placed_sigma = Vec::new();

    placed_sigma.resize(ast.len(), 0); // by instruction id
        
    let mut sub_schedules = sccs
        .iter()
        .enumerate()
        .map(|(scc_id, scc)| {
            let topology_order = &topology_order_in_sccs[scc_id]; // by instruction id
            let edge_map = &edge_maps[scc_id]; // by in-scc-index, to in-scc-index
            let rev_edge_map = &rev_edge_maps[scc_id]; // by in-scc-index, to in-scc-index
            let mut sub_schedule = ModuloSchedulingTable::new(ii, range);
            // The death counter is for aliasing resource conflict.
            let mut death_counter: Option<usize> = None;
            'loop_insn: for insn in topology_order.iter().copied() {
                let mut current_insn = Some(ast[insn].clone());

                'loop_lo: for lo in sigma_low[insn].. {
                    if let Some(hi) = sigma_up[insn] {
                        if lo > hi {
                            trace!("lo>hi");
                            return None; // Failed.
                        }
                    }
                    let ret = sub_schedule.insert_insn(insn, &mut current_insn, lo as usize);
                    match ret {
                        Ok(_) => {
                            // update lo and hi
                            let v = insn;
                            placed_sigma[insn] = lo;
                            for u in edge_map[in_scc_index[v]].iter().copied().map(|x| scc[x]) {
                                if let Some(costvu) = cost![v, u] {
                                    sigma_low[u] = std::cmp::max(sigma_low[u], lo + costvu);
                                } else {
                                    unreachable!(); // since u and v are in the same SCC.
                                }
                            }
                            for u in rev_edge_map[in_scc_index[v]].iter().copied().map(|x| scc[x]) {
                                if let Some(costuv) = cost![u, v] {
                                    let new_val = lo as isize - costuv;
                                    match sigma_up[u] {
                                        Some(x) => {
                                            sigma_up[u] = Some(std::cmp::min(x, new_val));
                                        }
                                        None => {
                                            sigma_up[u] = Some(new_val);
                                        }
                                    }
                                } else {
                                    unreachable!();
                                }
                            }
                            continue 'loop_insn;
                        }
                        Err(reason) => {
                            match reason {
                                ConflictReason::AliasConflict => {
                                    if let None = death_counter {
                                        if ii == 1 {
                                            trace!("Death 1!");
                                            return None;
                                        }
                                        // start death counter. At most II retries can be made.
                                        death_counter = Some(ii - 1);
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                    if let Some(cntr) = &mut death_counter {
                        *cntr = *cntr - 1;
                        if *cntr == 0 {
                            trace!("Death 2!");
                            return None;
                        }
                    }
                }
            }
            Some(sub_schedule)
        })
        .collect::<Option<Vec<ModuloSchedulingTable>>>()?;

    trace!("Start stage 2");
    let mut scc_sigma_lo = Vec::new();
    scc_sigma_lo.resize(sccs.len(), 0);

    let mut new_graph_map: BTreeMap<_, isize> = BTreeMap::new();

    for (u, v) in cross_edge_maps.iter().copied() {
        let scc_u = scc_index[u];
        let scc_v = scc_index[v];
        let k = cost![u, v].unwrap();
        let m = placed_sigma[u];
        let n = placed_sigma[v];
        let l = k + m - n;
        new_graph_map
            .entry((scc_u, scc_v))
            .and_modify(|x| *x = std::cmp::max(*x, l))
            .or_insert(l);
    }
    trace!("sub_schedules: {:?}", &sub_schedules);
    trace!("Try to place each scc");
    for scc in scc_topology_order.iter().copied() {
        let mut death_counter: Option<usize> = None;
        'loop_lo: for lo in scc_sigma_lo[scc].. {
            trace!("Trying {}", lo);
            match ms.merge_schedule(&mut sub_schedules[scc], lo) {
                Ok(_) => {
                    for v in scc_edges[scc].iter() {
                        let costuv = new_graph_map[&(scc, *v)];
                        scc_sigma_lo[*v] =
                            std::cmp::max(scc_sigma_lo[*v] as isize, lo as isize + costuv) as usize;
                    }
                    break 'loop_lo;
                }
                Err(ConflictReason::AliasConflict) => {
                    if let None = death_counter {
                        if ii == 1 {
                            trace!("Death 3!");
                            return None;
                        }
                        // start death counter. At most II retries can be made.
                        death_counter = Some(ii - 1);
                    }
                }
                Err(ConflictReason::QubitConflict) => {
                    trace!("Qubit Conflict");
                    //println!("{:?}\n{:?}", ms, sub_schedules[scc]);
                    //panic!();
                }
            }
            if let Some(death) = &mut death_counter {
                *death -= 1;
                if *death == 0 {
                    trace!("Death 4!");
                    return None;
                }
            }
        }
    }
    trace!("Final ms: {:?}", ms);
    Some(ms)
}
