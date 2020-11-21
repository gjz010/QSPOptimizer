use crate::arraydep::*;
use crate::ast::*;
use crate::graph::*;
use std::collections::BTreeMap;
use log::*;
#[derive(Clone, Copy, std::fmt::Debug)]
pub struct UnassignedEdge {
    pub min: isize,
    pub dif: isize,
}

impl UnassignedEdge {
    pub fn assign(&self, ii: isize) -> isize {
        self.min - ii * self.dif
    }
}

//type UnassignedGraph = Graph<usize, Option<UnassignedEdge>>;

pub type UnassignedGraph = BTreeMap<(usize, usize), UnassignedEdge>;

pub fn build_qdg<'a>(
    ast: &'a AST,
    range: &Option<Range>,
    disable_antidiagonal: bool,
) -> UnassignedGraph {
    let n = ast.len();
    let mut edges: BTreeMap<(usize, usize), UnassignedEdge> = BTreeMap::new();

    macro_rules! add_edge {
        (($i: expr, $j: expr),{$min: expr,$dif: expr}) => {
            edges
                .entry(($i, $j))
                .and_modify(|y| {
                    let x = std::cmp::min((y.dif, y.min), ($dif, $min));
                    *y = UnassignedEdge { min: x.1, dif: x.0 }
                })
                .or_insert(UnassignedEdge {
                    min: $min,
                    dif: $dif,
                });
        };
    }

    for i in 0..n {
        'lo: for j in 0..n {
            if i==j {
                continue 'lo;
            }
            fn check_conflict(
                range: &Option<Range>,
                base: &Qubit,
                target: &Qubit,
            ) -> Option<usize> {
                let a = *base;
                let b = *target;
                if a.0 == b.0 {
                    Some(dependency_find_minimal_delta(&a.1, &b.1, range)? as usize)
                } else {
                    None
                }
            }

            fn mincf(a: Option<usize>, b: Option<usize>) -> Option<usize> {
                match (a, b) {
                    (Some(a), Some(b)) => Some(std::cmp::min(a, b)),
                    (None, b) => b,
                    (a, None) => a,
                }
            }
            macro_rules! mincfs {
                ($x:expr) => ($x);
                ($x:expr, $($y:expr),+) => (
                    mincf($x, mincfs!($($y),+))
                )
            }

            let insn_i = &ast[i];
            let insn_j = &ast[j];
            match (insn_i, insn_j) {
                (ASTNode::SQ { qubit: aq, .. }, ASTNode::SQ { qubit: bq, .. }) => {
                    if let Some(cfs) = mincfs![check_conflict(range, &aq, &bq)] {
                        add_edge!((i, j), {0, cfs as isize});
                    }
                }
                (
                    ASTNode::SQ {
                        qubit: aq,
                        gate: ag,
                        ..
                    },
                    ASTNode::VCZ {
                        q1: bq1, q2: bq2, ..
                    },
                ) => {
                    if ag.get_type() == GateType::General || (ag.get_type() == GateType::AntiDiagonal && disable_antidiagonal) {
                        if let Some(cfs) = mincfs![
                            check_conflict(range, &aq, &bq1),
                            check_conflict(range, &aq, &bq2)
                        ] {
                            add_edge!((i, j), {1, cfs as isize});
                        }
                    }
                }
                (
                    ASTNode::VCZ {
                        q1: aq1, q2: aq2, ..
                    },
                    ASTNode::SQ {
                        qubit: bq,
                        gate: bg,
                        ..
                    },
                ) => {
                    if (bg.get_type() == GateType::AntiDiagonal && disable_antidiagonal) || bg.get_type() == GateType::General {
                        if let Some(cfs) = mincfs![
                            check_conflict(range, &aq1, &bq),
                            check_conflict(range, &aq2, &bq)
                        ] {
                            add_edge!((i, j), {1, cfs as isize});
                        }
                    }
                }
                (ASTNode::VCZ { .. }, ASTNode::VCZ { .. }) => {}
                _ => unreachable!(),
            }
        }
    }
    // In-loop dependencies last.
    for i in 0..n {
        fn check_conflict(range: &Option<Range>, q: &Qubit, target: &Qubit) -> bool {
            if q.0 == target.0 {
                dependency_check_in_loop(&q.1, &target.1, range)
            } else {
                false
            }
        }
        for j in (i + 1)..n {
            let insn_i = &ast[i];
            let insn_j = &ast[j];
            match (insn_i, insn_j) {
                (ASTNode::SQ { qubit: aq, .. }, ASTNode::SQ { qubit: bq, .. }) => {
                    if *aq == *bq {
                        //edges.entry((i, j)).or_insert(UnassignedEdge{min: 0, dif: 0});
                        add_edge!((i, j), {0, 0});
                    } else if check_conflict(range, aq, bq) {
                        add_edge!((i, j), {1, 0});
                    }
                }
                (
                    ASTNode::SQ {
                        qubit: aq,
                        gate: ag,
                        ..
                    },
                    ASTNode::VCZ {
                        q1: bq1, q2: bq2, ..
                    },
                ) => {
                    if ag.get_type() == GateType::General
                        || (ag.get_type() == GateType::AntiDiagonal && disable_antidiagonal)
                    {
                        if *aq == *bq1 || *aq == *bq2 {
                            //edges.entry((i, j)).or_insert(UnassignedEdge{min: 1, dif: 0});
                            add_edge!((i, j), {1, 0});
                        } else if check_conflict(range, aq, bq1) || check_conflict(range, aq, bq2) {
                            add_edge!((i, j), {1, 0});
                        }
                    }
                }
                (
                    ASTNode::VCZ {
                        q1: aq1, q2: aq2, ..
                    },
                    ASTNode::SQ {
                        qubit: bq,
                        gate: bg,
                        ..
                    },
                ) => {
                    if bg.get_type() == GateType::General
                        || (bg.get_type() == GateType::AntiDiagonal && disable_antidiagonal)
                    {
                        if *bq == *aq1 || *bq == *aq2 {
                            //edges.entry((i, j)).or_insert(UnassignedEdge{min: 1, dif: 0});
                            add_edge!((i, j), {1, 0});
                        } else if check_conflict(range, aq1, bq) || check_conflict(range, aq2, bq) {
                            add_edge!((i, j), {1, 0});
                        }
                    }
                }
                (ASTNode::VCZ { .. }, ASTNode::VCZ { .. }) => {}
                _ => unreachable!(),
            }
        }
    }

    edges
}

pub type AssignedGraph = Graph<usize, NotMinimum<isize>>;

pub fn assign<'a>(ast: &AST, g: &UnassignedGraph, ii: Bound) -> AssignedGraph {
    let mut t = BTreeMap::new();
    for i in g.iter() {
        t.insert(*i.0, i.1.min - ii as isize * i.1.dif);
    }
    Graph::new((0..ast.len()).collect(), t)
}

pub fn tarjan_qdg<'a>(ast: &AST, g: &UnassignedGraph) -> Vec<Vec<usize>> {
    let mut t = BTreeMap::new();
    for i in g.iter() {
        t.insert(*i.0, ());
    }
    let graph: Graph<_, bool> = Graph::new((0..ast.len()).collect(), t);
    trace!("{}\n{:?}", graph.print(), g);
    graph
        .tarjan()
        .into_iter()
        .map(|x| x.into_iter().copied().collect())
        .collect()
}
