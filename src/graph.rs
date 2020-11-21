use num_traits::identities::Zero;
use num_traits::Bounded;
use num_traits::Num;
use std::collections::btree_map::*;
#[derive(Clone)]
pub struct Graph<V: Ord, E> {
    vertices: Vec<V>,
    vindex: BTreeMap<V, usize>,
    edges: Vec<E>,
}
use itertools::*;
impl<V: Ord + std::fmt::Debug, E: Display> Graph<V, E>{
    pub fn print(&self)->String{
        let mut vertices=self.vertices.iter().collect::<Vec<_>>();
        vertices.sort_by_key(|x| *self.vindex.get(x).unwrap());
        format!("[Vertices: {}, Graph:\n{}\n]]", 
            vertices.into_iter().enumerate().map(|(i, x)| {format!("{}: {:?}", i, x)}).collect::<Vec<_>>().join(", "),
            self.edges.iter().chunks(self.vertices.len()).into_iter().map(|x| {x.map(|y| {format!("{}", y)}).collect::<Vec<_>>().join(" ")}).collect::<Vec<_>>().join("\n")
        )
    }
}
#[derive(Clone)]
pub struct FloydResult<E: Num> {
    data: Vec<E>,
}

// The trait for representing a type that has an invalid value.
// E.g. a pointer type with NULL as invalid, or a file-descriptor with -1 as invalid.
// This saves the trouble of unwrapping Some(i64) in Floyd algorithm.
pub trait NotVal {
    type E;
    fn is_valid(&self) -> bool;
    fn unwrap_ref(&self) -> &Self::E;
    fn valid(v: Self::E) -> Self;
    fn invalid() -> Self;
}
pub trait NotValExt: NotVal {
    fn to_option_ref(&self) -> Option<&Self::E>;
}
impl<T: NotVal> NotValExt for T {
    fn to_option_ref(&self) -> Option<&Self::E> {
        if self.is_valid() {
            Some(self.unwrap_ref())
        } else {
            None
        }
    }
}

// Option<T> is of course a NotVal
impl<T> NotVal for Option<T> {
    type E = T;
    fn is_valid(&self) -> bool {
        self.is_some()
    }
    fn unwrap_ref(&self) -> &Self::E {
        self.as_ref().unwrap()
    }
    fn valid(v: Self::E) -> Self {
        Some(v)
    }
    fn invalid() -> Self {
        None
    }
}

impl NotVal for bool {
    type E = ();
    fn is_valid(&self) -> bool {
        *self
    }
    fn unwrap_ref(&self) -> &Self::E {
        &()
    }
    fn valid(_v: Self::E) -> Self {
        true
    }
    fn invalid() -> Self {
        false
    }
}

// The wrapper for an integer, with the minimum value (i.e. 0b80000000_00000000 for i64 and 0 for u64) seen as invalid.
#[derive(Clone, Copy)]
pub struct NotMinimum<T: Bounded + Eq>(T);
use std::fmt::Display;
impl<T: Display + Bounded + Eq> Display for NotMinimum<T>{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>)->std::fmt::Result{
        if let Some(val)=self.to_option_ref(){
            f.write_str(&format!("{}", val))
        }else{
            use std::fmt::Write;
            f.write_char('X')
        }
    }
}

impl<T: Bounded + Eq> NotVal for NotMinimum<T> {
    type E = T;
    fn is_valid(&self) -> bool {
        self.0 != T::min_value()
    }
    fn unwrap_ref(&self) -> &T {
        if self.is_valid() {
            &self.0
        } else {
            panic!("unwrap_ref() for NotMinimum Failed");
        }
    }
    fn valid(v: T) -> Self {
        let val = NotMinimum(v);
        if val.is_valid() {
            val
        } else {
            panic!("Trying to construct a valid NotMinimum with invalid value.");
        }
    }
    fn invalid() -> Self {
        NotMinimum(T::min_value())
    }
}

impl<V: Ord, E> Graph<V, E> {
    pub fn size(&self) -> usize {
        self.vertices.len()
    }
    pub fn edge(&self, i: usize, j: usize) -> &E {
        &self.edges[i * self.size() + j]
    }
}
impl<V: Ord + Clone, E: NotVal> Graph<V, E> {
    pub fn new(vertices: Vec<V>, edges: BTreeMap<(V, V), E::E>) -> Self {
        let mut revindex = BTreeMap::new();
        for i in 0..vertices.len() {
            revindex.insert(vertices[i].clone(), i);
        }
        let mut edge_vec = Vec::new();
        let n = vertices.len();
        edge_vec.resize_with(n * n, E::invalid);

        for ((a, b), e) in edges {
            let ia = revindex[&a];
            let ib = revindex[&b];
            edge_vec[ia * n + ib] = E::valid(e);
        }
        Graph {
            vertices,
            vindex: revindex,
            edges: edge_vec,
        }
    }
}
impl<V: Ord, E: Clone> Graph<V, E> {
    pub fn adjacent_matrix(&self) -> Vec<E> {
        self.edges.clone()
    }
}
impl<V: Ord, E: NotVal> Graph<V, E> {
    pub fn adjacent_table(&self) -> Vec<Vec<usize>> {
        let mut ret = Vec::new();
        for v in 0..self.size() {
            let mut vec = Vec::new();
            for w in 0..self.size() {
                if self.edge(v, w).is_valid() {
                    vec.push(w);
                }
            }
            ret.push(vec);
        }
        ret
    }
    pub fn edge_option(&self, i: usize, j: usize) -> Option<&E::E> {
        let r = self.edge(i, j);
        r.to_option_ref()
    }
}
// Floyd algorithm.
impl<V: Ord, E: Clone + NotVal> Graph<V, E>
where
    E::E: Num + Clone + Ord,
{
    pub fn floyd(&self) -> Option<Vec<E>> {
        let n = self.size();
        let mut data = self.edges.clone();
        for k in 0..n {
            for i in 0..n {
                for j in 0..n {
                    let ik = &data[i * n + k];
                    if let Some(vik) = ik.to_option_ref() {
                        let kj = &data[k * n + j];
                        if let Some(vkj) = kj.to_option_ref() {
                            let new_ij = vik.clone() + vkj.clone();
                            let ij = &data[i * n + j];
                            if let Some(vij) = ij.to_option_ref() {
                                if new_ij > *vij {
                                    data[i * n + j] = E::valid(new_ij);
                                }
                            } else {
                                data[i * n + j] = E::valid(new_ij);
                            }
                        }
                    }
                }
            }
        }
        for k in 0..n {
            if let Some(vkk) = data[k * n + k].to_option_ref() {
                if *vkk > E::E::zero() {
                    return None;
                }
            }
        }
        Some(data)
    }
}

struct StrongConnectEnv<'a, V> {
    v_index: Vec<Option<usize>>,
    v_lowlink: Vec<usize>,
    v_onstack: Vec<bool>,
    stack: Vec<usize>,
    output: Vec<Vec<&'a V>>,
    index: usize,
}

impl<V: Ord, E: NotVal> Graph<V, E> {
    fn strong_connect<'a>(
        &'a self,
        adjt: &Vec<Vec<usize>>,
        senv: &mut StrongConnectEnv<'a, V>,
        v: usize,
    ) {
        let vi = senv.index;
        senv.v_index[v] = Some(vi);
        senv.v_lowlink[v] = senv.index;
        senv.index = senv.index + 1;
        senv.stack.push(v);
        senv.v_onstack[v] = true;
        for w in adjt[v].iter() {
            if let Some(wi) = senv.v_index[*w] {
                if senv.v_onstack[*w] {
                    senv.v_lowlink[v] = std::cmp::min(senv.v_lowlink[v], wi);
                }
            } else {
                self.strong_connect(adjt, senv, *w);
                senv.v_lowlink[v] = std::cmp::min(senv.v_lowlink[v], senv.v_lowlink[*w]);
            }
        }
        if senv.v_lowlink[v] == vi {
            let mut scc = Vec::new();
            loop {
                let w = senv.stack.pop().unwrap();
                senv.v_onstack[w] = false;
                scc.push(&self.vertices[w]);
                if w == v {
                    break;
                }
            }
            senv.output.push(scc);
        }
    }
    /// Tarjan algorithm
    pub fn tarjan(&self) -> Vec<Vec<&V>> {
        let mut senv = StrongConnectEnv {
            v_index: Vec::new(),
            v_lowlink: Vec::new(),
            v_onstack: Vec::new(),
            stack: Vec::new(),
            output: Vec::new(),
            index: 0,
        };
        let adjt = self.adjacent_table();
        for _v in 0..self.size() {
            senv.v_index.push(None);
            senv.v_lowlink.push(0);
            senv.v_onstack.push(false);
        }

        for v in 0..self.size() {
            if senv.v_index[v].is_none() {
                self.strong_connect(&adjt, &mut senv, v);
            }
        }
        senv.output
    }
}
