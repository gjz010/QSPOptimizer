use ndarray::array;
use ndarray::Array2;
use num::complex::Complex;
use std::iter::*;
use std::sync::Arc;
use std::vec::*;
pub type U2 = Array2<Complex<f64>>;

pub enum GateSet {
    I,
    X,
    Y,
    Z,
    H,
    S,
    T,
}
impl GateSet {
    pub fn matrix(self) -> U2 {
        use GateSet::*;
        let invsqrt2 = 0.7071067811865476;
        match self {
            I => array![
                [Complex::new(1.0, 0.0), Complex::new(0.0, 0.0)],
                [Complex::new(0.0, 0.0), Complex::new(1.0, 0.0)]
            ],
            X => array![
                [Complex::new(0.0, 0.0), Complex::new(1.0, 0.0)],
                [Complex::new(1.0, 0.0), Complex::new(0.0, 0.0)]
            ],
            Y => array![
                [Complex::new(0.0, 0.0), Complex::new(0.0, -1.0)],
                [Complex::new(0.0, 1.0), Complex::new(0.0, 0.0)]
            ],
            Z => array![
                [Complex::new(1.0, 0.0), Complex::new(0.0, 0.0)],
                [Complex::new(0.0, 0.0), Complex::new(-1.0, 0.0)]
            ],
            H => array![
                [Complex::new(invsqrt2, 0.0), Complex::new(invsqrt2, 0.0)],
                [Complex::new(invsqrt2, 0.0), Complex::new(-invsqrt2, 0.0)]
            ],
            S => array![
                [Complex::new(1.0, 0.0), Complex::new(0.0, 0.0)],
                [Complex::new(0.0, 0.0), Complex::new(0.0, 1.0)]
            ],
            T => array![
                [Complex::new(1.0, 0.0), Complex::new(0.0, 0.0)],
                [Complex::new(0.0, 0.0), Complex::new(invsqrt2, invsqrt2)]
            ],
        }
    }
}
pub trait U2Ext {
    fn get_type(&self) -> GateType;
    fn is_identity(&self) -> bool;
} 
impl U2Ext for U2 {
    fn get_type(&self) -> GateType {
        use GateType::*;
        let val = self[[0, 0]];
        let val2 = val.norm_sqr();
        if val2 >= 0.999 {
            Diagonal
        } else if val2 <= 0.001 {
            AntiDiagonal
        } else {
            General
        }
    }
    fn is_identity(&self)->bool{
        if self.get_type()!=GateType::Diagonal{
            return false;
        }
        let v1 = self[[0, 0]];
        let v2 = self[[1, 1]];
        let delta = v1-v2;
        let v = delta.norm_sqr();
        v<=0.001
    }
}
pub type QArray = usize;
pub type Bound = i32;

pub trait QOffsetOps{
    fn map_qoffset(&mut self, f: impl Fn(QOffset)->QOffset);
}

pub trait QOffsetOpsExt{
    fn incr(self) -> Self;
    fn incr_by(self, step: i32) -> Self;
    fn assign(&mut self, x: i32);
}

impl<T> QOffsetOpsExt for T where T: QOffsetOps{
    fn incr(self) -> Self {
        self.incr_by(1)
    }
    fn incr_by(mut self, step: i32) -> Self {
        self.map_qoffset(|s| QOffset {
            slope: s.slope,
            intercept: s.intercept + s.slope * step,
        });
        self
        
    }
    fn assign(&mut self, x: i32) {
        self.map_qoffset(|s| QOffset {
            intercept: x * s.slope + s.intercept,
            slope: 0
        });
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct QOffset {
    pub slope: i32,
    pub intercept: i32,
}

impl QOffsetOps for QOffset{
    fn map_qoffset(&mut self, f: impl Fn(QOffset)->QOffset){
        *self=f(*self);
    }
}
/*
impl QOffset {
    pub fn incr(self) -> Self {
        self.incr_by(1)
    }
    pub fn incr_by(self, step: i32) -> Self {
        QOffset {
            slope: self.slope,
            intercept: self.intercept + self.slope * step,
        }
    }
    pub fn assign(&mut self, x: i32) {
        self.intercept = x * self.slope + self.intercept;
        self.slope = 0;
    }
}
*/
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Qubit(pub QArray, pub QOffset);

impl QOffsetOps for Qubit{
    fn map_qoffset(&mut self, f: impl Fn(QOffset)->QOffset){
        self.1.map_qoffset(f);
    }
}
/*
impl Qubit {
    pub fn incr(self) -> Self {
        self.incr_by(1)
    }
    pub fn incr_by(self, step: i32) -> Self {
        Qubit(self.0, self.1.incr_by(step))
    }
    pub fn assign(&mut self, x: i32) {
        self.1.assign(x);
    }
}*/
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GateType {
    Diagonal,
    AntiDiagonal,
    General,
}

impl std::ops::Mul for GateType {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self {
        use GateType::*;
        match (self, rhs) {
            (General, _) => General,
            (Diagonal, Diagonal) => Diagonal,
            (AntiDiagonal, AntiDiagonal) => Diagonal,
            (Diagonal, AntiDiagonal) => AntiDiagonal,
            (a, b) => b * a,
        }
    }
}
#[derive(Clone, Copy, Debug)]
pub struct GateArray {
    pub index: usize,
    pub t: GateType,
}
#[derive(Clone, Debug)]
pub enum Gate {
    // Chosen if some hint is known in advance, e.g. all gates are the same across iterations.
    Known(Arc<U2>, GateType),
    // Chosen if the gate is also given as argument.
    Unknown(GateArray, QOffset /*offset*/),
}

impl QOffsetOps for Gate{
    fn map_qoffset(&mut self, f: impl Fn(QOffset)->QOffset){
        match self{
            Gate::Unknown(ga, qo) => qo.map_qoffset(f),
            _=>()
        }
    }
}
impl Gate {
    pub fn get_type(&self) -> GateType {
        match self {
            Gate::Known(_, a) => *a,
            Gate::Unknown(b, _) => b.t,
        }
    }
    pub fn known(mat: U2) -> Self {
        let t = mat.get_type();
        Self::Known(Arc::new(mat), t)
    }
    /*
    pub fn incr(self) -> Self {
        self.incr_by(1)
    }
    pub fn incr_by(self, step: i32) -> Self {
        match self {
            Gate::Unknown(ga, qo) => Gate::Unknown(ga, qo.incr_by(step)),
            a => a,
        }
    }
    pub fn assign(&mut self, x: i32) {
        match self{
            Gate::Unknown(ga, qo)=>qo.assign(x),
            a=>()
        }
    }
    */
}
/// We use the symbolic representation rather than the matrix representation.
/// This allows us to simply generalize to the case where matrices vary across iterations.
#[derive(Clone, Debug)]
pub struct SQGate {
    pub components: Vec<Gate>, // 0 is the first instruction and the leftmost instruction in circuit diagram.
    type_hint: GateType,
}


impl SQGate {
    pub fn new() -> Self {
        SQGate {
            // components[0] is executed first.
            components: Vec::new(),
            type_hint: GateType::Diagonal,
        }
    }
    pub fn singleton(gate: Gate) -> Self {
        SQGate {
            type_hint: gate.get_type(),
            components: vec![gate],
        }
    }
    pub fn is_empty(&self) -> bool {
        self.components.len() == 0
    }
    pub fn get_type(&self) -> GateType {
        self.type_hint
    }
    pub fn add_gate(&mut self, gate: Gate) {
        if let (Some(Gate::Known(a, b)), Gate::Known(c, _d)) = (self.components.last_mut(), &gate) {
            let mat = c.as_ref().dot(a.as_ref());
            if mat.is_identity(){
                self.components.pop();
            }else{
                let new_type = mat.get_type();
                *a = Arc::new(mat);
                *b = new_type;
            }
            
        } else {
            self.components.push(gate);
        }
        self.refresh_type();
    }
    // sqgate is executed ***after*** self.
    pub fn merge_sqgate(&mut self, sqgate: SQGate) {
        for g in sqgate.components {
            self.add_gate(g);
        }
        self.refresh_type();
    }
    // self is executed ***after** sqgate.
    pub fn merge_sqgate_left(&mut self, mut sqgate: SQGate) {
        let c = std::mem::replace(&mut self.components, Vec::new());
        for g in c {
            sqgate.add_gate(g);
        }
        *self = SQGate { ..sqgate };
        self.refresh_type();
    }
    // The procedure for regenerating hints.
    // Checks all matrices in components and compute the hint.
    pub fn refresh_type(&mut self) {
        self.type_hint = 'ret: {
            use GateType::*;
            let mut flag = false;
            for i in self.components.iter() {
                match i.get_type() {
                    Diagonal => {}
                    AntiDiagonal => {
                        flag = !flag;
                    }
                    General => {
                        break 'ret General;
                    }
                }
            }
            if flag {
                AntiDiagonal
            } else {
                Diagonal
            }
        }
    }
    /*
    pub fn incr(self) -> Self {
        self.incr_by(1)
    }
    pub fn incr_by(self, step: i32) -> Self {
        SQGate {
            components: self
                .components
                .into_iter()
                .map(|x| x.incr_by(step))
                .collect(),
            type_hint: self.type_hint,
        }
    }
    pub fn assign(&mut self, x: i32) {
        self.components.iter_mut().for_each(|g| g.assign(x));
    }
    */
}
impl QOffsetOps for SQGate{
    fn map_qoffset(&mut self, f: impl Fn(QOffset)->QOffset){
        let rf=&f;
        for q in self.components.iter_mut(){
            q.map_qoffset(rf);
        }
    }
}
impl FromIterator<Gate> for SQGate {
    fn from_iter<I: IntoIterator<Item=Gate>>(iter: I) -> Self {
        let mut gate=Self::new();
        for i in iter{
            gate.add_gate(i);
        }
        gate
    }
}
/// AST Structure for program.
#[derive(Clone, Debug)]
pub enum ASTNode {
    SQ {
        gate: SQGate,
        qubit: Qubit,
        moved: bool, //qubit_hint: (usize, usize)
    },
    VCZ {
        q1: Qubit,
        q2: Qubit,
        p1: bool,
        p2: bool,
        cancelled: bool,
        moved: bool, //q1_hint: (usize, usize),
                     //q2_hint: (usize, usize)
    },
}

pub type AST = Vec<ASTNode>;
pub type Range = (Bound, Bound);

impl QOffsetOps for ASTNode{
    fn map_qoffset(&mut self, f: impl Fn(QOffset)->QOffset){
        match self{
            Self::SQ{qubit, gate, moved}=>{qubit.map_qoffset(&f); gate.map_qoffset(&f);}
            Self::VCZ{q1, q2, ..}=>{q1.map_qoffset(&f); q2.map_qoffset(&f);}
        }
    }
}
/*
impl ASTNode {
    pub fn incr(self) -> Self {
        self.incr_by(1)
    }
    pub fn incr_by(self, step: i32) -> Self {
        match self {
            Self::SQ { qubit, gate, moved } => Self::SQ {
                gate: gate.incr_by(step),
                qubit: qubit.incr_by(step),
                moved, //qubit_hint
            },
            Self::VCZ {
                q1,
                q2,
                p1,
                p2,
                cancelled,
                moved,
            } => Self::VCZ {
                q1: q1.incr_by(step),
                q2: q2.incr_by(step),
                p1,
                p2,
                cancelled,
                moved,
            },
        }
    }
    pub fn assign(&mut self, x: i32) {
        match self{
            Self::SQ{qubit, gate, moved}=>{qubit.assign(x); gate.assign(x);}
            Self::VCZ{q1, q2, ..}=>{q1.assign(x); q2.assign(x);}
        }
    }
}
*/
/*
pub fn optimize_innermost_loops(ast: AST, optimize: fn(Bound, Bound, AST) -> AST) -> AST {
    fn is_innermost(node: &AST) -> bool {
        node.iter().all(|node: &ASTNode| {
            if let ASTNode::For { .. } = node {
                false
            } else {
                true
            }
        })
    }
    ast.into_iter()
        .flat_map(|node: ASTNode| match node {
            ASTNode::For {
                lower_bound,
                upper_bound,
                body,
            } => {
                if is_innermost(&body) {
                    optimize(lower_bound, upper_bound, body)
                } else {
                    optimize_innermost_loops(body, optimize)
                }
            }
            x => vec![x],
        })
        .collect()
}
*/
