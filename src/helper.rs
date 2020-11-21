use crate::ast::*;
use crate::frontend::*;
use ndarray::array;
use num::complex::Complex;
use log::*;
pub fn lu2_to_u2(mat: &LU2) -> U2 {
    macro_rules! conv_complex {
        ($x: expr) => {
            Complex::new($x.real, $x.imag)
        };
    }
    array![
        [conv_complex!(mat.0[0]), conv_complex!(mat.0[1])],
        [conv_complex!(mat.0[2]), conv_complex!(mat.0[3])]
    ]
}
pub fn u2_to_lu2(mat: &U2)->LU2{
    macro_rules! conv_lcomplex {
        ($x: expr) => {
            LComplex{real: $x.re, imag: $x.im}
        };
    }
    LU2([conv_lcomplex!(mat[[0,0]]),conv_lcomplex!(mat[[0,1]]),conv_lcomplex!(mat[[1,0]]),conv_lcomplex!(mat[[1,1]])])
}

pub trait OptimizationMethod{
    fn optimize(&mut self, _:LIdent,_:Result<Range, (LExpr, LExpr)>,_:Vec<ASTNode>) -> Vec<LAST>;
    fn name(&self)->String;
    fn benchmark(&self)->String;
}

pub trait OptimizationMethodExt{
    fn optimize_with_benchmark(&mut self, _:LIdent,_:Result<Range, (LExpr, LExpr)>,_:Vec<ASTNode>)->(String, Vec<LAST>);
}
impl OptimizationMethodExt for dyn OptimizationMethod{
    fn optimize_with_benchmark(&mut self, i:LIdent,r:Result<Range, (LExpr, LExpr)>,ast:Vec<ASTNode>)->(String, Vec<LAST>){
        let optimized_ast=self.optimize(i, r, ast);
        let benchmark_result = format!["[{}]\n{}",self.name(),self.benchmark()];
        (benchmark_result, optimized_ast)
    }
}

fn transform_qoffset(allowed_var: LIdent, expr: &LExpr) -> Option<QOffset> {
    macro_rules! guard {
        ($x: expr) => {
            if allowed_var != $x {
                return None;
            }
        };
    }
    macro_rules! off {
        ($k: expr, $b: expr) => {
            Some(QOffset {
                intercept: $b,
                slope: $k,
            })
        };
    }
    match expr {
        LExpr::IConst(c) => off!(0, *c as i32),
        LExpr::Var(x) => {
            guard!(*x);
            off!(1, 0)
        }
        LExpr::Mul(box LExpr::IConst(k), expr) => {
            let e = transform_qoffset(allowed_var, &*expr)?;
            off!((*k as i32) * e.slope, (*k as i32) * e.intercept)
        }
        LExpr::Mul(expr, box LExpr::IConst(k)) => {
            let e = transform_qoffset(allowed_var, &*expr)?;
            off!((*k as i32) * e.slope, (*k as i32) * e.intercept)
        }
        LExpr::Add(a, b) => {
            let lhs = transform_qoffset(allowed_var, &*a)?;
            let rhs = transform_qoffset(allowed_var, &*b)?;
            off!(lhs.slope + rhs.slope, lhs.intercept + rhs.intercept)
        }
        LExpr::Sub(a, b)=>{
            let lhs = transform_qoffset(allowed_var, &*a)?;
            let rhs = transform_qoffset(allowed_var, &*b)?;
            off!(lhs.slope - rhs.slope, lhs.intercept - rhs.intercept)
        }

        _ => None,
    }
}
fn transform_qubit_ref(allowed_var: LIdent, qubit: &LQubit) -> Option<Qubit> {
    let qoffset = transform_qoffset(allowed_var, &qubit.offset)?;
    Some(Qubit(qubit.array, qoffset))
}
fn transform_gate_ref(
    allowed_var: LIdent,
    qarray: &Vec<LGateArray>,
    gateref: &LGateRef,
) -> Option<Gate> {
    match gateref{
        LGateRef::LKnown{mat}=>{
            Some(Gate::known(lu2_to_u2(mat)))
        }
        LGateRef::LUnknown{offset, array}=>{
            let goffset = transform_qoffset(allowed_var, &offset)?;
            match (qarray.get(*array), goffset.slope) {
                (
                    Some(LGateArray {
                        known_gates: Some(list),
                        ..
                    }),
                    0,
                ) => {
                    let mat = list
                        .get({
                            if goffset.intercept < 0 || (goffset.intercept as usize) >= list.len() {
                                panic!(
                                    "Bad reference {} to gate array {} !",
                                    goffset.intercept, array
                                );
                            } else {
                                goffset.intercept as usize
                            }
                        })
                        .unwrap();
                    Some(Gate::known(lu2_to_u2(mat)))
                }
                (Some(LGateArray { hint, .. }), _) => Some(Gate::Unknown(
                    GateArray {
                        index: *array,
                        t: {
                            match hint {
                                LGateArrayHint::Diagonal => GateType::Diagonal,
                                LGateArrayHint::AntiDiagonal => GateType::AntiDiagonal,
                                LGateArrayHint::General => GateType::General,
                            }
                        },
                    },
                    goffset,
                )),
                (None, _) => {
                    panic!("Bad gate array {}", array);
                }
            }
        }
    }
    
    
}

fn is_valid_loop(
    defined_gates: &Vec<LGateArray>,
    node: &LAST,
) -> Option<(Result<Range, (LExpr, LExpr)>, AST, LIdent)> {
    trace!("LOOP:{:?}", node);
    if let LAST::For {
        variable,
        lo,
        hi,
        body,
    } = node
    {
        macro_rules! make_gate {
            ($x: expr) => {
                transform_gate_ref(*variable, defined_gates, &$x)
            };
        }
        macro_rules! make_qubit {
            ($x: expr) => {
                transform_qubit_ref(*variable, &$x)?
            };
        }
        let mut ast = AST::new();

        for insn in body.iter() {
            let new_insn = match insn {
                LAST::SQ { gate, dst: qubit } => {
                    ASTNode::SQ {
                        gate: gate.into_iter().map(|x| make_gate!(x)).collect::<Option<SQGate>>()?,
                        qubit: make_qubit!(qubit),
                        moved: false, //qubit_hint: (114, 514) // invalid value for debugging
                    }
                }
                LAST::CZ { q1, q2 } => {
                    ASTNode::VCZ {
                        q1: make_qubit!(q1),
                        q2: make_qubit!(q2),
                        p1: false,
                        p2: false,
                        cancelled: false,
                        moved: false, //q1_hint: (114, 514),
                                      //q2_hint: (114, 514)
                    }
                }
                _ => {
                    return None;
                }
            };
            ast.push(new_insn);
        }
        match (lo, hi) {
            (LExpr::IConst(a), LExpr::IConst(b)) => {
                return Some((Ok((*a as i32, *b as i32)),ast, *variable));
            }
            _ => {
                return Some((Err((lo.clone(), hi.clone())), ast, *variable));
            }
        }
    } else {
        return None;
    }
}
// This takes out innermost loops and optimize them.
// After this, a benchmark is done.
pub fn optimize_loops_with_benchmark(
    l_ast: &mut LProgram,
    mut methods: Vec<&mut (dyn OptimizationMethod+'static)>,
) {
    
    let mut ops=std::mem::replace(&mut l_ast.operations, Vec::new());
    fn map_op(def_gates: &Vec<LGateArray>, nodes: LAST, methods: &mut Vec<&mut (dyn OptimizationMethod + 'static)>)->LAST{
        if let Some((range, ast, id)) = is_valid_loop(def_gates, &nodes){
            let mut v=Vec::new();
            for x in methods.iter_mut(){
                v.push(x.optimize_with_benchmark(id, range.clone(), ast.clone()));
            }
            LAST::Benchmark{
                original_node: Box::new(nodes),
                benchmarks: v
            }
        }else if let LAST::For{variable, lo, hi, body} = nodes{
            let new_body = body.into_iter().map(|x| map_op(def_gates, x, methods)).collect::<Vec<_>>();
            LAST::For{variable, lo, hi, body: new_body}
        }else{
            nodes
        }
    }
    ops=ops.into_iter().map(|x| {
        map_op(&l_ast.defined_gates, x, &mut methods)
        /*if let Some((range, ast, id)) = is_valid_loop(&l_ast.defined_gates, &x){
            
            
            
        }else{
            x
        }*/
    }).collect();
    l_ast.operations=ops;
    /*
    l_ast.operations=l_ast.operations.into_iter().flat_map(|x| {
        unimplemented!()
    }).collect();
    l_ast
    */
}

pub fn ast_qubit_iterator<'b>(
    ast: impl Iterator<Item = &'b ASTNode> + 'b,
) -> impl Iterator<Item = Qubit> + 'b {
    ast.flat_map(|x| match x {
        ASTNode::VCZ { q1, q2, .. } => Some(*q1).into_iter().chain(Some(*q2).into_iter()),
        ASTNode::SQ { qubit, .. } => Some(*qubit).into_iter().chain(None.into_iter()),
    })
}

#[macro_export]
macro_rules! lexpr {
    ($k: expr ; $x: expr ; $b: expr) => {
        LExpr::Add(
            Box::new(
                LExpr::Mul(
                    Box::new(LExpr::IConst($k as isize)),
                    Box::new($x)
                )
            ),
            Box::new(
                LExpr::IConst($b as isize)
            )
        )
    };
}
#[macro_export]
macro_rules! lqubit {
    ($arr: expr, $offset: expr) => {
        LQubit{
            array: $arr,
            offset: $offset
        }
    };
}
pub fn ast_to_last_ident(loopvar: LIdent, astnode: &ASTNode)->LAST{
    ast_to_last(LExpr::Var(loopvar), astnode)
}
pub fn ast_to_last(loopvar: LExpr, astnode: &ASTNode)->LAST{
    match astnode{
        ASTNode::VCZ{q1, q2, p1, p2, cancelled, moved}=>{
            assert_eq!(*cancelled, false);
            assert_eq!(*moved, false);
            assert_eq!(*p1, false);
            assert_eq!(*p2, false);
            LAST::CZ{
                q1: lqubit![q1.0, lexpr!(q1.1.slope ; loopvar.clone() ; q1.1.intercept)],
                q2: lqubit![q2.0, lexpr!(q2.1.slope ; loopvar ; q2.1.intercept)],
            }
        }
        ASTNode::SQ{qubit, gate, moved}=>{
            assert_eq!(*moved, false);
            LAST::SQ{
                dst: lqubit![qubit.0, lexpr!(qubit.1.slope ; loopvar.clone() ; qubit.1.intercept)],
                gate: gate.components.iter().map(|x| {
                    match x{
                        Gate::Known(u2, _)=>{
                            LGateRef::LKnown{mat: u2_to_lu2(u2)}
                        }
                        Gate::Unknown(array, offset)=>{
                            LGateRef::LUnknown{array: array.index, offset: lexpr!(offset.slope; loopvar.clone(); offset.intercept)}
                        }
                    }
                }).collect()
            }
        }
    }
}

pub struct DebugAST<'a>(pub &'a [ASTNode]);

impl<'a> std::fmt::Debug for DebugAST<'a>{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let last=self.0.iter().map(|x| ast_to_last(liconst(0), x)).collect::<Vec<_>>();
        let temp_prog = LProgram {defined_gates: vec![], defined_qarrays: vec![], operations: last};
        let s=temp_prog.print();
        f.write_str(&s)
    }
}

pub struct DebugParAST<'a>(pub &'a[Vec<ASTNode>]);
impl<'a> std::fmt::Debug for DebugParAST<'a>{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>)->std::fmt::Result {
        let last=self.0.iter().map(|x| LAST::Parallel{ops: x.iter().map(|x| ast_to_last(liconst(0), x)).collect::<Vec<_>>()}).collect::<Vec<_>>();
        let temp_prog = LProgram {defined_gates: vec![], defined_qarrays: vec![], operations: last};
        let s=temp_prog.print();
        f.write_str(&s)
    }
}