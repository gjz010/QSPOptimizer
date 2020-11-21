mod circsched;
mod invfix;
mod merge;
mod qdg;
mod sp;
mod unroll;

use crate::ast::*;
use crate::frontend::*;
use crate::lexpr;
use crate::helper::*;
use circsched::*;
use invfix::*;
use merge::*;
use qdg::*;
use sp::*;
use unroll::*;
use log::*;
pub trait ResultExt<T>{
    fn ok_ref(&self)->Option<T>;
}
impl<T: Clone, E> ResultExt<T> for Result<T, E>{
    fn ok_ref(&self)->Option<T>{
        match self{
            Ok(x)=>Some(x.clone()),
            Err(_)=>None
        }
    }
}
pub trait ResultExt2<E>{
    fn err_ref(&self)->Option<E>;
}
impl<T, E: Clone> ResultExt2<E> for Result<T, E>{
    fn err_ref(&self)->Option<E>{
        match self{
            Ok(_)=>None,
            Err(y)=>Some(y.clone())
        }
    }
}

#[derive(Clone, Debug)]
pub struct OptimizeInfo{
    info_rotated_version: String,
    info_ms1: String,
    info_ms2: String
}

fn unrolled_depth(lo: isize, hi: isize, body: &Vec<ASTNode>)->Vec<Vec<ASTNode>>{
    //let body=left_merge_adjacent_gates(body, Some((lo as i32, hi as i32)));
    
    let body=(lo..=hi).flat_map(|x| {body.iter().cloned().map(move |mut y| {y.assign(x as i32); y})}).collect::<Vec<_>>();
    let ret=asap_schedule(left_merge_adjacent_gates(body, None), &None);
    trace!("unrolled_depth_after_asap:\n{:?}", ret.len());
    ret
    //unimplemented!()
}

pub fn optimize(
    variable: LIdent,
    range: Result<Range, (LExpr, LExpr)>,
    ast: Vec<ASTNode>,
    max_unroll_iterations: Option<Bound>,
) -> Result<(Vec<LAST>, OptimizeInfo), Vec<LAST>> {
    let mut original_version=ast.clone();
    let original_range = range.ok_ref();
    let original_lrange = range.err_ref();
    let fail_range = range.clone();
    macro_rules! par {
        ($variable: expr, $x: expr) => {
            par!($variable, $x, iter).collect()
        };
        ($variable: expr,$x: expr, iter)=>{
            $x.map(|x| LAST::Parallel{ops: x.iter().map(|y| ast_to_last_ident($variable, &y)).collect()})
        };
        ($variable: expr,$x: expr, $var: expr)=>{
            $x.map(|x| LAST::Parallel{ops: x.iter().map(|y| ast_to_last($var.clone(), &y)).collect()})
        }
    }
    macro_rules! fail {
        ($original_version: expr, $fail_range: expr, $variable: expr) => {
            let body=par!($variable, asap_schedule($original_version, &$fail_range.ok_ref()).into_iter());
            let (m,n) = match $fail_range{
                Ok((p,q))=>(liconst(p as isize), liconst(q as isize)),
                Err(r)=>r
            };
            return Err(vec!(LAST::For{
                variable: $variable,
                lo: m,
                hi: n,
                body
            }));
        };
    }
    let r = range.ok_ref();
    debug!("Performing left merging");
    let lnf = left_merge_adjacent_gates(ast, r);
    let lnf_copy = lnf.clone();
    //trace!("lnf = {:?}", &lnf);
    let lnf_asap = asap_schedule(lnf_copy, &r);
    //trace!("lnf_asap = {:?}", &lnf_asap);
    debug!("Finding unroll time");
    let unroll_time = find_unroll_time(&lnf, max_unroll_iterations);
    debug!("Unrolling loop");
    let unrolled_version = unroll_loop(lnf, unroll_time, r);
    //trace!("unroll_loop_version({}):\n{:?}", unroll_time, &unrolled_version);
    // for in in 0..((b-a+1)/T) do{}
    let (final_tree, r0, info_1) = match range.clone(){
        Ok((lo, hi))=>{
            let upper_bound=(hi-lo+1)/unroll_time as i32-1;
            let tail_start = lo+(upper_bound+1)*unroll_time as i32;
            if upper_bound<0{
                fail!(original_version, fail_range, variable);
            }
            let final_tree = if hi>=tail_start{
                Some(LAST::For{
                    variable,
                    lo: LExpr::IConst(tail_start as isize),
                    hi: LExpr::IConst(hi as isize),
                    body: par!(variable, asap_schedule(original_version.clone(), &Some((tail_start, hi))).into_iter())
                })
            }else{
                None
            };
            let r = Some((0, upper_bound));
            let unrolled_lnf_asap = asap_schedule(left_merge_adjacent_gates(unrolled_version.clone(), r), &r);
            let total_unroll_len = unrolled_depth(lo as isize, hi as isize, &original_version);
            //trace!("v1=\n{:?}\nv2=\n{:?}\n", &unrolled_lnf_asap, &total_unroll_len);

            //let unrolled_version_unrolled_len = unrolled_depth(0, upper_bound as isize, &unrolled_version);
            //trace!("COMPARE_V1:\n{:?}", DebugParAST(&unrolled_version_unrolled_len));
            let info_1 = format!("unroll_time = {}, lo={}, hi={}, |lnf_asap| = {}, original depth = {}, |unrolled_lnf_asap| = {}, optimal_unroll_depth = {}, unrolled_unrolled_len = {}", 
                    unroll_time, lo, hi, lnf_asap.len(), (hi-lo+1) as usize * lnf_asap.len(), unrolled_lnf_asap.len(), total_unroll_len.len(), -1);
            (final_tree, Ok((0, upper_bound)), info_1)
        }
        Err((llo, lhi))=>{
            let upper_bound = lsub(ldiv(ladd(lsub(lhi.clone(), llo.clone()), liconst(1)), liconst(unroll_time as isize)), liconst(1));
            let tail_start = ladd(llo.clone(), lmul(ladd(upper_bound, liconst(1)), liconst(unroll_time as isize)));
            let final_tree = Some(LAST::For{    
                variable,
                lo: tail_start.clone(),
                hi: lhi,
                body: asap_schedule(original_version.clone(), &None).into_iter().map(|x| LAST::Parallel{ops: x.iter().map(|y| ast_to_last_ident(variable, &y)).collect()}).collect()
            });
            (final_tree, Err((liconst(0), tail_start)), String::new())
        }
    };

    
    // This should be executed on every branch of unrolling.
    fn schedule_steps_after_unroll(
        unrolled_version: &Vec<ASTNode>, r0: Option<(i32,i32)>, unroll_time: usize,
        fail_range: Result<Range, (LExpr, LExpr)>, original_version: Vec<ASTNode>, variable: LIdent)->
        Result<(impl Iterator<Item=ASTNode>, Vec<Vec<ASTNode>>, impl Iterator<Item=ASTNode>, i32, i32, Vec<ASTNode>, Result<Range, (LExpr, LExpr)>), Vec<LAST>>{
        debug!("Loop rotation");
        let (rotated_prologue, rotated_kernel, rotated_epilogue, rotated_bound) =
            rotate_ast(unrolled_version.clone(), r0);
        // first time modulo scheduling.
        let r1 = match r0{
            Some((lo, hi))=>{
                // b'
                let hi=hi-rotated_bound as i32;
                if hi<lo{
                    fail!(original_version, fail_range, variable);
                }
                // TODO: prologue and epilogue also need to be assigned. We ignore them since we are working on known case.
                //let unrolled_rotated_version = rotated_prologue.iter().cloned().chain((lo..=hi).flat_map(|i| rotated_kernel.iter().cloned().map(move |mut x| {x.assign(i); x}))).chain(rotated_epilogue.iter().cloned());
                //trace!("|unrolled_rotated_version|: {}", asap_schedule(left_merge_adjacent_gates(unrolled_rotated_version.collect(), None), &None).len());
                Some((lo, hi))
            }
            None=>{
                None
            }
        };
        
        let info_rotated_version = format!(
            "rotated_prologue: {:?}\nrotated_kernel:{:?}\nrotated_epilogue:{:?}\nrotated_bound:{:?}\nr1: {:?}", 
            DebugAST(&rotated_prologue), 
            DebugAST(&rotated_kernel), 
            DebugAST(&rotated_epilogue), 
            &rotated_bound, &r1);
        debug!("info_rotated_version: \n{}\n", &info_rotated_version);
        let r1_known=r1;
        let qdg1 = build_qdg(&rotated_kernel, &r1_known, false);
        let n1 = rotated_kernel.len();
        debug!("Modulo scheduling #1");
        let ms1 = try_modulo_scheduling_in_range(&rotated_kernel, qdg1, r1_known);
        if let None=&ms1{
            fail!(original_version, fail_range, variable);
        }
        let ms1=ms1.unwrap();
        let fixed_schedule_1 = fix_inversion_pairs(n1, ms1);
        let (p1, k1, e1, f1, l1, a1, b1) = generate_code_unknown(fixed_schedule_1);
        let info_ms1=format!("ms1_prologue: {:?}\nms1_kernel: {:?}, ms1_epilogue: {:?}\n, first: {:?}\nlast: {:?}, a: {:?}, b: {:?}", 
        DebugAST(&p1), 
        DebugParAST(&k1), 
        DebugAST(&e1), f1, l1, a1, b1);
        debug!("info_ms1: \n{}\n", &info_ms1);
        let k1 = k1.into_iter().flat_map(|x| x.into_iter()).collect();
        // shortened range.
        let r2 = match r1{
            Some((lo, hi))=>{
                let lo=lo+f1 as i32;
                let hi=hi+l1 as i32;
                if hi<lo{
                    fail!(original_version, fail_range, variable);
                }
                Some((lo, hi))
            }
            None=>{
                None
            }
        };
        let r2_known=r2;
        let qdg2 = build_qdg(&k1, &r2_known, true);
        let n2 = k1.len();
        debug!("Modulo scheduling #2");
        let ms2 = try_modulo_scheduling_in_range(&k1, qdg2, r2_known);
        if let None=&ms2{
            fail!(original_version, fail_range, variable);
        }
        let ms2=ms2.unwrap();
        // No need to fix inversion pairs!
        let fixed_schedule_2 = ms2
            .instructions
            .into_iter()
            .map(|x| {
                x.into_iter()
                    .map(|y| FlattenedInstruction {
                        node: y,
                        appended_zs: Vec::new(),
                    })
                    .collect()
            })
            .collect();
        let (p2, k2, e2, f2, l2, a2, b2) = generate_code_unknown(fixed_schedule_2);
        let info_ms2=format!("ms2_prologue: {:?}\nms2_kernel: {:?}, ms2_epilogue: {:?}\n, first: {:?}\nlast: {:?}, a: {:?}, b: {:?}", DebugAST(&p2), DebugParAST(&k2), DebugAST(&e2), f2, l2, a2, b2);
        debug!("info_ms2: \n{}\n", &info_ms2);
        // Now let's consider prologue and epilogue after loop unrolling.
        // for range [a, b] after loop unrolling:
        // (a, a-M1, (a-M1+D1)-M2)=>(0, -M1, -M1+D1-M2)
        // (b-T+1, (b-T)-m1, (b-T-D1)-m2)=>(-T+1, -T+m1, -T-D1+m2)
        let prologue = rotated_prologue.into_iter().chain(
            p1.into_iter().map(move |x| x.incr_by(a1 as i32))
        ).chain(
            p2.into_iter().map(move |x| x.incr_by(f1 as i32 + a2 as i32))
        );
        let epilogue = e2.into_iter().map(move |x| x.incr_by(-(unroll_time as i32) + l1 as i32 + b2 as i32)).chain(
            e1.into_iter().map(move |x| x.incr_by(-(unroll_time as i32) + b1 as i32))
        ).chain(
            rotated_epilogue.into_iter().map(move |x| x.incr_by(-(unroll_time as i32) +1))
        );
        let r3 = match r2{
            Some((lo, hi))=>{
                let lo=lo+f2 as i32;
                let hi=hi+l2 as i32;
                if hi<lo{
                    fail!(original_version, fail_range, variable);
                }
                Some((lo, hi))
            }
            None=>{
                None
            }
        };
        let bound_a = (f1+f2) as i32;
        let bound_b = -(rotated_bound as i32)+l1 as i32+l2 as i32;
        debug!(">>>>>>>>>>>>>>>>>>>>>");
        debug!("Constructing code");
        debug!("bound_a = {}, bound_b = {}, r0 = {:?}", &bound_a, &bound_b, r0);
        debug!("<<<<<<<<<<<<<<<<<<<<<");
        Ok((prologue, k2, epilogue, bound_a, bound_b, original_version, fail_range))
    }
    Ok(match r0{
        Ok((lo, hi))=>{
            let (prologue, k2, epilogue, bound_a, bound_b, original_version, fail_range) = 
            schedule_steps_after_unroll(&unrolled_version, Some((lo, hi)), unroll_time, fail_range, original_version, variable)?;
            let ast_prologue=left_merge_adjacent_gates(prologue.map(|mut x| {x.assign(lo); x}).collect(), None);
            let ast_epilogue=left_merge_adjacent_gates(epilogue.map(|mut x| {x.assign(hi); x}).collect(), None);
            let unrolled_ms_version = ast_prologue.iter().cloned().chain((lo+bound_a..=hi+bound_b).flat_map(|i| k2.iter().flat_map(|line| line.iter().cloned()).map(move |mut x| {x.assign(i); x}))).chain(ast_epilogue.iter().cloned()).collect::<Vec<_>>();
            let astp_prologue= asap_schedule(ast_prologue, &None);
            let astp_epilogue= asap_schedule(ast_epilogue, &None);
            let prologue=par!(variable, astp_prologue.into_iter(), iter).collect::<Vec<_>>();
            let epilogue=par!(variable, astp_epilogue.into_iter(), iter).collect::<Vec<_>>();
            //trace!("COMPARE_V2:\n{:?}", DebugParAST(&asap_schedule(left_merge_adjacent_gates(unrolled_ms_version, None), &None)));
            let info_2 = {
                let prologue_length = prologue.len();
                let epilogue_length = epilogue.len();
                let kernel_length = k2.len();
                let lo = (lo+bound_a) as isize;
                let hi = (hi+bound_b) as isize;
                let span = (hi-lo+1) as usize;
                format!("lo={}, hi={}, pl={}, kl={}, el={}, total={}", lo, hi, prologue_length, kernel_length, epilogue_length, span*kernel_length+prologue_length+epilogue_length)
            };
            (
                prologue.into_iter().chain(
                    Some(LAST::For{
                        variable,
                        lo: liconst((lo+bound_a) as isize),
                        hi: liconst((hi+bound_b) as isize),
                        body: par!(variable, k2.into_iter())
                    }).into_iter()
                ).chain(
                    epilogue.into_iter()
                ).chain(final_tree.into_iter()).collect::<Vec<_>>(), 
                OptimizeInfo{info_rotated_version: String::new(), info_ms1: info_1, info_ms2: info_2}
            )
            //unimplemented!();
        }
        Err((llo, lhi))=>{
            let (m,_)=original_lrange.unwrap();
            let p=ldiv(m.clone(), liconst(unroll_time as isize));
            let pc=lmul(p, liconst(unroll_time as isize));
            let q_expr=lmod(m, liconst(unroll_time as isize));
            let mut moved_fail_range = Some(fail_range);
            let mut moved_original_version=Some(original_version);
            let branches = (0..unroll_time).map(|q| ( {
                let kernel = unrolled_version.iter().cloned()
                                .map(|mut x| {x.map_qoffset(|qo| QOffset {slope: qo.slope, intercept: qo.intercept + qo.slope * q as i32});x})
                                .collect::<Vec<_>>();
                let (prologue, k2, epilogue, bound_a, bound_b, original_version_2, fail_range_2) = 
                    schedule_steps_after_unroll(&kernel, None, unroll_time, std::mem::replace(&mut moved_fail_range, None).unwrap(), 
                        std::mem::replace(&mut moved_original_version, None).unwrap(), variable)?;
                moved_original_version = Some(original_version_2);
                moved_fail_range = Some(fail_range_2);
                let prologue = par!(variable, asap_schedule(left_merge_adjacent_gates(prologue.collect(), None), &None).into_iter(), llo).collect::<Vec<_>>();
                let epilogue = par!(variable, asap_schedule(left_merge_adjacent_gates(epilogue.collect(), None), &None).into_iter(), lhi).collect::<Vec<_>>();
                let info_2 = {
                    let prologue_length = prologue.len();
                    let epilogue_length = epilogue.len();
                    let kernel_length = k2.len();
                    trace!("C={} bound_a = {}, bound_b = {}, pl={}, kl={}, el={}", q, bound_a, bound_b, prologue_length, kernel_length, epilogue_length)
                };
                let body = prologue.into_iter().chain(
                    Some(
                        LAST::For{
                            variable,
                            lo: ladd(llo.clone(), liconst(bound_a as isize)),
                            hi: ladd(lhi.clone(), liconst(bound_b as isize)),
                            body: par!(variable, k2.into_iter())
                        }
                    ).into_iter()
                ).chain(epilogue.into_iter()).collect::<Vec<_>>();
                
                Ok((lcmp(q_expr.clone(), liconst(q as isize), LOrdering::EQ), body))
            })).collect::<Result<Vec<_>, Vec<_>>>()?;
            let fail_range = moved_fail_range.unwrap();
            let (olo, ohi)= fail_range.unwrap_err();
            let original_version = moved_original_version.unwrap();
            
            let branch_guard = Some(LAST::Guard{
                branches,
                otherwise: vec![]
            }).into_iter().chain(final_tree.into_iter()).collect::<Vec<_>>();
            let result=vec!(LAST::Guard{
                branches: vec![
                    (lcmp(lhi, llo, LOrdering::GE), branch_guard   
                    )
                ],
                otherwise: vec![LAST::For{
                    variable,
                    lo: olo,
                    hi: ohi,
                    body: par!(variable, asap_schedule(original_version, &None).into_iter())
                }]
            });
            (result, OptimizeInfo{info_rotated_version: String::new(), info_ms1: String::new(), info_ms2: String::new()})
        }
    })/*
    Ok((match r0{
        Ok((lo, hi))=>{
            let prologue=par!(variable, asap_schedule(prologue.map(|mut x| {x.assign(lo); x}).collect(), &None).into_iter(), iter);
            let epilogue=par!(variable, asap_schedule(epilogue.map(|mut x| {x.assign(hi); x}).collect(), &None).into_iter(), iter);
            prologue.chain(
                Some(LAST::For{
                    variable,
                    lo: liconst((lo+bound_a) as isize),
                    hi: liconst((hi+bound_b) as isize),
                    body: par!(k2.into_iter())
                }).into_iter()
            ).chain(
                epilogue
            ).chain(final_tree.into_iter()).collect::<Vec<_>>()
        }
        Err((llo, lhi))=>{
            let prologue = par!(variable, asap_schedule(prologue.collect(), &None).into_iter(), llo);
            let epilogue = par!(variable, asap_schedule(epilogue.collect(), &None).into_iter(), lhi);
            let body = prologue.chain(
                Some(
                    LAST::For{
                        variable,
                        lo: llo.clone(),
                        hi: lhi.clone(),
                        body: par!(k2.into_iter())
                    }
                ).into_iter()
            ).chain(epilogue).chain(final_tree.into_iter()).collect();
            let (olo, ohi)= original_lrange.unwrap();
            vec!(LAST::Guard{
                branches: vec![
                    (lcmp(lhi, llo, LOrdering::GE), body)
                ],
                otherwise: vec![LAST::For{
                    variable,
                    lo: olo,
                    hi: ohi,
                    body: par!(asap_schedule(original_version, &None).into_iter())
                }]
            })
        }
    },
    OptimizeInfo{
        info_rotated_version,
        info_ms1,
        info_ms2
    }
    ))
    */
}

#[derive(Debug)]
pub struct QSPOptimizer{
    unroll_time: Option<usize>,
    success: bool,
    info: Option<OptimizeInfo>
}

impl QSPOptimizer{
    pub fn new(unroll_time: Option<usize>)->Self{
        QSPOptimizer{
            unroll_time,
            success:false,
            info: None
        }
    }
}
impl OptimizationMethod for QSPOptimizer{
    fn optimize(&mut self, i:LIdent,r:Result<Range, (LExpr, LExpr)>,ast:Vec<ASTNode>) -> Vec<LAST>{
        trace!("{:#?}", ast);
        match optimize(i,r,ast,self.unroll_time.map(|x| x as _)){
            Ok((x,y))=>{
                self.success=true;
                self.info = Some(y);
                x
            }
            Err(x)=>{
                x
            }
        }
    }
    fn name(&self)->String{
        format!("QSPOptimizer unroll_time={:?}", self.unroll_time)
    }
    fn benchmark(&self)->String{
        format!("{:#?}", self)
    }
}