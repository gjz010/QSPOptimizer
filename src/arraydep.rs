// Module for finding array dependency.
// We use GCD test for dependency check with unknown bounds, and Z3 (ILP) for dependency check with known bounds.
use crate::ast::*;
use crate::gcd::*;
use log::*;
// Use GCD test to check whether there is solution in integer set.
// Also we can compute the dif of the dependency.

pub fn may_conflict_in_loop(a: &Qubit, b: &Qubit, range: &Option<(Bound, Bound)>) -> bool {
    if a.0 == b.0 {
        if a.1 == b.1 {
            true
        } else {
            dependency_check_in_loop(&a.1, &b.1, range)
        }
    } else {
        false
    }
}

pub fn dependency_check_in_loop(a: &QOffset, b: &QOffset, range: &Option<(Bound, Bound)>) -> bool {
    if a.slope == b.slope {
        false
    } else {
        let dk = a.slope - b.slope; // !=0
        let db = b.intercept - a.intercept;
        if db % dk == 0 {
            let r = db / dk;
            if let Some((lo, hi)) = range {
                r >= *lo && r <= *hi
            } else {
                true
            }
        } else {
            false
        }
    }
}

// Find minimal di s.t. k1i+b1 = k2(i+di)+b2 where di>=1 with (i) and (i+di) in range
// Anyway this function can be seen O(1)
pub fn dependency_find_minimal_delta(
    a: &QOffset,
    b: &QOffset,
    range: &Option<(Bound, Bound)>,
) -> Option<i32> {
    
    pub fn go(
        a: &QOffset,
        b: &QOffset,
        range: &Option<(Bound, Bound)>,
    ) -> Option<i32> {
        let k1 = a.slope;
        let b1 = a.intercept;
        let k2 = b.slope;
        let b2 = b.intercept;
        let k = k2 - k1;
        let b = b1 - b2;
        let test_in_range = |x: Bound| match range {
            None => false,
            Some((lo, hi)) => x >= *lo && x <= *hi,
        };
        if k1 == 0 && k2 == 0 {
            if b1 == b2 {
                return Some(1);
            } else {
                return None;
            }
        } else if k1 == 0 {
            // b1 = k2(i+di) + b2
            if (b1 - b2) % k2 == 0 {
                let s = (b1 - b2) / k2;
                if let Some((lo, hi)) = *range {
                    if s < lo || s > hi || (s - 1) < lo || (s - 1) > hi {
                        return None;
                    } else {
                        return Some(1);
                    }
                } else {
                    return Some(1);
                }
            } else {
                return None;
            }
        } else if k2 == 0 {
            // k1i+b1=b2
            if (b2 - b1) % k1 == 0 {
                let s = (b2 - b1) / k1;
                if let Some((lo, hi)) = *range {
                    if s < lo || s > hi || (s + 1) < lo || (s + 1) > hi {
                        return None;
                    } else {
                        return Some(1);
                    }
                } else {
                    return Some(1);
                }
            } else {
                return None;
            }
        }
        
        // The problem: solve Diophatine equation k2 * di + k*i = b  where di>=1, find minimal di.
        let ((x, dx), (y, dy)) = diophantine(k2, k, b)?;
        trace!("({},{},{}) ({},{}) ({},{})", k2, k, b, x, dx, y, dy);
        if dx==0{
            return {if x>=1{
                Some(x)
            }else{
                None
            }}
        }
        // x=di+t*(-dx)
        let mut di = x.rem_euclid(-dx);
        let t = x.div_euclid(-dx);
        assert_eq!(x, di - t * dx);
        if di == 0 {
            di = di + (dx.abs());
        }

        match *range {
            None => {
                // find minimal di>=1 s.t. di=x+tdx where t is integer.
                Some(di)
            }
            Some((lo, hi)) => {
                // find minimal di>=1 s.t. di=x+tdx where t is integer and the following conditions are satisfied:
                // y+tdy>=lo
                // y+tdy<=hi
                assert_eq!((b - k2 * di) % k, 0);
                let base_y = (b - k2 * di) / k;
                assert_eq!(base_y * k + k2 * di, b);
                let (dx, dy) = {
                    if dx < 0 {
                        (-dx, -dy)
                    } else {
                        (dx, dy)
                    }
                };
                // note that by changing the basis, we change the constraint to t>=0.
                // that is k2 * (di+t*dx) + k*(base_y+t*dy) = b.
                // we need to find minimal t>=0, s.t. base_y+t*dy satisfies the two constraints.
                if dy > 0 {
                    let p1 = lo - base_y;
                    if p1 < 0 {
                        Some(di)
                    } else {
                        let t_min = (p1 + dy - 1) / dy;
                        Some(di + t_min * dx)
                    }
                } else {
                    let p1 = base_y - hi;
                    if p1 < 0 {
                        Some(di)
                    } else {
                        let dy = -dy;
                        let t_min = (p1 + dy - 1) / dy;
                        Some(di + t_min * dx)
                    }
                }
            }
        }
    }
    trace!("dependency_find_minimal_delta({:?},{:?},{:?});", a, b, range);
    let ret = go(a,b,range);
    trace!("Result = {:?}", (ret));
    ret
}