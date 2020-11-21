use num_traits::Num;
pub fn exgcd<T>(a: T, b: T) -> (T, T, T)
where
    T: Num + Ord + Copy + std::fmt::Debug,
{
    let z = T::zero();
    let sz = T::one();
    assert_ne!(a, z);
    assert_ne!(b, z);
    let (a, flag1) = if a > z { (a, false) } else { (z - a, true) };
    let (b, flag2) = if b > z { (b, false) } else { (z - b, true) };
    let (x, y, g) = uexgcd(a, b);
    (
        if flag1 { z - x } else { x },
        if flag2 { z - y } else { y },
        g,
    )
}

// return solution family ((x, dx), (y, dy))
pub fn diophantine<T>(a: T, b: T, c: T) -> Option<((T, T), (T, T))>
where
    T: Num + Ord + Copy + std::fmt::Debug,
{
    let z = T::zero();
    match (a == z, b == z) {
        (true, true) => {
            if c == z {
                Some(((z, z), (z, z)))
            } else {
                None
            }
        } // ill-formed case
        (false, true) => {
            if c % a == z {
                Some(((c / a, z), (z, z)))
            } else {
                None
            }
        }
        (true, false) => {
            if c % b == z {
                Some(((z, z), (c / b, z)))
            } else {
                None
            }
        }
        (false, false) => {
            let (x, y, gcd) = exgcd(a, b);
            if c % gcd == z {
                let t = c / gcd;
                // special solution ax+by=c
                let x = t * x;
                let y = t * y;
                // general solution ax+by=0
                let dx = b / gcd;
                let dy = (z - a) / gcd;
                Some(((x, dx), (y, dy)))
            } else {
                None
            }
        }
    }
}

pub fn uexgcd<T>(a: T, b: T) -> (T, T, T)
where
    T: Num + Ord + Copy + std::fmt::Debug,
{
    let z = T::zero();
    let sz = T::one();
    assert_eq!(a > z, true);
    assert_eq!(b > z, true);
    let mut x = z;
    let mut y = z;
    fn go<T>(a: T, b: T, x: &mut T, y: &mut T) -> T
    where
        T: Num + Ord + Copy + std::fmt::Debug,
    {
        let z = T::zero();
        let sz = T::one();
        if b == z {
            *x = sz;
            *y = z;
            return a;
        } else {
            let d = go(b, a % b, x, y);
            let t = *x;
            *x = *y;
            *y = t - (a / b) * (*y);
            return d;
        }
    }
    let gcd = go(a, b, &mut x, &mut y);
    (x, y, gcd)
}

pub fn gcd<T>(a: T, b: T) -> T
where
    T: Num + Ord + Copy + std::fmt::Debug,
{
    let z = T::zero();
    let sz = T::one();
    assert_eq!(a > z, true);
    assert_eq!(b > z, true);
    let (g, _, _) = exgcd(a, b);
    g
}

