use std::marker::PhantomData;
pub struct State<F, S, T> where F: FnOnce(S)->(S, T){
    f: F,
    _s: PhantomData<dyn FnOnce(S)->(S, T)>,
}

impl<F, S, T> State<F, S, T> where F: FnOnce(S)->(S, T){
    pub fn new(f: F)->Self{
        State{
            f,
            _s: PhantomData
        }
    }
    pub fn run_state(self, s: S)->(S, T){
        (self.f)(s)
    }
    pub fn combine<F2, L, T2>(self, f: L)->State<impl FnOnce(S)->(S, T2), S, T2> 
    where
        F2: FnOnce(S)->(S, T2),
        L: FnOnce(T)->State<F2, S, T2>{
    
        State::new(
            |s|{
                let (s, t)=self.run_state(s);
                f(t).run_state(s)
            }
        )
    }
}

pub fn pure<S, T>(t: T)->State<impl FnOnce(S)->(S, T), S, T>{
    State::new(|s| (s, t))
}



#[cfg(test)]
mod test{
    use super::*;
    #[test]
    pub fn st_test1(){
        let s1=pure(1);
        let s1=s1.combine(|t| State::new(move |s| {let s=s+t; (s, t+1)}));
        let s1=s1.combine(|t| State::new(move |s| {let s=s+t; (s, t+1)}));
        let s1=s1.combine(|t| State::new(move |s| {let s=s+t; (s, t+1)}));
        let s1=s1.combine(|t| State::new(move |s| {let s=s+t; (s, t+1)}));
        println!("{:?}", s1.run_state(0usize));

    }
}
