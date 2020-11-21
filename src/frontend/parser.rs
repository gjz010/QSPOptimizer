use super::*;
use super::lexer::*;
use nom::*;
use nom_methods::method as mm;
use nom_methods::call_m;
use std::collections::BTreeMap;
use std::str::FromStr;
use std::cell::RefCell;
#[derive(Clone, Copy, Debug)]
enum Idx{
    Var(usize),
    QArr(usize),
    Gate(usize)
}

#[derive(Clone, Debug)]
struct Scope{
    variable_counter: usize,
    idents: BTreeMap<String, Idx>
}
use Idx::*;
impl Scope{
    pub fn new()->Self{
        Scope{
            variable_counter: 0,
            idents: BTreeMap::new()
        }
    }
    pub fn ident(&self, ident: &str)->Option<Idx>{
        self.idents.get(ident.clone()).copied()
    }
    pub fn decl_qarr(&mut self, ident: &str, qarr: usize){
        self.idents.insert(String::from(ident), QArr(qarr));
    }
    pub fn decl_gate(&mut self, ident: &str, gate: usize){
        self.idents.insert(String::from(ident), Gate(gate));
    }
    pub fn decl_var(&mut self, ident: &str)->usize{
        self.idents.insert(String::from(ident), Var(self.variable_counter));
        self.variable_counter+=1;
        self.variable_counter-1
    }
}

struct Parser{
    program: RefCell<LProgram>,
    ident_map: RefCell<Vec<Scope>>
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TokenStream<'a>(pub &'a [Token]);
impl<'a> InputIter for TokenStream<'a>{
    type Item=Token;
    type Iter = impl Iterator<Item=(usize, Self::Item)>;
    type IterElem = impl Iterator<Item = Self::Item>;
    #[inline]
    fn iter_indices(&self) -> Self::Iter {
      self.iter_elements().enumerate()
    }
    #[inline]
    fn iter_elements(&self) -> Self::IterElem {
      self.0.iter().cloned()
    }
    #[inline]
    fn position<P>(&self, predicate: P) -> Option<usize>
    where
      P: Fn(Self::Item) -> bool,
    {
      self.0.iter().position(|b| predicate(b.clone()))
    }
    #[inline]
    fn slice_index(&self, count: usize) -> Option<usize> {
      if self.0.len() >= count {
        Some(count)
      } else {
        None
      }
    }
}
impl<'a> InputTake for TokenStream<'a>{
    fn take(&self, count: usize) -> Self { TokenStream(&self.0[0..count]) }
    fn take_split(&self, count: usize) -> (Self, Self) {
        let (prefix, suffix) = self.0.split_at(count);
        (TokenStream(suffix), TokenStream(prefix))
    }
}

impl<'a> TokenStream<'a>{
    pub fn get(&'a self, index: usize)->Option<&'a Token>{
        self.0.get(index)
    }
}
use Token::*;

named!(next<TokenStream, Token>, map!(take!(1), |x| {let y=x.get(0).unwrap().clone(); y}));
macro_rules! token {
    ($i: tt,ident) => {
        map!($i, verify!(next, |x: &Token| {if let Ident(y)=x {true} else{ false }}), |x| {if let Ident(y)=x {y} else {unreachable!()}})
    };
    ($i: tt,float) => {
        map!($i, verify!(next, |x: &Token| {if let Real(y)=x {if let Ok(t)=f64::from_str(&y){true}else{false}} else{ false }}), |x| {
            if let Real(y)=x{
                f64::from_str(&y).unwrap()
            }else{
                unreachable!()
            }

        })
    };
    ($i: tt,int) => {
        map!($i, verify!(next, |x: &Token| {if let Real(y)=x {if let Ok(t)=isize::from_str(&y){true}else{false}} else{ false }}), |x| {
            if let Real(y)=x{
                isize::from_str(&y).unwrap()
            }else{
                unreachable!()
            }

        })
    };
    ($i: tt,uint) => {
        map!($i, verify!(next, |x: &Token| {if let Real(y)=x {if let Ok(t)=usize::from_str(&y){true}else{false}} else{ false }}), |x| {
            if let Real(y)=x{
                usize::from_str(&y).unwrap()
            }else{
                unreachable!()
            }

        })
    };
    ($i: tt,imag) => {
        map!($i, verify!(next, |x: &Token| {if let Imag(y)=x {if let Ok(t)=f64::from_str(&y){true}else{false}} else{ false }}), |x| {
            if let Imag(y)=x{
                f64::from_str(&y).unwrap()
            }else{
                unreachable!()
            }

        })
    };
    ($i: tt, $x: tt) => {
        value!($i, (), verify!(next, |x: &Token| {if let $x=x {true} else{ false }}))
    };
    ($i:tt, strict, $x: tt)=>{
        value!($i, (), return_error!(nom::error::ErrorKind::Verify,verify!(next, |x: &Token| {if let $x=x {true} else{ false }})))
    };
    ($i: tt, fetch, $x: tt) => {
        verify!($i, next, |x: &Token| {if let $x=x {true} else{ false }} )
    };

}

named!(parse_complex<TokenStream, LComplex>, alt!(do_parse!(
        s1:alt!(value!(Some(2), token!(TMinus)) | opt!(value!(1, token!(TPlus))))
        >>f1:token!(float)
        >>s2:alt!(value!(1, token!(TPlus)) | value!(2, token!(TMinus)))
        >>f2:token!(imag)
        >> ({
            LComplex{
                real: if s1==Some(2) {-f1} else {f1},
                imag: if s2==1 {f2} else {-f2}
            }
        }))
    |
    do_parse!(
        s2:alt!(value!(Some(2), token!(TMinus)) | opt!(value!(1, token!(TPlus))))
        >>f2:token!(imag)
        >>({
            LComplex{
                real: 0f64,
                imag: if s2==Some(2) {-f2} else {f2}
            }
        }))
    |
    do_parse!(
        s1:alt!(value!(Some(2), token!(TMinus)) | opt!(value!(1, token!(TPlus))))
        >>f1:token!(float)
        >>({
            LComplex{
                real: if s1==Some(2) {-f1} else {f1},
                imag: 0f64
            }
        }))
    
));
named!(u2<TokenStream, LU2>, 
        delimited!(
            token!(TLBracket),
            do_parse!(
                a: parse_complex
                >>token!(strict, TComma)
                >>b: parse_complex
                >>token!(TComma)
                >>c: parse_complex
                >>token!(TComma)
                >>d: parse_complex
                >>({LU2([a,b,c,d])})
            ),
            token!(TRBracket)
        )
    );

use Token::*;
impl<'a> Parser{
    pub fn new()->Self{
        Parser{
            program: RefCell::new(LProgram{defined_gates: vec![], defined_qarrays: vec![], operations: vec![]}),
            ident_map: RefCell::new(vec![Scope::new()])
        }
    }
    pub fn into_program(self)->LProgram{
        self.program.into_inner()
    }


    fn scope(&self){
        //println!("Scope");
        let last=self.ident_map.borrow().last().unwrap().clone();
        self.ident_map.borrow_mut().push(last)
    }
    fn unscope(&self){
        //println!("Unscope");
        self.ident_map.borrow_mut().pop();
    }

    fn get_variable(&self, ident: &str)->Option<usize>{
        if let Some(Var(i))=self.ident_map.borrow().last().unwrap().ident(ident){
            Some(i)
        }else{
            None
        }
    }
    fn get_qarray(&self, ident: &str)->Option<usize>{
        if let Some(QArr(i))=self.ident_map.borrow().last().unwrap().ident(ident){
            Some(i)
        }else{
            None
        }
    }
    fn get_gatearr(&self, ident: &str)->Option<usize>{
        if let Some(Gate(i))=self.ident_map.borrow().last().unwrap().ident(ident){
            Some(i)
        }else{
            None
        }
    }
    fn declare_qarray(&self, ident: &str, size: usize){
        let qarr=self.program.borrow().defined_qarrays.len();
        self.program.borrow_mut().defined_qarrays.push(size);
        self.ident_map.borrow_mut().last_mut().unwrap().decl_qarr(ident, qarr)
    }
    fn declare_known_gatearray(&self, ident: &str, ga: Vec<LU2>){
        let qgarr=self.program.borrow().defined_gates.len();
        self.program.borrow_mut().defined_gates.push(LGateArray{known_gates:Some(ga), hint: LGateArrayHint::General});
        self.ident_map.borrow_mut().last_mut().unwrap().decl_gate(ident, qgarr)
    }
    fn declare_unknown_gatearray(&self, ident: &str, hint: LGateArrayHint){
        let qgarr=self.program.borrow().defined_gates.len();
        self.program.borrow_mut().defined_gates.push(LGateArray{known_gates:None, hint});
        self.ident_map.borrow_mut().last_mut().unwrap().decl_gate(ident, qgarr)
    }
    mm!(pub decl_qubit<TokenStream<'a>, Vec<LAST>>, &self, do_parse!(
        token!(TQubit) >> 
        name: token!(ident) >> 
        size: delimited!(
            token!(TLBracket),
            token!(uint),
            token!(TRBracket)
            )
        >> token!(TSemiColon)
        >>({self.declare_qarray(&name, size); vec![]})
        )
    );
    

    

    mm!(pub decl_gatearray<TokenStream<'a>, Vec<LAST>>, &self, do_parse!(
        token!(TDefgate)>>
        name: token!(ident)>>
        token!(strict, TAssign)>>
        ga: alt!(
            do_parse!(token!(TDiagonal) >> ({self.declare_unknown_gatearray(&name, LGateArrayHint::Diagonal)}))
            | do_parse!(token!(TAntiDiagonal) >> ({self.declare_unknown_gatearray(&name, LGateArrayHint::AntiDiagonal)}))
            | do_parse!(token!(TGeneral) >> ({self.declare_unknown_gatearray(&name, LGateArrayHint::General)}))
            | delimited!(
                token!(strict, TLBracket),do_parse!(v: separated_list!(token!(TComma), u2) >> ({self.declare_known_gatearray(&name, v)})),
                token!(strict, TRBracket)
                )
        )
        >> token!(strict, TSemiColon)
        >>(vec![])
    ));
    mm!(integer_expr_term<TokenStream<'a>, LExpr>, &self, alt!(
        do_parse!(i: token!(int) >> (liconst(i)))
        | delimited!(token!(TLParenthese), call_m!(self.integer_expr), token!(TRParenthese))
        | do_parse!(
            var_name:return_error!(nom::error::ErrorKind::Verify,verify!(token!(ident), |x| self.get_variable(&x).is_some()))
            >> (LExpr::Var(self.get_variable(&var_name).unwrap()))
        )
        
    ));
    mm!(integer_expr_F<TokenStream<'a>, LExpr>, &self, do_parse!(
        first_term: call_m!(self.integer_expr_term)>>
        following_terms: many0!(pair!(token!(fetch, TMul), call_m!(self.integer_expr_term)))>>
        ({following_terms.into_iter().fold(first_term, |a, (i, x)| {
            match i{
                TMul=>{
                    lmul(a, x)
                }
                _=>{unreachable!()}
            }
        })})
    ));
    mm!(pub integer_expr<TokenStream<'a>, LExpr>, &self, do_parse!(
        first_sign: opt!(alt!(token!(fetch, TPlus) | token!(fetch, TMinus)))>>
        first_term: call_m!(self.integer_expr_F)>>
        following_terms: many0!(pair!(alt!(token!(fetch, TPlus) | token!(fetch, TMinus)), call_m!(self.integer_expr_F)))>>
        ({following_terms.into_iter().fold({
            match first_sign{
                Some(TMinus)=>{
                    if let LExpr::IConst(x) = first_term{
                        LExpr::IConst(-x)
                    }else{
                        lsub(liconst(0), first_term)
                    }
                    
                }
                None=>{
                    first_term
                }
                Some(TPlus)=>{
                    first_term
                }
                _=>{unreachable!()}
            }
            
        }, |a, (i, x)| {
            match i{
                TPlus=>{
                    ladd(a, x)
                }
                TMinus=>{
                    lsub(a, x)
                }
                _=>{unreachable!()}
            }
        })})
    ));
    mm!(pub parse_qubit<TokenStream<'a>, LQubit>, &self, do_parse!(
            array: return_error!(nom::error::ErrorKind::Verify,verify!(token!(ident), |x| self.get_qarray(&x).is_some()))
            >> offset: delimited!(token!(TLBracket), call_m!(self.integer_expr), token!(TRBracket))
            >> (LQubit{array: self.get_qarray(&array).unwrap(), offset})
    ));
    mm!(pub parse_gateref<TokenStream<'a>, LGateRef>, &self, alt!(
        map!(u2, |x| LGateRef::LKnown{mat: x})
        |do_parse!(array: 
            map!(return_error!(nom::error::ErrorKind::Verify,verify!(token!(ident), |x| self.get_gatearr(&x).is_some())), |x| {self.get_gatearr(&x).unwrap()})
        >> offset: delimited!(
            token!(TLBracket), 
            call_m!(self.integer_expr),
            token!(TRBracket)
            )
        >>(LGateRef::LUnknown{array, offset})
        )
    ));

    mm!(pub sq_op<TokenStream<'a>, Vec<LAST>>, &self, do_parse!(
        token!(TUnitary)
        >> gate: delimited!(token!(TLParenthese), call_m!(self.parse_gateref), token!(TRParenthese))
        >> qubit: call_m!(self.parse_qubit)
        >> token!(TSemiColon)
        >> (vec![LAST::SQ{gate: vec![gate],  dst: qubit}])
    ));
    mm!(pub cz_op<TokenStream<'a>, Vec<LAST>>, &self, do_parse!(
        token!(TCZ)
        >> q1: call_m!(self.parse_qubit)
        >> opt!(token!(TComma))
        >> q2: call_m!(self.parse_qubit)
        >> token!(TSemiColon)
        >>(vec![LAST::CZ{q1, q2}])
    ));
    mm!(pub for_op<TokenStream<'a>, Vec<LAST>>, &self, do_parse!(
        token!(TFor)
        >> varname: token!(ident)
        >> token!(TIn)
        >> lo: return_error!(nom::error::ErrorKind::Verify,call_m!(self.integer_expr))
        >> token!(TDots)
        >> hi: return_error!(nom::error::ErrorKind::Verify,call_m!(self.integer_expr))
        >> id:do_parse!(return_error!(nom::error::ErrorKind::Verify,token!(TLBrace)) >> ({self.scope(); self.ident_map.borrow_mut().last_mut().unwrap().decl_var(&varname)}))
        >> ops: return_error!(nom::error::ErrorKind::Verify,call_m!(self.ops))
        >> do_parse!(return_error!(nom::error::ErrorKind::Verify,token!(TRBrace)) >> ({self.unscope();}))
        >> (vec![LAST::For{
            variable: id,
            lo,
            hi,
            body:ops
        }])
    ));
    mm!(pub ops<TokenStream<'a>, Vec<LAST>>, &self, map!(many0!(
        complete!(alt!(
            call_m!(self.decl_qubit) | call_m!(self.decl_gatearray) | call_m!(self.for_op) | call_m!(self.sq_op) | call_m!(self.cz_op) 
        ))
    ), |x| {x.into_iter().flat_map(|y| y.into_iter()).collect()}));

    mm!(pub parse_ast<TokenStream<'a>, ()>, &self, do_parse!(
        o: call_m!(self.ops) >> ({/*println!("{:?}", o);*/self.program.borrow_mut().operations=o;})
    ));
}   


pub fn parse<'a>(token: Vec<Token>)->LProgram{
    let mut parser=Parser::new();
    let ret=parser.parse_ast(TokenStream(&token));
    //println!("{:?}", ret);
    let ret=ret.unwrap();
    parser.into_program()
}


#[cfg(test)]
mod tests{
    #[test]
    pub fn parser_test_1(){
        let code=(r#"
        qubit a[10];
        qubit b[20];
        defgate H=[[0.707+0.707j,0.707+0.707j,0.707+0.707j,0.707-0.707j]];
        for i in 1 to 999{
            unitary(H[0]) a[i];
            cz a[i] b[i+3];
            unitary([0.707+0.707j,0.707+0.707j,0.707+0.707j,0.707-0.707j]) b[i+3];
        }"#);
        let lexer_result=super::super::lexer::tokenize(code);
        println!("{:?}", lexer_result);
        let l=lexer_result.unwrap().1;
        let parse_result=super::parse(l);
        println!("{:?}", parse_result);
        println!("{}", parse_result.print());
    }
}