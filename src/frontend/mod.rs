use serde::{Deserialize, Serialize};
use serde_json::Result;
use std::sync::Arc;
pub mod lexer;
pub mod parser;
pub mod state;
// Instead of having some lexer-parser here, we define a literal-ast for qasm.
// An AST that does not involve pointers or references and can be parsed as plain json.

pub type LIdent = usize;



#[derive(Serialize, Deserialize, Debug, Clone, Copy)]
pub struct LComplex {
    pub real: f64,
    pub imag: f64,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct LU2(pub [LComplex; 4]);

#[derive(Serialize, Deserialize, Debug)]
pub enum LGateArrayHint {
    Diagonal,
    AntiDiagonal,
    General,
}
#[derive(Serialize, Deserialize, Debug)]
pub struct LGateArray {
    pub known_gates: Option<Vec<LU2>>,
    pub hint: LGateArrayHint,
}
#[derive(Serialize, Deserialize, Debug)]
pub struct LProgram {
    pub defined_qarrays: Vec<usize>, // for qubit arrays' size
    pub defined_gates: Vec<LGateArray>,
    pub operations: Vec<LAST>,
}

impl LProgram{
    pub fn print(&self)->String{
        let mut printer=Printer{indent:0};
        [printer.print_header(self), printer.print_astlist(&self.operations)].concat()
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum LOrdering {
    EQ,
    NE,
    LT,
    GT,
    LE,
    GE,
}
#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum LExpr {
    Var(LIdent),
    IConst(isize),
    Add(Box<LExpr>, Box<LExpr>),
    Sub(Box<LExpr>, Box<LExpr>),
    Mul(Box<LExpr>, Box<LExpr>),
    Div(Box<LExpr>, Box<LExpr>),
    Mod(Box<LExpr>, Box<LExpr>),
    Cmp(Box<LExpr>, Box<LExpr>, LOrdering),
    And(Box<LExpr>, Box<LExpr>),
    Or(Box<LExpr>, Box<LExpr>),
    Not(Box<LExpr>),
}

pub fn lvar(x: LIdent)->LExpr{LExpr::Var(x)}
pub fn liconst(x: isize)->LExpr{LExpr::IConst(x)}
pub fn lcmp(a: LExpr, b: LExpr, c: LOrdering)->LExpr{LExpr::Cmp(Box::new(a), Box::new(b), c)}
pub fn lnot(a: LExpr)->LExpr{LExpr::Not(Box::new(a))}
macro_rules! lbinary_op {
    ($a: ident, $b: ident) => {
        pub fn $a(a: LExpr, b: LExpr)->LExpr{LExpr::$b(Box::new(a), Box::new(b))}
    };
}
lbinary_op!(ladd, Add);
lbinary_op!(lsub, Sub);
lbinary_op!(lmul, Mul);
lbinary_op!(ldiv, Div);
lbinary_op!(lmod, Mod);
lbinary_op!(land, And);
lbinary_op!(lor, Or);

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct LQubit {
    pub array: LIdent,
    pub offset: LExpr,
}
#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum LGateRef {
    LKnown{
        mat: LU2
    },
    LUnknown{
        array: LIdent,
        offset: LExpr,
    }
   
}
#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum LAST {
    SQ {
        gate: Vec<LGateRef>,
        dst: LQubit,
    },
    CZ {
        q1: LQubit,
        q2: LQubit,
    },
    For {
        variable: LIdent,
        lo: LExpr,
        hi: LExpr,
        body: Vec<LAST>,
    },
    // This statement is for output only.
    Guard {
        branches: Vec<(LExpr, Vec<LAST>)>,
        otherwise: Vec<LAST>,
    },
    // This statement is for output only.
    Parallel {
        ops: Vec<LAST>,
    },
    Benchmark {
        original_node: Box<LAST>,
        benchmarks: Vec<(String, Vec<LAST>)>,
    },
}


pub struct Printer{
    indent: usize
}
impl Printer{
    pub fn new()->Self{
        Printer{indent:0}
    }
    pub fn print_expr(&self, expr: &LExpr)->String{
        match expr{
            LExpr::Var(ident)=>{
                format!("i_{}", ident)
            }
            LExpr::Add(box l, box r)=>{
                format!("({}+{})", self.print_expr(l), self.print_expr(r))
            }
            LExpr::Sub(box l, box r)=>{
                format!("({}-{})", self.print_expr(l), self.print_expr(r))
            }
            LExpr::Mul(box l, box r)=>{
                format!("({}*{})", self.print_expr(l), self.print_expr(r))
            }
            LExpr::Div(box l, box r)=>{
                format!("({} `euclid_div` {})", self.print_expr(l), self.print_expr(r))
            }
            LExpr::Cmp(box l, box r, ordering)=>{
                format!("({}{}{})", self.print_expr(l), match ordering {
                    LOrdering::EQ=>"==",
                    LOrdering::GE=>">=",
                    LOrdering::GT=>">",
                    LOrdering::LE=>"<=",
                    LOrdering::LT=>"<",
                    LOrdering::NE=>"!="
                }, self.print_expr(r))
            }
            LExpr::IConst(i)=>{
                format!("({})", i)
            }
            LExpr::Mod(box l, box r)=>{
                format!("({} `euclid_mod` {})", self.print_expr(l), self.print_expr(r))
            }
            LExpr::Not(box l)=>{
                format!("(!{})", self.print_expr(l))
            }
            LExpr::And(box l, box r)=>{
                format!("({}&&{})", self.print_expr(l), self.print_expr(r))
            }
            LExpr::Or(box l, box r)=>{
                format!("({}||{})", self.print_expr(l), self.print_expr(r))
            }
            
        }
    }
    pub fn print_complex(&self, complex: &LComplex)->String{
        format!("({})+({}j)", complex.real, complex.imag)
    }
    pub fn print_u2(&self, u2: &LU2)->String{
        format!("[{},{},{},{}]", self.print_complex(&u2.0[0]),self.print_complex(&u2.0[1]),self.print_complex(&u2.0[2]),self.print_complex(&u2.0[3]))
    }
    pub fn print_qubit(&self, qubit: &LQubit)->String{
        format!("q_{}[{}]", qubit.array, self.print_expr(&qubit.offset))
    }
    pub fn print_gateref(&self, gateref: &LGateRef)->String{
        match gateref{
            LGateRef::LKnown{mat: u2}=>format!("{}", self.print_u2(u2)),
            LGateRef::LUnknown{array, offset}=>format!("g_{}[{}]", array, self.print_expr(offset))
        }
    }
    pub fn print_lgatearray(&self, ga: &LGateArray)->String{
        match &ga.known_gates{
            None=>{
                match ga.hint{
                    LGateArrayHint::AntiDiagonal=>String::from("antidiagonal"),
                    LGateArrayHint::Diagonal=>String::from("diagonal"),
                    LGateArrayHint::General=>String::from("general")
                }
            }
            Some(v)=>{
                format!("[{}]", v.iter().map(|x| self.print_u2(x)).collect::<Vec<_>>().join(","))
            }
        }
    }
    pub fn print_header(&self, ast: &LProgram)->String{
        format!("{}\n{}\n", 
            ast.defined_qarrays.iter().enumerate().map(|(i,x)| {format!("qubit q_{}[{}];", i, x)}).collect::<Vec<_>>().join("\n"),
            ast.defined_gates.iter().enumerate().map(|(i,x)| {format!("defgate g_{}={};", i, self.print_lgatearray(x))}).collect::<Vec<_>>().join("\n"),
        )
    }
    pub fn print_astlist(&mut self, body: &[LAST])->String{
        body.iter().map(|x| self.print_ast(x)).collect::<Vec<_>>().join("\n")
    }
    pub fn print_ast(&mut self, ast: &LAST)->String{
        let indent=(0..self.indent).map(|_| " ").collect::<Vec<_>>().concat();
        match ast{
            LAST::SQ{gate, dst}=>{
                format!("{}unitary({}) {};", indent, gate.iter().map(|x| self.print_gateref(x)).collect::<Vec<_>>().join(", "), self.print_qubit(dst))
            }
            LAST::CZ{q1, q2}=>{
                format!("{}cz {}, {};", indent, self.print_qubit(q1), self.print_qubit(q2))
            }
            LAST::For{variable, lo, hi, body}=>{
                self.indent+=4;
                let x=format!("{}for i_{} in {} to {} {{\n{}\n{}}}", 
                indent, 
                variable, 
                self.print_expr(lo), 
                self.print_expr(hi), 
                self.print_astlist(body),
                indent    
                );
                self.indent-=4;
                x
            }
            LAST::Guard{branches, otherwise}=>{
                format!("{}guard {{\n{}{}    otherwise=>{{\n{}{}\n    }}{}}}\n", 
                indent, 
                branches.iter().map(|x| {self.indent+=8;let x=format!("{}    {}=>{{\n{}\n{}}}", indent, self.print_expr(&x.0), self.print_astlist(&x.1), indent); self.indent-=8; x}).collect::<Vec<_>>().concat(), 
                indent,
                {self.indent+=8;let x=self.print_astlist(otherwise); self.indent-=8; x},
                indent,
                indent)
            }
            LAST::Benchmark{original_node,benchmarks}=>{
                Some(format!("{}/*{}*/\n{}\n", indent, "Original Version", self.print_ast(original_node))).into_iter()
                .chain(benchmarks.iter().map(|x| format!("{}/*{}*/\n{}\n", indent, x.0, self.print_astlist(&x.1)))).collect::<Vec<_>>().concat()
            }
            LAST::Parallel{ops}=>{
                self.indent+=4;
                let x=format!("{}par {{\n{}\n{}}}", indent, self.print_astlist(ops), indent);
                self.indent-=4;
                x
            }
        }
    }
}
