extern crate sploop;
use sploop::frontend::{LExpr::*, LOrdering::*, LAST::*, *};
use std::io::{self, Read};
use std::process::*;
use log::*;
fn print_example() {
    
    let example_program = LProgram{
        defined_gates: vec![
            LGateArray{
                known_gates: Some(vec![
                    LU2([LComplex{real: 0.707, imag: 0.0}, LComplex{real: 0.707, imag: 0.0}, LComplex{real: 0.707, imag: 0.0}, LComplex{real: -0.707, imag: 0.0}])
                ]),
                hint: LGateArrayHint::General
            }
        ],
        defined_qarrays: vec![100],
        operations: vec![
            For{
                variable: 0,
                lo: IConst(0),
                hi: IConst(9),
                body: vec![
                    SQ{
                        dst: LQubit{
                            array:0,
                            offset: Add(
                                Box::new(
                                    Mul(
                                        Box::new(IConst(10)),
                                        Box::new(Var(0))
                                    )
                                ),
                                Box::new(
                                    IConst(0)
                                )
                            )
                        },
                        gate: vec![LGateRef::LUnknown{array:0,offset:liconst(0)}]
                    },
                    Guard{
                        branches: vec![
                            (Cmp(Box::new(Var(0)), Box::new(IConst(0)), EQ), vec![])
                        ],
                        otherwise: vec![
                            CZ{
                                q1: LQubit{
                                    array:0,
                                    offset: Add(
                                        Box::new(
                                            Mul(
                                                Box::new(IConst(10)),
                                                Box::new(Var(0))
                                            )
                                        ),
                                        Box::new(
                                            IConst(0)
                                        )
                                    )
                                },
                                q2: LQubit{
                                    array:0,
                                    offset: Add(
                                        Box::new(
                                            Mul(
                                                Box::new(IConst(10)),
                                                Box::new(Var(0))
                                            )
                                        ),
                                        Box::new(
                                            IConst(-10)
                                        )
                                    )
                                }
                            }
                        ]
                    }
                ]
            }
        ]
    };

    let serialized=serde_json::to_string(&example_program).unwrap();

    println!("{}", serialized);
}
fn main() -> std::io::Result<()> {
    //let mut buffer = String::new();
    //io::stdin().read_to_string(&mut buffer)?;
    env_logger::init();
    debug!("Software pipelining for quantum loop programs: demo");
    if std::env::args().any(|x| x == "--example") {
        print_example();
        return Ok(());
    }
    
    
    use std::io::{self, Write};
    let buffer=if std::env::args().any(|x| x=="--preprocess"){
        let cpp=Command::new("cpp").arg("-P").stdin(Stdio::inherit()).output().expect("Spawning C Preprocessor Failed!");
        if let Some(error_code)=cpp.status.code(){
            if error_code == 0 {

            }else{
                error!("Error while preprocessing:{}\n{}", error_code, String::from_utf8(cpp.stderr).unwrap());
                return Err(std::io::Error::new(std::io::ErrorKind::Other, "C Preprocessor error"));
            }
        }else{
            error!("Error while preprocessing: Interrupted by signal\n{}", String::from_utf8(cpp.stderr).unwrap());
            return Err(std::io::Error::new(std::io::ErrorKind::Other, "C Preprocessor error"));
        }
        String::from_utf8(cpp.stdout).unwrap()
    }else{
        let mut buffer = String::new();
        io::stdin().read_to_string(&mut buffer)?;
        buffer
    };
    let mut l_ast = if std::env::args().any(|x| x == "--json") {
        serde_json::from_str(&buffer)?
    }else {
        let tokens=sploop::frontend::lexer::tokenize(&buffer);
        //println!("{:?}", tokens);
        let tokens=tokens.unwrap().1;
        sploop::frontend::parser::parse(tokens)
    };
    if std::env::args().any(|x| x == "--optimize-only"){
        let mut opt3=sploop::passes::QSPOptimizer::new(Some(3));
        let methods: Vec<&mut (dyn sploop::helper::OptimizationMethod)>=vec![&mut opt3];
        sploop::helper::optimize_loops_with_benchmark(&mut l_ast, methods);
        println!("{}", l_ast.print());
    }else if std::env::args().any(|x| x == "--compile"){
        let mut opt3=sploop::passes::QSPOptimizer::new(Some(3));
        let mut opt5=sploop::passes::QSPOptimizer::new(Some(5));
        let methods: Vec<&mut (dyn sploop::helper::OptimizationMethod)>=vec![&mut opt3, &mut opt5];
        sploop::helper::optimize_loops_with_benchmark(&mut l_ast, methods);
        println!("{}", l_ast.print());
    }else if std::env::args().any(|x| x == "--ast"){
        println!("{}", serde_json::to_string_pretty(&l_ast)?);
    }else if std::env::args().any(|x| x == "--print"){
        println!("{}", l_ast.print());
    }else{
        println!("{:?}", l_ast);
    }
    
    Ok(())
}
