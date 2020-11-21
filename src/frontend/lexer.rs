use nom::*;
use nom::character::complete::*;
use nom::character::*;
use std::str::FromStr;
use nom::number::complete::double;
#[derive(Debug, Clone, PartialOrd, PartialEq)]
pub enum Token{
    Ident(String),
    Real(String),
    Imag(String),
    TPlus,
    TMinus,
    TMul,
    TEq,
    TAssign,
    TLParenthese,
    TRParenthese,
    TLBrace,
    TRBrace,
    TLBracket,
    TRBracket,
    TComma,
    TQubit,
    TDefgate,
    TDiagonal,
    TAntiDiagonal,
    TGeneral,
    TFor,
    TIn,
    TDots,
    TSemiColon,
    TComment,
    TCZ,
    TUnitary,
}
type Bytes<'a> = &'a str;
named!(t_comment<Bytes, Token>, 
    map!(delimited!(tag!("/*"), anychar, tag!("*/")),|x| {Token::TComment})
);
named!(t_ident_allowed_first_character<Bytes, char>, verify!(anychar, |c| c.is_alphabetic() || *c=='_'));
named!(t_ident_allowed_following_character<Bytes, char>, verify!(anychar, |c| c.is_alphanumeric() || *c=='_'));
named!(t_ident<Bytes, Token>, map!(recognize!(
    preceded!(t_ident_allowed_first_character,many0!(t_ident_allowed_first_character))
), |x| {Token::Ident(x.to_string())}));
named!(t_real<Bytes, Token>, map!(recognize!(double), |x| {Token::Real(x.to_string())}));
named!(t_imag<Bytes, Token>, map!(terminated!(recognize!(double), tag!("j")), |x| {Token::Imag(x.to_string())}));
named!(t_plus<Bytes, Token>, map!(tag!("+"), |x| {Token::TPlus}));
named!(t_minus<Bytes, Token>, map!(tag!("-"), |x| {Token::TMinus}));
named!(t_mul<Bytes, Token>, map!(tag!("*"), |x| {Token::TMul}));
named!(t_eq<Bytes, Token>, map!(tag!("=="), |x| {Token::TEq}));
named!(t_assign<Bytes, Token>, map!(tag!("="), |x| {Token::TAssign}));
named!(t_lbracket<Bytes, Token>, map!(tag!("["), |x| {Token::TLBracket}));
named!(t_rbracket<Bytes, Token>, map!(tag!("]"), |x| {Token::TRBracket}));
named!(t_lparen<Bytes, Token>, map!(tag!("("), |x| {Token::TLParenthese}));
named!(t_rparen<Bytes, Token>, map!(tag!(")"), |x| {Token::TRParenthese}));
named!(t_lbrace<Bytes, Token>, map!(tag!("{"), |x| {Token::TLBrace}));
named!(t_rbrace<Bytes, Token>, map!(tag!("}"), |x| {Token::TRBrace}));
named!(t_qubit<Bytes, Token>, map!(tag!("qubit"), |x| {Token::TQubit}));
named!(t_diagonal<Bytes, Token>, map!(tag!("diagonal"), |x| {Token::TDiagonal}));
named!(t_antidiagonal<Bytes, Token>, map!(tag!("antidiagonal"), |x| {Token::TAntiDiagonal}));
named!(t_general<Bytes, Token>, map!(tag!("general"), |x| {Token::TGeneral}));
named!(t_defgate<Bytes, Token>, map!(tag!("defgate"), |x| {Token::TDefgate}));
named!(t_for<Bytes, Token>, map!(tag!("for"), |x| {Token::TFor}));
named!(t_in<Bytes, Token>, map!(tag!("in"), |x| {Token::TIn}));
named!(t_cz<Bytes, Token>, map!(tag!("cz"), |x| {Token::TCZ}));
named!(t_unitary<Bytes, Token>, map!(tag!("unitary"), |x| {Token::TUnitary}));
named!(t_semicolon<Bytes, Token>, map!(tag!(";"), |x| {Token::TSemiColon}));
named!(t_comma<Bytes, Token>, map!(tag!(","), |x| {Token::TComma}));
named!(t_dots<Bytes, Token>, map!(tag!("to"), |x| {Token::TDots}));

named!(t_token<Bytes, Token>, alt!(
    t_plus | 
    t_minus |
    t_imag |  
    t_real | 
    t_mul | 
    t_eq | 
    t_assign | 
    t_lbracket | 
    t_rbracket | 
    t_lparen | 
    t_rparen | 
    t_lbrace | 
    t_rbrace | 
    t_qubit | 
    t_diagonal |
    t_antidiagonal |
    t_general |
    t_defgate | 
    t_for | 
    t_in | 
    t_cz |
    t_unitary |
    t_semicolon |
    t_comma |
    t_dots|
    t_ident));

named!(pub tokenize<Bytes, Vec<Token>>, map!(
    exact!(many0!(complete!(delimited!(multispace0, t_token, multispace0)))), 
    |x| {x.into_iter().filter(|x| if let Token::TComment = x {false} else {true}).collect::<Vec<_>>()}));

#[cfg(test)]
mod tests{
    #[test]
    fn lexer_t1() {
        use super::tokenize;
        let code=r#"
        qubit a[10];
        qubit b[20];
        for i in 1 to 999{
            H(a[i], b[i+1]);
        }"#;
        println!("{:?}", tokenize(code));
    }
}