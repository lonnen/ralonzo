//! # ralonzo
//!
//! Yet another lisp interpreter
//! because "make a Lisp" is a sport
//! in any other language

use std::collections::HashMap;

/// all supported kinds of values
#[derive(Clone)]
enum LispExp {
    Symbol(String),
    Number(f64),
    List(Vec<LispExp>),
}

/// all supported kinds of errors
#[derive(Debug)]
enum LispError {
    SyntaxErr(u32, u32), // line num, column num
    UnbalancedParens(usize), // # of missing close parens
    Reason(String), // catch all for first impl
}

impl ToString for LispError {
    fn to_string(&self) -> String {
        match self {
            LispError::SyntaxErr(l, c) => format!("syntax error at line {}, col {}", l, c),
            LispError::UnbalancedParens(n) => format!("missing {} parens", n),
            LispError::Reason(s) => String::from(s),
        }
    }
}

/// memory and execution environment
#[derive(Clone)]
struct LispEnv {
    data: HashMap<String, LispEnv>,
}

/// normalize program input into a token stream
fn tokenize(expr: String) -> Vec<String> {
    expr
        .replace("(", " ( ")
        .replace(")", " ) ")
        .split_whitespace() // quick 'n dirty
        .map(|x|  x.to_string())
        .collect()
}

// parse the token stream into expressions
fn parse<'a>(tokens: &'a [String]) -> Result<(LispExp, &'a [String]), LispError> {
    let (token, rest) = tokens.split_first()
        .ok_or(
            LispError::Reason("could not get token".to_string())
        )?;
    match &token[..] {
        "(" => read_seq(rest),
        ")" => Err(LispError::Reason("unexpected ')'".to_string())),
        _ => Ok((parse_atom(token), rest)),
    }
}

fn read_seq<'a>(tokens:  &'a [String]) -> Result<(LispExp, &'a [String]), LispError> {
    let es: Vec<LispExp> = vec![];
    let xs = tokens;
    
    loop {

    }
}

fn parse_atom(token: &str) -> LispExp {
    LispExp::Number(7.0)
}


fn main() {
    // PROGRAM -> parse -> AST -> eval -> RESULT
    println!("Hello, world!");
}
