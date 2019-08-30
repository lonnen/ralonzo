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

/// read a Scheme expression from a string.
fn parse<'a>(tokens: &'a [String]) -> Result<(LispExp, &'a [String]), LispError> {
    let (token, rest) = tokens.split_first()
        .ok_or(
            LispError::Reason("could not get token".to_string())
        )?;
    match &token[..] {
        "(" => read_from_tokens(rest),
        ")" => Err(LispError::Reason("unexpected ')'".to_string())),
        _ => Ok((parse_atom(token), rest)),
    }
}

/// read an expression from a sequence of tokens
fn read_from_tokens<'a>(tokens:  &'a [String]) -> Result<(LispExp, &'a [String]), LispError> {
    let mut expressions: Vec<LispExp> = vec![];
    let mut xs = tokens;
    
    loop {
        let (next_token, rest) = xs
            .split_first()
            .ok_or(LispError::Reason("could not find closing ')".to_string()))
            ?;
        match next_token.as_ref() {
            ")" => return Ok((LispExp::List(expressions), rest)),
            _ => {
                let (expression, new_xs) = parse(&xs)?;
                expressions.push(expression);
                xs = new_xs;
            }
        }
    }
}

fn parse_atom(token: &str) -> LispExp {
    LispExp::Number(7.0)
}


fn main() {
    // PROGRAM -> parse -> AST -> eval -> RESULT
    println!("Hello, world!");
}
