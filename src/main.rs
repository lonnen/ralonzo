//! # ralonzo
//!
//! Yet another lisp interpreter
//! because "make a Lisp" is a sport
//! in any other language

use std::collections::HashMap;
use std::fmt;
use std::io;
use std::num::ParseFloatError;

/// all supported kinds of values
#[derive(Clone)]
enum LispExp {
    Symbol(String),
    Number(f64),
    List(Vec<LispExp>),
    Func(fn(&[LispExp]) -> Result<LispExp, LispError>), // lambda
}

impl fmt::Display for LispExp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let str  = match self {
            LispExp::Symbol(s) => s.clone(),
            LispExp::Number(n) => n.to_string(),
            LispExp::List(list) => {
                let xs: Vec<String> = list
                    .iter()
                    .map(|x| x.to_string())
                    .collect();
                format!("({})", xs.join(","))
            },
            LispExp::Func(_) => "Function {}".to_string(),
        };

        write!(f, "{}", str)
    }
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
    data: HashMap<String, LispExp>,
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
        ")" => Err(LispError::Reason("unexpected ')'".to_string())),  // should be unreachable?
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

/// yield an atom from a token
fn parse_atom(token: &str) -> LispExp {
    let potential_float:  Result<f64, ParseFloatError> =  token.parse();
    match potential_float {
        Ok(v) => LispExp::Number(v),
        Err(_) => LispExp::Symbol(token.to_string().clone())
    }
}


/// The Environment is where Things Happen
/// Users will augment this usin (`define` symbol val)
fn default_env() -> LispEnv {
    let mut data: HashMap<String, LispExp> = HashMap::new();
    data.insert(
        "+".to_string(),
        LispExp::Func(
            |args: &[LispExp]| -> Result<LispExp, LispError> {
                let sum = parse_list_of_floats(args)?
                    .iter()
                    .fold(0.0, |sum, a| sum + a);
                Ok(LispExp::Number(sum))
            }
        ),
    );
    data.insert(
        "-".to_string(),
         LispExp::Func(
            |args: &[LispExp]| -> Result<LispExp, LispError> {
                let floats = parse_list_of_floats(args)?;

                let first = *floats.first().ok_or(LispError::Reason("expected at least one number".to_string()))?;
                let sum_of_rest = floats[1..].iter().fold(0.0, |sum, a| sum + a);

                Ok(LispExp::Number(first - sum_of_rest))
            }
        ),
    );

    LispEnv {data}
}

fn parse_list_of_floats(args: &[LispExp]) ->  Result<Vec<f64>, LispError> {
    args
        .iter()
        .map(|x| parse_single_float(x))
        .collect()
}

fn parse_single_float(exp: &LispExp) -> Result<f64, LispError> {
    match exp {
        LispExp::Number(num) => Ok(*num),
        _ => Err(LispError::Reason("expected a number".to_string())),
    }
}

fn eval(exp: &LispExp, env: &mut LispEnv) -> Result<LispExp, LispError> {
    match exp {
        LispExp::Symbol(k) =>
            env.data.get(k)
                .ok_or(
                    LispError::Reason(
                        format!("unexpected symbol k='{}'", k)
                    )
                )
                .map(|x| x.clone())
        ,
        LispExp::Number(_a) => Ok(exp.clone()),
        LispExp::List(list) => {
            let first_form = list
                .first()
                .ok_or(LispError::Reason("expected a non-empty list".to_string()))?;
            let arg_forms = &list[1..];
            let first_eval = eval(first_form, env)?;
            match first_eval {
                LispExp::Func(f) => {
                    let args_eval = arg_forms
                        .iter()
                        .map(|x| eval(x, env))
                        .collect::<Result<Vec<LispExp>, LispError>>();
                    f(&args_eval?)
                },
                _ => Err(
                    LispError::Reason("first form must be a function".to_string())
                )
            }
        },
        LispExp::Func(_) => Err(
            LispError::Reason("unexpected form".to_string())
        )
    }
}

fn parse_eval(expr: String, env: &mut LispEnv) -> Result<LispExp, LispError> {
    let (parsed_exp, _) = parse(&tokenize(expr))?;
    let evaled_exp = eval(&parsed_exp, env)?;

    Ok(evaled_exp)
}

fn slurp_expr() -> String {
    let mut expr = String::new();

    io::stdin().read_line(&mut expr)
        .expect("Failed to read line");

    expr
}

fn main() {
    // PROGRAM -> parse -> AST -> eval -> RESULT
    let env = &mut default_env();
    loop {
        println!("lisp >");
        let expr = slurp_expr();
        match parse_eval(expr, env) {
            Ok(res) => println!("// 🥰 => {}", res),
            Err(e) => match e {
                LispError::Reason(msg) => println!("// 🤔 => {}", msg),
                LispError::SyntaxErr(l, c) => println!("syntax error at line {}, col {}", l, c),
                LispError::UnbalancedParens(n) => println!("missing {} parens", n),
            }
        }
    }
}
