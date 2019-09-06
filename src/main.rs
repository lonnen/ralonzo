//! # ralonzo
//!
//! Yet another lisp interpreter
//! because "make a Lisp" is a sport
//! in any other language

use std::collections::HashMap;
use std::fmt;
use std::io;
use std::num::ParseFloatError;
use std::rc::Rc;

/// all supported kinds of values
#[derive(Clone)]
enum LispExp {
    Bool(bool),
    Func(fn(&[LispExp]) -> Result<LispExp, LispError>),
    Lambda(LispLambda),
    List(Vec<LispExp>),
    Number(f64),
    Symbol(String),
}

#[derive(Clone)]
struct LispLambda {
    params_exp: Rc<LispExp>,
    body_exp: Rc<LispExp>,
}

impl fmt::Display for LispExp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let str = match self {
            LispExp::Bool(b) => b.to_string(),
            LispExp::Func(_) => "Function {}".to_string(),
            LispExp::Lambda(_) => "Lambda {}".to_string(),
            LispExp::List(list) => {
                let xs: Vec<String> = list.iter().map(|x| x.to_string()).collect();
                format!("({})", xs.join(","))
            }
            LispExp::Number(n) => n.to_string(),
            LispExp::Symbol(s) => s.clone(),
        };

        write!(f, "{}", str)
    }
}

/// all supported kinds of errors
#[derive(Debug)]
enum LispError {
    SyntaxErr(u32, u32),     // line num, column num
    UnbalancedParens(usize), // # of missing close parens
    Reason(String),          // catch all for first impl
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
struct LispEnv<'a> {
    data: HashMap<String, LispExp>,
    outer: Option<&'a LispEnv<'a>>,
}

/// normalize program input into a token stream
fn tokenize(expr: String) -> Vec<String> {
    expr.replace("(", " ( ")
        .replace(")", " ) ")
        .split_whitespace() // quick 'n dirty
        .map(|x| x.to_string())
        .collect()
}

/// read a Scheme expression from a string.
fn parse<'a>(tokens: &'a [String]) -> Result<(LispExp, &'a [String]), LispError> {
    let (token, rest) = tokens
        .split_first()
        .ok_or(LispError::Reason("could not get token".to_string()))?;
    match &token[..] {
        "(" => read_from_tokens(rest),
        ")" => Err(LispError::Reason("unexpected ')'".to_string())), // should be unreachable?
        _ => Ok((parse_atom(token), rest)),
    }
}

/// read an expression from a sequence of tokens
fn read_from_tokens<'a>(tokens: &'a [String]) -> Result<(LispExp, &'a [String]), LispError> {
    let mut expressions: Vec<LispExp> = vec![];
    let mut xs = tokens;

    loop {
        let (next_token, rest) = xs
            .split_first()
            .ok_or(LispError::Reason("could not find closing ')".to_string()))?;
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
    match token.as_ref() {
        "true" => LispExp::Bool(true),
        "false" => LispExp::Bool(false),
        _ => {
            let potential_float: Result<f64, ParseFloatError> = token.parse();
            match potential_float {
                Ok(v) => LispExp::Number(v),
                Err(_) => LispExp::Symbol(token.to_string().clone()),
            }
        }
    }
}

macro_rules! ensure_tonicity {
    ($check_fn:expr) => {{
        |args: &[LispExp]| -> Result<LispExp, LispError> {
            let floats = parse_list_of_floats(args)?;
            let first = floats.first().ok_or(LispError::Reason(
                "expected at least one number".to_string(),
            ))?;
            let rest = &floats[1..];
            fn f(prev: &f64, xs: &[f64]) -> bool {
                match xs.first() {
                    Some(x) => $check_fn(prev, x) && f(x, &xs[1..]),
                    None => true,
                }
            };
            Ok(LispExp::Bool(f(first, rest)))
        }
    }};
}

fn default_env<'a>() -> LispEnv<'a> {
    let mut data: HashMap<String, LispExp> = HashMap::new();
    data.insert(
        "+".to_string(),
        LispExp::Func(|args: &[LispExp]| -> Result<LispExp, LispError> {
            let sum = parse_list_of_floats(args)?
                .iter()
                .fold(0.0, |sum, a| sum + a);
            Ok(LispExp::Number(sum))
        }),
    );
    data.insert(
        "-".to_string(),
        LispExp::Func(|args: &[LispExp]| -> Result<LispExp, LispError> {
            let floats = parse_list_of_floats(args)?;

            let first = *floats.first().ok_or(LispError::Reason(
                "expected at least one number".to_string(),
            ))?;
            let sum_of_rest = floats[1..].iter().fold(0.0, |sum, a| sum + a);

            Ok(LispExp::Number(first - sum_of_rest))
        }),
    );
    data.insert(
        "=".to_string(),
        LispExp::Func(ensure_tonicity!(|a, b| a == b)),
    );
    data.insert(
        ">".to_string(),
        LispExp::Func(ensure_tonicity!(|a, b| a > b)),
    );
    data.insert(
        ">=".to_string(),
        LispExp::Func(ensure_tonicity!(|a, b| a >= b)),
    );
    data.insert(
        "<".to_string(),
        LispExp::Func(ensure_tonicity!(|a, b| a < b)),
    );
    data.insert(
        "<=".to_string(),
        LispExp::Func(ensure_tonicity!(|a, b| a <= b)),
    );

    LispEnv { data, outer: None }
}

fn parse_list_of_floats(args: &[LispExp]) -> Result<Vec<f64>, LispError> {
    args.iter().map(|x| parse_single_float(x)).collect()
}

fn parse_single_float(exp: &LispExp) -> Result<f64, LispError> {
    match exp {
        LispExp::Number(num) => Ok(*num),
        _ => Err(LispError::Reason("expected a number".to_string())),
    }
}

fn env_get(k: &str, env: &LispEnv) -> Option<LispExp> {
    match env.data.get(k) {
        Some(exp) => Some(exp.clone()),
        None => match &env.outer {
            Some(outer_env) => env_get(k, &outer_env),
            None => None,
        },
    }
}

fn eval(exp: &LispExp, env: &mut LispEnv) -> Result<LispExp, LispError> {
    match exp {
        LispExp::Bool(_b) => Ok(exp.clone()),
        LispExp::Func(_) => Err(LispError::Reason("unexpected form".to_string())),
        LispExp::Lambda(_) => Err(LispError::Reason("unexpected form".to_string())),
        LispExp::List(list) => {
            let first_form = list
                .first()
                .ok_or(LispError::Reason("expected a non-empty list".to_string()))?;
            let arg_forms = &list[1..];
            match eval_built_in_form(first_form, arg_forms, env) {
                Some(res) => res,
                None => {
                    let first_eval = eval(first_form, env)?;
                    match first_eval {
                        LispExp::Func(f) => f(&eval_forms(arg_forms, env)?),
                        LispExp::Lambda(lambda) => {
                            let new_env = &mut env_for_lambda(lambda.params_exp, arg_forms, env)?;
                            eval(&lambda.body_exp, new_env)
                        }
                        _ => Err(LispError::Reason(
                            "first form must be a function".to_string(),
                        )),
                    }
                }
            }
        }
        LispExp::Number(_a) => Ok(exp.clone()),
        LispExp::Symbol(k) => env_get(k, env)
            .ok_or(LispError::Reason(format!("unexpected symbol k='{}'", k)))
            .map(|x| x.clone()),
    }
}

fn eval_forms(arg_forms: &[LispExp], env: &mut LispEnv) -> Result<Vec<LispExp>, LispError> {
    arg_forms.iter().map(|x| eval(x, env)).collect()
}

fn env_for_lambda<'a>(
    params: Rc<LispExp>,
    arg_forms: &[LispExp],
    outer_env: &'a mut LispEnv,
) -> Result<LispEnv<'a>, LispError> {
    let ks = parse_list_of_symbol_strings(params)?;
    if ks.len() != arg_forms.len() {
        return Err(LispError::Reason(format!(
            "expected {} arguments, got {}",
            ks.len(),
            arg_forms.len()
        )));
    }
    let vs = eval_forms(arg_forms, outer_env)?;
    let mut data: HashMap<String, LispExp> = HashMap::new();
    for (k, v) in ks.iter().zip(vs.iter()) {
        data.insert(k.clone(), v.clone());
    }
    Ok(LispEnv {
        data,
        outer: Some(outer_env),
    })
}

fn parse_list_of_symbol_strings(form: Rc<LispExp>) -> Result<Vec<String>, LispError> {
    let list = match form.as_ref() {
        LispExp::List(s) => Ok(s.clone()),
        _ => Err(LispError::Reason(
            "expected args form to be a list".to_string(),
        )),
    }?;
    list.iter()
        .map(|x| match x {
            LispExp::Symbol(s) => Ok(s.clone()),
            _ => Err(LispError::Reason(
                "expected symbols in the argument list".to_string(),
            )),
        })
        .collect()
}

fn parse_eval(expr: String, env: &mut LispEnv) -> Result<LispExp, LispError> {
    let (parsed_exp, _) = parse(&tokenize(expr))?;
    let evaled_exp = eval(&parsed_exp, env)?;

    Ok(evaled_exp)
}

fn slurp_expr() -> String {
    let mut expr = String::new();

    io::stdin()
        .read_line(&mut expr)
        .expect("Failed to read line");

    expr
}

fn eval_built_in_form(
    exp: &LispExp,
    arg_forms: &[LispExp],
    env: &mut LispEnv,
) -> Option<Result<LispExp, LispError>> {
    match exp {
        LispExp::Symbol(s) => match s.as_ref() {
            "if" => Some(eval_if_args(arg_forms, env)),
            "def" => Some(eval_def_args(arg_forms, env)),
            "fn" => Some(eval_lambda_args(arg_forms)),
            _ => None,
        },
        _ => None,
    }
}

fn eval_lambda_args(arg_forms: &[LispExp]) -> Result<LispExp, LispError> {
    let params_exp = arg_forms
        .first()
        .ok_or(LispError::Reason("unexpected args form".to_string()))?;
    let body_exp = arg_forms
        .get(1)
        .ok_or(LispError::Reason("expected second form".to_string()))?;
    if arg_forms.len() > 2 {
        return Err(LispError::Reason(
            "fn definition can only have two forms".to_string(),
        ));
    }

    Ok(LispExp::Lambda(LispLambda {
        body_exp: Rc::new(body_exp.clone()),
        params_exp: Rc::new(params_exp.clone()),
    }))
}

fn eval_if_args(arg_forms: &[LispExp], env: &mut LispEnv) -> Result<LispExp, LispError> {
    let test_form = arg_forms
        .first()
        .ok_or(LispError::Reason("expected test form".to_string()))?;
    let test_eval = eval(test_form, env)?;
    match test_eval {
        LispExp::Bool(b) => {
            let form_idx = if b { 1 } else { 2 };
            let res_form = arg_forms
                .get(form_idx)
                .ok_or(LispError::Reason(format!("expected form idx={}", form_idx)))?;
            let res_eval = eval(res_form, env);

            res_eval
        }
        _ => Err(LispError::Reason(format!(
            "unexpected test form = `{}`",
            test_form.to_string()
        ))),
    }
}

fn eval_def_args(arg_forms: &[LispExp], env: &mut LispEnv) -> Result<LispExp, LispError> {
    let first_form = arg_forms
        .first()
        .ok_or(LispError::Reason("expected first form".to_string()))?;
    let first_str = match first_form {
        LispExp::Symbol(s) => Ok(s.clone()),
        _ => Err(LispError::Reason(
            "expected first form to be a symbol".to_string(),
        )),
    }?;
    let second_form = arg_forms
        .get(1)
        .ok_or(LispError::Reason("expected second form".to_string()))?;
    if arg_forms.len() > 2 {
        return Err(LispError::Reason("def can only have two forms".to_string()));
    }
    let second_eval = eval(second_form, env)?;
    env.data.insert(first_str, second_eval);

    Ok(first_form.clone())
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
            },
        }
    }
}
