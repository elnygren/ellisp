use regex::Regex;
use std::collections::HashMap;
use std::io;
use std::io::prelude::*;

#[macro_use]
extern crate itertools;

/// Token is just a string
/// ellisp program is a string that is first tokenised `String => Vec<Token>`
/// and then atomized by our parser `Token => Atom`
type Token = String;

/// Atom
/// parser produces an AST of Atoms
#[derive(Debug, Clone)]
enum Atom {
  Bool(bool),
  Symbol(String),
  Number(i32),
  // TODO: support doubles, strings... ?
}

/// AST Node is either a single Atom, eg. `Atom::Number(1)` or `Atom::Symbol("sum")`
/// or a list of Atoms: (following is just some pseudo code) `AST["sum", 1, 2, AST["sum", 1, 2]]`
#[derive(Debug, Clone)]
struct AST {
  atom: Option<Atom>,
  children: Option<Box<Vec<AST>>>,
}

/// Dynamic Environment contains all user defined symbols
/// Per-procedure (lambda) environmnts link to their parent
#[derive(Debug, Clone)]
struct DynamicEnv {
  parent: Option<Box<DynamicEnv>>,
  env: HashMap<String, Expr>,
}

/// Method implementations for DynamicEnv
/// Recursive .find to look up a symbol from local & parent environments
/// This is how closures are implemented :-)
impl DynamicEnv {
  fn find(&self, key: &str) -> Expr {
    let v = self.env.get(key);
    match v {
      Some(v) => *v,
      None => {
        return self
          .parent
          .as_ref()
          .expect("DEnv parent not defined")
          .find(key);
      }
    }
  }
}

#[derive(Debug)]
struct LambdaContext {
  body: AST,
  param_names: Vec<String>,
  param_values: Vec<Expr>,
}

impl LambdaContext {
  fn new(body: &AST, param_names: Vec<String>) -> LambdaContext {
    return LambdaContext {
      body: body.clone(),
      param_names: param_names,
      param_values: Vec::new(),
    };
  }
}

type LambdaContextStore = Vec<LambdaContext>;

/// During evaluation, our AST is turned into Expressions
#[derive(Debug, Copy, Clone)]
enum Expr {
  Number(i32),
  Function(fn(args: Vec<Expr>) -> Expr),
  Bool(bool),
  LambdaId(usize), // "pointer" to our ProcedureStore
  Nop,
  // TODO: support doubles, strings... ?
}

/// we only support s-expressions and comments
/// so we can just expand spaces everywhere and split by it
fn tokenize(ellisp_program: &str) -> Vec<Token> {
  let space = " ";
  // TODO: this comment regexp is going to give us trouble with string literals
  let comment_re = Regex::new(r";.*\n").unwrap();
  return comment_re
    .replace_all(ellisp_program, "")
    .replace("\n", space)
    .replace("\t", space)
    .replace("(", " ( ")
    .replace(")", " ) ")
    .split(space)
    .map(|s| s.to_string())
    .filter(|s| !s.is_empty())
    .collect();
}

/// parser builds an Tree tree of Atoms
/// currently our language has Symbols and Numbers defined in Atom
fn atomize(token: Token) -> Atom {
  if token == "false" {
    return Atom::Bool(false);
  }
  if token == "true" {
    return Atom::Bool(true);
  }

  let num = token.parse::<i32>();
  return match num {
    Ok(n) => Atom::Number(n),
    Err(_) => Atom::Symbol(token),
  };
}

/// build AST Tree recursively
fn parser(tokens: &mut Vec<Token>) -> AST {
  let token = tokens.remove(0);
  let results: AST = match token.as_str() {
    "(" => {
      let mut l = Vec::new();
      while tokens[0] != ")" {
        l.push(parser(tokens));
      }
      tokens.remove(0);
      return AST {
        atom: None,
        children: Some(Box::new(l)),
      };
    }
    ")" => panic!("unexpected `)`"),
    _ => AST {
      atom: Some(atomize(token)),
      children: None,
    },
  };

  return results;
}

// Here be some builtins

fn ellisp_sum(args: Vec<Expr>) -> i32 {
  let mut sum = 0;
  for v in args {
    match v {
      Expr::Number(n) => sum += n,
      _ => panic!("sum is only defined for numbers"),
    }
  }
  return sum;
}

fn ellisp_minus(args: Vec<Expr>) -> i32 {
  let mut acc = match args[0] {
    Expr::Number(n) => n,
    _ => panic!("minus is only defined for numbers"),
  };
  for v in args.iter().skip(1) {
    match v {
      Expr::Number(n) => acc -= n,
      _ => panic!("minus is only defined for numbers"),
    }
  }
  return acc;
}

fn ellisp_begin(args: Vec<Expr>) -> Expr {
  let expr: Expr = *args.last().expect("do block received invalid data");
  return expr;
}

fn wrapped_ellisp_sum(args: Vec<Expr>) -> Expr {
  return Expr::Number(ellisp_sum(args));
}

fn wrapped_ellisp_minus(args: Vec<Expr>) -> Expr {
  return Expr::Number(ellisp_minus(args));
}

fn ellisp_smaller_than(args: Vec<Expr>) -> Expr {
  let args: Vec<&i32> = args
    .iter()
    .map(|expr| match expr {
      Expr::Number(n) => n,
      _ => panic!("< only supports numbers"),
    })
    .collect();

  for (i, v) in args.iter().enumerate().skip(1) {
    if args[i - 1] > v {
      return Expr::Bool(false);
    }
  }
  return Expr::Bool(true);
}

fn ellisp_equal(args: Vec<Expr>) -> Expr {
  let result = args.iter().all(|x| match (x, args[0]) {
    (Expr::Number(curr), Expr::Number(first)) => first == *curr,
    (Expr::Bool(curr), Expr::Bool(first)) => first == *curr,
    _ => false,
  });

  return Expr::Bool(result);
}

/// helper for eval
/// perform a lambda call using a local environment
fn lambda_call(
  args: &Vec<Expr>,
  denv: &mut Box<DynamicEnv>,
  pstore: &mut LambdaContextStore,
  lambda_id: usize,
) -> Expr {
  // println!("lambda call {:? }{:?}", args, pstore);
  let ctx = &mut pstore[lambda_id];

  let mut local_env = HashMap::new();
  for (arg_name, arg_value) in izip!(&ctx.param_names, args) {
    local_env.insert(arg_name.to_string(), *arg_value);
  }

  // TODO: wish we didn't need to `denv.clone()` :/
  let denv = &mut Box::new(DynamicEnv {
    env: local_env,
    parent: Some(denv.clone()),
  });
  // TODO: wish we didn't need to `ctx.body.clone()` :/
  return eval(&ctx.body.clone(), denv, pstore);
}

/// helper for eval
/// evaluates a proc and all of it's arguments and then calls it
fn proc_call(
  ast: &AST,
  denv: &mut Box<DynamicEnv>,
  pstore: &mut LambdaContextStore,
  first: &AST,
) -> Expr {
  let proc: Expr = eval(&first, denv, pstore);
  let mut args: Vec<Expr> = Vec::new();
  for arg in ast
    .children
    .as_ref()
    .expect("error at proc_call")
    .iter()
    .skip(1)
  {
    let evalued_arg = eval(&arg, denv, pstore);
    args.push(evalued_arg);
  }

  return match proc {
    Expr::Function(f) => f(args),
    Expr::LambdaId(lambda_id) => lambda_call(&args, denv, pstore, lambda_id),
    _ => panic!("Expected Expr::Function"),
  };
}

/// Eval processes the AST into experssions and evaluates them
/// We traverse the AST recursively evaluating every leaf so we can reduce everything
/// to (proc arg1 arg2 ... argN) form and just call the function corresponding to proc
/// eg. (sum 1 2 3 ... N) -> sum([1,2,3...N]) (in rust)
fn eval(ast: &AST, denv: &mut Box<DynamicEnv>, pstore: &mut LambdaContextStore) -> Expr {
  // println!("eval called: \n	{:?}\n	{:?}", ast, denv);
  match &ast.atom {
    // we are processing an AST Atom; a leaf of the AST
    // if the Atom is a Number/Bool, then it's a Number/Bool expression
    // if it's a symbol:
    // 	a) try to find the atom from ellisp static environment
    // 	b) TODO: try to find the atom from ellisp dynamic environment
    Some(atom) => match atom {
      Atom::Symbol(x) => match x.as_str() {
        // static env
        "sum" => Expr::Function(wrapped_ellisp_sum),
        "+" => Expr::Function(wrapped_ellisp_sum),
        "-" => Expr::Function(wrapped_ellisp_minus),
        "begin" => Expr::Function(ellisp_begin),
        "do" => Expr::Function(ellisp_begin),
        "=" => Expr::Function(ellisp_equal),
        "<" => Expr::Function(ellisp_smaller_than),
        // dynamic env
        _ => {
          return denv.find(x.as_str());
        }
      },
      Atom::Number(x) => Expr::Number(*x),
      Atom::Bool(x) => Expr::Bool(*x),
    },

    // We are processing a full expression, eg. (sum 1 2) of the form
    // (proc arg1 arg2 arg3 ... )
    // every argN can be a full expression or a single atom; both are checked recursively
    None => {
      let children = ast.children.as_ref().expect("error: children is None");
      let first = &children[0];

      // println!("else block, first {:?} rest {:?}", first, ast.children.as_ref().unwrap());

      return match first.atom.as_ref() {
        None => proc_call(&ast, denv, pstore, &first),
        Some(atom) => match atom {
          Atom::Symbol(sym) => match sym.as_str() {
            "def" => {
              let (name, expr) = (&children[1], &children[2]);
              match name
                .atom
                .as_ref()
                .expect("error: `def` has wrong amount of args")
              {
                Atom::Symbol(name) => {
                  let res = eval(&expr, denv, pstore);
                  denv.env.insert(name.to_string(), res);
                }
                _ => panic!("def first arg must be a symbol!"),
              }
              return Expr::Nop;
            }
            "if" => {
              let (test, then, alt) = (&children[1], &children[2], &children[3]);
              return match eval(&test, denv, pstore) {
                Expr::Bool(b) => {
                  if b {
                    eval(&then, denv, pstore)
                  } else {
                    eval(&alt, denv, pstore)
                  }
                }
                _ => panic!("`if` requires a boolean test value"),
              };
            }
            "lambda" => {
              let arg_names = &children[1]
                .children
                .as_ref()
                .expect("error: unwrapping lambda arg_names failed");
              let body = &children[2];

              let arg_names: Vec<String> = arg_names
                .iter()
                .map(|node| {
                  match node
                    .atom
                    .as_ref()
                    .expect("error: unwrapping lambda arg_name failed")
                  {
                    Atom::Symbol(s) => s.to_string(),
                    _ => panic!("error: lambda's argument name is not a Symbol"),
                  }
                })
                .collect();

              pstore.push(LambdaContext::new(&body, arg_names));
              return Expr::LambdaId(pstore.len() - 1);
            }
            _ => proc_call(&ast, denv, pstore, &first),
          },
          // Note: these might be dangerous but now they accomplish that
          // `(true)` is same as `true` amd `(1)` is `1`
          // eg. `(def a (true))` or `(lambda () (true))` work...
          Atom::Number(x) => Expr::Number(*x),
          Atom::Bool(x) => Expr::Bool(*x),
        },
      };
    }
  }
}

fn repl(env: &mut Box<DynamicEnv>, pstore: &mut LambdaContextStore) {
  println!("ellisp 0.1 REPL\n\n> ");
  let _ = io::stdout().flush();
  let stdin = io::stdin();
  let stdin_lock = stdin.lock();
  stdin_lock.lines().filter_map(|l| l.ok()).for_each(|s| {
    let ast = parser(&mut tokenize(s.as_str()));
    let out = eval(&ast, env, pstore);
    print!("{:?}\n> ", out);
    let _ = io::stdout().flush();
  });
}

fn main() {
  let mut dynamic_env = Box::new(DynamicEnv {
    env: HashMap::new(),
    parent: None,
  });
  let mut pstore: LambdaContextStore = Vec::new();
  repl(&mut dynamic_env, &mut pstore);
}
