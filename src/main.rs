use regex::Regex;
use std::cell::RefCell;
use std::collections::HashMap;
use std::env;
use std::io;
use std::io::prelude::*;
use std::rc::Rc;

mod builtins;
mod types;
mod utils;

use builtins::{ellisp_begin, ellisp_equal, ellisp_minus, ellisp_smaller_than, ellisp_sum};
use types::{Atom, Expr, AST};
use utils::print_output;

#[macro_use]
extern crate itertools;

/// Token is just a string
/// ellisp program is a string that is first tokenised `String => Vec<Token>`
/// and then atomized by our parser `Token => Atom`
type Token = String;

/// Dynamic Environment contains all user defined symbols
/// Per-procedure (lambda) environmnts link to their parent
#[derive(Debug)]
struct DynamicEnv<'a> {
  // parent: Option<DynamicEnv>,
  parent: Rc<Option<&'a DynamicEnv<'a>>>,
  env: HashMap<String, Expr>,
}

/// Method implementations for DynamicEnv
/// Recursive .find to look up a symbol from local & parent environments
/// This is how closures are implemented :-)
impl<'a> DynamicEnv<'a> {
  fn find(&self, key: &str) -> Expr {
    let v = self.env.get(key);
    match v {
      Some(v) => v.clone(),
      None => {
        return self.parent.unwrap().find(key);
      }
    }
  }
}

/// Story a lambda's body's AST and the param names
/// so we can look them up when the lambda is called at `fn lambda_call`
/// where we eval the body with an environment that has the arguments
/// set to their corresponding param_names
#[derive(Debug)]
struct LambdaContext {
  body: AST,
  param_names: Vec<String>,
}

impl LambdaContext {
  /// Copy the body's AST tree and take ownership of function param names
  fn new(body: &AST, param_names: Vec<String>) -> LambdaContext {
    return LambdaContext {
      body: body.clone(),
      param_names: param_names,
    };
  }
}

type LambdaContextStore = Vec<LambdaContext>;

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

/// helper for eval
/// perform a lambda call using a local environment
fn lambda_call(
  args: &Vec<Expr>,
  denv: &Rc<RefCell<DynamicEnv>>,
  pstore: &Rc<RefCell<LambdaContextStore>>,
  lambda_id: usize,
) -> Expr {
  // println!("lambda call {:? }{:?}", args, pstore);
  let ctx = &pstore.try_borrow().expect("error: mut borrowing pstore at lambda_call failed")[lambda_id];

  let mut local_env = HashMap::new();
  for (arg_name, arg_value) in izip!(&ctx.param_names, args) {
    local_env.insert(arg_name.to_string(), arg_value.clone());
  }

  let denv = denv.try_borrow().expect("error: borrowing denv at lambda_call failed");
  let denv = Rc::new(RefCell::new(DynamicEnv {
    env: local_env,
    parent: Rc::new(Some(&denv)),
  }));
  return eval(&ctx.body, &denv, pstore);
}

/// helper for eval
/// evaluates a proc and all of it's arguments and then calls it
fn proc_call(
  ast: &AST,
  denv: &Rc<RefCell<DynamicEnv>>,
  pstore: &Rc<RefCell<LambdaContextStore>>,
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
fn eval(
  ast: &AST,
  denv: &Rc<RefCell<DynamicEnv>>,
  pstore: &Rc<RefCell<LambdaContextStore>>,
) -> Expr {
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
        "sum" => Expr::Function(ellisp_sum),
        "+" => Expr::Function(ellisp_sum),
        "-" => Expr::Function(ellisp_minus),
        "begin" => Expr::Function(ellisp_begin),
        "do" => Expr::Function(ellisp_begin),
        "=" => Expr::Function(ellisp_equal),
        "<" => Expr::Function(ellisp_smaller_than),
        // dynamic env
        _ => {
          return denv
            .try_borrow()
            .expect("error: borrowing denv failed")
            .find(x.as_str());
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
                  denv.borrow_mut().env.insert(name.to_string(), res);
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
            "set!" => {
              let (symbol, exp) = (&children[1], &children[2]);
              let key = match symbol
                .atom
                .as_ref()
                .expect("error: set! expects aa symbol as first arg")
              {
                Atom::Symbol(s) => s,
                _ => panic!("error: set! expects a symbol as first arg"),
              };
              let value = eval(exp, denv, pstore);
              let _ = denv.borrow_mut().env.insert(key.to_string(), value);
              return Expr::Nop;
            }
            "quote" => {
              let sexp = &children[1];
              return Expr::Sexp(sexp.clone());
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

              pstore
                .try_borrow_mut()
								.expect("error: mut borrowing pstore at lambda form failed")
                .push(LambdaContext::new(&body, arg_names));
              return Expr::LambdaId(
                pstore
                  .try_borrow()
                  .expect("error: borrowing pstore at lambda form failed")
                  .len()
                  - 1,
              );
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

/// the iconic lisp repl
fn repl(env: &Rc<RefCell<DynamicEnv>>, pstore: &Rc<RefCell<LambdaContextStore>>) {
  print!("ellisp 0.1 REPL\n\n> ");
  let _ = io::stdout().flush();
  let stdin = io::stdin();
  let stdin_lock = stdin.lock();
  stdin_lock.lines().filter_map(|l| l.ok()).for_each(|s| {
    let ast = parser(&mut tokenize(s.as_str()));
    let out = eval(&ast, env, pstore);
    print!("{}\n> ", print_output(out));
    let _ = io::stdout().flush();
  });
}

/// quick & dirty way to run some program
fn driver(env: &Rc<RefCell<DynamicEnv>>, pstore: &Rc<RefCell<LambdaContextStore>>) {
  let program = "
  ; def + lambdas
  ; (def a (lambda (x) (+ x 40)))
  ; (def b (lambda (x) (+ 0 2)))
  ; (def c (24))
  ; (def d -24)
  ; (def result-1 (+ (a 2) (b 666) c d))
  ; (do
    ; (set! result-1 666)
    ; result-1)
	;(quote (well hello (there darkness) my old friend))
  ;(a (a 20))
  (def fib (lambda (n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))))
	(fib 3)
  ";

  let program = format!(
    "\n(do\n  ; programs are wrapped in a do-block\n{}\n)",
    program
  );
  let program = program.as_str();
  let mut tokens = tokenize(program);
  // println!("tokens are: {:?}", tokens);
  let ast = parser(&mut tokens);
  // println!("AST: {:?}", ast);
  let out = eval(&ast, &env, &pstore);
  println!("Program: {}", program);
  print!("{}\n> ", print_output(out));
}

fn main() {
  // build dynamic env and procedure store
  let dynamic_env = Rc::new(RefCell::new(DynamicEnv {
    env: HashMap::new(),
    parent: Rc::new(None),
  }));
  let pstore = Rc::new(RefCell::new(Vec::new()));

  // parse args & run
  let args: Vec<_> = env::args().collect();
  if args.len() > 1 && args[1] == "repl" {
    repl(&dynamic_env, &pstore);
  } else {
    driver(&dynamic_env, &pstore);
  }
}
