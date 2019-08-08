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
struct DynamicEnv {
  parent: Option<Rc<RefCell<DynamicEnv>>>,
  data: HashMap<String, Expr>,
}

/// Method implementations for DynamicEnv
/// Recursive .find to look up a symbol from local & parent environments
/// This is how closures are implemented :-)
impl DynamicEnv {
  fn find(&self, key: &str) -> Expr {
    let v = self.data.get(key);
    match v {
      Some(v) => v.clone(),
      None => {
        return self
          .parent
          .as_ref()
          .expect("error: DynamicEnv does not have a parent")
          .try_borrow()
          .expect("error: symbol not found")
          .find(key);
      }
    }
  }
}

/// Story a lambda's body's AST and the param names
/// so we can look them up when the lambda is called at `fn lambda_call`
/// where we eval the body with an environment that has the arguments
/// set to their corresponding arg_names
#[derive(Debug)]
struct LambdaContext {
  body: Rc<AST>,
  arg_names: Vec<String>,
  env: Rc<RefCell<DynamicEnv>>,
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
        l.push(Rc::new(parser(tokens)));
      }
      tokens.remove(0);
      return AST {
        atom: None,
        children: Some(l),
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

/// Eval processes the AST into experssions and evaluates them
/// We traverse the AST recursively evaluating every leaf so we can reduce everything
/// to (proc arg1 arg2 ... argN) form and just call the function corresponding to proc
/// eg. (sum 1 2 3 ... N) -> sum([1,2,3...N]) (in rust)
fn eval(
  // we have refences to the program's AST and to lambda ASTs stored in LambdaContextStore
  arg_ast: Rc<AST>,
  // each per-Procedure env points to their parent env, Rc to keeps track of those references
  arg_denv: Rc<RefCell<DynamicEnv>>,
  pstore: &mut LambdaContextStore,
) -> Expr {
  // println!("eval called (recur): {}", arg_ast);

  let mut ast = arg_ast;
  let mut denv = arg_denv;

  loop {
    // println!("eval called (loop): {}", ast);
    if ast.atom.is_some() {
      // we are processing an AST Atom; a leaf of the AST
      // if the Atom is a Number/Bool, then it's a Number/Bool expression
      // if it's a symbol:
      // 	a) try to find the atom from ellisp static environment
      // 	b) TODO: try to find the atom from ellisp dynamic environment
      return match &ast.atom.as_ref().unwrap() {
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
      };
    };

    // AST node is *not* an atom:
    // We are processing a full expression, eg. (sum 1 2) of the form
    // (proc arg1 arg2 arg3 ... )
    // every argN can be a full expression or a single atom; both are checked recursively
    let children = ast.children.as_ref().expect("error: children is None");
    let first = &children[0];

    if first.is_keyword("def") || first.is_keyword("define") {
      let (name, expr) = (&children[1], &children[2]);
      let name = name.get_atom_symbol("error: `def` expects a symbol & expr, eg; (def a 5).");
      let res = eval(Rc::clone(expr), Rc::clone(&denv), pstore);
      denv.borrow_mut().data.insert(name, res);
      return Expr::Nop;
    } else if first.is_keyword("if") {
      let (test, then, alt) = (&children[1], &children[2], &children[3]);
      match eval(Rc::clone(test), Rc::clone(&denv), pstore) {
        Expr::Bool(b) => match b {
          true => ast = Rc::clone(then),
          false => ast = Rc::clone(alt),
        },
        _ => panic!("`if` requires a boolean test value"),
      }
    } else if first.is_keyword("set!") {
      let (symbol, exp) = (&children[1], &children[2]);
      let key = symbol.get_atom_symbol("error: set! expects a symbol as first arg");
      let value = eval(Rc::clone(exp), Rc::clone(&denv), pstore);
      denv.borrow_mut().data.insert(key, value);
      return Expr::Nop;
    } else if first.is_keyword("quote") {
      return Expr::Sexp((*children[1]).clone());
    } else if first.is_keyword("lambda") {
      // store lambda's AST body & arg_names for later (re)use
      // we clone data here because LambdaContext stores lambda bodies after the entire AST has been freed
      // for example, in repl each input is a new AST but we still want to have our old lambdas
      let arg_names: Vec<String> = children[1]
        .children
        .as_ref()
        .expect("error: unwrapping lambda arg_names failed")
        .iter()
        .map(|node| node.get_atom_symbol("error: lambda expects symbols as arg names"))
        .collect();
      let body = children[2].clone();
      pstore.push(LambdaContext {
        body: Rc::clone(&body),
        arg_names: arg_names,
        env: Rc::clone(&denv),
      });
      return Expr::LambdaId(pstore.len() - 1);
    } else {
      // proc call; (proc expr1 expr2 ...)
      let mut exprs: Vec<Expr> = children
        .iter()
        .map(|node| eval(Rc::clone(node), Rc::clone(&denv), pstore))
        .collect();
      let proc = exprs.remove(0);

      let res = match proc {
        Expr::Function(f) => Some(f(&exprs)),
        Expr::LambdaId(lambda_id) => {
          let ctx = &pstore[lambda_id];
          let mut local_env = ctx.env.borrow_mut();
          // TODO: here check that the lists are equal length!
          for (arg_name, arg_value) in izip!(&ctx.arg_names, exprs) {
            local_env.data.insert(arg_name.to_string(), arg_value);
          }

          // tail call optimisation; set new ast, denv and continue with loop
          denv = Rc::clone(&ctx.env);
          ast = Rc::clone(&ctx.body);
          None
        }
        _ => panic!("Expected Expr::Function or Expr::Lambda"),
      };

      if res.is_some() {
        return res.unwrap();
      }
    }
  }
}

/// the iconic lisp repl
fn repl(env: Rc<RefCell<DynamicEnv>>, pstore: &mut LambdaContextStore) {
  print!("ellisp 0.1 REPL\n\n> ");
  let _ = io::stdout().flush();
  let stdin = io::stdin();
  let stdin_lock = stdin.lock();

  stdin_lock.lines().filter_map(|l| l.ok()).for_each(|s| {
    let ast = parser(&mut tokenize(s.as_str()));
    let out = eval(Rc::new(ast), Rc::clone(&env), pstore);
    print!("{}\n> ", print_output(out));
    let _ = io::stdout().flush();
  });
}

/// quick & dirty way to run some program
fn driver(env: Rc<RefCell<DynamicEnv>>, pstore: &mut LambdaContextStore) {
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
  ;(define (sum-to n) (if (= n 0) 0 (+ n (sum-to (- n 1)))))
	;(fib 30)
	(define sum-to (lambda (n) (if (= n 0) 0 (+ n (sum-to (- n 1))))))
	(define sum2 (lambda (n acc) (if (= n 0) acc (sum2 (- n 1) (+ n acc)))))
	;(sum-to 10000)
	(sum2 1000000 0)
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
  let out = eval(Rc::new(ast), env, pstore);
  println!("Program: {}", program);
  print!("{}\n> ", print_output(out));
}

fn main() {
  // build dynamic env and procedure store
  let dynamic_env = Rc::new(RefCell::new(DynamicEnv {
    data: HashMap::new(),
    parent: None,
  }));
  let mut pstore = Vec::new();

  // parse args & run
  let args: Vec<_> = env::args().collect();
  if args.len() > 1 && args[1] == "repl" {
    repl(dynamic_env, &mut pstore);
  } else {
    driver(dynamic_env, &mut pstore);
  }
}
