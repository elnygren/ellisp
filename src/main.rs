use std::cell::RefCell;
use std::collections::HashMap;
use std::env;
use std::io;
use std::io::prelude::*;
use std::rc::Rc;

mod interpreter;
mod builtins;
mod types;
mod utils;
mod parser;

use utils::print_output;
use parser::{parser, tokenize};
use interpreter::{eval};
use types::{DynamicEnv, LambdaContextStore};

#[macro_use]
extern crate itertools;

/// Note: If you are reading this code:
/// - types.rs contains custom types like AST, Atom, Expr...
/// - parser.rs is where everything starts; tokenize + parser
/// - interpreter.rs contains `fn eval` which is the beef here


/// the iconic lisp repl
fn repl(env: Rc<RefCell<DynamicEnv>>, pstore: &mut LambdaContextStore) {
  print!("ellisp 0.1 REPL\n\n> ");
  let _ = io::stdout().flush();
  let stdin = io::stdin();
  let stdin_lock = stdin.lock();

  stdin_lock.lines().filter_map(|l| l.ok()).for_each(|s| {
    let ast = parser(&mut tokenize(s.as_str()));
    let out = eval(Rc::new(ast), Rc::clone(&env), pstore);
    print!("{}\n> ", print_output(&out));
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
	(sum2 10000 0)
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
  print!("{}\n> ", print_output(&out));
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


