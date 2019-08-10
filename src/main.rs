use std::cell::RefCell;
use std::collections::HashMap;
use std::env;
use std::io;
use std::io::prelude::*;
use std::rc::Rc;

mod builtins;
mod interpreter;
mod parser;
mod types;
mod utils;

use interpreter::eval;
use parser::{parser, tokenize};
use types::{DynamicEnv, LambdaContextStore};
use utils::print_output;

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
    (quote (replace me))
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
  println!("Program: {}\n", program);
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
