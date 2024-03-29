use std::cell::RefCell;
use std::collections::HashMap;
use std::env;
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

use rustyline::error::ReadlineError;
use rustyline::Editor;

#[macro_use]
extern crate itertools;

/// Note: If you are reading this code:
/// - types.rs contains custom types like AST, Atom, Expr...
/// - parser.rs is where everything starts; tokenize + parser
/// - interpreter.rs contains `fn eval` which is the beef here

/// the iconic lisp repl with rustyline (GNU Readline implementation in Rust)
fn repl(env: Rc<RefCell<DynamicEnv>>, pstore: &mut LambdaContextStore) {
  let mut rl = Editor::<()>::new();
  println!("ellisp 0.1 REPL");
  loop {
    let readline = rl.readline(">>> ");
    match readline {
      Ok(line) => {
        let s = line.as_str();
        if s.trim_start().len() > 0 {
          rl.add_history_entry(s);
          // parse, eval & print
          let ast = parser(&mut tokenize(s));
          let out = eval(Rc::new(ast), Rc::clone(&env), pstore);
          print!("{}", print_output(&out));
        }
      }
      Err(ReadlineError::Interrupted) => {
        println!("CTRL-C");
        break;
      }
      Err(ReadlineError::Eof) => {
        println!("CTRL-D");
        break;
      }
      Err(err) => {
        println!("Error: {:?}", err);
        break;
      }
    }
  }
}

/// quick & dirty way to run some program
fn driver(program: &str, env: Rc<RefCell<DynamicEnv>>, pstore: &mut LambdaContextStore) {
  let program = format!("\n(do\n{}\n)", program);
  let program = program.as_str();
  let mut tokens = tokenize(program);
  // println!("tokens are: {:?}", tokens);
  let ast = parser(&mut tokens);
  // println!("AST: {:?}", ast);
  let out = eval(Rc::new(ast), env, pstore);
  // println!("Program: {}\n", program);
  println!("{}", print_output(&out));
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
  if args.len() > 1 {
    match args[1].as_str() {
      "repl" => repl(dynamic_env, &mut pstore),
      "--" => driver(&args[2..].join(" "), dynamic_env, &mut pstore),
      v => panic!("Unknown arg: {}", v),
    }
  } else {
    repl(dynamic_env, &mut pstore)
  }
}
