use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use crate::utils::pretty_print;
/// Common types

/// Atom
/// parser produces an AST of Atoms
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Atom {
  Bool(bool),
  Symbol(String),
  Number(i32),
  // TODO: support doubles, strings... ?
}

/// AST Node is either a single Atom, eg. `Atom::Number(1)` or `Atom::Symbol("sum")`
/// or a list of Atoms: (following is just some pseudo code) `AST["sum", 1, 2, AST["sum", 1, 2]]`
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct AST {
  pub atom: Option<Atom>,
  pub children: Option<Vec<Rc<AST>>>,
}

impl AST {
  /// Helper for taking an AST (that is an Atom) node's Atom::Symbol(String) content
  ///
  /// note: it would probably be better Rust to return a Result but the current API
  /// seems fine for it's usecase (for now)
  pub fn get_atom_symbol(&self, error: &str) -> String {
    return match self.atom.as_ref().expect(error) {
      Atom::Symbol(s) => s.to_string(),
      _ => panic!("error: `get_atom_symbol` received a non-symbol atom"),
    };
  }

  /// Helper for checking if the AST node is a given keyword
  pub fn is_keyword(&self, kw: &str) -> bool {
    return match self.atom.as_ref() {
      None => false,
      Some(atom) => match atom {
        Atom::Symbol(s) => s == kw,
        _ => false,
      },
    };
  }
}

impl fmt::Display for AST {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let s = pretty_print(self);
    return write!(f, "{}", s);
  }
}

/// During evaluation, our AST is turned into Expressions
/// All of our builtins are Vec<Expr> -> Expr (eg. sum, minus, ...)
#[derive(Clone)]
pub enum Expr {
  Bool(bool),
  Function(fn(args: Vec<Expr>) -> Expr),
  LambdaId(usize), // "pointer" to our ProcedureStore
  List(Vec<Expr>),
  Nop,
  Number(i32),
  Sexp(Rc<AST>),
  // TODO: support doubles, strings... ?
}

impl std::fmt::Debug for Expr {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::result::Result<(), std::fmt::Error> {
    let s = match self {
      Expr::Number(i) => format!("{}", i),
      Expr::Function(_) => String::from("Expr::Function"),
      Expr::Bool(b) => format!("{}", b),
      Expr::LambdaId(i) => format!("Expr::Lambda: {}", i),
      Expr::Nop => String::from("Expr::Nop"),
      Expr::Sexp(ast) => format!("Expr::sexp {}", ast),
      Expr::List(list) => format!("{:?}", list),
    };
    return write!(f, "{}", s);
  }
}

/// Dynamic Environment contains all user defined symbols
/// Per-procedure (lambda) environmnts link to their parent
#[derive(Debug)]
pub struct DynamicEnv {
  pub parent: Option<Rc<RefCell<DynamicEnv>>>,
  pub data: HashMap<String, Expr>,
}

/// Method implementations for DynamicEnv
/// Recursive .find to look up a symbol from local & parent environments
/// This is how closures are implemented :-)
impl DynamicEnv {
  pub fn find(&self, key: &str) -> Expr {
    let v = self.data.get(key);
    match v {
      Some(v) => v.clone(),
      None => {
        return self
          .parent
          .as_ref()
          .expect(&format!("error: symbol `{}` not found", key).to_string())
          .try_borrow()
          .expect(&format!("error: symbol `{}` not found", key).to_string())
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
pub struct LambdaContext {
  pub body: Rc<AST>,
  pub arg_names: Vec<String>,
  pub env: Rc<RefCell<DynamicEnv>>,
}

pub type LambdaContextStore = Vec<LambdaContext>;
