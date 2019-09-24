use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt;
use std::rc::Rc;

use crate::utils::pretty_print;

/// Ellisp has special syntactic forms that begin with one of these keywords
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Keyword {
  /// Declare a variable: `(def name expr)`
  /// eg. `(define a 5)` or `(def a 5)`
  Def,
  /// (if test-expr then-expr else-expr)
  /// eg. `(if (= a 5) 1 0)`
  If,
  /// Mutate a variable: `(set! name expr)`
  /// eg. `(set! a 6)`
  Set,
  /// Quote an expr so it's not evalued
  /// eg. `(quote (something literally (not evalued directly)))`
  Quote,
  /// Declare a function: `(lambda params expr)`
  /// eg. `(def plus2 (lambda (x y) (+ x y)))`
  Lambda,
}

impl fmt::Display for Keyword {
  #[rustfmt::skip]
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", match self {
      Keyword::Def => "def",
      Keyword::If => "if",
      Keyword::Set => "set!",
      Keyword::Quote => "quote",
      Keyword::Lambda => "lambda",
    })
  }
}

impl TryFrom<&str> for Keyword {
  type Error = &'static str;
  fn try_from(v: &str) -> Result<Keyword, Self::Error> {
    match v {
      "def" => Ok(Keyword::Def),
      "define" => Ok(Keyword::Def),
      "if" => Ok(Keyword::If),
      "set!" => Ok(Keyword::Set),
      "quote" => Ok(Keyword::Quote),
      "lambda" => Ok(Keyword::Lambda),
      _ => Err("not a keyword"),
    }
  }
}

/// Atom
/// parser produces an AST of Atoms
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Atom {
  Bool(bool),
  Symbol(String),
  Keyword(Keyword),
  Number(i32),
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
    match self.atom.as_ref().expect(error) {
      Atom::Symbol(s) => s.to_string(),
      _ => panic!("error: `get_atom_symbol` received a non-symbol atom"),
    }
  }
}

impl fmt::Display for AST {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", pretty_print(self))
  }
}

/// During evaluation, our AST is turned into Expressions
/// All of our builtins are Vec<Expr> -> Expr (eg. sum, minus, ...)
#[derive(Clone, Debug)]
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
    match self.data.get(key) {
      Some(v) => v.clone(),
      None => self
        .parent
        .as_ref()
        .unwrap_or_else(|| panic!("error: symbol `{}` not found", key))
        .try_borrow()
        .unwrap_or_else(|_| panic!("error: symbol `{}` not found", key))
        .find(key),
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
