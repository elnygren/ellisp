use crate::utils::pretty_print;
use std::fmt;
use std::rc::Rc;
/// Common types

/// Atom
/// parser produces an AST of Atoms
#[derive(Debug, Clone)]
pub enum Atom {
  Bool(bool),
  Symbol(String),
  Number(i32),
  // TODO: support doubles, strings... ?
}

/// AST Node is either a single Atom, eg. `Atom::Number(1)` or `Atom::Symbol("sum")`
/// or a list of Atoms: (following is just some pseudo code) `AST["sum", 1, 2, AST["sum", 1, 2]]`
#[derive(Debug, Clone)]
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
  Number(i32),
  Function(fn(args: &Vec<Expr>) -> Expr),
  Bool(bool),
  LambdaId(usize), // "pointer" to our ProcedureStore
  Nop,
  Sexp(AST),
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
			Expr::Sexp(ast) => format!("{}", ast),
		};
		return write!(f, "{}", s);
	}
}
