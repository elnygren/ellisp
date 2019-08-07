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
  pub children: Option<Box<Vec<AST>>>,
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
}

/// During evaluation, our AST is turned into Expressions
/// All of our builtins are Vec<Expr> -> Expr (eg. sum, minus, ...)
#[derive(Debug, Clone)]
pub enum Expr {
  Number(i32),
  Function(fn(args: Vec<Expr>) -> Expr),
  Bool(bool),
  LambdaId(usize), // "pointer" to our ProcedureStore
  Nop,
  Sexp(AST),
  // TODO: support doubles, strings... ?
}
