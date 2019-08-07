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
