/// Common types


/// During evaluation, our AST is turned into Expressions
/// All of our builtins are Vec<Expr> -> Expr (eg. sum, minus, ...)
#[derive(Debug, Copy, Clone)]
pub enum Expr {
  Number(i32),
  Function(fn(args: Vec<Expr>) -> Expr),
  Bool(bool),
  LambdaId(usize), // "pointer" to our ProcedureStore
  Nop,
  // TODO: support doubles, strings... ?
}
