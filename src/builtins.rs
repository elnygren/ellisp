
use crate::types::Expr;

pub fn ellisp_sum(args: Vec<Expr>) -> Expr {
  let mut sum = 0;
  for v in args {
    match v {
      Expr::Number(n) => sum += n,
      _ => panic!("sum is only defined for numbers"),
    }
  }
  return Expr::Number(sum);
}

pub fn ellisp_minus(args: Vec<Expr>) -> Expr {
  let mut acc = match args[0] {
    Expr::Number(n) => n,
    _ => panic!("minus is only defined for numbers"),
  };
  for v in args.iter().skip(1) {
    match v {
      Expr::Number(n) => acc -= n,
      _ => panic!("minus is only defined for numbers"),
    }
  }
  return Expr::Number(acc);
}

pub fn ellisp_begin(args: Vec<Expr>) -> Expr {
  // TODO: can we not clone here ?
  return args.last().expect("do block received invalid data").clone();
}

pub fn ellisp_smaller_than(args: Vec<Expr>) -> Expr {
  let args: Vec<&i32> = args
    .iter()
    .map(|expr| match expr {
      Expr::Number(n) => n,
      _ => panic!("< only supports numbers"),
    })
    .collect();

  for (i, v) in args.iter().enumerate().skip(1) {
    if args[i - 1] > v {
      return Expr::Bool(false);
    }
  }
  return Expr::Bool(true);
}

pub fn ellisp_equal(args: Vec<Expr>) -> Expr {
  let result = args.iter().all(|x| match (x, &args[0]) {
    (Expr::Number(curr), Expr::Number(first)) => first == curr,
    (Expr::Bool(curr), Expr::Bool(first)) => first == curr,
    _ => false,
  });

  return Expr::Bool(result);
}
