use crate::types::Expr;

fn take_numbers(args: &Vec<Expr>) -> Vec<&i32> {
  return args
    .iter()
    .map(|expr| match expr {
      Expr::Number(n) => n,
      _ => panic!("< only supports numbers"),
    })
    .collect();
}

pub fn ellisp_sum(args: &Vec<Expr>) -> Expr {
  return Expr::Number(
    args
      .iter()
      .map(|v| match v {
        Expr::Number(n) => n,
        _ => panic!("sum is only defined for numbers"),
      })
      .fold(0, |acc, x| acc + x),
  );
}

pub fn ellisp_minus(args: &Vec<Expr>) -> Expr {
  return Expr::Number(
    args
      .iter()
      .skip(1)
      .map(|v| match v {
        Expr::Number(n) => n,
        _ => panic!("minus is only defined for numbers"),
      })
      .fold(
        match args[0] {
          Expr::Number(n) => n,
          _ => panic!("minus is only defined for numbers"),
        },
        |acc, x| acc - x,
      ),
  );
}

pub fn ellisp_begin(args: &Vec<Expr>) -> Expr {
  // TODO: can we not clone here ?
  return args.last().expect("do block received invalid data").clone();
}

pub fn ellisp_smaller_than(args: &Vec<Expr>) -> Expr {
  let args = take_numbers(args);
  for (i, v) in args.iter().enumerate().skip(1) {
    if args[i - 1] >= v {
      return Expr::Bool(false);
    }
  }
  return Expr::Bool(true);
}

pub fn ellisp_smaller_or_equal_than(args: &Vec<Expr>) -> Expr {
  let args = take_numbers(args);
  for (i, v) in args.iter().enumerate().skip(1) {
    if args[i - 1] > v {
      return Expr::Bool(false);
    }
  }
  return Expr::Bool(true);
}

pub fn ellisp_larger_than(args: &Vec<Expr>) -> Expr {
  let args = take_numbers(args);
  for (i, v) in args.iter().enumerate().skip(1) {
    if args[i - 1] <= v {
      return Expr::Bool(false);
    }
  }
  return Expr::Bool(true);
}

pub fn ellisp_larger_or_equal_than(args: &Vec<Expr>) -> Expr {
  let args = take_numbers(args);
  for (i, v) in args.iter().enumerate().skip(1) {
    if args[i - 1] < v {
      return Expr::Bool(false);
    }
  }
  return Expr::Bool(true);
}

pub fn ellisp_equal(args: &Vec<Expr>) -> Expr {
  let result = args.iter().all(|x| match (x, &args[0]) {
    (Expr::Number(curr), Expr::Number(first)) => first == curr,
    (Expr::Bool(curr), Expr::Bool(first)) => first == curr,
    _ => false,
  });

  return Expr::Bool(result);
}

pub fn ellisp_multiply(args: &Vec<Expr>) -> Expr {
  return Expr::Number(
    args
      .iter()
      .map(|v| match v {
        Expr::Number(n) => n,
        _ => panic!("multiply is only defined for numbers"),
      })
      .fold(1, |acc, x| acc * x),
  );
}
