use crate::types::Expr;
use std::convert::TryInto;

fn take_numbers(args: Vec<Expr>) -> Vec<i32> {
  args
    .iter()
    .map(|expr| match expr {
      Expr::Number(n) => *n,
      _ => panic!("< only supports numbers"),
    })
    .collect()
}

pub fn ellisp_sum(args: Vec<Expr>) -> Expr {
  Expr::Number(
    args
      .iter()
      .map(|v| match v {
        Expr::Number(n) => n,
        _ => panic!("sum is only defined for numbers"),
      })
      .sum(),
  )
}

pub fn ellisp_minus(args: Vec<Expr>) -> Expr {
  Expr::Number(
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
  )
}

pub fn ellisp_begin(mut args: Vec<Expr>) -> Expr {
  args.remove(args.len() - 1)
}

pub fn ellisp_smaller_than(args: Vec<Expr>) -> Expr {
  let args = take_numbers(args);
  for (i, v) in args.iter().enumerate().skip(1) {
    if args[i - 1] >= *v {
      return Expr::Bool(false);
    }
  }
  Expr::Bool(true)
}

pub fn ellisp_smaller_or_equal_than(args: Vec<Expr>) -> Expr {
  let args = take_numbers(args);
  for (i, v) in args.iter().enumerate().skip(1) {
    if args[i - 1] > *v {
      return Expr::Bool(false);
    }
  }
  Expr::Bool(true)
}

pub fn ellisp_larger_than(args: Vec<Expr>) -> Expr {
  let args = take_numbers(args);
  for (i, v) in args.iter().enumerate().skip(1) {
    if args[i - 1] <= *v {
      return Expr::Bool(false);
    }
  }
  Expr::Bool(true)
}

pub fn ellisp_larger_or_equal_than(args: Vec<Expr>) -> Expr {
  let args = take_numbers(args);
  for (i, v) in args.iter().enumerate().skip(1) {
    if args[i - 1] < *v {
      return Expr::Bool(false);
    }
  }
  Expr::Bool(true)
}

pub fn ellisp_equal(args: Vec<Expr>) -> Expr {
  let result = args.iter().all(|x| match (x, &args[0]) {
    (Expr::Number(curr), Expr::Number(first)) => first == curr,
    (Expr::Bool(curr), Expr::Bool(first)) => first == curr,
    _ => false,
  });

  Expr::Bool(result)
}

pub fn ellisp_multiply(args: Vec<Expr>) -> Expr {
  Expr::Number(
    args
      .iter()
      .map(|v| match v {
        Expr::Number(n) => n,
        _ => panic!("multiply is only defined for numbers"),
      })
      .product(),
  )
}

pub fn ellisp_div(args: Vec<Expr>) -> Expr {
  Expr::Number(
    args
      .iter()
      .skip(1)
      .map(|v| match v {
        Expr::Number(n) => n,
        _ => panic!("div is only defined for numbers"),
      })
      .fold(
        match args[0] {
          Expr::Number(n) => n,
          _ => panic!("div is only defined for numbers"),
        },
        |acc, x| acc / x,
      ),
  )
}

pub fn ellisp_list(args: Vec<Expr>) -> Expr {
  Expr::List(args.to_vec())
}

/// (cons 1 (list 2 3)) -> (1 2 3)
pub fn ellisp_cons(mut args: Vec<Expr>) -> Expr {
  if args.len() != 2 {
    panic!("cons expects two arguments");
  }
  let fst = args.swap_remove(0);
  let mut snd = args.remove(0);

  match &mut snd {
    Expr::List(l1) => {
      let mut result: Vec<Expr> = Vec::new();
      result.push(fst);
      result.append(l1);
      Expr::List(result)
    }
    _ => panic!("cons second argument must be a list"),
  }
}

pub fn ellisp_car(mut args: Vec<Expr>) -> Expr {
  match &mut args.swap_remove(0) {
    Expr::List(list) => list.swap_remove(0),
    _ => panic!("cdr expects a list"),
  }
}

pub fn ellisp_cdr(args: Vec<Expr>) -> Expr {
  match &args[0] {
    Expr::List(list) => match list.len() {
      0 => Expr::List([].to_vec()),
      1 => Expr::List([].to_vec()),
      _ => Expr::List(list[1..].to_vec()),
    },
    // _ => Expr::Nop,
    _ => panic!("car expects a list"),
  }
}

pub fn ellisp_isnull(args: Vec<Expr>) -> Expr {
  match args[0].clone() {
    Expr::List(list) => match list.len() {
      0 => Expr::Bool(true),
      _ => Expr::Bool(false),
    },
    _ => panic!("null? looks for empty list!"),
  }
}

pub fn ellisp_append(mut args: Vec<Expr>) -> Expr {
  println!("append, {:?}", args);
  if args.len() != 2 {
    panic!("append expects two arguments");
  }
  let mut fst = args.swap_remove(0);
  let mut snd = args.remove(0);

  match (&mut fst, &mut snd) {
    (Expr::List(l1), Expr::List(l2)) => {
      let mut result: Vec<Expr> = Vec::new();
      result.append(l1);
      result.append(l2);
      Expr::List(result)
    }
    (Expr::List(l1), _) => {
      let mut result: Vec<Expr> = Vec::new();
      result.append(l1);
      result.push(snd);
      Expr::List(result)
    }
    _ => panic!("append first argument must be a list"),
  }
}

pub fn ellisp_length(args: Vec<Expr>) -> Expr {
  match &args[0] {
    Expr::List(list) => Expr::Number(list.len().try_into().unwrap()),
    _ => panic!("length expects a list"),
  }
}
