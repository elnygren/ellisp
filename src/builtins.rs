use crate::types::Expr;
use std::convert::TryInto;

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

pub fn ellisp_div(args: &Vec<Expr>) -> Expr {
  return Expr::Number(
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
  );
}

pub fn ellisp_list(args: &Vec<Expr>) -> Expr {
  return Expr::List(args.to_vec());
}

pub fn ellisp_cons(args: &Vec<Expr>) -> Expr {
  let lst = args[1].clone();
  return match lst {
    Expr::List(vv) => {
      let mut result: Vec<Expr> = Vec::new();
      result.push(args[0].clone());
      result.append(&mut vv.clone());
      return Expr::List(result);
    }
    _ => panic!("cons second argument must be a list"),
  };
}

pub fn ellisp_car(args: &Vec<Expr>) -> Expr {
  return match args[0].clone() {
    Expr::List(list) => list[0].clone(),
    _ => panic!("cdr expects a list"),
  };
}

pub fn ellisp_cdr(args: &Vec<Expr>) -> Expr {
  return match args[0].clone() {
    Expr::List(list) => match list.len() {
      0 => Expr::List([].to_vec()),
      1 => Expr::List([].to_vec()),
      _ => Expr::List(list[1..].to_vec()),
    },
    // _ => Expr::Nop,
    _ => panic!("car expects a list"),
  };
}

pub fn ellisp_isnull(args: &Vec<Expr>) -> Expr {
  return match args[0].clone() {
    Expr::List(list) => match list.len() {
      0 => Expr::Bool(true),
      _ => Expr::Bool(false),
    },
    _ => panic!("null? looks for empty list!"),
  };
}

pub fn ellisp_append(args: &Vec<Expr>) -> Expr {
  let lst = args[0].clone();
  let snd = args[1].clone();
  return match (lst, snd) {
    (Expr::List(l1), Expr::List(l2)) => {
      let mut result: Vec<Expr> = l1.clone();
      result.append(&mut l2.clone());
      return Expr::List(result);
    }
    (Expr::List(vv), _) => {
      let mut result: Vec<Expr> = vv.clone();
      result.push(args[1].clone());
      return Expr::List(result);
    }
    _ => panic!("append first argument must be a list"),
  };
}

pub fn ellisp_length(args: &Vec<Expr>) -> Expr {
  return match args[0].clone() {
    Expr::List(list) => Expr::Number(list.len().try_into().unwrap()),
    _ => panic!("length expects a list"),
  };
}
