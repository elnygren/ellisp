use crate::types::{Atom, Expr, AST};

pub fn pretty_print(ast: &AST) -> String {
  match ast.atom.as_ref() {
    Some(atom) => match atom {
      Atom::Bool(b) => match b {
        true => String::from("true"),
        false => String::from("false"),
      },
      Atom::Symbol(sym) => sym.to_string(),
      Atom::Keyword(sym) => sym.to_string(),
      Atom::Number(i) => format!("{}", i),
    },
    None => {
      let s = &mut String::from("");
      s.push('(');
      s.push_str(
        &ast
          .children
          .as_ref()
          .unwrap()
          .iter()
          .map(|ch| pretty_print(ch))
          .collect::<Vec<String>>()
          .join(" "),
      );
      s.push(')');
      s.clone()
    }
  }
}

pub fn print_output(expr: &Expr) -> String {
  match expr {
    Expr::Number(i) => format!("{}", i),
    Expr::Function(_) => String::from("Function"),
    Expr::Bool(b) => match b {
      true => String::from("true"),
      false => String::from("false"),
    },
    Expr::LambdaId(_) => String::from("Lambda"),
    Expr::Nop => String::from(""),
    Expr::Sexp(ast) => pretty_print(&ast),
    Expr::List(list) => {
      "(".to_string()
        + &list
          .iter()
          .map(|x| print_output(x))
          .collect::<Vec<String>>()
          .join(" ")
        + &")".to_string()
    }
  }
}
