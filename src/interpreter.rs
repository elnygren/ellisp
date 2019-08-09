use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::builtins::*;
use crate::types::{Atom, DynamicEnv, Expr, LambdaContext, LambdaContextStore, AST};

/// Eval processes the AST into experssions and evaluates them
/// We traverse the AST recursively evaluating every leaf so we can reduce everything
/// to (proc arg1 arg2 ... argN) form and just call the function corresponding to proc
/// eg. (sum 1 2 3 ... N) -> sum([1,2,3...N]) (in rust)
pub fn eval(
  // we have refences to the program's AST and to lambda ASTs stored in LambdaContextStore
  arg_ast: Rc<AST>,
  // each per-Procedure env points to their parent env, Rc to keeps track of those references
  arg_denv: Rc<RefCell<DynamicEnv>>,
  pstore: &mut LambdaContextStore,
) -> Expr {
  // println!("eval called (recur): {}", arg_ast);

  let mut ast = arg_ast;
  let mut denv = arg_denv;

  loop {
    // println!("eval called (loop): {}", ast);
    if ast.atom.is_some() {
      // we are processing an AST Atom; a leaf of the AST
      // if the Atom is a Number/Bool, then it's a Number/Bool expression
      // if it's a symbol:
      // 	a) try to find the atom from ellisp static environment
      // 	b) TODO: try to find the atom from ellisp dynamic environment
      return match &ast.atom.as_ref().unwrap() {
        Atom::Symbol(x) => match x.as_str() {
          // static env
          "sum" => Expr::Function(ellisp_sum),
          "+" => Expr::Function(ellisp_sum),
          "-" => Expr::Function(ellisp_minus),
          "*" => Expr::Function(ellisp_multiply),
          "begin" => Expr::Function(ellisp_begin),
          "do" => Expr::Function(ellisp_begin),
          "=" => Expr::Function(ellisp_equal),
          "<" => Expr::Function(ellisp_smaller_than),
          "<=" => Expr::Function(ellisp_smaller_or_equal_than),
          ">" => Expr::Function(ellisp_larger_than),
          ">=" => Expr::Function(ellisp_larger_or_equal_than),
          // dynamic env
          _ => {
            return denv
              .try_borrow()
              .expect("error: borrowing denv failed")
              .find(x.as_str());
          }
        },
        Atom::Number(x) => Expr::Number(*x),
        Atom::Bool(x) => Expr::Bool(*x),
      };
    };

    // AST node is *not* an atom:
    // We are processing a full expression, eg. (sum 1 2) of the form
    // (proc arg1 arg2 arg3 ... )
    // every argN can be a full expression or a single atom; both are checked recursively
    let children = ast.children.as_ref().expect("error: children is None");
    let first = &children[0];

    if first.is_keyword("def") || first.is_keyword("define") {
      let (name, expr) = (&children[1], &children[2]);
      let name = name.get_atom_symbol("error: `def` expects a symbol & expr, eg; (def a 5).");
      let res = eval(Rc::clone(expr), Rc::clone(&denv), pstore);
      denv.borrow_mut().data.insert(name, res);
      return Expr::Nop;
    } else if first.is_keyword("if") {
      let (test, then, alt) = (&children[1], &children[2], &children[3]);
      match eval(Rc::clone(test), Rc::clone(&denv), pstore) {
        Expr::Bool(b) => match b {
          true => ast = Rc::clone(then),
          false => ast = Rc::clone(alt),
        },
        _ => panic!("`if` requires a boolean test value"),
      }
    } else if first.is_keyword("set!") {
      let (symbol, exp) = (&children[1], &children[2]);
      let key = symbol.get_atom_symbol("error: set! expects a symbol as first arg");
      let value = eval(Rc::clone(exp), Rc::clone(&denv), pstore);
      denv.borrow_mut().data.insert(key, value);
      return Expr::Nop;
    } else if first.is_keyword("quote") {
      return Expr::Sexp((*children[1]).clone());
    } else if first.is_keyword("lambda") {
      // store lambda's AST body & arg_names for later (re)use
      // we clone data here because LambdaContext stores lambda bodies after the entire AST has been freed
      // for example, in repl each input is a new AST but we still want to have our old lambdas
      let arg_names: Vec<String> = children[1]
        .children
        .as_ref()
        .expect("error: unwrapping lambda arg_names failed")
        .iter()
        .map(|node| node.get_atom_symbol("error: lambda expects symbols as arg names"))
        .collect();
      let body = children[2].clone();
      pstore.push(LambdaContext {
        body: Rc::clone(&body),
        arg_names: arg_names,
        env: Rc::clone(&denv),
      });
      return Expr::LambdaId(pstore.len() - 1);
    } else {
      // proc call; (proc expr1 expr2 ...)
      let mut exprs: Vec<Expr> = children
        .iter()
        .map(|node| eval(Rc::clone(node), Rc::clone(&denv), pstore))
        .collect();
      let proc = exprs.remove(0);

      let res = match proc {
        Expr::Function(f) => Some(f(&exprs)),
        Expr::LambdaId(lambda_id) => {
          let ctx = &pstore[lambda_id];

          // create new dynamic env with arg names & values set for executing lambda body
          let local_env = Rc::new(RefCell::new(DynamicEnv {
            data: HashMap::new(),
            parent: Some(Rc::clone(&ctx.env)),
          }));
          // TODO: here check that the lists are equal length!
          for (arg_name, arg_value) in izip!(&ctx.arg_names, exprs) {
            local_env
              .borrow_mut()
              .data
              .insert(arg_name.to_string(), arg_value);
          }

          // tail call optimisation; set new ast, denv and continue with loop
          // lambda gets executed correctly on the next iteration
          denv = Rc::clone(&local_env);
          ast = Rc::clone(&ctx.body);
          None
        }
        _ => panic!("Expected Expr::Function or Expr::Lambda"),
      };

      if res.is_some() {
        return res.unwrap();
      }
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::parser::{parser, tokenize};
  use crate::types::{DynamicEnv, Expr};
  use crate::utils::print_output;
  use std::cell::RefCell;
  use std::collections::HashMap;
  use std::rc::Rc;

  fn call_many(inputs: Vec<&str>) -> Vec<Expr> {
    let env = Rc::new(RefCell::new(DynamicEnv {
      data: HashMap::new(),
      parent: None,
    }));
    let mut pstore = Vec::new();

    return inputs
      .iter()
      .map(|s| {
        let ast = parser(&mut tokenize(s));
        let out = eval(Rc::new(ast), Rc::clone(&env), &mut pstore);
        return out;
      })
      .collect();
  }

  fn call_many_print(inputs: Vec<&str>) -> Vec<String> {
    return call_many(inputs)
      .iter()
      .map(|out| print_output(out))
      .collect();
  }

  fn call_eval(s: &str) -> Expr {
    let dynamic_env = Rc::new(RefCell::new(DynamicEnv {
      data: HashMap::new(),
      parent: None,
    }));
    return eval(
      Rc::new(parser(&mut tokenize(s))),
      dynamic_env,
      &mut Vec::new(),
    );
  }

  fn call_eval_print(s: &str) -> String {
    return print_output(&call_eval(s));
  }

  #[test]
  fn test_simple_builtin_calls() {
    assert_eq!(call_eval_print("(+ 1 2 3)"), "6");
    assert_eq!(call_eval_print("(- 3 2 1)"), "0");
    assert_eq!(call_eval_print("(- 3 2 1)"), "0");
    assert_eq!(call_eval_print("(= 1 1 2)"), "false");
    assert_eq!(call_eval_print("(= true true true)"), "true");
    assert_eq!(call_eval_print("(begin (+ 1 2 3) (+ 3 3 3))"), "9");

    assert_eq!(call_eval_print("(< 1 2 3)"), "true");
    assert_eq!(call_eval_print("(<= 1 2 3)"), "true");
    assert_eq!(call_eval_print("(< 1 1 3)"), "false");
    assert_eq!(call_eval_print("(<= 1 1 1)"), "true");
    assert_eq!(call_eval_print("(> 3 2 1)"), "true");
    assert_eq!(call_eval_print("(>= 3 2 1)"), "true");
    assert_eq!(call_eval_print("(> 3 3 2)"), "false");
    assert_eq!(call_eval_print("(>= 3 2 1)"), "true");
  }

  #[test]
  fn test_simple_special_forms() {
    assert_eq!(call_eval_print("(begin (def a 6) (sum a a a))"), "18");
    assert_eq!(call_eval_print("(begin (def a 6) (set! a 3) a)"), "3");
    assert_eq!(call_eval_print("(quote (sum 1 2 3))"), "(sum 1 2 3)");
    assert_eq!(
      call_eval_print("(begin (def f (lambda (a b) (+ a b))) (f 1 2))"),
      "3"
    );
    assert_eq!(call_eval_print("(begin (def a (if (= 1 2) 3 6)) a)"), "6");
  }

  /// Testcases from Norvig's lispy.py
  /// https://norvig.com/lispy2.html
  #[test]
  fn test_norvigs_lispy_suite() {
    // ("(quote (testing 1 (2.0) -3.14e159))", ['testing', 1, [2.0], -3.14e159]),
    assert_eq!(call_eval_print("(+ 2 2)"), "4");
    assert_eq!(call_eval_print("(+ (* 2 100) (* 1 10))"), "210");
    assert_eq!(call_eval_print("(if (> 6 5) (+ 1 1) (+ 2 2))"), "2");
    assert_eq!(call_eval_print("(if (< 6 5) (+ 1 1) (+ 2 2))"), "4");

    assert_eq!(
      call_many_print(
        [
          "(define x 3)",
          "x",
          "(+ x x)",
          "(begin (define x 1) (set! x (+ x 1)) (+ x 1))",
        ]
        .to_vec(),
      ),
      ["", "3", "6", "3",].to_vec()
    );

    assert_eq!(
      call_many_print(
        [
          "(define fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))",
          "(fact 3)",
          "(fact 6)",
          "(fact 9)"
        ]
        .to_vec(),
      ),
      ["", "6", "720", "362880"].to_vec()
    );

    assert_eq!(
      call_many_print(
        [
          "(define fib (lambda (n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))))",
          "(fib 1)",
          "(fib 2)",
          "(fib 3)",
          "(fib 4)",
          "(fib 5)",
          "(fib 6)",
          "(fib 10)"
        ]
        .to_vec()
      ),
      ["", "1", "2", "3", "5", "8", "13", "89"].to_vec()
    );
  }
}
