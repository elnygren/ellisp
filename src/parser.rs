use crate::types::{Atom, Keyword, AST};
use regex::Regex;
use std::convert::TryFrom;
use std::rc::Rc;

/// Token is just a string
/// ellisp program is a string that is first tokenised `String => Vec<Token>`
/// and then atomized by our parser `Token => Atom`
type Token = String;

/// we only support s-expressions and comments
/// so we can just expand spaces everywhere and split by it
pub fn tokenize(ellisp_program: &str) -> Vec<Token> {
  let space = " ";
  // TODO: this comment regexp is going to give us trouble with string literals
  let comment_re = Regex::new(r";.*\n").unwrap();
  comment_re
    .replace_all(ellisp_program, "")
    .replace("\n", space)
    .replace("\t", space)
    .replace("(", " ( ")
    .replace(")", " ) ")
    .split(space)
    .map(|s| s.to_string())
    .filter(|s| !s.is_empty())
    .collect()
}

/// parser builds an Tree tree of Atoms
/// currently our language has Symbols and Numbers defined in Atom
fn atomize(token: Token) -> Atom {
  match token.as_str() {
    "false" => Atom::Bool(false),
    "true" => Atom::Bool(true),
    _ => match token.parse::<i32>() {
      Ok(n) => Atom::Number(n),
      Err(_) => match Keyword::try_from(token.as_str()) {
        Ok(kw) => Atom::Keyword(kw),
        Err(_) => Atom::Symbol(token),
      },
    },
  }
}

/// build AST Tree recursively
pub fn parser(tokens: &mut Vec<Token>) -> AST {
  let token = tokens.remove(0);
  match token.as_str() {
    "(" => {
      let mut l = Vec::new();
      while tokens[0] != ")" {
        l.push(Rc::new(parser(tokens)));
      }
      tokens.remove(0);
      AST {
        atom: None,
        children: Some(l),
      }
    }
    ")" => panic!("unexpected `)`"),
    _ => AST {
      atom: Some(atomize(token)),
      children: None,
    },
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  static INPUT: &str = "
		; this is a program


		(hello
			(there
				(this is some)
				(code 1 2 3 4)
			)
		)";

  static EXPECTED_ARR: [&str; 18] = [
    "(", "hello", "(", "there", "(", "this", "is", "some", ")", "(", "code", "1", "2", "3", "4",
    ")", ")", ")",
  ];

  #[test]
  fn test_tokenize() {
    let result = tokenize(INPUT);
    let expected: Vec<String> = EXPECTED_ARR.iter().map(|x| x.to_string()).collect();
    assert_eq!(result, expected);
  }

  #[test]
  fn test_parser() {
    let mut tokens = tokenize(INPUT);
    let result = parser(&mut tokens);

    #[rustfmt::skip]
		let expected =
			AST { atom: None, children: Some([
				Rc::new(AST { atom: Some(Atom::Symbol("hello".to_string())), children: None }),
				Rc::new(AST { atom: None, children: Some([
					Rc::new(AST { atom: Some(Atom::Symbol("there".to_string())), children: None }),
					Rc::new(AST { atom: None, children: Some([
						Rc::new(AST { atom: Some(Atom::Symbol("this".to_string())), children: None }),
						Rc::new(AST { atom: Some(Atom::Symbol("is".to_string())), children: None }),
						Rc::new(AST { atom: Some(Atom::Symbol("some".to_string())), children: None }),
					].to_vec()) }),
					Rc::new(AST { atom: None, children: Some([
						Rc::new(AST { atom: Some(Atom::Symbol("code".to_string())), children: None }),
						Rc::new(AST { atom: Some(Atom::Number(1)), children: None }),
						Rc::new(AST { atom: Some(Atom::Number(2)), children: None }),
						Rc::new(AST { atom: Some(Atom::Number(3)), children: None }),
						Rc::new(AST { atom: Some(Atom::Number(4)), children: None }),
					].to_vec()) })
				].to_vec()) })
			].to_vec()) };

    assert_eq!(result, expected);
  }
}
