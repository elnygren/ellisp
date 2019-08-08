use regex::Regex;
use std::rc::Rc;
use crate::types::{Atom, AST};

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
  return comment_re
    .replace_all(ellisp_program, "")
    .replace("\n", space)
    .replace("\t", space)
    .replace("(", " ( ")
    .replace(")", " ) ")
    .split(space)
    .map(|s| s.to_string())
    .filter(|s| !s.is_empty())
    .collect();
}

/// parser builds an Tree tree of Atoms
/// currently our language has Symbols and Numbers defined in Atom
fn atomize(token: Token) -> Atom {
  if token == "false" {
    return Atom::Bool(false);
  }
  if token == "true" {
    return Atom::Bool(true);
  }

  let num = token.parse::<i32>();
  return match num {
    Ok(n) => Atom::Number(n),
    Err(_) => Atom::Symbol(token),
  };
}

/// build AST Tree recursively
pub fn parser(tokens: &mut Vec<Token>) -> AST {
  let token = tokens.remove(0);
  let results: AST = match token.as_str() {
    "(" => {
      let mut l = Vec::new();
      while tokens[0] != ")" {
        l.push(Rc::new(parser(tokens)));
      }
      tokens.remove(0);
      return AST {
        atom: None,
        children: Some(l),
      };
    }
    ")" => panic!("unexpected `)`"),
    _ => AST {
      atom: Some(atomize(token)),
      children: None,
    },
  };
  return results;
}
