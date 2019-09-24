# ellisp

ellisp is a lisp parser &amp; interpreter made with Rust for fun.

## Usage

* running the repl

```bash
cargo run
```

* release build, running the binary from cli

```bash
cargo build --release
./target/release/ellisp -- "(+ 1 2 3)"
```

### REPL example

```bash
> cargo run

ellisp 0.1 REPL
>>> (def a 666)
>>> (def b (lambda (x) (+ x a)))
>>> (b 42)
708
>>> (def fib (lambda (n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))))
>>> (fib 4)
5
>>> (fib 10)
89
```

## Language

The language is a Lisp with s-expressions that must follow our defined *special forms* or *procedure calls*.
An s-expression starting with a keyword is a *special form*, other lists are *procedure calls* (function calls).
ellisp also supports booleans (`true` and `false`) and numbers (integers).

### Syntactic forms

Special syntactic forms start with a keyword, everything else is a procedure call.

```
"def"     (def a <expr>)              (def a 42)
"if"      (if <expr> <then> <else>)   (if (= a 5) 1 0)
"set!"    (set! a <expr>)             (set! a 6)
"quote"   (quote <expr>)              (quote (hello darkness (my old) friend))
"lambda"  (lambda (a b) <expr>)       (def fn (lambda (a, b) (+ a b)))

; oh and this would be a comment, it's a total regexp hack in the tokenizer
```

### Procedures

ellisp provides a small standard library. Users can create their own procedures with `(def name (lambda ...))`.
Some functions take N arguments where it makes sense (as in Clojure).

Some examples of what's built in:

```
; basic math
sum, +, -, *, /, 
=, <, <=, >, >=

; lispy things
begin, do, null?, 
list, cons, car, cdr, 
append, length, len
```

## How does it work?

1. Program code (string) is tokenised `fn tokenize(p: &str) -> Vec<Token>`
 * just some simple string processing
2. Tokens are parsed into the AST `fn parser(tokens: &mut Vec<Token>) -> AST`
 * recursive algorithm that checks for `(` and `)`, it's a LISP afterall :-)
3. AST is evaluated `fn eval(ast: &AST, denv: &mut Box<DynamicEnv>, pstore: &mut LambdaContextStore) -> Expr`
 * DynamicEnv contains all runtime definitions like `(def a 5)`. Each procedure (lambda) has it's own environment that extends the top-level env
 * LambdaContextStore contains data required to execute a procedure (lambda)
 

If you want to see the code, start with `main.rs` -> `fn main()`. From there you can follow how the input program goes through tokenization, parsing and finally eval.

## TODO

* macro system
  * prebuilt macros like `defn` (`define` + `lambda`)
* performance improvements
  * `DynamicEnv` find has to go N hops up the tree when there are many nested closures or in certain recursion scenarios.
    Perhaps a better data structure is needed here...
