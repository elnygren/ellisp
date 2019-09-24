# ellisp

ellisp is a lisp parser &amp; interpreter made with Rust for fun.

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

ellisp provides a standard library. User's can extend and/or override these with `def` and `set!`.

Note: some functions take N arguments where it makes sense (as in Clojure).

```
+   (+ 1 2 3 4)
-   (- 1 2 3 4)
=   (= true false true)
<   (< 1 2 3 4) ; => true
do  (do <expr1> <expr2> ... <final-expr>) ; => returns result of final expr
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

### Programmatic usage example
```rust
let mut dynamic_env = Box::new(DynamicEnv {
  env: HashMap::new(),
  parent: None,
});
let mut pstore: LambdaContextStore = Vec::new();

let program = "
  ; this is a simple program:
  (do
    (def a (lambda (x) (+ x 40)))
    (def b (lambda (x) (+ 0 2)))
    (def c (24))
    (def d -24)
    (+ (a 2) (b 666) c d)
  )
";
let mut tokens = tokenize(program);
// println!("Tokens are: {:?}", tokens);
let ast = parser(&mut tokens);
// println!("AST: {:?}", ast);
let output = eval(&ast, &mut dynamic_env, &mut pstore);
println!("Program: {}", program);
println!("=> {:?}", output)
```

## How does it work?

1. Program code (string) is tokenised `fn tokenize(p: &str) -> Vec<Token>`
 * just some simple string processing
2. Tokens are parsed into the AST `fn parser(tokens: &mut Vec<Token>) -> AST`
 * recursive algorithm that checks for `(` and `)`, it's a LISP afterall :-)
3. AST is evaluated `fn eval(ast: &AST, denv: &mut Box<DynamicEnv>, pstore: &mut LambdaContextStore) -> Expr`
 * DynamicEnv contains all runtime definitions like `(def a 5)`. Each procedure (lambda) has it's own environment that extends the top-level env
 * LambdaContextStore contains data required to execute a procedure (lambda)
