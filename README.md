# ellisp

ellisp is a lisp parser &amp; interpreter made with Rust for fun.

## How does it work?

1. Program code (string) is tokenised `fn tokenize(p: &str) -> Vec<Token>`
 * just some simple string processing
2. Tokens are parsed into the AST `fn parser(tokens: &mut Vec<Token>) -> AST`
 * recursive algorithm that checks for `(` and `)`, it's a LISP afterall :-)
3. AST is evaluated `fn eval(ast: &AST, denv: &mut Box<DynamicEnv>, pstore: &mut LambdaContextStore) -> Expr`
 * DynamicEnv contains all runtime definitions like `(def a 5)`. Each procedure (lambda) has it's own environment that extends the top-level env
 * LambdaContextStore contains data required to execute a procedure (lambda)

### REPL example

```
> git clone ...
> cargo run

ellisp 0.1 REPL


> (def a 666)
Nop
> (def b (lambda (x) (+ x a)))
Nop
> (b 42)
Number(708)
> (def fib (lambda (n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))))
Nop
> (fib 4)
Number(3)
> (fib 10)
Number(55)
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
