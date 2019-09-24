#!/bin/sh

cd .. && cargo build --release && cd performance

# program to profile
PROGRAM="
  (define fib (lambda (n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))))
  (fib 20)
"

# run dtrace
sudo dtrace \
  -c "../target/release/ellisp -- $PROGRAM" \
  -o out.stacks \
  -n 'profile-997 /execname == "ellisp"/ { @[ustack(100)] = count(); }'

# process dtrace with flamegraph
./vendor/FlameGraph/stackcollapse.pl ./out.stacks > out.stacks.collapsed
./vendor/FlameGraph/flamegraph.pl ./out.stacks.collapsed > out.svg

open out.svg
