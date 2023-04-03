  $ cat > curry1.lama <<-EOF
  > fun f (x) { if 1 then x+2 else x+3 fi }
  > f (0)
  > EOF
  $ cat curry1.lama
  fun f (x) { if 1 then x+2 else x+3 fi }
  f (0)
  $ ./Driver.exe -llvmsm curry1.lama -o curry1.o
  src/Driver.ml 221
  Scope ([("f", (`Local, `Fun (["x"], Scope ([], If (Const (1), Scope ([], Binop ("+", Var ("x"), Const (2))), Scope ([], Binop ("+", Var ("x"), Const (3))))))))], Call (Var ("f"), [Const (0)]))
  fun f (x) 
  {
    if 1 then {
                x + 2 } else {
                 x + 3 } fi
  }
  f (0)
  
  [1]
  $ gcc stdlib.o output.o || echo $?
  $ ./a.out
  2

  $ cat > curry1.lama <<-EOF
  > fun fib (n) { if n<2 then 1 else fib (n-1) + fib (n-2) fi }
  > fib (5)
  > EOF
  $ cat curry1.lama
  fun fib (n) { if n<2 then 1 else fib (n-1) + fib (n-2) fi }
  fib (5)
  $ ./Driver.exe -llvmsm curry1.lama -o curry1.o
  src/Driver.ml 221
  Scope ([("fib", (`Local, `Fun (["n"], Scope ([], If (Binop ("<", Var ("n"), Const (2)), Scope ([], Const (1)), Scope ([], Binop ("+", Call (Var ("fib"), [Binop ("-", Var ("n"), Const (1))]), Call (Var ("fib"), [Binop ("-", Var ("n"), Const (2))]))))))))], Call (Var ("fib"), [Const (5)]))
  fun fib (n) 
  {
    if n < 2 then {
                 1 } else {
                 fib (n - 1) + fib (n - 2) } fi
  }
  fib (5)
  
  [1]
  $ gcc stdlib.o output.o || echo $?
  $ ./a.out
  8
