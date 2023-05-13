  $ cat > curry1.lama <<-EOF
  > fun f (x) { if 1 then x+2 else x+3 fi }
  > f (0)
  > EOF
  $ cat curry1.lama
  fun f (x) { if 1 then x+2 else x+3 fi }
  f (0)
  $ ./Driver.exe -ds -llvmsm curry1.lama -o curry1.o
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
  $ gcc -fPIE stdlib.o output.o || echo $?
  $ ./a.out || echo $?
  2
