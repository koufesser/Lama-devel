  $ ./Driver.exe -llvm llvm1.lama -o llvm1.o
  src/Driver.ml 221
  Scope ([("foo", (`Local, `Fun (["x"], Scope ([], Binop ("+", Var ("x"), Const (1)))))); ("boo", (`Local, `Fun (["y"], Scope ([], Call (Var ("foo"), [Binop ("*", Var ("y"), Var ("y"))])))))], Call (Var ("boo"), [Const (42)]))
  fun foo (x) 
  {
    x + 1
  }
  fun boo (y) 
  {
    foo (y * y)
  }
  boo (42)
  
  [1]
  $ gcc stdlib.o output.o; echo $?
  0
  $ ./a.out
  1765
