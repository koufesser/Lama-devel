$ rm -fr *.o
  $ cat > curry10.lama <<-EOF
  > fun f (x,y) { x+y/x }
  > --fun g () { f(5) }
  > --g ()(7)
  > fun h(yyy) { yyy(15) + yyy(30) }
  > h(f(5))
  > EOF
  $ cat curry10.lama
  fun f (x,y) { x+y/x }
  --fun g () { f(5) }
  --g ()(7)
  fun h(yyy) { yyy(15) + yyy(30) }
  h(f(5))

  $ rm -fr output.o
  $ ./Driver.exe -llvm curry10.lama
  src/Driver.ml 221
  Scope ([("f", (`Local, `Fun (["x"; "y"], Scope ([], Binop ("+", Var ("x"), Binop ("/", Var ("y"), Var ("x"))))))); ("h", (`Local, `Fun (["yyy"], Scope ([], Binop ("+", Call (Var ("yyy"), [Const (15)]), Call (Var ("yyy"), [Const (30)]))))))], Call (Var ("h"), [Call (Var ("f"), [Const (5)])]))
  fun f (x,y) 
  {
    x + y / x
  }
  fun h (yyy) 
  {
    yyy (15) + yyy (30)
  }
  h (f (5))
  
  [1]

  $ gcc -g stdlib.o output.o || echo $?
  $ ./a.out
  19
