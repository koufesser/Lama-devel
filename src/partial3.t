  $ rm -fr *.o
  $ cat > curry1.lama <<-EOF
  > fun f (x,y) { x+y }
  > fun g () { f(5) }
  > g ()(7)
  > --fun h(xxx) { xxx(1) + xxx(71) }
  > --h(g())
  > EOF
  $ cat curry1.lama
  fun f (x,y) { x+y }
  fun g () { f(5) }
  g ()(7)
  --fun h(xxx) { xxx(1) + xxx(71) }
  --h(g())

  $ ./Driver.exe -llvm curry1.lama -o curry1.o
  src/Driver.ml 221
  Scope ([("f", (`Local, `Fun (["x"; "y"], Scope ([], Binop ("+", Var ("x"), Var ("y")))))); ("g", (`Local, `Fun ([], Scope ([], Call (Var ("f"), [Const (5)])))))], Call (Call (Var ("g"), []), [Const (7)]))
  [1]
  $ gcc -g lama_stdlib.c -c -o stdlib.o

  $ gcc -g stdlib.o output.o || echo $?
  $ ./a.out
  12
