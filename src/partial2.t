$ rm -fr *.o
  $ cat > curry1.lama <<-EOF
  > fun f () { 5+15 }
  > fun g () { f }
  > g ()()
  > EOF
  $ cat curry1.lama
  fun f () { 5+15 }
  fun g () { f }
  g ()()
  $ ./Driver.exe -llvm curry1.lama -o curry1.o
  src/Driver.ml 221
  Scope ([("f", (`Local, `Fun ([], Scope ([], Binop ("+", Const (5), Const (15)))))); ("g", (`Local, `Fun ([], Scope ([], Var ("f")))))], Call (Call (Var ("g"), []), []))
  fun f () 
  {
    5 + 15
  }
  fun g () 
  {
    f
  }
  g () ()
  
  [1]
  $ gcc stdlib.o output.o || echo $?
  $ ./a.out
  20

  $ cat > curry2.lama <<-EOF
  > fun f (x) { x+15 }
  > fun g () { f }
  > g ()(1)
  > EOF
  $ cat curry2.lama
  fun f (x) { x+15 }
  fun g () { f }
  g ()(1)
  $ ./Driver.exe -llvm curry2.lama -o curry1.o
  src/Driver.ml 221
  Scope ([("f", (`Local, `Fun (["x"], Scope ([], Binop ("+", Var ("x"), Const (15)))))); ("g", (`Local, `Fun ([], Scope ([], Var ("f")))))], Call (Call (Var ("g"), []), [Const (1)]))
  fun f (x) 
  {
    x + 15
  }
  fun g () 
  {
    f
  }
  g () (1)
  
  [1]
  $ gcc stdlib.o output.o || echo $?
  $ ./a.out
  16

  $ cat > curry3.lama <<-EOF
  > fun f (x) { x+1 }
  > fun g (foo) { foo(41) }
  > g (f)
  > EOF
  $ cat curry3.lama
  fun f (x) { x+1 }
  fun g (foo) { foo(41) }
  g (f)
  $ ./Driver.exe -llvm curry3.lama -o curry1.o
  src/Driver.ml 221
  Scope ([("f", (`Local, `Fun (["x"], Scope ([], Binop ("+", Var ("x"), Const (1)))))); ("g", (`Local, `Fun (["foo"], Scope ([], Call (Var ("foo"), [Const (41)])))))], Call (Var ("g"), [Var ("f")]))
  fun f (x) 
  {
    x + 1
  }
  fun g (foo) 
  {
    foo (41)
  }
  g (f)
  
  [1]
$ gcc lama_stdlib.c -c -o stdlib.o
  $ gcc stdlib.o output.o || echo $?
  $ ./a.out
  42
