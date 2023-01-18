  $ ./Driver.exe -llvm llvm1.lama -o llvm1.o
  src/Driver.ml 221
  Scope ([("foo", (`Local, `Fun (["x"], Scope ([], Binop ("+", Var ("x"), Const (1))))))], Call (Var ("foo"), [Const (41)]))

  $ file output.o
  output.o: ELF 64-bit LSB relocatable, x86-64, version 1 (SYSV), not stripped
  $ gcc output.o
  $ ./a.out || echo "FUCK" $?
  FUCK 42
  $ echo $?
  0
