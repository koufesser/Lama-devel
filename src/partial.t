  $ rm -fr *.o
  $ cat > curry1.lama <<-EOF
  > fun plus (a, b) { a+b }
  > fun add1 () { plus(1) }
  > add1(7)
  > EOF
  $ cat curry1.lama
  fun plus (a, b) { a+b }
  fun add1 () { plus(1) }
  add1(7)
  $ ./Driver.exe -llvm curry1.lama -o curry1.o
  src/Driver.ml 221
  Scope ([("plus", (`Local, `Fun (["a"; "b"], Scope ([], Binop ("+", Var ("a"), Var ("b")))))); ("add1", (`Local, `Fun ([], Scope ([], Call (Var ("plus"), [Const (1)])))))], Call (Var ("add1"), [Const (7)]))
  Fatal error: exception Failure("incorrect number of arguments plus")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from LLVMIR.build.codegen_expr in file "src/LLVMIR.ml", line 228, characters 13-80
  Called from LLVMIR.build.gen_func in file "src/LLVMIR.ml", line 171, characters 21-38
  Called from Stdlib__List.iter in file "list.ml", line 110, characters 12-15
  Called from LLVMIR.build in file "src/LLVMIR.ml", line 241, characters 8-207
  Called from Dune__exe__Driver.main in file "src/Driver.ml", line 222, characters 12-33
  [2]

  $ gcc lama_stdlib.c -c -o stdlib.o
  $ gcc stdlib.o output.o; echo $?
  /usr/bin/ld: cannot find output.o: No such file or directory
  collect2: error: ld returned 1 exit status
  1
  $ ./a.out
  ./a.out: not found
  [127]
