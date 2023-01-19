  $ ./Driver.exe -llvm llvm1.lama -o llvm1.o
  src/Driver.ml 221
  Scope ([("foo", (`Local, `Fun (["x"], Scope ([], Var ("x"))))); ("boo", (`Local, `Fun (["y"], Scope ([], Binop ("+", Var ("y"), Const (1))))))], Call (Var ("boo"), [Const (41)]))
  ; ModuleID = 'main'
  source_filename = "main"
  target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"

  define i32* @foo(i32* %x) {
  entry:
    ret i32* %x
  }

  define i32* @boo(i32* %y) {
  entry:
    ret i32* inttoptr (i32 add (i32 ptrtoint (i32* %y to i32), i32 1) to i32*)
  }

  define i32* @main() {
  entry:
    %calltmp = call i32* @boo(i32* inttoptr (i32 41 to i32*))
    ret i32* %calltmp
  }

  $ gcc output.o
  $ cat pizda.ll
  $ llc pizda.ll
  $ clang pizda.s; ./a.out ; echo "lli says " $?
  $ ./a.out ; echo "FUCK" $?
  FF
  $ echo $?
  0
