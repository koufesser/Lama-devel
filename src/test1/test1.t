  $ ./hack1test.exe
  adding 'i32 52' and 'i32 ptrtoint (i32* %x to i32)'
   creating binop src/test1/hack1test.ml 63
  ; ModuleID = 'main'
  source_filename = "main"
  target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
  
  define i32* @main(i32* %x) {
  entry:
    ret i32* inttoptr (i32 add (i32 ptrtoint (i32* %x to i32), i32 52) to i32*)
  }
  define i32* @main(i32* %x) {
  entry:
    ret i32* inttoptr (i32 add (i32 ptrtoint (i32* %x to i32), i32 52) to i32*)
  }
