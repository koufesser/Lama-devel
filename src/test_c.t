  $ cat > c1.c << EOF
  > #include <stdint.h>
  > void hack(int64_t *x) { *x = 42; }
  > int64_t* fuck (int64_t  addr) { return (int64_t*)addr; }
  > int64_t* fuck1(int64_t *addr) { return addr; }
  > int64_t  fuck2(int64_t *addr) { return *addr; }
  $ clang -Wno-return-stack-address -emit-llvm -S c1.c -o -
  ; ModuleID = 'c1.c'
  source_filename = "c1.c"
  target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
  target triple = "x86_64-pc-linux-gnu"

  ; Function Attrs: noinline nounwind optnone uwtable
  define dso_local void @hack(ptr noundef %0) #0 {
    %2 = alloca ptr, align 8
    store ptr %0, ptr %2, align 8
    %3 = load ptr, ptr %2, align 8
    store i64 42, ptr %3, align 8
    ret void
  }

  ; Function Attrs: noinline nounwind optnone uwtable
  define dso_local ptr @fuck(i64 noundef %0) #0 {
    %2 = alloca i64, align 8
    store i64 %0, ptr %2, align 8
    %3 = load i64, ptr %2, align 8
    %4 = inttoptr i64 %3 to ptr
    ret ptr %4
  }

  ; Function Attrs: noinline nounwind optnone uwtable
  define dso_local ptr @fuck1(ptr noundef %0) #0 {
    %2 = alloca ptr, align 8
    store ptr %0, ptr %2, align 8
    %3 = load ptr, ptr %2, align 8
    ret ptr %3
  }

  ; Function Attrs: noinline nounwind optnone uwtable
  define dso_local i64 @fuck2(ptr noundef %0) #0 {
    %2 = alloca ptr, align 8
    store ptr %0, ptr %2, align 8
    %3 = load ptr, ptr %2, align 8
    %4 = load i64, ptr %3, align 8
    ret i64 %4
  }

  attributes #0 = { noinline nounwind optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

  !llvm.module.flags = !{!0, !1, !2, !3, !4}
  !llvm.ident = !{!5}

  !0 = !{i32 1, !"wchar_size", i32 4}
  !1 = !{i32 7, !"PIC Level", i32 2}
  !2 = !{i32 7, !"PIE Level", i32 2}
  !3 = !{i32 7, !"uwtable", i32 2}
  !4 = !{i32 7, !"frame-pointer", i32 2}
  !5 = !{!"Ubuntu clang version 15.0.2-1"}
