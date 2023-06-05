; ModuleID = 'main'
source_filename = "main"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

@s = global i64 0
@i = global i64 0
@"31" = private unnamed_addr constant [11 x i8] c"result: %d\00", align 1

define i64 @main(i64 %0, i64 %1) {
entry:
  %"8" = alloca i64, align 8
  %"1" = alloca i64, align 8
  store i64 0, i64* %"1", align 8
  %"2" = load i64, i64* %"1", align 8
  store i64 %"2", i64* @s, align 8
  store i64 0, i64* %"1", align 8
  %"4" = load i64, i64* %"1", align 8
  store i64 %"4", i64* @i, align 8
  br label %L14

L13:                                              ; preds = %L14
  %"6" = load i64, i64* @s, align 8
  store i64 %"6", i64* %"1", align 8
  %"7" = load i64, i64* @i, align 8
  store i64 %"7", i64* %"8", align 8
  %"9" = load i64, i64* %"8", align 8
  %"10" = load i64, i64* %"1", align 8
  %"11" = add i64 %"10", %"9"
  store i64 %"11", i64* %"1", align 8
  %"13" = load i64, i64* %"1", align 8
  store i64 %"13", i64* @s, align 8
  %"15" = load i64, i64* @i, align 8
  store i64 %"15", i64* %"1", align 8
  store i64 1, i64* %"8", align 8
  %"16" = load i64, i64* %"8", align 8
  %"17" = load i64, i64* %"1", align 8
  %"18" = add i64 %"17", %"16"
  store i64 %"18", i64* %"1", align 8
  %"20" = load i64, i64* %"1", align 8
  store i64 %"20", i64* @i, align 8
  br label %L14

L14:                                              ; preds = %L13, %entry
  %"22" = load i64, i64* @i, align 8
  store i64 %"22", i64* %"1", align 8
  store i64 10, i64* %"8", align 8
  %"23" = load i64, i64* %"8", align 8
  %"24" = load i64, i64* %"1", align 8
  %"25" = icmp slt i64 %"24", %"23"
  %"26" = sext i1 %"25" to i64
  store i64 %"26", i64* %"1", align 8
  %"28" = load i64, i64* %"1", align 8
  %"29" = icmp ne i64 %"28", 0
  br i1 %"29", label %L13, label %"27_fnb"

"27_fnb":                                         ; preds = %L14
  %"30_start_of_string_allocation" = alloca i64, align 8
  %"32_string_result: %d" = alloca i64, i64 13, align 8
  %"331" = bitcast i64* %"32_string_result: %d" to i64*
  %"34" = getelementptr i64, i64* %"32_string_result: %d", i64 1
  store i64 3, i64* %"331", align 8
  store i64 11, i64* %"34", align 8
  %"35" = getelementptr i64, i64* %"32_string_result: %d", i64 2
  %"36" = bitcast i64* %"35" to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %"36", i8* getelementptr inbounds ([11 x i8], [11 x i8]* @"31", i32 0, i32 0), i64 11, i1 false)
  %"37_end_of_string_allocation" = alloca i64, align 8
  %"38" = ptrtoint i8* %"36" to i64
  store i64 %"38", i64* %"1", align 8
  %"39" = load i64, i64* @s, align 8
  store i64 %"39", i64* %"8", align 8
  %"40" = load i64, i64* %"8", align 8
  %"41" = load i64, i64* %"1", align 8
  %"42" = inttoptr i64 %"41" to i8*
  %"43" = inttoptr i64 %"40" to i8*
  %"44" = call i64 (i8*, ...) @printf(i8* %"42", i8* %"43")
  store i64 %"44", i64* %"1", align 8
  %"45" = load i64, i64* %"1", align 8
  ret i64 %"45"
}

; Function Attrs: argmemonly nofree nounwind willreturn
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* noalias nocapture writeonly, i8* noalias nocapture readonly, i64, i1 immarg) #0

declare i64 @printf(i8*, ...)

attributes #0 = { argmemonly nofree nounwind willreturn }
