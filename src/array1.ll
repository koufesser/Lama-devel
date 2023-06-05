; ModuleID = 'main'
source_filename = "main"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

@"6" = private unnamed_addr constant [2 x i8] c"E\00", align 1
@"40" = private unnamed_addr constant [2 x i8] c"A\00", align 1
@"62" = private unnamed_addr constant [20 x i8] c"Nope, this is A.\\\\n\00", align 1
@"94" = private unnamed_addr constant [3 x i8] c"AB\00", align 1
@"116" = private unnamed_addr constant [21 x i8] c"Nope, this is AB.\\\\n\00", align 1
@"148" = private unnamed_addr constant [3 x i8] c"AB\00", align 1
@"185" = private unnamed_addr constant [17 x i8] c"Nope, this is AB\00", align 1
@"216" = private unnamed_addr constant [2 x i8] c"E\00", align 1
@"238" = private unnamed_addr constant [19 x i8] c"Yes, this is E.\\\\n\00", align 1
@"249" = private unnamed_addr constant [15 x i8] c"program failed\00", align 1

define i64 @main(i64 %0, i64 %1) {
entry:
  %"183" = alloca i64, align 8
  %"170" = alloca i64, align 8
  %"143" = alloca i64, align 8
  %"89" = alloca i64, align 8
  %"35" = alloca i64, align 8
  %"20" = alloca i64, align 8
  %"18" = alloca i64, align 8
  %"16" = alloca i64, align 8
  %"1_array" = alloca i64, i64 3, align 8
  %"21" = bitcast i64* %"1_array" to i64*
  %"3" = getelementptr i64, i64* %"1_array", i64 1
  %"4" = getelementptr i64, i64* %"1_array", i64 2
  %"5_start_of_string_allocation" = alloca i64, align 8
  %"7_string_E" = alloca i64, i64 4, align 8
  %"86" = bitcast i64* %"7_string_E" to i64*
  %"9" = getelementptr i64, i64* %"7_string_E", i64 1
  store i64 3, i64* %"86", align 8
  store i64 2, i64* %"9", align 8
  %"10" = getelementptr i64, i64* %"7_string_E", i64 2
  %"11" = bitcast i64* %"10" to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %"11", i8* getelementptr inbounds ([2 x i8], [2 x i8]* @"6", i32 0, i32 0), i64 2, i1 false)
  %"12_end_of_string_allocation" = alloca i64, align 8
  %"13" = ptrtoint i8* %"11" to i64
  store i64 %"13", i64* %"21", align 8
  store i64 2, i64* %"3", align 8
  store i64 0, i64* %"4", align 8
  %"14" = getelementptr i64, i64* %"1_array", i64 3
  %"15sexp_created" = ptrtoint i64* %"14" to i64
  store i64 %"15sexp_created", i64* %"16", align 8
  %"17" = load i64, i64* %"16", align 8
  store i64 %"17", i64* %"18", align 8
  %"19" = load i64, i64* %"18", align 8
  store i64 %"19", i64* %"20", align 8
  %"21_tag_start_A" = alloca i64, align 8
  %"22_tag_comp" = alloca i64, i64 2, align 8
  %"237" = bitcast i64* %"22_tag_comp" to i64*
  %"24" = getelementptr i64, i64* %"22_tag_comp", i64 1
  store i64 2, i64* %"237", align 8
  store i64 0, i64* %"24", align 8
  %"25" = load i64, i64* %"20", align 8
  %"26_sexp" = inttoptr i64 %"25" to i64*
  %"27" = getelementptr i64, i64* %"26_sexp", i64 -2
  %"28" = bitcast i64* %"27" to i8*
  %"29" = bitcast i64* %"22_tag_comp" to i8*
  %"30" = call i64 @memcmp(i8* %"28", i8* %"29", i64 16)
  %"31" = icmp ne i64 %"30", 0
  br i1 %"31", label %"34_tagblock_1", label %"33_tagblock_2_1"

"34_tagblock_1":                                  ; preds = %"33_tagblock_2_1", %entry
  store i64 0, i64* %"35", align 8
  br label %"32_tagblock_3"

"33_tagblock_2_1":                                ; preds = %entry
  %"36" = getelementptr i64, i64* %"27", i64 -1
  %"37_str_st" = load i64, i64* %"36", align 8
  %"38" = inttoptr i64 %"37_str_st" to i8*
  %"39_start_of_string_allocation" = alloca i64, align 8
  %"41_string_A" = alloca i64, i64 4, align 8
  %"428" = bitcast i64* %"41_string_A" to i64*
  %"43" = getelementptr i64, i64* %"41_string_A", i64 1
  store i64 3, i64* %"428", align 8
  store i64 2, i64* %"43", align 8
  %"44" = getelementptr i64, i64* %"41_string_A", i64 2
  %"45" = bitcast i64* %"44" to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %"45", i8* getelementptr inbounds ([2 x i8], [2 x i8]* @"40", i32 0, i32 0), i64 2, i1 false)
  %"46_end_of_string_allocation" = alloca i64, align 8
  %"47" = bitcast i8* %"38" to i64*
  %"48" = getelementptr i64, i64* %"47", i64 -1
  %"49" = load i64, i64* %"48", align 8
  %"50" = icmp ne i64 %"49", 2
  br i1 %"50", label %"34_tagblock_1", label %"51_tagblock_2_2"

"51_tagblock_2_2":                                ; preds = %"33_tagblock_2_1"
  %2 = bitcast i64* %"44" to i8*
  %3 = inttoptr i64 %"37_str_st" to i8*
  %"54" = call i64 @memcmp(i8* %3, i8* %2, i64 2)
  %"55" = icmp eq i64 %"54", 0
  %"56" = sext i1 %"55" to i64
  store i64 %"56", i64* %"35", align 8
  %"57_tag_end" = alloca i64, align 8
  br label %"32_tagblock_3"

"32_tagblock_3":                                  ; preds = %"51_tagblock_2_2", %"34_tagblock_1"
  %"59" = load i64, i64* %"35", align 8
  %"60" = icmp ne i64 %"59", 0
  br i1 %"60", label %L7, label %"58_cjmp"

"58_cjmp":                                        ; preds = %"32_tagblock_3"
  %"73" = load i64, i64* %"18", align 8
  store i64 %"73", i64* %"20", align 8
  %"74" = load i64, i64* %"20", align 8
  store i64 %"74", i64* %"35", align 8
  %"75_tag_start_AB" = alloca i64, align 8
  %"76_tag_comp" = alloca i64, i64 2, align 8
  %"779" = bitcast i64* %"76_tag_comp" to i64*
  %"78" = getelementptr i64, i64* %"76_tag_comp", i64 1
  store i64 2, i64* %"779", align 8
  store i64 0, i64* %"78", align 8
  %"79" = load i64, i64* %"35", align 8
  %"80_sexp" = inttoptr i64 %"79" to i64*
  %"81" = getelementptr i64, i64* %"80_sexp", i64 -2
  %"82" = bitcast i64* %"81" to i8*
  %"83" = bitcast i64* %"76_tag_comp" to i8*
  %"84" = call i64 @memcmp(i8* %"82", i8* %"83", i64 16)
  %"85" = icmp ne i64 %"84", 0
  br i1 %"85", label %"88_tagblock_1", label %"87_tagblock_2_1"

L7:                                               ; preds = %"32_tagblock_3"
  %"61_start_of_string_allocation" = alloca i64, align 8
  %"63_string_Nope, this is A.\\\\n" = alloca i64, i64 22, align 8
  %"6410" = bitcast i64* %"63_string_Nope, this is A.\\\\n" to i64*
  %"65" = getelementptr i64, i64* %"63_string_Nope, this is A.\\\\n", i64 1
  store i64 3, i64* %"6410", align 8
  store i64 20, i64* %"65", align 8
  %"66" = getelementptr i64, i64* %"63_string_Nope, this is A.\\\\n", i64 2
  %"67" = bitcast i64* %"66" to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %"67", i8* getelementptr inbounds ([20 x i8], [20 x i8]* @"62", i32 0, i32 0), i64 20, i1 false)
  %"68_end_of_string_allocation" = alloca i64, align 8
  %"69" = ptrtoint i8* %"67" to i64
  store i64 %"69", i64* %"18", align 8
  %"70" = load i64, i64* %"18", align 8
  %"71" = inttoptr i64 %"70" to i8*
  %"72" = call i64 (i8*, ...) @printf(i8* %"71")
  store i64 %"72", i64* %"18", align 8
  br label %L0

"88_tagblock_1":                                  ; preds = %"87_tagblock_2_1", %"58_cjmp"
  store i64 0, i64* %"89", align 8
  br label %"86_tagblock_3"

"87_tagblock_2_1":                                ; preds = %"58_cjmp"
  %"90" = getelementptr i64, i64* %"81", i64 -1
  %"91_str_st" = load i64, i64* %"90", align 8
  %"92" = inttoptr i64 %"91_str_st" to i8*
  %"93_start_of_string_allocation" = alloca i64, align 8
  %"95_string_AB" = alloca i64, i64 5, align 8
  %"9611" = bitcast i64* %"95_string_AB" to i64*
  %"97" = getelementptr i64, i64* %"95_string_AB", i64 1
  store i64 3, i64* %"9611", align 8
  store i64 3, i64* %"97", align 8
  %"98" = getelementptr i64, i64* %"95_string_AB", i64 2
  %"99" = bitcast i64* %"98" to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %"99", i8* getelementptr inbounds ([3 x i8], [3 x i8]* @"94", i32 0, i32 0), i64 3, i1 false)
  %"100_end_of_string_allocation" = alloca i64, align 8
  %"101" = bitcast i8* %"92" to i64*
  %"102" = getelementptr i64, i64* %"101", i64 -1
  %"103" = load i64, i64* %"102", align 8
  %"104" = icmp ne i64 %"103", 3
  br i1 %"104", label %"88_tagblock_1", label %"105_tagblock_2_2"

"105_tagblock_2_2":                               ; preds = %"87_tagblock_2_1"
  %4 = bitcast i64* %"98" to i8*
  %5 = inttoptr i64 %"91_str_st" to i8*
  %"108" = call i64 @memcmp(i8* %5, i8* %4, i64 3)
  %"109" = icmp eq i64 %"108", 0
  %"110" = sext i1 %"109" to i64
  store i64 %"110", i64* %"89", align 8
  %"111_tag_end" = alloca i64, align 8
  br label %"86_tagblock_3"

"86_tagblock_3":                                  ; preds = %"105_tagblock_2_2", %"88_tagblock_1"
  %"113" = load i64, i64* %"89", align 8
  %"114" = icmp ne i64 %"113", 0
  br i1 %"114", label %L15, label %"112_cjmp"

"112_cjmp":                                       ; preds = %"86_tagblock_3"
  %"127" = load i64, i64* %"20", align 8
  store i64 %"127", i64* %"35", align 8
  %"128" = load i64, i64* %"35", align 8
  store i64 %"128", i64* %"89", align 8
  %"129_tag_start_AB" = alloca i64, align 8
  %"130_tag_comp" = alloca i64, i64 2, align 8
  %"13112" = bitcast i64* %"130_tag_comp" to i64*
  %"132" = getelementptr i64, i64* %"130_tag_comp", i64 1
  store i64 2, i64* %"13112", align 8
  store i64 1, i64* %"132", align 8
  %"133" = load i64, i64* %"89", align 8
  %"134_sexp" = inttoptr i64 %"133" to i64*
  %"135" = getelementptr i64, i64* %"134_sexp", i64 -2
  %"136" = bitcast i64* %"135" to i8*
  %"137" = bitcast i64* %"130_tag_comp" to i8*
  %"138" = call i64 @memcmp(i8* %"136", i8* %"137", i64 16)
  %"139" = icmp ne i64 %"138", 0
  br i1 %"139", label %"142_tagblock_1", label %"141_tagblock_2_1"

L15:                                              ; preds = %"86_tagblock_3"
  %"115_start_of_string_allocation" = alloca i64, align 8
  %"117_string_Nope, this is AB.\\\\n" = alloca i64, i64 23, align 8
  %"11813" = bitcast i64* %"117_string_Nope, this is AB.\\\\n" to i64*
  %"119" = getelementptr i64, i64* %"117_string_Nope, this is AB.\\\\n", i64 1
  store i64 3, i64* %"11813", align 8
  store i64 21, i64* %"119", align 8
  %"120" = getelementptr i64, i64* %"117_string_Nope, this is AB.\\\\n", i64 2
  %"121" = bitcast i64* %"120" to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %"121", i8* getelementptr inbounds ([21 x i8], [21 x i8]* @"116", i32 0, i32 0), i64 21, i1 false)
  %"122_end_of_string_allocation" = alloca i64, align 8
  %"123" = ptrtoint i8* %"121" to i64
  store i64 %"123", i64* %"20", align 8
  %"124" = load i64, i64* %"20", align 8
  %"125" = inttoptr i64 %"124" to i8*
  %"126" = call i64 (i8*, ...) @printf(i8* %"125")
  store i64 %"126", i64* %"20", align 8
  br label %L0

"142_tagblock_1":                                 ; preds = %"141_tagblock_2_1", %"112_cjmp"
  store i64 0, i64* %"143", align 8
  br label %"140_tagblock_3"

"141_tagblock_2_1":                               ; preds = %"112_cjmp"
  %"144" = getelementptr i64, i64* %"135", i64 -1
  %"145_str_st" = load i64, i64* %"144", align 8
  %"146" = inttoptr i64 %"145_str_st" to i8*
  %"147_start_of_string_allocation" = alloca i64, align 8
  %"149_string_AB" = alloca i64, i64 5, align 8
  %"15014" = bitcast i64* %"149_string_AB" to i64*
  %"151" = getelementptr i64, i64* %"149_string_AB", i64 1
  store i64 3, i64* %"15014", align 8
  store i64 3, i64* %"151", align 8
  %"152" = getelementptr i64, i64* %"149_string_AB", i64 2
  %"153" = bitcast i64* %"152" to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %"153", i8* getelementptr inbounds ([3 x i8], [3 x i8]* @"148", i32 0, i32 0), i64 3, i1 false)
  %"154_end_of_string_allocation" = alloca i64, align 8
  %"155" = bitcast i8* %"146" to i64*
  %"156" = getelementptr i64, i64* %"155", i64 -1
  %"157" = load i64, i64* %"156", align 8
  %"158" = icmp ne i64 %"157", 3
  br i1 %"158", label %"142_tagblock_1", label %"159_tagblock_2_2"

"159_tagblock_2_2":                               ; preds = %"141_tagblock_2_1"
  %6 = bitcast i64* %"152" to i8*
  %7 = inttoptr i64 %"145_str_st" to i8*
  %"162" = call i64 @memcmp(i8* %7, i8* %6, i64 3)
  %"163" = icmp eq i64 %"162", 0
  %"164" = sext i1 %"163" to i64
  store i64 %"164", i64* %"143", align 8
  %"165_tag_end" = alloca i64, align 8
  br label %"140_tagblock_3"

"140_tagblock_3":                                 ; preds = %"159_tagblock_2_2", %"142_tagblock_1"
  %"167" = load i64, i64* %"143", align 8
  %"168" = icmp ne i64 %"167", 0
  br i1 %"168", label %L23, label %"166_cjmp"

"166_cjmp":                                       ; preds = %"140_tagblock_3"
  %"196" = load i64, i64* %"35", align 8
  store i64 %"196", i64* %"89", align 8
  %"197" = load i64, i64* %"89", align 8
  store i64 %"197", i64* %"143", align 8
  %"198_tag_start_E" = alloca i64, align 8
  %"199_tag_comp" = alloca i64, i64 2, align 8
  %"20015" = bitcast i64* %"199_tag_comp" to i64*
  %"201" = getelementptr i64, i64* %"199_tag_comp", i64 1
  store i64 2, i64* %"20015", align 8
  store i64 0, i64* %"201", align 8
  %"202" = load i64, i64* %"143", align 8
  %"203_sexp" = inttoptr i64 %"202" to i64*
  %"204" = getelementptr i64, i64* %"203_sexp", i64 -2
  %"205" = bitcast i64* %"204" to i8*
  %"206" = bitcast i64* %"199_tag_comp" to i8*
  %"207" = call i64 @memcmp(i8* %"205", i8* %"206", i64 16)
  %"208" = icmp ne i64 %"207", 0
  br i1 %"208", label %"211_tagblock_1", label %"210_tagblock_2_1"

L23:                                              ; preds = %"140_tagblock_3"
  %"169" = load i64, i64* %"89", align 8
  store i64 %"169", i64* %"143", align 8
  store i64 0, i64* %"170", align 8
  %"171" = load i64, i64* %"170", align 8
  %"172" = load i64, i64* %"143", align 8
  %"173" = inttoptr i64 %"172" to i64*
  %"174" = getelementptr i64, i64* %"173", i64 %"171"
  %"175" = load i64, i64* %"174", align 8
  store i64 %"175", i64* %"143", align 8
  %"176" = load i64, i64* %"35", align 8
  store i64 %"176", i64* %"89", align 8
  store i64 0, i64* %"143", align 8
  %"177" = load i64, i64* %"143", align 8
  %"178" = load i64, i64* %"89", align 8
  %"179" = inttoptr i64 %"178" to i64*
  %"180" = getelementptr i64, i64* %"179", i64 %"177"
  %"181" = load i64, i64* %"180", align 8
  store i64 %"181", i64* %"89", align 8
  %"182" = load i64, i64* %"89", align 8
  store i64 %"182", i64* %"183", align 8
  %"184_start_of_string_allocation" = alloca i64, align 8
  %"186_string_Nope, this is AB" = alloca i64, i64 19, align 8
  %"18716" = bitcast i64* %"186_string_Nope, this is AB" to i64*
  %"188" = getelementptr i64, i64* %"186_string_Nope, this is AB", i64 1
  store i64 3, i64* %"18716", align 8
  store i64 17, i64* %"188", align 8
  %"189" = getelementptr i64, i64* %"186_string_Nope, this is AB", i64 2
  %"190" = bitcast i64* %"189" to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %"190", i8* getelementptr inbounds ([17 x i8], [17 x i8]* @"185", i32 0, i32 0), i64 17, i1 false)
  %"191_end_of_string_allocation" = alloca i64, align 8
  %"192" = ptrtoint i8* %"190" to i64
  store i64 %"192", i64* %"35", align 8
  %"193" = load i64, i64* %"35", align 8
  %"194" = inttoptr i64 %"193" to i8*
  %"195" = call i64 (i8*, ...) @printf(i8* %"194")
  store i64 %"195", i64* %"35", align 8
  br label %L0

"211_tagblock_1":                                 ; preds = %"210_tagblock_2_1", %"166_cjmp"
  store i64 0, i64* %"170", align 8
  br label %"209_tagblock_3"

"210_tagblock_2_1":                               ; preds = %"166_cjmp"
  %"212" = getelementptr i64, i64* %"204", i64 -1
  %"213_str_st" = load i64, i64* %"212", align 8
  %"214" = inttoptr i64 %"213_str_st" to i8*
  %"215_start_of_string_allocation" = alloca i64, align 8
  %"217_string_E" = alloca i64, i64 4, align 8
  %"21817" = bitcast i64* %"217_string_E" to i64*
  %"219" = getelementptr i64, i64* %"217_string_E", i64 1
  store i64 3, i64* %"21817", align 8
  store i64 2, i64* %"219", align 8
  %"220" = getelementptr i64, i64* %"217_string_E", i64 2
  %"221" = bitcast i64* %"220" to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %"221", i8* getelementptr inbounds ([2 x i8], [2 x i8]* @"216", i32 0, i32 0), i64 2, i1 false)
  %"222_end_of_string_allocation" = alloca i64, align 8
  %"223" = bitcast i8* %"214" to i64*
  %"224" = getelementptr i64, i64* %"223", i64 -1
  %"225" = load i64, i64* %"224", align 8
  %"226" = icmp ne i64 %"225", 2
  br i1 %"226", label %"211_tagblock_1", label %"227_tagblock_2_2"

"227_tagblock_2_2":                               ; preds = %"210_tagblock_2_1"
  %8 = bitcast i64* %"220" to i8*
  %9 = inttoptr i64 %"213_str_st" to i8*
  %"230" = call i64 @memcmp(i8* %9, i8* %8, i64 2)
  %"231" = icmp eq i64 %"230", 0
  %"232" = sext i1 %"231" to i64
  store i64 %"232", i64* %"170", align 8
  %"233_tag_end" = alloca i64, align 8
  br label %"209_tagblock_3"

"209_tagblock_3":                                 ; preds = %"227_tagblock_2_2", %"211_tagblock_1"
  %"235" = load i64, i64* %"170", align 8
  %"236" = icmp ne i64 %"235", 0
  br i1 %"236", label %L30, label %"234_cjmp"

"234_cjmp":                                       ; preds = %"209_tagblock_3"
  %"250" = call i64 (i8*, ...) @printf(i8* getelementptr inbounds ([15 x i8], [15 x i8]* @"249", i32 0, i32 0))
  br label %L0

L30:                                              ; preds = %"209_tagblock_3"
  %"237_start_of_string_allocation" = alloca i64, align 8
  %"239_string_Yes, this is E.\\\\n" = alloca i64, i64 21, align 8
  %"24018" = bitcast i64* %"239_string_Yes, this is E.\\\\n" to i64*
  %"241" = getelementptr i64, i64* %"239_string_Yes, this is E.\\\\n", i64 1
  store i64 3, i64* %"24018", align 8
  store i64 19, i64* %"241", align 8
  %"242" = getelementptr i64, i64* %"239_string_Yes, this is E.\\\\n", i64 2
  %"243" = bitcast i64* %"242" to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %"243", i8* getelementptr inbounds ([19 x i8], [19 x i8]* @"238", i32 0, i32 0), i64 19, i1 false)
  %"244_end_of_string_allocation" = alloca i64, align 8
  %"245" = ptrtoint i8* %"243" to i64
  store i64 %"245", i64* %"89", align 8
  %"246" = load i64, i64* %"89", align 8
  %"247" = inttoptr i64 %"246" to i8*
  %"248" = call i64 (i8*, ...) @printf(i8* %"247")
  store i64 %"248", i64* %"89", align 8
  br label %L0

L0:                                               ; preds = %"234_cjmp", %L30, %L23, %L15, %L7
  %"251" = load i64, i64* %"89", align 8
  ret i64 %"251"
}

; Function Attrs: argmemonly nofree nounwind willreturn
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* noalias nocapture writeonly, i8* noalias nocapture readonly, i64, i1 immarg) #0

declare i64 @memcmp(i8*, i8*, i64)

declare i64 @printf(i8*, ...)

attributes #0 = { argmemonly nofree nounwind willreturn }
