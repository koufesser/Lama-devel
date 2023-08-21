open Constants
open Function_c

let define_read () = 
  let ftype = Llvm.function_type int_type [| |] in 
  let func = Llvm.define_function "Lread" ftype main_module in 
  let scanf_type =  Llvm.var_arg_function_type int32_type [|i8_ptr_type|] in
  let scan_f = Llvm.declare_function "scanf" scanf_type main_module in 
  let printf_type = Llvm.var_arg_function_type int32_type [|i8_ptr_type|] in
  let printf_f = Llvm.declare_function "printf" printf_type main_module in 
  let entry = Llvm.entry_block func in
  Llvm.position_at_end entry builder;
  let str = Llvm.build_global_stringptr "> " (get_name ()) builder in
  let _ = Llvm.build_call printf_f [| str |] "" builder in 
  let str = Llvm.build_global_stringptr "%d" (get_name ()) builder in 
  let alloca = Llvm.build_alloca int32_type (get_name()) builder in
  let alloca_i8 = Llvm.build_bitcast alloca i8_ptr_type (get_name()) builder in 
  let _ = Llvm.build_call scan_f [| str; alloca_i8 |] "" builder in 
  let ret = Llvm.build_load alloca (get_name ()) builder in 
  let ret = Llvm.build_intcast ret int_type (get_name()) builder in 
  let ret =  LL.build_add ~name:(get_name()) one (LL.build_mul ~name:(get_name()) two ret) in
  Llvm.build_ret ret builder 

let define_write () =
  let ftype = Llvm.function_type int_type [| int_type |] in 
  let func = Llvm.define_function "Lwrite" ftype main_module in 
  let printf_type = Llvm.var_arg_function_type int32_type [|i8_ptr_type|] in
  let printf_f = Llvm.declare_function "printf" printf_type main_module in 
  let entry = Llvm.entry_block func in
  Llvm.position_at_end entry builder;
  let str = Llvm.build_global_stringptr "%d\n" (get_name ()) builder in
  let arg = Llvm.param func 0 in
  let arg = LL.build_sdiv ~name:(get_name()) (LL.build_sub ~name:(get_name()) arg one) two in 
  let ret = Llvm.build_call printf_f [| str; arg |] "" builder in 
  let ret = Llvm.build_intcast ret int_type (get_name()) builder in 
  Llvm.build_ret ret builder 

let define_lhd () = 
  let ftype = Llvm.function_type int_type [| int_type |] in 
  let func = Llvm.define_function "Lhd" ftype main_module in  
  let entry = Llvm.entry_block func in
  Llvm.position_at_end entry builder;
  let arg = Llvm.param func 0 in
  let arg = Llvm.build_inttoptr arg int_ptr_type (get_name()) builder in 
  let ptr = Llvm.build_gep arg [| zero |] (get_name()) builder in 
  let ret = Llvm.build_load ptr (get_name()) builder in 
  Llvm.build_ret ret builder 
  
let define_ltl () = 
  let ftype = Llvm.function_type int_type [| int_type |] in 
  let func = Llvm.define_function "Ltl" ftype main_module in  
  let entry = Llvm.entry_block func in
  Llvm.position_at_end entry builder;
  let arg = Llvm.param func 0 in
  let arg = Llvm.build_inttoptr arg int_ptr_type (get_name()) builder in 
  let ptr = Llvm.build_gep arg [| one |] (get_name()) builder in 
  let ret = Llvm.build_load ptr (get_name()) builder in 
  Llvm.build_ret ret builder 
  

let create_array_1 () = 
  let ftype = Llvm.function_type int_type [| int_type; int_type |] in 
  let func = Llvm.define_function ".array" ftype main_module in 
  let entry = Llvm.entry_block func in
  let array_size = Llvm.param func 1 in
  let values = Llvm.param func 0 in
  Llvm.position_at_end entry builder;
  let full_size = LL.build_add ~name:(get_name()) two array_size in
  let array = Llvm.build_array_malloc int_type full_size (get_name() ^ "_array") builder in
  (* setting type and size *)
  let zero_ptr = Llvm.build_gep array [| zero |] (get_name ()) builder in
  let one_ptr = Llvm.build_gep array [| one |] (get_name ()) builder in
  ignore @@ Llvm.build_store  array_code zero_ptr builder;
  ignore @@ Llvm.build_store array_size one_ptr builder;
  let array_start  = Llvm.build_gep array [| two |] (get_name ()) builder in 
  let ar_st_i8 = Llvm.build_bitcast  array_start i8_ptr_type (get_name ()) builder in 
  let values_i8 = Llvm.build_inttoptr  values i8_ptr_type (get_name ()) builder in 

  let size = LL.build_mul ~name:(get_name()) eight array_size  in 
  let f = Llvm.declare_function memcpy_name memcpy_type main_module in 
  let _ = Llvm.build_call f [| ar_st_i8; values_i8; size; lfalse |] "" builder in 
  let ret = Llvm.build_ptrtoint array_start int_type (get_name ()) builder in 
  Llvm.build_ret ret builder  

