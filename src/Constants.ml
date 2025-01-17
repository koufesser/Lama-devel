let context = Llvm.global_context ()

let int_type = Llvm.i64_type context
let int32_type = Llvm.i32_type context
let int_ptr_type = Llvm.pointer_type int_type
let i8_type = Llvm.i8_type context 
let i8_ptr_type = Llvm.pointer_type i8_type
let i1_type = Llvm.i1_type context
let zero = Llvm.const_int int_type 0 
let one = Llvm.const_int int_type 1 
let two = Llvm.const_int int_type 2 
let eight = Llvm.const_int int_type 8 
let three = Llvm.const_int int_type 3 
let mone = Llvm.const_int int_type @@ -1 
let mtwo = Llvm.const_int int_type @@ -2 
let mthree = Llvm.const_int int_type @@ -3 
let lfalse = Llvm.const_int i1_type 0
let ltrue = Llvm.const_int i1_type 1
let last_bit = 31
let builtIns = ["Lprintf"; "Lstrcmp"; ".array"]
let array_code = Llvm.const_int int_type 0x00000003
let sexp_code = Llvm.const_int int_type 0x00000005
let string_code = Llvm.const_int int_type 0x00000001
let closure_code = Llvm.const_int int_type 0x00000007 

module BuiltInsMapType = Map.Make(String)
type args_type = 
  INT | INT_PTR | BITE_PTR

let builtInsMap =
  BuiltInsMapType.of_seq (List.to_seq [
    ("Lprintf", ("printf",  [| BITE_PTR |]));
    ("Lwrite", ("Lwrite",  [| BITE_PTR |])); 
    ("Lstrcmp", ("strcmp",  [| BITE_PTR ; BITE_PTR |]));
    ("Lread", ("Lread",  [| BITE_PTR |]));
    (".array", (".array",  [| BITE_PTR |]));
    ("Lhd", ("Lhd",  [| INT_PTR |]));
    ("Ltl", ("Ltl",  [| INT_PTR |]));
  ])

let get_function_signature s = 
  (* let (name, signat) = BuiltInsMapType.find s builtInsMap in 
  let casted_array = Array.map (function x -> match x with 
  | INT -> int_type
  | INT_PTR -> int_ptr_type
  | BITE_PTR -> i8_ptr_type) signat in *)
  let (name, _) = BuiltInsMapType.find s builtInsMap in
  let function_type = 
    match name with 
    | "printf" -> Llvm.var_arg_function_type int_type [|i8_ptr_type|]   
    | "strcmp" -> Llvm.function_type int_type [| i8_ptr_type; i8_ptr_type |]
    | "Lread" -> Llvm.var_arg_function_type int_type [|  |]
    | "Lwrite" -> Llvm.function_type int_type [| int_type |]
    | ".array" -> Llvm.function_type int_type [| int_type; int_type |]
    | "Lhd" -> Llvm.function_type int_type [| int_type |]
    | "Ltl" -> Llvm.function_type int_type [| int_type |]
    | _ -> failwith "No such function" in 

  (name, function_type) 
 

let lamaFunctions = [".array"]

let memcpy_type = Llvm.function_type (Llvm.void_type context) [| i8_ptr_type; i8_ptr_type; int_type; i1_type |]
let memcpy_name = "llvm.memcpy.p0i8.p0i8.i64"
let memcmp_type = Llvm.function_type int_type [| i8_ptr_type; i8_ptr_type; int_type |]
let memcmp_name = "memcmp"
let int_size = 8