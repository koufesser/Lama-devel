let context = Llvm.global_context ()

let int_type = Llvm.i64_type context
let int_ptr_type = Llvm.pointer_type int_type
let i8_type = Llvm.i8_type context 
let i8_ptr_type = Llvm.pointer_type i8_type
let i1_type = Llvm.i1_type context
let zero = Llvm.const_int int_type 0 
let one = Llvm.const_int int_type 1 
let two = Llvm.const_int int_type 2 
let three = Llvm.const_int int_type 3 
let mone = Llvm.const_int int_type @@ -1 
let mtwo = Llvm.const_int int_type @@ -2 
let mthree = Llvm.const_int int_type @@ -3 
let lfalse = Llvm.const_int i1_type 0
let ltrue = Llvm.const_int i1_type 1
let last_bit = 31
let builtIns = [".tag"; ".string"]
let cFunctions = ["Lprintf"; "Lstrcmp"]
let array_code = Llvm.const_int int_type 1
let sexp_code = Llvm.const_int int_type 2
let string_code = Llvm.const_int int_type 3

module CFunctionType = Map.Make(String)

let get_cfunction_signature s = 
    match s with 
    | "Lprintf" -> ("printf", Llvm.var_arg_function_type int_type [|i8_ptr_type|])   
    | "Lstrcmp" -> ("strcmp", Llvm.function_type int_type [| i8_ptr_type; i8_ptr_type |])
    | _ -> failwith "No such function"
let memcpy_type = Llvm.function_type (Llvm.void_type context) [| i8_ptr_type; i8_ptr_type; int_type; i1_type |]
let memcpy_name = "llvm.memcpy.p0i8.p0i8.i64"
let memcmp_type = Llvm.function_type int_type [| i8_ptr_type; i8_ptr_type; int_type |]
let memcmp_name = "memcmp"
let int_size = 8
