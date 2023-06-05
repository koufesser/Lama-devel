open Function_c
open Constants 

let subprg blab elab prg = 
  let rec cut_out (prg : SM.prg) = 
    match prg with
    | hd::tl -> if hd = SLABEL blab then tl else cut_out tl
    | [] -> failwith "Can not found blab in program" in
  let rec add (prg : SM.prg) ret = 
    match prg with
    | hd::tl -> if hd = SLABEL elab then ret else add tl (hd :: ret)
    | [] -> failwith "Can not found elab in program" in
   let list = cut_out prg in 
   List.rev  @@ add list []

let () =
  Llvm.PassManager.initialize the_fpm |> ignore

let failwiths fmt = Format.kasprintf failwith fmt
let log fmt = Format.kasprintf (fun s -> Format.printf "%s\n%!" s) fmt


let assert_int num_type = 
  match num_type with 
    | Int -> ()
    | _ -> failwith "not an int"


let show_desig (desig : Language.Value.designation) =
  match desig with
  | Global s -> print_endline (">>> Global: " ^ s)
  | Local i -> print_endline (">>> Local: " ^ string_of_int i)
  | Arg i -> print_endline (">>> Arg: " ^ string_of_int i)
  | Access i -> print_endline (">>> Access: " ^ string_of_int i)
  | Fun s -> print_endline (">>> Fun: " ^ s)
  
  let show_desig_list (desig : Language.Value.designation list) = 
    List.iter show_desig desig 

let print_module () = 
  let () = print_endline "" in
  let () = print_endline "Variables stack:" in
  let () = print_endline "\n" in
  Llvm.dump_module main_module


let print_basic_block_name bb =
  let name = Llvm.value_name bb in
  print_endline ("Basic block name: " ^ name)

let print_basic_blocks func =
  Llvm.fold_left_blocks (fun () bb -> print_basic_block_name @@ Llvm.value_of_block bb) () func

let create_string s = 
  let _ = Llvm.build_alloca int_type (get_name () ^ "_start_of_string_allocation") builder in 
  let str = Llvm.build_global_stringptr s (get_name ()) builder in 
  (*   call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 16 %5, i8* align 1 %6, i64 %9, i1 false) *)
  let f = Llvm.declare_function memcpy_name memcpy_type main_module in 
  let array_size = Llvm.const_int int_type @@ String.length s + 1 in  
  let full_size = Llvm.const_int int_type @@ String.length s + 3   in 
  let array = Llvm.build_array_alloca  int_type full_size (get_name () ^ "_string_" ^ s) builder in 
  
  let zero_ptr = Llvm.build_gep array [| zero |] (get_name ()) builder in
  let one_ptr = Llvm.build_gep array [| one |] (get_name ()) builder in
  ignore @@ Llvm.build_store string_code zero_ptr builder;
  ignore @@ Llvm.build_store array_size one_ptr builder;

  let array_start = Llvm.build_gep array [| two |]  (get_name ()) builder in 
  let array_start = Llvm.build_bitcast array_start i8_ptr_type (get_name ()) builder in 
  let _ = Llvm.build_call f [| array_start; str; array_size; lfalse |] "" builder in 
  let _ = Llvm.build_alloca int_type (get_name () ^ "_end_of_string_allocation") builder in
  array_start

let create_array n (func : function_c) = 
  let full_size = Llvm.const_int int_type (n + 2) in
  let array_size = Llvm.const_int int_type n in
  let array = Llvm.build_array_alloca int_type full_size (get_name() ^ "_array") builder in
  (* setting type and size *)
  let zero_ptr = Llvm.build_gep array [| zero |] (get_name ()) builder in
  let one_ptr = Llvm.build_gep array [| one |] (get_name ()) builder in
  ignore @@ Llvm.build_store  array_code zero_ptr builder;
  ignore @@ Llvm.build_store   array_size one_ptr builder;
  for variable = 2 to n + 1 do
    let num = Llvm.const_int int_type variable in
    let value_ptr = Llvm.build_gep array [| num |] (get_name ()) builder in
    let value = func#store in
    func#drop;
    ignore @@ Llvm.build_store value value_ptr builder
  done;
  Llvm.build_gep array [| two |] (get_name ()) builder

let create_sexp size (tag : string) (func : function_c) =   
  let full_size = Llvm.const_int int_type (size + 3) in
  let array_size = Llvm.const_int int_type size in
  let array = Llvm.build_array_alloca int_type full_size (get_name() ^ "_array") builder in
  
  let zero_ptr = Llvm.build_gep array [| zero |] (get_name ()) builder in
  let one_ptr = Llvm.build_gep array [| one |] (get_name ()) builder in
  let two_ptr = Llvm.build_gep array [| two |] (get_name ()) builder in

  let string_ptr = create_string tag in 
  let cstring_ptr = Llvm.build_ptrtoint string_ptr int_type (get_name ()) builder in
  ignore @@ Llvm.build_store cstring_ptr zero_ptr builder;
  ignore @@ Llvm.build_store sexp_code one_ptr builder;
  ignore @@ Llvm.build_store array_size two_ptr builder;
  
  for variable = 3 to size + 2 do
    let num = Llvm.const_int int_type variable in
    let value_ptr = Llvm.build_gep array [| num |] (get_name ()) builder in
    let value = func#store in
    func#drop;
    ignore @@ Llvm.build_store value value_ptr builder
  done;
  Llvm.build_gep array [| three |] (get_name ()) builder

let find_next_block func  = 
  let last_block = Llvm.insertion_block builder in
  print_endline @@ "find_next_block called\n current block: " ^ Llvm.value_name @@ Llvm.value_of_block last_block;  
  let blocks =  Llvm.basic_blocks func#get_function in
  let size = Array.length blocks in 
  let rec find a x n =
    if n = size - 1 then Llvm.append_block context (get_name () ^ "_fnb") func#get_function
    else if a.(n) = x then a.(n + 1) 
    else find a x (n+1) in
  let nblock = find blocks last_block 0 in 
  print_endline @@ "next block is: " ^ Llvm.value_name @@ Llvm.value_of_block nblock;
  nblock




class global_c = object (self)
  (* val mutable functions = SignatureMap.empty *)
  val mutable functions_prg = StringMap.empty
  val mutable functions = StringMap.empty
  val mutable extern_functions = StringSet.empty
  val mutable globals = StringMap.empty

  method add_function_insns (name: string) (desc : SM.prg) = 
    functions_prg <- StringMap.add name desc functions_prg

  method get_llfunction  (name : string) (args : int) =
    (* (val_type : variable) *)
    print_endline @@ "get_llfunction called" ^ name;
    if self#has_extern_function name then (
      print_endline @@ "Extern function: " ^ name;
    let (name, funType) = get_function_signature name in 
       Llvm.declare_function name funType  main_module)
    else 
    (if not @@ StringMap.mem name functions then 
      self#create_function name args;
    (StringMap.find name functions)#get_function
    )

  method cast_to_llvalue (value : Llvm.llvalue) (t: args_type) = 
    match t with 
    | INT -> value
    | INT_PTR -> Llvm.build_inttoptr value int_ptr_type (get_name()) builder
    | BITE_PTR ->  Llvm.build_inttoptr value int_ptr_type (get_name()) builder

  method insert_block (func : function_c) s =
    let insertion_block = Llvm.insertion_block builder in
    let all_blocks = Llvm.basic_blocks func#get_function in  
    let last_block = all_blocks.(Array.length all_blocks - 1) in 
    if insertion_block = last_block then 
      let block = Llvm.append_block context (get_name ()) func#get_function in 
      block
    else    
      let next_block = find_next_block func in 
      let new_block = Llvm.insert_block context s next_block in 
      new_block

  method cast_args s (args : Llvm.llvalue list) = 
    let casted_array = List.map (function y ->  Llvm.build_inttoptr y i8_ptr_type (get_name()) builder)  args in 
    casted_array


  method make_printf (s: string) (func : function_c) =
    let ptr = Llvm.build_global_stringptr s (get_name ()) builder in 
    let strint = Llvm.build_ptrtoint ptr int_type (get_name ()) builder in
    ignore @@ func#load strint;
    let get_args () : SM.insn list = [CALL ("Lprintf", 1, false); DROP] in
    List.iter (function x -> self#build_one x func ) @@ get_args ()


  method create_function (name : string) (args : int) = 
    print_endline @@ "create function called " ^ name;
    let llfunc = LL.define_function name args  in 
    let func = new function_c name llfunc in
    let prg = StringMap.find name functions_prg in  
    self#define_labels prg func;
    List.iter (function x -> self#build_one x func) prg;
    functions <- StringMap.add name func functions

  method add_extern_function (func : string) = 
    extern_functions <- StringSet.add func extern_functions

  method has_extern_function (name : string) = 
    StringSet.mem name extern_functions || List.mem name builtIns

  method has_global (s : string) = 
    StringMap.mem s globals

  method store_global (value : Llvm.llvalue) (s : string)  = 
    let global = if self#has_global s then StringMap.find s globals else 
      let gl = Llvm.define_global s  (Llvm.const_int int_type 0)  main_module in 
      let _ = globals <- StringMap.add s gl globals in 
      gl in
    ignore @@ Llvm.build_store (Llvm.build_bitcast value int_type (get_name ()) builder) global builder

  method check_opcode (func :function_c) (block : Llvm.llbasicblock) = 
    if func#get_opcode then func#set_opcode false
    else ignore @@ Llvm.build_br block builder 

    method load_global (s : string) = 
    if not @@ self#has_global s then failwith "Trying to store non existent global";
    let global =  StringMap.find s globals in
    let value =  Llvm.build_load  global (get_name ()) builder in 
    value
(*   
  method append_blocks (n : int) (func : function_c) = 
    let llvm_function = func#get_function in
    for _ = 0 to n - 1 do
      let block =  Llvm.append_block context (get_name ()) llvm_function in 
      Llvm.position_at_end block builder
    done  *)

  method define_labels (insns:SM.prg) (func : function_c) = 
  List.iter (function (x : SM.insn) -> 
    match x with
    | BEGIN (name, nargs, nlocals, closure, args, scopes) -> 
      let llvm_func = func#get_function in 
      let entry = Llvm.entry_block llvm_func in
      print_endline @@ "position at end: entry " ^ name;
      Llvm.position_at_end entry builder
    | LABEL s | FLABEL s -> 
      let () = print_endline @@ ">>> Label " ^ s in
        let llvm_function = Llvm.block_parent (Llvm.insertion_block builder) in
        let function_name = Llvm.value_name llvm_function in
        let () = print_endline function_name in
        let block =  Llvm.append_block context s llvm_function in
        (* let _ = Llvm.build_br  block builder in  *)
        print_endline @@ "position at end: label " ^ s;
        let () = Llvm.position_at_end block builder in
        func#add_label (new label_c s block)
    | SLABEL s ->  ()
    | END -> 
      print_endline "end"
    | CJMP (_, s) ->  ()
      (* let llvm_function = func#get_function in *)
      (* let function_name = Llvm.value_name llvm_function in *)
      (* let () = print_endline function_name in *)
      (* let name = get_name () ^ "_cjmp" in 
      let block =  Llvm.append_block context name llvm_function in
      Llvm.position_at_end block builder
    | TAG (_, _) ->  append_blocks 2 func *)

    | _ -> ()
  
    ) insns
  
  method build_one (insn : SM.insn) (func : function_c)  = 
    match insn with
    | BINOP op ->
      let () = print_endline (">>> Binary operator: " ^ op) in
      let a_value  = func#store in
      func#drop;
      let b_value  = func#store in
      func#drop;
      let operand, op_name =
      match op with
        | "+" -> (LL.build_add, "add")
        | "-" -> (LL.build_sub, "sub")
        | "/" -> (LL.build_sdiv, "div")
        | "*" -> (LL.build_mul, "mul")
        | ">" -> (LL.build_Sgt, "sgt")
        | "<" -> (LL.build_Slt, "slt")
        | _ ->
            Format.kasprintf failwith
              "Only +,/,*,- are supported by now but %s appeared" op
      in
      let name = get_name () in
      let temp = operand b_value a_value  ~name:(name) in
      (* assert_in_scope (); *)
      (* push_variable @@ new variable_c  (Value temp) (create_parent ()) *)
      let ctmp = Llvm.build_intcast temp int_type (get_name ()) builder in  
      ignore @@ func#load ctmp 
      (* push_variable @@ LL.build_inttoptr temp lama_ptr_type ~name:(op_name ^ "tmp1") *)
    | CONST i ->
      let () = print_endline @@ ">>> Constant: " ^ (string_of_int i) in
      let int_val = Llvm.const_int (int_type) i in
      (* assert_in_scope (); *)
      (* push_variable @@ new variable_c  (Value int_val) (create_parent ())  *)
      ignore @@ func#load int_val;
      print_endline "const end"
      | STRING s ->
      let () = print_endline (">>> String: " ^ s) in
      let string_ptr = create_string s in
      let cstring_ptr = Llvm.build_ptrtoint string_ptr int_type (get_name ()) builder in
      (* assert_in_scope (); *)
      (* push_variable @@ new variable_c  (Value string_val) (create_parent ()) *)
      ignore @@ func#load cstring_ptr 
    | SEXP (tag, size) ->
        let sexp = create_sexp size tag func in 
        let lsexp = Llvm.build_ptrtoint sexp int_type (get_name () ^ "sexp_created") builder in
        ignore @@ func#load lsexp
    | LD desig ->
        print_endline @@ ">>> Load variable";
        show_desig desig;
        (* assert_in_scope (); *)
        (match desig with
        | Global s -> let global =  Llvm.declare_global int_type s main_module in 
                      let value = Llvm.build_load global (get_name()) builder in 
                      ignore @@ func#load value 
        | Local i ->  let value = func#store_local i in 
                      ignore @@ func#load value  
        | Arg i ->
          ignore @@ func#load (Llvm.param func#get_function i)
        | Access _ -> failwiths "access load not implemented"
        | Fun _ -> failwiths "function load not implemented"
        )
        (* (match desig with
        | Global s ->  push_variable @@ new variable_c (Ptr (Llvm.declare_global int_type s main_module )) (create_function_parent current_function) 
        | Local i -> push_variable @@ new variable_c (Value (find_local i)) (create_parent())
        | Arg i -> push_variable @@ new variable_c (Value (Llvm.param current_function#get_function i) ) (create_parent())   
        (* add to function values *)
        | Access i -> failwiths "access load not implemented"
        | Fun s -> failwiths "function load not implemented"
        ) *)
    | LDA desig ->
        print_endline ">>> Load variable address";
        show_desig desig;
        print_endline "Not implemented\n"
    | ST desig ->
        (* handle store variable *)
        print_endline ">>> Store variable";
        show_desig desig;
        (match desig with
        | Global s  -> let value = func#store in
                      self#store_global value s  
        | Local i   -> 
          let value = func#store in
           ignore @@ func#load_local i value
        | Arg i     ->  failwith "trying to store in argument"
        | Access i  -> failwiths "trying to store in access"
        | Fun s     -> failwiths "trying to store in fun"
        );
        print_endline "store ended"
        (* (match desig with
        | Global s  -> ignore @@ Llvm.build_store (pop_variable ()) (Llvm.declare_global int_type s main_module ) builder
        | Local i   ->  ignore @@ store_in_local i 
        | Arg i     ->  failwith "trying to store in argument"
        | Access i  -> failwiths "trying to store in access"
        | Fun s     -> failwiths "trying to store in fun"
        ) *)
    | STI ->
        (* handle store reference *)
        print_endline ">>> Store reference";
        print_endline "Not implemented\n"
    | STA ->
        (* handle store array/sexp/string *)
        print_endline ">>> Store array/sexp/string";
        print_endline "Not implemented\n"
    | ELEM ->
      print_endline "elem";
      (* handle array/string/sexp element *)
      let num =  func#store in
      func#drop;
      let array = func#store in
      func#drop;
      let array = Llvm.build_inttoptr array (int_ptr_type) (get_name ()) builder in
      let value_ptr = Llvm.build_gep array [| num |] (get_name ()) builder in
      ignore @@ func#load_from_ptr value_ptr 
    | FLABEL s -> 
      let label = func#get_label s in
      func#set_reachable true;
      print_endline @@ "position at end: label " ^ s;
      ignore @@ self#check_opcode func label#get_block; 
      Llvm.position_at_end label#get_block builder;
    | LABEL s -> 
      print_endline (">>> (F)Label: " ^ s);
      let label = func#get_label s in
      if label#is_set && func#is_reachable && label#get_depth != func#get_depth then  
        failwith "Depths does not match";
      if label#is_set then (
        func#set_reachable true;
        print_endline @@ "setting depth: " ^ string_of_int label#get_depth;
        func#set_depth label#get_depth
      );
      if not @@ func#is_reachable then 
        failwith "Code is not reachable";
      if not (func#is_reachable) && label#is_set then 
        func#set_depth label#get_depth;
      ignore @@ self#check_opcode func label#get_block; 
      print_endline @@ "position at end: label " ^ s; 
      Llvm.position_at_end label#get_block builder;

    | SLABEL s -> () 
        (* if (current_function ())#get_name = "main" then (
          if (s = "L1") then 
            current_con := Scope ((as_function !current_con "LLVMIRSM/SLABEL")#get_scope s)
          else if (s = "L2") then 
            (go_up ();
            current_con := Function (current_function ()))
          else () 
        ) 
        else 
        let () = print_string @@ ">>> scope label " ^ s in
        (match  !current_con with
        | Scope x -> if (s = x#get_elab) then(
            print_endline " end slabel"; go_up ())
          else (
            print_endline "start slabel";
            if x#is_if && not (x#is_return_ptr_set) then (x#set_return_ptr @@
              new variable_c (Ptr ((current_function ())#get_free_ptr)) ((create_function_parent (current_function ())));
            );
            current_con := Scope (get_scope !current_con s)
          )
        | Function x -> 
          if (x#has_scope s) then
            current_con  := Scope (x#get_scope s) 
          else
            failwith "No such label in function"
          | Global _ -> failwith "Looking for scope in global") *)
    | JMP s ->
      let () = print_endline (">>> Unconditional jump: " ^ s) in
      let label =  func#get_label s in
      func#update_label_depth s func#get_depth;
      func#set_reachable false;
      func#set_opcode true;
      ignore @@ Llvm.build_br label#get_block builder 
    | CJMP (cond, s) ->
      let () = print_endline (">>> Conditional jump: " ^ cond ^ " " ^ s) in
      let insertion_block = Llvm.insertion_block builder in
      let instn_block_name = Llvm.value_name @@ Llvm.value_of_block insertion_block in  
      let () = print_endline (">>> Current block: " ^ instn_block_name ^ " " ^ s) in
      let after_bb = self#insert_block func (get_name () ^ "_cjmp") in
      let zero = Llvm.const_int int_type 0 in
      let value = func#store in 
      let cond_ = Llvm.build_icmp Llvm.Icmp.Ne value zero (get_name ()) builder  in
      let dest_block =  func#get_label s in
      func#drop;
      func#update_label_depth s func#get_depth;
      ignore (match cond with
        | "z" ->  
          Llvm.build_cond_br  cond_ after_bb dest_block#get_block builder 
        | "nz" -> 
          Llvm.build_cond_br cond_  dest_block#get_block after_bb builder
        | _ -> failwiths "wtf is this cjmp"
        );
      (* print_endline @@ "position at end: afterbb " ^ Llvm.value_name @@ Llvm.value_of_block after_bb;
      Llvm.position_at_end after_bb builder; *)
      Llvm.position_at_end after_bb builder
    | BEGIN (name, _, _, _, _, _) ->
      (* print_endline @@ "BEGIN " ^ name ;  *)
      let llvm_func = func#get_function in
      let entry = Llvm.entry_block llvm_func in 
      print_endline @@ "position at end: entry " ^ name;
      Llvm.position_at_end entry builder
      (* print_function_c func; *)
    | END ->
      print_endline "end";
      let value = Llvm.build_load func#get_taken (get_name ()) builder in
      func#set_opcode true;
      ignore @@ Llvm.build_ret value builder;
      print_endline "end end"; 
        (* print_endline ">>> End procedure definition"; *)
    | CLOSURE (name, args) ->
        print_endline (">>> Closure: " ^ name);
        print_endline "Not implemented\n"
    | PROTO (name, ret) ->
        print_endline (">>> Proto closure: " ^ name ^ " " ^ ret);
        print_endline "Not implemented\n"
    | PPROTO (name, ret) ->
        print_endline (">>> Proto closure to possible constant: " ^ name ^ " " ^ ret);
        print_endline "Not implemented\n"
    | PCALLC (nargs, has_closure) ->
        print_endline "pcalc";
        print_endline "Not implemented\n"
    | CALLC (arity, is_tail_call) ->
      print_endline (">>> Call closure with arity " ^ string_of_int arity ^ " (tail call: " ^ string_of_bool is_tail_call ^ ")");
      print_endline "Not implemented\n"
    | CALL (func_name, arity, is_tail_call) ->
      let () = print_endline (">>> Call function/procedure " ^ func_name ^ " with arity " ^ string_of_int arity ^ " (tail call: " ^ string_of_bool is_tail_call ^ ")")  in
      (match func_name with 
      ".array" ->
        let array = create_array arity func in 
        let array_to_int = Llvm.build_ptrtoint array int_type (get_name()^"_array_to_int_") builder in 
        ignore @@ func#load array_to_int 
      | _ -> 
      let args = ref [] in 
      let () =
        for _ = 1 to arity do
          let value = func#store in
          args := value :: !args;
          func#drop
        done in 
      let curblock = Llvm.insertion_block builder in 
      let llvm_func =  self#get_llfunction func_name arity in
      let _ = if self#has_extern_function func_name then
      args := self#cast_args func_name !args in 
      let args = Array.of_list !args in
      print_endline @@ "position at end: after call";
      Llvm.position_at_end curblock builder;
      let name =  get_name () in
      let var = Llvm.build_call llvm_func args name builder in 
      ignore @@ func#load var)
    | RET ->
      (* print_endline ">>> Return from function"; *)
      let value = func#store in 
      ignore @@ Llvm.build_ret value builder;
      func#drop;
      if func#get_depth != 0 then failwith "depth in the ret != 0" 
    | DROP ->
      print_endline "drop";
      func#drop;
      print_endline "drop_end";
    | DUP ->
      func#dup  
    | SWAP ->
      func#swap
    | TAG (tag_name, arity) -> 
      let _ = Llvm.build_alloca int_type (get_name () ^ "_tag_start_" ^ tag_name) builder in 
      let f = Llvm.declare_function memcmp_name memcmp_type main_module in 
      let comparison_array = Llvm.build_array_alloca int_type two (get_name() ^ "_tag_comp") builder in 
      let zero_ptr = Llvm.build_gep comparison_array [| zero |] (get_name ()) builder in 
      let one_ptr = Llvm.build_gep comparison_array [| one |] (get_name ()) builder in 
      let lsize = Llvm.const_int int_type arity in   
      ignore @@ Llvm.build_store sexp_code zero_ptr builder;
      ignore @@ Llvm.build_store lsize one_ptr builder;
      let sexp = func#store in 
      let ptr_to_sexp = Llvm.build_inttoptr sexp int_ptr_type (get_name () ^ "_sexp") builder in 
      let ptr_to_sexp = Llvm.build_gep ptr_to_sexp [| mtwo |] (get_name ()) builder in
      let i8ptr_to_sexp = Llvm.build_bitcast ptr_to_sexp i8_ptr_type (get_name ()) builder in
      let i8_ptr_comparison_array = Llvm.build_bitcast comparison_array i8_ptr_type (get_name()) builder in 
      let comp = Llvm.build_call f [| i8ptr_to_sexp; i8_ptr_comparison_array; (Llvm.const_int int_type @@ int_size * 2) |] (get_name()) builder in 
      let cond = Llvm.build_icmp Ne comp zero (get_name()) builder in
      let block_3 = self#insert_block func (get_name() ^ "_tagblock_3") in 
      let block_2 = self#insert_block func (get_name() ^ "_tagblock_2_1") in 
      let block_1 = self#insert_block func (get_name() ^ "_tagblock_1") in 
      let _ = Llvm.build_cond_br  cond block_1 block_2 builder in 
      (*  alrady false *)
      print_endline @@ "position at end: tag " ^ tag_name ^ " tagblock_1"; 
      let _ = Llvm.position_at_end block_1 builder in
      (* self#make_printf ("came to " ^ tag_name ^ "_tagblock_1: it is not equal sexp") func; *)
      let _ = func#load zero in 
      let _ = Llvm.build_br block_3 builder in
      (* if correct compare tag *)
      func#drop;
      print_endline @@ "position at end: tag " ^ tag_name ^ " tagblock_2";
      let _ = Llvm.position_at_end block_2 builder in
      (* self#make_printf ("came to " ^ tag_name ^ "_tagblock_2: it is sexp and size is equal") func; *)
      let ptr_ptr_string = Llvm.build_gep ptr_to_sexp [| mone |] (get_name ()) builder in
      let ptr_string = Llvm.build_load ptr_ptr_string (get_name ()^"_str_st") builder in  
      let ptr_string = Llvm.build_inttoptr ptr_string i8_ptr_type (get_name()) builder in  
      let comparison_string = create_string tag_name in 
      let int_ptr_string = Llvm.build_bitcast ptr_string int_ptr_type (get_name()) builder in  
      let ptr_string_size = Llvm.build_gep int_ptr_string [| mone |] (get_name()) builder in
      let string_size = Llvm.build_load ptr_string_size (get_name()) builder in
      let cond = Llvm.build_icmp Ne string_size (Llvm.const_int int_type (String.length tag_name + 1)) (get_name ()) builder in  
      let block_2_2 = self#insert_block func (get_name() ^ "_tagblock_2_2") in 
      let _ = Llvm.build_cond_br cond block_1 block_2_2 builder in
      print_endline @@ "position at end: tag " ^ tag_name ^ " tagblock_2_2";
      let _ = Llvm.position_at_end block_2_2 builder in
      (* self#make_printf ("came to " ^ tag_name ^ "_tagblock_2_2: size of tags are equal") func; *)
      let ptr_string_i8 = Llvm.build_bitcast ptr_string i8_ptr_type (get_name ()) builder in  
      let comparison_string_i8 = Llvm.build_bitcast comparison_string i8_ptr_type (get_name ()) builder in  
      
      let comp = Llvm.build_call f [| ptr_string_i8; comparison_string_i8; (Llvm.const_int int_type (String.length tag_name + 1)) |] (get_name ()) builder in  
      let cond = Llvm.build_icmp Eq comp zero (get_name ()) builder in 
      let cond = Llvm.build_intcast cond int_type (get_name ()) builder in  
      let _ = func#load cond in  
      let _ = Llvm.build_alloca int_type (get_name () ^ "_tag_end") builder in
      let _ = Llvm.build_br block_3 builder in
      print_endline @@ "position at end: tag " ^ tag_name ^ " tagblock_3";
      Llvm.position_at_end block_3 builder 
      
    | ARRAY size -> print_endline @@ "array " ^ string_of_int size
        (* print_endline (">>> Check array with size " ^ string_of_int size);
        let structure_type_ptr = Llvm.pointer_type @@ Llvm.struct_type context [| int_type; i8_ptr_type |] in
        let ptr = func#store in  
        let casted = Llvm.build_bitcast ptr structure_type_ptr (get_name ()) builder in 
        let int_field_ptr = Llvm.build_struct_gep casted 0 (get_name ()) builder in
        let loaded = Llvm.build_load int_field_ptr (get_name ()) builder in 
        let size = Llvm.const_int int_type size in 
        ignore @@ Llvm.build_icmp Eq loaded size (get_name ()) builder *)
    | PATT patt ->
        print_endline (">>> Check pattern " ^ SM.show_patt patt);
        print_endline "Not implemented\n"
    | FAIL (loc, leave_value) ->
      let printf = self#get_llfunction "Lprintf" 0 in 
      let (a, b) = loc in 
      let str = Llvm.build_global_stringptr "program failed" (get_name()) builder in 
      ignore @@ Llvm.build_call printf [|str|] (get_name ()) builder
    | EXTERN str ->
      self#add_extern_function str
    | PUBLIC str -> ()
    | IMPORT str ->
        print_endline (">>> IMPORT " ^ str);
        print_endline "Not implemented\n"
    | LINE line ->
      print_endline (">>> Line info " ^ string_of_int line);
      latest_line_info := string_of_int line

    method cut_functions (insns:SM.prg) =
      let s = ref "" in 
      let finsns = ref [] in
      List.iter (function (x : SM.insn) -> 
        match x with
        | BEGIN (name, nargs, nlocals, closure, args, scopes) ->
          s:= name;
          finsns := [x];
        | END -> finsns := x :: !finsns;
          finsns := List.rev !finsns; 
          self#add_function_insns !s !finsns 
        | EXTERN s -> self#add_extern_function s 
        | _ ->  finsns := x :: !finsns
        
      ) insns
end


let build (insns:SM.prg) =
  let global = new global_c in 
  global#cut_functions insns;
  global#create_function "main" 2;
  print_endline "main created";
  let filename = "output.ll" in 
  Llvm.print_module filename main_module;
  print_endline ("LLVM module written to: " ^ filename);
  
  dump_to_object ~the_fpm;
  print_endline "dumped to object";
  Llvm.dump_module main_module

let%test _ = true
