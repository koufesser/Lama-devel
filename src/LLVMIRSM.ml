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


(* let show_desig (desig : Language.Value.designation) =
  match desig with
  | Global s -> print_endline (">>> Global: " ^ s)
  | Local i -> print_endline (">>> Local: " ^ string_of_int i)
  | Arg i -> print_endline (">>> Arg: " ^ string_of_int i)
  | Access i -> print_endline (">>> Access: " ^ string_of_int i)
  | Fun s -> print_endline (">>> Fun: " ^ s)
  
  let show_desig_list (desig : Language.Value.designation list) = 
    List.iter show_desig desig  *)

let print_module () = 
  let () = print_endline "" in
  let () = print_endline "Variables stack:" in
  let () = print_endline "\n" in
  Llvm.dump_module main_module

(* 
let print_basic_block_name bb =
  let name = Llvm.value_name bb in
  print_endline ("Basic block name: " ^ name)

let print_basic_blocks func =
  Llvm.fold_left_blocks (fun () bb -> print_basic_block_name @@ Llvm.value_of_block bb) () func *)

let create_string s = 
  let _ = Llvm.build_alloca int_type (get_name () ^ "_start_of_string_allocation") builder in 
  let str = Llvm.build_global_stringptr s (get_name ()) builder in 
  (*   call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 16 %5, i8* align 1 %6, i64 %9, i1 false) *)
  let f = Llvm.declare_function memcpy_name memcpy_type main_module in 
  let array_size = Llvm.const_int int_type @@ String.length s + 1 in  
  let string_size = Llvm.const_int int_type @@ String.length s in 
  let full_size = Llvm.const_int int_type @@ String.length s + 3   in 
  let array = Llvm.build_array_alloca  int_type full_size (get_name () ^ "_string_" ^ s) builder in 
  
  let zero_ptr = Llvm.build_gep array [| zero |] (get_name ()) builder in
  let one_ptr = Llvm.build_gep array [| one |] (get_name ()) builder in
  ignore @@ Llvm.build_store string_code zero_ptr builder;
  ignore @@ Llvm.build_store string_size one_ptr builder;

  let array_start = Llvm.build_gep array [| two |]  (get_name ()) builder in 
  let array_start = Llvm.build_bitcast array_start i8_ptr_type (get_name ()) builder in 
  let _ = Llvm.build_call f [| array_start; str; array_size; lfalse |] "" builder in 
  let _ = Llvm.build_alloca int_type (get_name () ^ "_end_of_string_allocation") builder in
  array_start

let create_array n (func : function_c) = 
  let full_size = Llvm.const_int int_type (n + 2) in
  let array_size = Llvm.const_int int_type n in
  let array = Llvm.build_array_malloc int_type full_size (get_name() ^ "_array") builder in
  (* setting type and size *)
  let zero_ptr = Llvm.build_gep array [| zero |] (get_name ()) builder in
  let one_ptr = Llvm.build_gep array [| one |] (get_name ()) builder in
  ignore @@ Llvm.build_store  array_code zero_ptr builder;
  ignore @@ Llvm.build_store array_size one_ptr builder;
  let array_start  = Llvm.build_gep array [| two |] (get_name ()) builder in 
  let values = func#store_and_drop_n n in 
  let ar_st_i8 = Llvm.build_bitcast  array_start i8_ptr_type (get_name ()) builder in 
  let values_i8 = Llvm.build_bitcast  values i8_ptr_type (get_name ()) builder in 
  let size = Llvm.const_int int_type (n * 8) in 
  let f = Llvm.declare_function memcpy_name memcpy_type main_module in 
  let _ = Llvm.build_call f [| ar_st_i8; values_i8; size; lfalse |] "" builder in 
  array_start 

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
  (* print_endline @@ "find_next_block called\n current block: " ^ Llvm.value_name @@ Llvm.value_of_block last_block;   *)
  let blocks =  Llvm.basic_blocks func#get_function in
  let size = Array.length blocks in 
  let rec find a x n =
    if n = size - 1 then Llvm.append_block context (get_name () ^ "_fnb") func#get_function
    else if a.(n) = x then a.(n + 1) 
    else find a x (n+1) in
  let nblock = find blocks last_block 0 in 
  (* print_endline @@ "next block is: " ^ Llvm.value_name @@ Llvm.value_of_block nblock; *)
  nblock




class global_c = object (self)
  (* val mutable functions = SignatureMap.empty *)
  val mutable functions_prg = StringMap.empty
  val mutable functions = StringMap.empty
  val mutable extern_functions = StringSet.empty
  val mutable globals = StringMap.empty

  method add_function_insns (name: string) (desc : SM.prg) (nargs : int) = 
    functions_prg <- StringMap.add name (desc, nargs) functions_prg

  method get_llfunction  (name : string)  =
    if self#has_extern_function name then (
    let (_, funType) = get_function_signature name in 
       (* print_endline @@ "function declaration: " ^ name;  *)
       Llvm.declare_function name funType  main_module)
    else 
    (if not @@ StringMap.mem name functions then 
      (let curblock = Llvm.insertion_block builder in 
      ignore @@ self#create_function name;
      ignore @@ Llvm.position_at_end curblock builder;);
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

  (* method cast_args s (args : Llvm.llvalue list) = 
    let casted_array = List.map (function y ->  Llvm.build_inttoptr y i8_ptr_type (get_name()) builder)  args in 
    casted_array *)


  method make_printf (s: string) (func : function_c) =
    let ptr = Llvm.build_global_stringptr s (get_name ()) builder in 
    let strint = Llvm.build_ptrtoint ptr int_type (get_name ()) builder in
    ignore @@ func#load strint;
    let get_args () : SM.insn list = [CALL ("Lprintf", 1, false); DROP] in
    List.iter (function x -> self#build_one x func ) @@ get_args ()


  method create_function (name : string) = 
    print_endline @@ "function name: " ^ name;
    let (prg, nargs) = StringMap.find name functions_prg in  
    let llfunc = LL.define_function name @@ nargs + 1 in
    let func = new function_c name llfunc @@ nargs + 1 in
    functions <- StringMap.add name func functions; 
    self#define_labels prg func;
    List.iter (function x -> self#build_one x func) prg

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

  method define_labels (insns:SM.prg) (func : function_c) = 
  let fullness = ref 0 in 
  let max_size = ref 0 in   
  List.iter (function (x : SM.insn) -> 
    max_size := max !max_size !fullness;
    match x with
    | BEGIN (name, nargs, nlocals, closure, args, scopes) -> 
      let llvm_func = func#get_function in 
      let entry = Llvm.entry_block llvm_func in
      Llvm.position_at_end entry builder
    | LABEL s | FLABEL s -> 
        let llvm_function = Llvm.block_parent (Llvm.insertion_block builder) in
        (* let function_name = Llvm.value_name llvm_function in *)
        (* let () = print_endline function_name in *)
        let block =  Llvm.append_block context s llvm_function in
        let () = Llvm.position_at_end block builder in
        func#add_label (new label_c s block)
    | LD _ | DUP | CONST _ | STRING _-> fullness := !fullness + 1
    | DROP | BINOP _ ->  fullness := !fullness - 1
    | CALL (_, arity, _) -> fullness := !fullness - arity + 1 
    | CALLC (arity, _) | PCALLC (arity, _) -> fullness := !fullness - arity
    | _ -> ();
    max_size := max !max_size !fullness
    ) insns;
    func#create_stack !max_size
  
  method build_one (insn : SM.insn) (func : function_c)  = 
    match insn with
    | BINOP op ->
      let a_value  = func#store in
      func#drop;
      let b_value  = func#store in
      func#drop;
      let name = get_name () in 
      let operand, op_name =
      match op with
        | "+" -> (LL.build_add, "add")
        | "-" -> (LL.build_sub, "sub")
        | "/" -> (LL.build_sdiv, "div")
        | "*" -> (LL.build_mul, "mul")
        | "%" -> (LL.build_urem, "urem")
        | ">" -> (LL.build_Sgt name, "sgt")
        | ">=" -> (LL.build_Sge name, "sge")
        | "<" -> (LL.build_Slt name, "slt")
        | "<=" -> (LL.build_Sle name, "sle")
        | "==" -> (LL.build_eq name, "sle")
        | "!=" -> (LL.build_ne name, "sle")
        | "&&" -> (LL.build_and, "sle")
        | _ ->
            Format.kasprintf failwith
              "Only +,/,*,- are supported by now but %s appeared" op
      in
      let name = get_name () in
      let temp = operand b_value a_value  ~name:(name) in
      let ctmp = Llvm.build_intcast temp int_type (get_name ()) builder in  
      ignore @@ func#load ctmp 
    | CONST i ->
      let int_val = Llvm.const_int (int_type) i in
      ignore @@ func#load int_val;
      | STRING s ->
      let string_ptr = create_string s in
      let cstring_ptr = Llvm.build_ptrtoint string_ptr int_type (get_name ()) builder in
      ignore @@ func#load cstring_ptr 
    | SEXP (tag, size) ->
        let sexp = create_sexp size tag func in 
        let lsexp = Llvm.build_ptrtoint sexp int_type (get_name () ^ "sexp_created") builder in
        ignore @@ func#load lsexp
    | LD desig ->
        (match desig with
        | Global s -> let global =  Llvm.declare_global int_type s main_module in 
                      let value = Llvm.build_load global (get_name()) builder in 
                      ignore @@ func#load value 
        | Local i ->  let value = func#store_local i in 
                      ignore @@ func#load value  
        | Arg i ->
          ignore @@ func#load @@ func#get_param i 
        | Access i -> 
          let cl_var = func#get_closure_variables in 
          let li = Llvm.const_int int_type i in 
          let cl_var = Llvm.build_inttoptr cl_var int_ptr_type (get_name ()) builder in 
          let iptr = Llvm.build_gep cl_var [| li |] (get_name ()) builder in 
          let loaded = Llvm.build_load  iptr (get_name()) builder in 
          ignore @@ func#load loaded
        | Fun _ -> failwiths "function load not implemented"
        )
    | LDA desig -> failwith "LDA"
    | ST desig ->
        (match desig with
        | Global s  -> let value = func#store in
                      self#store_global value s  
        | Local i   -> 
          let value = func#store in
           ignore @@ func#load_local i value
        | Arg i     ->  failwith "trying to store in argument"
        | Access i  -> failwiths "trying to store in access"
        | Fun s     -> failwiths "trying to store in fun"
        )
    | STI -> failwith "STI"
    | STA -> 
      let value =  func#store in
      func#drop;
      let i = func#store in
      func#drop;
      let array = func#store in
      let nblock = self#insert_block func (get_name()) in 
      let block_string = self#insert_block func (get_name() ^ "_stablock_string") in 
      let block_array = self#insert_block func (get_name() ^ "_stablock_array") in
      let int_array = Llvm.build_inttoptr array int_ptr_type (get_name ()) builder in 
      let type_ref = Llvm.build_gep int_array [| mtwo |] (get_name ()) builder in 
      let array_type = Llvm.build_load type_ref (get_name ()) builder in 
      let comp = Llvm.build_icmp Llvm.Icmp.Eq string_code array_type (get_name ()) builder in 
      let _ = Llvm.build_cond_br comp block_string block_array builder in 
      let _ = Llvm.position_at_end block_string builder in 
      let i8_array = Llvm.build_inttoptr array i8_ptr_type (get_name()) builder in
      let i8_value = Llvm.build_intcast value i8_type (get_name ()) builder in   
      let ref = Llvm.build_gep i8_array [| i |] (get_name ()) builder in 
      let _ = Llvm.build_store i8_value ref builder in 
      let _ = Llvm.build_br nblock builder in 
      let _ = Llvm.position_at_end block_array builder in 
      let ref = Llvm.build_gep int_array [| i |] (get_name ()) builder in 
      let _ = Llvm.build_store value ref builder in 
      let _ = Llvm.build_br nblock builder in 
      Llvm.position_at_end nblock builder
    | ELEM ->
      let i = func#store in
      func#drop;
      let array = func#store in
      func#drop;
      let nblock = self#insert_block func (get_name()) in  
      let block_string = self#insert_block func (get_name() ^ "_elemblock_string") in 
      let block_array = self#insert_block func (get_name() ^ "_elemblock_array") in
      let int_array = Llvm.build_inttoptr array int_ptr_type (get_name ()) builder in 
      let type_ref = Llvm.build_gep int_array [| mtwo |] (get_name ()) builder in 
      let array_type = Llvm.build_load type_ref (get_name ()) builder in 
      let comp = Llvm.build_icmp Llvm.Icmp.Eq string_code array_type (get_name ()) builder in 
      let _ = Llvm.build_cond_br comp block_string block_array builder in 
      let _ = Llvm.position_at_end block_string builder in 

      let i8_array = Llvm.build_inttoptr array i8_ptr_type (get_name()) builder in
      let ref = Llvm.build_gep i8_array [| i |] (get_name ()) builder in
      let value = Llvm.build_load ref (get_name()) builder in
      let value = Llvm.build_intcast value int_type (get_name ()) builder in 
      ignore @@ func#load value; 
      func#drop;  
      let _ = Llvm.build_br nblock builder in 
      let _ = Llvm.position_at_end block_array builder in 
      
      let ref = Llvm.build_gep int_array [| i |] (get_name ()) builder in
      ignore @@ func#load_from_ptr ref;
      let _ = Llvm.build_br nblock builder in 
      Llvm.position_at_end nblock builder

      (* let num =  func#store in
      func#drop;
      let array = func#store in
      func#drop;
      let array = Llvm.build_inttoptr array (int_ptr_type) (get_name () ^ "_elem") builder in
      let value_ptr = Llvm.build_gep array [| num |] (get_name ()) builder in
      ignore @@ func#load_from_ptr value_ptr  *)
    | FLABEL s -> 
      let label = func#get_label s in
      func#set_reachable true;
      ignore @@ self#check_opcode func label#get_block; 
      Llvm.position_at_end label#get_block builder;
    | LABEL s -> 
      let label = func#get_label s in
      if label#is_set && func#is_reachable && label#get_depth != func#get_depth then  
        failwith @@ "Depths does not match " ^ s;
      if label#is_set then (
        func#set_reachable true;
        func#set_depth label#get_depth
      );
      if not @@ func#is_reachable then 
        failwith "Code is not reachable";
      if not (func#is_reachable) && label#is_set then 
        func#set_depth label#get_depth;
      ignore @@ self#check_opcode func label#get_block; 
      Llvm.position_at_end label#get_block builder;

    | SLABEL s -> () 
    | JMP s ->
      let label =  func#get_label s in
      func#update_label_depth s func#get_depth;
      func#set_reachable false;
      func#set_opcode true;
      ignore @@ Llvm.build_br label#get_block builder 
    | CJMP (cond, s) ->
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

      Llvm.position_at_end after_bb builder
    | BEGIN (name, _, _, _, _, _) ->
      let llvm_func = func#get_function in
      let entry = Llvm.entry_block llvm_func in 
      Llvm.position_at_end entry builder
    | END ->
      let value = func#store in
      func#set_opcode true;
      ignore @@ Llvm.build_ret value builder;
    | CLOSURE (name, args) ->
      let closure = self#get_llfunction name  in 
      let closure = Llvm.build_ptrtoint closure int_type (get_name ()) builder in 
      let array = Llvm.build_array_malloc int_type two (get_name ()) builder in 
      let cl_var_ptr = Llvm.build_gep array [| zero |] (get_name ()) builder in 
      let cl_ptr = Llvm.build_gep array [| one |] (get_name ()) builder in 
      let sz = List.length args in 

      (* making an array *)
      let full_size = Llvm.const_int int_type (sz + 2) in
      let array_size = Llvm.const_int int_type sz in
      let sm_array = Llvm.build_array_malloc int_type full_size (get_name() ^ "_array") builder in
      (* setting type and size *)
      let zero_ptr = Llvm.build_gep sm_array [| zero |] (get_name ()) builder in
      let one_ptr = Llvm.build_gep sm_array [| one |] (get_name ()) builder in
      ignore @@ Llvm.build_store array_code zero_ptr builder;
      ignore @@ Llvm.build_store array_size one_ptr builder;
      let args = ref args in 
      for variable = 2 to sz + 1 do
        let num = Llvm.const_int int_type variable in
        let value_ptr = Llvm.build_gep sm_array [| num |] (get_name ()) builder in
        let value = (match !args with 
        a:: b -> args := b; a
        | [] -> failwith "empty args") in
        let value = (match value with 
        | Global s -> let global =  Llvm.declare_global int_type s main_module in 
          Llvm.build_load global (get_name()) builder 
        | Local i -> func#store_local i
        | Arg i ->
          func#get_param i
        | Access i -> 
          let cl_var = func#get_closure_variables in 
          let li = Llvm.const_int int_type i in 
          let iptr = Llvm.build_gep cl_var [| li |] (get_name ()) builder in 
          Llvm.build_load  iptr (get_name()) builder
        | Fun _ -> failwiths "function load not implemented"
        )  in
        ignore @@ Llvm.build_store value value_ptr builder
      done;
      let sm_array = Llvm.build_gep sm_array [| two |] (get_name ()) builder in
      let sm_array = Llvm.build_ptrtoint sm_array int_type (get_name ()) builder in
      let _ = Llvm.build_store sm_array cl_var_ptr  builder in
      let _ = Llvm.build_store closure cl_ptr   builder in
      let array = Llvm.build_ptrtoint array int_type (get_name ()) builder in 
      ignore @@ func#load array
    | PROTO (name, ret) ->
      failwith "PROTO"
    | PPROTO (name, ret) ->
      failwith "PPROTO"
    | PCALLC (nargs, has_closure) ->
        failwith "PCALLC"
    | CALLC (arity, is_tail_call) ->
      let args = ref [] in 
      let () =
        for _ = 1 to arity do
          let value = func#store in
          args := value :: !args;
          func#drop
        done in 
      let curblock = Llvm.insertion_block builder in 
      let name =  get_name () in
      let closure = func#store in 
      let array = Llvm.build_inttoptr closure (int_ptr_type) (get_name ()) builder in
      let closure_variables_ptr = Llvm.build_gep array [| zero |] (get_name ()) builder in
      let closure_variables = Llvm.build_load closure_variables_ptr (get_name ()) builder in 
      let args = Array.of_list (closure_variables :: !args) in
      let function_t = Llvm.function_type int_type ( Array.make (arity + 1) int_type) in
      let closure_ptr =  Llvm.build_gep array [| one |] (get_name ()) builder in
      let closure = Llvm.build_load closure_ptr (get_name ()) builder in 
      let closure = Llvm.build_inttoptr closure (Llvm.pointer_type function_t) (get_name ()) builder in 
      let var = Llvm.build_call closure args name builder in 
      Llvm.position_at_end curblock builder;
      ignore @@ func#load var

    | CALL (func_name, arity, is_tail_call) ->
      (* let () = print_endline (">>> Call function/procedure " ^ func_name ^ " with arity " ^ string_of_int arity ^ " (tail call: " ^ string_of_bool is_tail_call ^ ")")  in *)
      (
        match func_name with 
        ".array" ->
          let array = create_array arity func in 
          let array_to_int = Llvm.build_ptrtoint array int_type (get_name()^"_array_to_int_") builder in 
          ignore @@ func#load array_to_int 
        | "Llength" -> 
        let array = func#store in 
        func#drop;
        let array = Llvm.build_inttoptr array int_ptr_type (get_name ()) builder in  
        let sz_ptr = Llvm.build_gep array [| mone |] (get_name()) builder in 
        let sz = Llvm.build_load sz_ptr (get_name ()) builder in 
        ignore @@ func#load sz
      | _ -> 
      let args = ref [] in 
      let () =
        for _ = 1 to arity do
          let value = func#store in
          args := value :: !args;
          func#drop
        done in 
      let llvm_func =  self#get_llfunction func_name in
      let _ = if not @@ self#has_extern_function func_name then
      args := zero :: !args in 
      (* add zero instead of closure variables *)
      let args = Array.of_list !args in 
      let name =  get_name () in
      let var = Llvm.build_call llvm_func args name builder in 
      ignore @@ func#load var)
    | RET ->
      let value = func#store in 
      ignore @@ Llvm.build_ret value builder;
      func#drop;
      if func#get_depth != 0 then failwith "depth in the ret != 0" 
    | DROP ->
      func#drop;
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
      let _ = Llvm.position_at_end block_1 builder in
      let _ = func#load zero in 
      let _ = Llvm.build_br block_3 builder in
      (* if correct compare tag *)
      func#drop;
      let _ = Llvm.position_at_end block_2 builder in
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
      let _ = Llvm.position_at_end block_2_2 builder in
      let ptr_string_i8 = Llvm.build_bitcast ptr_string i8_ptr_type (get_name ()) builder in  
      let comparison_string_i8 = Llvm.build_bitcast comparison_string i8_ptr_type (get_name ()) builder in  
      
      let comp = Llvm.build_call f [| ptr_string_i8; comparison_string_i8; (Llvm.const_int int_type (String.length tag_name + 1)) |] (get_name ()) builder in  
      let cond = Llvm.build_icmp Eq comp zero (get_name ()) builder in 
      let cond = Llvm.build_intcast cond int_type (get_name ()) builder in  
      let _ = func#load cond in  
      let _ = Llvm.build_alloca int_type (get_name () ^ "_tag_end") builder in
      let _ = Llvm.build_br block_3 builder in
      Llvm.position_at_end block_3 builder 
      
    | ARRAY size -> 
      failwith "ARRAY"
    | PATT patt -> failwith "PATT"
    | FAIL (loc, leave_value) ->
      let printf = self#get_llfunction "Lprintf" in 
      let (a, b) = loc in 
      let str = Llvm.build_global_stringptr "program failed" (get_name()) builder in 
      ignore @@ Llvm.build_call printf [|str|] (get_name ()) builder
    | EXTERN str ->
      self#add_extern_function str
    | PUBLIC str -> ()
    | IMPORT str -> failwith "import"
    | LINE line ->
      latest_line_info := string_of_int line

    method cut_functions (insns:SM.prg) =
      let s = ref "" in 
      let n_args = ref 0 in 
      let finsns = ref [] in
      List.iter (function (x : SM.insn) -> 
        match x with
        | BEGIN (name, nargs, nlocals, closure, args, scopes) ->
          s:= name;
          n_args := nargs;
          finsns := [x];
        | END -> finsns := x :: !finsns;
          finsns := List.rev !finsns; 
          self#add_function_insns !s !finsns !n_args
        | EXTERN s -> self#add_extern_function s 
        | _ ->  finsns := x :: !finsns
      ) insns
end


let build (insns:SM.prg) =
  let global = new global_c in 
  global#cut_functions insns;
  global#create_function "main";
  (* print_endline "main created"; *)
  let filename = "output.ll" in 
  Llvm.print_module filename main_module;
  (* print_endline ("LLVM module written to: " ^ filename); *)
  
  dump_to_object ~the_fpm
  (* print_endline "dumped to object"; *)
  (* Llvm.dump_module main_module *)

let%test _ = true

