open Function_c

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

(* let rec create_scope (s : scope) (parent:parent_c) (func) prg= 
  let sclass = new scope_c s parent in 
  let array : scope_c list ref = ref [] in
  (* List.iter (function (x, y) -> sclass#add_local y @@
    create_entry_block_alloca (get_name ()) func) s.names; *)
  let parent_self = create_scope_parent sclass in
  List.iter (function x -> array := (create_scope x parent_self func prg) :: !array) s.subs;
  sclass#set_inner_scopes !array;
  (* (if (Match.match_if_pattern (subprg sclass#get_blab sclass#get_elab prg) sclass) then 
    sclass#set_if); *)
  sclass *)
    
(* let create_function (name : string) (func : Llvm.llvalue) (s : scope list) prg =
  let sclass = new function_c  name parent func in 
  let array = ref [] in
  let parent_self = create_function_parent sclass in
  List.iter (function x -> array := (create_scope x parent_self func prg) :: !array) s;
  sclass#set_inner_scopes !array;
  sclass *)


let () =
  Llvm.PassManager.initialize the_fpm |> ignore

let failwiths fmt = Format.kasprintf failwith fmt
let log fmt = Format.kasprintf (fun s -> Format.printf "%s\n%!" s) fmt


let assert_int num_type = 
  match num_type with 
    | Int -> ()
    | _ -> failwith "not an int"
(* let create_entry_block_alloca the_function var_name =
  let builder =
    Llvm.builder_at context (Llvm.instr_begin (Llvm.entry_block the_function))
  in
  Llvm.build_alloca lama_ptr_type var_name builder *)

  (* pop variable from stack *)
(* let pop_variable () =
  let rec _pop_variable con =
    let scope = (as_scope con "LLVMIRSM/pop_variable") in
    if scope#is_empty then 
    _pop_variable @@ parent_to_con scope#get_parent 
  else 
    scope#get_from_stack in 
  _pop_variable !current_con *)

(* let push_variable value =
  let scope = (as_scope !current_con "LLVMIRSM/push_variable") in
    scope#add_to_stack value *)

(* let add_local num = 
  (current_function ())#add_local num  *)
(* 
let create_argument_allocas the_function =
  ArrayLabels.iteri (Llvm.params the_function) ~f:(fun i ai ->
    let var_name = get_name () in
    (* Create an alloca for this variable. *)
    let alloca = create_entry_block_alloca the_function var_name in
    (* Store the initial value into the alloca. *)
    let _ = Llvm.build_store ai alloca builder in
    (* Add arguments to variable symbol table. *)
    let variable = new variable_c (get_name ()) alloca Arg (LL.current_function ()) in 
    add_variable variable; 
  ) *)

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
 
(*let create_parent () = 
  match !current_con with 
  | Global _ -> create_global_parent ()
  | Function x -> create_function_parent x
  | Scope x -> create_scope_parent x *)

(* let assert_in_scope () = 
  match !current_con with 
  | Global _ -> failwith "Not in scope, in global"
  | Function _ -> failwith "Not in scope, in function"
  | Scope _ -> () *)

let create_array n func = 
  let num = Llvm.const_int i32_type n in
  let array = Llvm.build_array_alloca i32_ptr_type num (get_name()) builder in
  let array_types = ref [] in
  for variable = 0 to n - 1 do
    let num = Llvm.const_int i32_type variable in
    let value_ptr = Llvm.build_gep array [| num |] (get_name ()) builder in
    let (value, value_type) = func#store in
    array_types := value_type :: !array_types; 
    ignore @@ Llvm.build_store value value_ptr builder
  done;
  (array, !array_types)

(* let last_block : Llvm.llbasicblock option ref = ref None  *)

let find_next_block func  = 
  let last_block = func#get_last_block in 
  let blocks =  Llvm.basic_blocks func#get_function in
  let rec find a x n =
    if a.(n) = x then a.(n + 1) 
    else find a x (n+1) in
  find blocks last_block 0


class global_c = object (self)
  val mutable functions = SignatureMap.empty
  val mutable functions_prg = StringMap.empty
  val mutable extern_functions = StringSet.empty
  val mutable globals = StringMap.empty

  method add_function_insns (name: string) (desc : SM.prg) = 
    functions_prg <- StringMap.add name desc functions_prg

  method get_llfunction  (name : string) (sign : variable list) =
    print_endline @@ "get_llfunction called" ^ name;
    if self#has_extern_function name then (
      print_endline @@ "Extern function: " ^ name;
       Llvm.declare_function name (create_function_signature Int sign) main_module)
    else (
    (if not @@ SignatureMap.mem (name, sign) functions then 
      self#create_function name sign);
    (SignatureMap.find (name, sign) functions)#get_function
    )

  method create_function (name : string) (sign : variable list) = 
    print_endline @@ "create function called " ^ name;
    let llfunc = Llvm.define_function name (create_function_signature Int sign) main_module in 
    let func = new function_c name llfunc sign in
    let prg = StringMap.find name functions_prg in  
    self#define_labels prg func;
    List.iter (function x -> self#build_one x func) prg;
    functions <- SignatureMap.add (name, sign) func functions
    


    method add_extern_function (func : string) = 
      extern_functions <- StringSet.add func extern_functions

    method has_extern_function (name : string) = 
      StringSet.mem name extern_functions
  
    method has_global (s : string) = 
      StringMap.mem s globals
  
    method set_global (s : string) (value : Llvm.llvalue) (val_type : variable) = 
      if self#has_global s then globals <- StringMap.remove s globals;
      let global = Llvm.declare_global lama_int_type s main_module in
      ignore @@ Llvm.build_store (Llvm.build_bitcast value i32_type (get_name ()) builder) global builder;
      globals <- StringMap.add s val_type globals
  
    method get_global (s : string) = 
      if not @@ self#has_global s then failwith "Trying to store non existent global";
      let global = Llvm.declare_global lama_int_type s main_module in
      let value =  Llvm.build_load  global (get_name ()) builder in 
      let val_type = StringMap.find s globals in 
      (value, val_type)
  
  method define_labels (insns:SM.prg) (func : function_c) = 
  List.iter (function (x : SM.insn) -> 
    match x with
    | BEGIN (name, nargs, nlocals, closure, args, scopes) -> 
      let llvm_func = func#get_function in 
      let entry = Llvm.entry_block llvm_func in
      Llvm.position_at_end entry builder
      (* while (nargs >= !counter) do 
          counter := !counter + 1
      done;
        let () = print_endline ">>> begin" in
      (* print_endline @@ "name: /" ^ name ^ "/ " ^ string_of_int nargs ^ " " ^ string_of_int nlocals ^ "\n args: "; *)
  (*List.iter (function x -> print_string x) args;
      print_endline "\n scopes: ";
      List.iter (function x -> print_scope x) scopes;
      show_desig_list closure;
      List.iter (function x -> print_string x) args;
      List.iter (function x -> print_scope x) scopes; *)
      (* Create basic block and builder *)
      let llvm_func = LL.define_function name nargs in 
      let entry = Llvm.entry_block llvm_func in
      let () = Llvm.position_at_end entry builder in
      (* Create inner map for variables*)
      let new_function = 
      (if name = "main" then 
      (
      create_function name (create_global_parent ()) llvm_func [{
      blab = "L1";
      elab = "L2";
      names = [];
      subs = []
    }] insns)
    else 
      create_function name (create_global_parent ()) llvm_func scopes insns) in
      (* print_function_class new_function; *)
      global_scope#add_function new_function;
      current_con := Function new_function;
      new_function#add_label (new label_c "entry" entry);
      print_function_c new_function *)
      (* create_argument_allocas llvm_func *)
    | LABEL s | FLABEL s -> 
      let () = print_endline @@ ">>> Label " ^ s in
       
        let llvm_function = Llvm.block_parent (Llvm.insertion_block builder) in
        let function_name = Llvm.value_name llvm_function in
        let () = print_endline function_name in
        let block =  Llvm.append_block context s llvm_function in
        let () = Llvm.position_at_end block builder in
        func#add_label (new label_c s block)

    | SLABEL s ->  ()
        (* if (current_function ())#get_name = "main" then (
        if (s = "L1") then 
          current_con := Scope ((as_function !current_con "LLVMIRSM/SLABEL")#get_scope s)
        else if (s = "L2") then 
          current_con := Function (current_function ())    ) 
        else 
          let () = print_endline @@ ">>> scope label " ^ s in
          (match  !current_con with
            | Scope x -> if (s = x#get_elab) then(
                print_endline " end slabel";
                current_con := parent_to_con x#get_parent)
              else (
                print_endline "start slabel";
                current_con := Scope (get_scope !current_con s)
              )
            | Function x -> 
              if (x#has_scope s) then
                current_con  := Scope (x#get_scope s) 
              else
                failwith "No such label in function"
            | Global _ -> failwith "Looking for scope in global") *)
    | END -> 
      print_endline "end"
    | CJMP (_, s) -> 
      let llvm_function = Llvm.block_parent (Llvm.insertion_block builder) in
      (* let function_name = Llvm.value_name llvm_function in *)
      (* let () = print_endline function_name in *)
      let name = get_name () in 
      let block =  Llvm.append_block context name llvm_function in
      Llvm.position_at_end block builder
    | _ -> ()
    ) insns


  method build_one (insn : SM.insn) (func : function_c)  = 
    match insn with
    | BINOP op ->
      let () = print_endline (">>> Binary operator: " ^ op) in
      let (a_value, a_type)  = func#store in
      func#drop;
      let (b_value, b_type)  = func#store in
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
      let temp = operand a_value b_value  ~name:(name) in
      (* assert_in_scope (); *)
      (* push_variable @@ new variable_c  (Value temp) (create_parent ()) *)
      ignore @@ func#load temp Int
      (* push_variable @@ LL.build_inttoptr temp lama_ptr_type ~name:(op_name ^ "tmp1") *)
    | CONST i ->
      let () = print_endline @@ ">>> Constant: " ^ (string_of_int i) in
      let int_val = Llvm.const_int (Llvm.i32_type context) i in
      (* assert_in_scope (); *)
      (* push_variable @@ new variable_c  (Value int_val) (create_parent ())  *)
      ignore @@ func#load int_val Int;
      print_endline "const end"
      | STRING s ->
      let () = print_endline (">>> String: " ^ s) in
      let string_val = Llvm.build_global_string s (get_name ()) builder in
      (* assert_in_scope (); *)
      (* push_variable @@ new variable_c  (Value string_val) (create_parent ()) *)
      ignore @@ func#load string_val (String (Llvm.type_of string_val))
    | SEXP (tag, size) ->
        (* handle S-expression *)
        print_endline (">>> S-expression: " ^ tag ^ " " ^ string_of_int size);
        print_endline ">>> Not implemented\n"
    | LD desig ->
        print_endline @@ ">>> Load variable";
        show_desig desig;
        (* assert_in_scope (); *)
        (match desig with
        | Global s ->  let (value, val_type) = self#get_global s in
        ignore @@ func#load value val_type 
        | Local i -> failwith "Load local failed" 
        | Arg i ->
          ignore @@ func#load (Llvm.param func#get_function i) @@ func#get_argument_type i
        | Access _ -> failwiths "access load not implemented"
        | Fun _ -> failwiths "function load not implemented"
        )
        (* (match desig with
        | Global s ->  push_variable @@ new variable_c (Ptr (Llvm.declare_global lama_int_type s main_module )) (create_function_parent current_function) 
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
        | Global s  -> let (value, val_type) = func#store in
                        self#set_global s value val_type 
        | Local i   ->  failwith "You should implement store in local" 
        | Arg i     ->  failwith "trying to store in argument"
        | Access i  -> failwiths "trying to store in access"
        | Fun s     -> failwiths "trying to store in fun"
        );
        print_endline "store ended"
        (* (match desig with
        | Global s  -> ignore @@ Llvm.build_store (pop_variable ()) (Llvm.declare_global lama_int_type s main_module ) builder
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
      let (num, num_type) =  func#store in
      func#drop;
      let (array, array_type) = func#store in
      func#drop;
      let array = Llvm.build_bitcast array (i32_ptr_type) (get_name ()) builder in
      let value_ptr = Llvm.build_gep array [| num |] (get_name ()) builder in
      ignore @@ func#load_from_ptr value_ptr 
    | FLABEL s | LABEL s -> 
      print_endline (">>> (F)Label: " ^ s);
      let label = func#get_label s in
      if label#is_set && func#is_reachable && label#get_depth != func#get_depth then  
        failwith "Depths does not match";
      if not (func#is_reachable) && not label#is_set then 
        failwith "Code is not reachable";
      if not (func#is_reachable) && label#is_set then 
        func#set_depth label#get_depth;
      
      Llvm.position_at_end label#get_block builder;
      func#set_last_block label#get_block

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
      ignore @@ Llvm.build_br label#get_block builder 
    | CJMP (cond, s) ->
      let () = print_endline (">>> Conditional jump: " ^ cond ^ " " ^ s) in
      let after_bb = find_next_block func in
      let zero = Llvm.const_int i32_type 0 in
      let (value, value_type) = func#store in 
      assert_int value_type;
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
      (* print_endline @@ "BEGIN " ^ name ;  *)
      let llvm_func = func#get_function in
      let entry = Llvm.entry_block llvm_func in 
      func#set_last_block entry;
      Llvm.position_at_end entry builder
      (* print_function_c func; *)
    | END ->
      print_endline "end";
      let value = Llvm.build_load func#get_taken (get_name ()) builder in
      ignore @@ Llvm.build_ret value builder;
      Llvm.dump_module main_module
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
        let (value, value_type) = create_array arity func in 
        ignore @@ func#load value (Array (value_type, List.length value_type))
      | _ -> 
      let args = ref [] in 
      let args_type = ref [] in
      let () =
        for _ = 1 to arity do
          let (value, val_type) = func#store in
          args := value :: !args;
          args_type :=  val_type :: !args_type;
          func#drop
        done in 
      let args = Array.of_list !args in
      let curblock = Llvm.insertion_block builder in 
      let llvm_func =  self#get_llfunction func_name !args_type  in
      Llvm.position_at_end curblock builder;
      let name =  get_name () in
      let var = Llvm.build_call llvm_func args name builder in 
      ignore @@ func#load var Int)
    | RET ->
        (* print_endline ">>> Return from function"; *)
        let (value, val_type) = func#store in 
        assert_int val_type;
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
        print_endline (">>> Check tag " ^ tag_name ^ " with arity " ^ string_of_int arity);
        print_endline "Not implemented\n"
    | ARRAY size ->
        print_endline (">>> Check array with size " ^ string_of_int size);
        print_endline "Not implemented\n"
    | PATT patt ->
        print_endline (">>> Check pattern " ^ SM.show_patt patt);
        print_endline "Not implemented\n"
    | FAIL (loc, leave_value) ->
        (* let leave_val_str = if leave_value then "leave a value" else "terminate program" in *)
        print_endline (">>> Match failure ");
        (* print_endline ("Match failure at location " ^ Loc.to_string loc ^ " (" ^ leave_val_str ^ ")"); *)
        print_endline "Not implemented\n"
    | EXTERN str ->
      print_endline (">>> External definition " ^ str);
      self#add_extern_function str
    | PUBLIC str ->
      print_endline (">>> PUBLIC " ^ str)
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
  global#create_function "main" [Int; Int];
  dump_to_object ~the_fpm;
  Llvm.dump_module main_module

let%test _ = true
