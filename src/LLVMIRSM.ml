open Scope

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

let rec create_scope (s : scope) (parent:parent_c) (func) prg= 
  let sclass = new scope_c s parent in 
  let array : scope_c list ref = ref [] in
  List.iter (function (x, y) -> sclass#add_local y @@
    create_entry_block_alloca (get_name ()) func) s.names;
  let parent_self = create_scope_parent sclass in
  List.iter (function x -> array := (create_scope x parent_self func prg) :: !array) s.subs;
  sclass#set_inner_scopes !array;
  (if (Match.match_if_pattern (subprg sclass#get_blab sclass#get_elab prg) sclass) then 
    sclass#set_if);
  sclass
    
let create_function (name : string) (parent : parent_c) (func : Llvm.llvalue) (s : scope list) prg =
  let sclass = new function_c  name parent func in 
  let array = ref [] in
  let parent_self = create_function_parent sclass in
  List.iter (function x -> array := (create_scope x parent_self func prg) :: !array) s;
  sclass#set_inner_scopes !array;
  sclass


let () =
  (* (* Promote allocas to registers. *)
     Llvm_scalar_opts.add_memory_to_register_promotion the_fpm;
     (* Do simple "peephole" optimizations and bit-twiddling optzn. *)
     Llvm_scalar_opts.add_instruction_combination the_fpm;
     (* reassociate expressions. *)
     Llvm_scalar_opts.add_reassociation the_fpm;
     (* Eliminate Common SubExpressions. *)
     Llvm_scalar_opts.add_gvn the_fpm;
     (* Simplify the control flow graph (deleting unreachable blocks, etc). *)
     Llvm_scalar_opts.add_cfg_simplification the_fpm; *)
  Llvm.PassManager.initialize the_fpm |> ignore

let failwiths fmt = Format.kasprintf failwith fmt
let log fmt = Format.kasprintf (fun s -> Format.printf "%s\n%!" s) fmt


(* let create_entry_block_alloca the_function var_name =
  let builder =
    Llvm.builder_at context (Llvm.instr_begin (Llvm.entry_block the_function))
  in
  Llvm.build_alloca lama_ptr_type var_name builder *)

  (* pop variable from stack *)
let pop_variable () =
  let rec _pop_variable con =
    let scope = (as_scope con "LLVMIRSM/pop_variable") in
    if scope#is_empty then 
    _pop_variable @@ parent_to_con scope#get_parent 
  else 
    scope#get_from_stack in 
  _pop_variable !current_con

let push_variable value =
  let scope = (as_scope !current_con "LLVMIRSM/push_variable") in
    scope#add_to_stack value

let add_local num (variable : Llvm.llvalue) = 
  (as_scope !current_con "llvmirsm/add_local")#add_local num variable 
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


let define_functions_and_labels (insns:SM.prg) = 
  List.iter (function (x : SM.insn) -> 
    match x with
    | BEGIN (name, nargs, nlocals, closure, args, scopes) ->
      while (nargs >= !counter) do 
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
      let arguments_array = Array.make nargs @@ Llvm.i32_type context in 
      let llvm_func = Llvm.define_function name (Llvm.function_type (Llvm.i32_type context) arguments_array) main_module in 
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
      new_function#add_label entry "entry";
      print_function_c new_function
      (* create_argument_allocas llvm_func *)
    | LABEL s | FLABEL s -> 
      let () = print_endline @@ ">>> Label " ^ s in
      (match !current_con with 
          | Global _ | Function _-> ()
          |  Scope x -> 
      let llvm_function = Llvm.block_parent (Llvm.insertion_block builder) in
      let function_name = Llvm.value_name llvm_function in
      let () = print_endline function_name in
      let block =  Llvm.append_block context s llvm_function in
      let () = Llvm.position_at_end block builder in
      x#add_label block s
      )
    | SLABEL s ->      
       if (current_function ())#get_name = "main" then (
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
            | Global _ -> failwith "Looking for scope in global")
    | END -> 
      let () = print_endline "end" in
      current_con :=  Global global_scope
    | CJMP (cond, s) -> 
      let llvm_function = Llvm.block_parent (Llvm.insertion_block builder) in
      (* let function_name = Llvm.value_name llvm_function in *)
      (* let () = print_endline function_name in *)
      let name = get_name () in 
      let block =  Llvm.append_block context name llvm_function in
      Llvm.position_at_end block builder
    | _ -> ()
    ) insns

let print_basic_block_name bb =
  let name = Llvm.value_name bb in
  print_endline ("Basic block name: " ^ name)

let print_basic_blocks func =
  Llvm.fold_left_blocks (fun () bb -> print_basic_block_name @@ Llvm.value_of_block bb) () func

let create_parent () = 
  match !current_con with 
  | Global _ -> create_global_parent ()
  | Function x -> create_function_parent x
  | Scope x -> create_scope_parent x

let assert_in_scope () = 
  match !current_con with 
  | Global _ -> failwith "Not in scope, in global"
  | Function _ -> failwith "Not in scope, in function"
  | Scope _ -> ()

let last_block : Llvm.llbasicblock option ref = ref None 

let find_next_block () = 
  match !last_block with 
  | None -> failwith "No last_block"
  | Some x -> 
    let blocks =  Llvm.basic_blocks (current_function ())#get_function in
    let rec find a x n =
      if a.(n) = x then a.(n + 1) 
      else find a x (n+1) in
    find blocks x 0



let build_one (insn : SM.insn) (prg : SM.prg) = 
  match insn with
  | BINOP op ->
    let () = print_endline (">>> Binary operator: " ^ op) in
    let a_value  = pop_variable () in
    let b_value  = pop_variable () in
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
    assert_in_scope ();
    push_variable @@ new variable_c  (Value temp) (create_parent ())
    (* push_variable @@ LL.build_inttoptr temp lama_ptr_type ~name:(op_name ^ "tmp1") *)
  | CONST i ->
    let () = print_endline @@ ">>> Constant: " ^ (string_of_int i) in
    let int_val = Llvm.const_int (Llvm.i32_type context) i in
    assert_in_scope ();
    push_variable @@ new variable_c  (Value int_val) (create_parent ()) 
(* constants should get out of scope 
   Maybe make an assert when going out of scope
   Maybe make a type for variable_c to not store const in stack ptr*)
    | STRING s ->
    let () = print_endline (">>> String: " ^ s) in
    let string_val = Llvm.const_string  context s in
    assert_in_scope ();
    push_variable @@ new variable_c  (Value string_val) (create_parent ())
  | SEXP (tag, size) ->
      (* handle S-expression *)
      print_endline (">>> S-expression: " ^ tag ^ " " ^ string_of_int size);
      print_endline ">>> Not implemented\n"
  | LD desig ->
      print_endline @@ ">>> Load variable";
      show_desig desig;
      
      let current_function = current_function () in
      let llvm_function = current_function # get_function in
      assert_in_scope ();
      (match desig with
      | Global s ->  push_variable @@ new variable_c (Ptr (Llvm.declare_global lama_int_type s main_module )) (create_function_parent current_function) 
      | Local i -> push_variable @@ new variable_c (Value (find_local i)) (create_parent())
      | Arg i -> push_variable @@ new variable_c (Value (Llvm.param current_function#get_function i) ) (create_parent())   
      (* add to function values *)
      | Access i -> failwiths "access load not implemented"
      | Fun s -> failwiths "function load not implemented"
      )
  | LDA desig ->
      print_endline ">>> Load variable address";
      show_desig desig;
      print_endline "Not implemented\n"
  | ST desig ->
      (* handle store variable *)
      print_endline ">>> Store variable";
      show_desig desig;
      (match desig with
      | Global s  -> ignore @@ Llvm.build_store (pop_variable ()) (Llvm.declare_global lama_int_type s main_module ) builder
      | Local i   ->  store_local (pop_variable ()) i 
      | Arg i     ->  failwith "trying to store in argument"
      | Access i  -> failwiths "trying to store in access"
      | Fun s     -> failwiths "trying to store in fun"
      )
  | STI ->
      (* handle store reference *)
      print_endline ">>> Store reference";
      print_endline "Not implemented\n"
  | STA ->
      (* handle store array/sexp/string *)
      print_endline ">>> Store array/sexp/string";
      print_endline "Not implemented\n"
  | ELEM ->
      (* handle array/string/sexp element *)
      print_endline ">>> Element";
      print_endline "Not implemented\n"
  | FLABEL s | LABEL s -> 
    print_endline (">>> (F)Label: " ^ s);
    (match !current_con with
    | Global _ -> ()
    | _ -> 
      let block = find_label s in
      Llvm.position_at_end block builder;
      last_block := Some block)

  | SLABEL s -> 
      if (current_function ())#get_name = "main" then (
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
          if x#is_if && x#is_stack_set then (x#add_to_stack @@
            new variable_c (Ptr ((current_function ())#get_free_ptr)) ((create_function_parent (current_function ())));
            x#set_stack
          );
          current_con := Scope (get_scope !current_con s)
        )
      | Function x -> 
        if (x#has_scope s) then
          current_con  := Scope (x#get_scope s) 
        else
          failwith "No such label in function"
        | Global _ -> failwith "Looking for scope in global")
  | JMP s ->
    let () = print_endline (">>> Unconditional jump: " ^ s) in
    let block = find_label s in 
    ignore @@ Llvm.build_br block builder 
  | CJMP (cond, s) ->
    let () = print_endline (">>> Conditional jump: " ^ cond ^ " " ^ s) in
    let current_function = LL.current_function () in
    let after_bb = find_next_block () in
    let dest_block = find_label s in
    let cond_ = (pop_variable ()) in
    let dest_block = find_label s in
    ignore (match cond with
      | "z" ->  
        Llvm.build_cond_br cond_  after_bb dest_block builder 
      | "nz" -> 
        Llvm.build_cond_br cond_  dest_block after_bb builder
      | _ -> failwiths "wtf is this cjmp"
      );
    Llvm.position_at_end after_bb builder
  | BEGIN (name, _, _, _, _, _) ->
    (* print_endline @@ "BEGIN " ^ name ;  *)
    let llvm_func = LL.lookup_func_exn name in
    let entry = Llvm.entry_block llvm_func in 
    last_block := Some entry;
    Llvm.position_at_end entry builder;
    let func =  (as_global !current_con "build_one/begin")#get_function name in
    print_function_c func;
    current_con := Function func
  | END ->
    let value = Llvm.build_load (as_function !current_con "LLVMIRSM/END")#get_taken_ptr (get_name ()) builder in
    ignore @@ Llvm.build_ret value builder;
    current_con := Global global_scope;
    Llvm.dump_module main_module
      (* print_endline ">>> End procedure definition"; *)
  | CLOSURE (name, args) ->
      (* handle closure *)
      print_endline (">>> Closure: " ^ name);
      print_endline "Not implemented\n"
  | PROTO (name, ret) ->
      (* handle proto closure *)
      print_endline (">>> Proto closure: " ^ name ^ " " ^ ret);
      print_endline "Not implemented\n"
  | PPROTO (name, ret) ->
      (* handle proto closure to possible constant *)
      print_endline (">>> Proto closure to possible constant: " ^ name ^ " " ^ ret);
      print_endline "Not implemented\n"
  | PCALLC (nargs, has_closure) ->
      (* handle proto call *)
      print_endline "pcalc";
      print_endline "Not implemented\n"
  | CALLC (arity, is_tail_call) ->
    print_endline (">>> Call closure with arity " ^ string_of_int arity ^ " (tail call: " ^ string_of_bool is_tail_call ^ ")");
    print_endline "Not implemented\n"
  | CALL (func_name, arity, is_tail_call) ->
      let () = print_endline (">>> Call function/procedure " ^ func_name ^ " with arity " ^ string_of_int arity ^ " (tail call: " ^ string_of_bool is_tail_call ^ ")")  in
      let func = LL.declare_function func_name arity in
      let args = ref [] in 
      let () =
        for _ = 1 to arity do
          args := (as_scope !current_con "build_one/call")#get_from_stack :: !args 
        done in 
      let args = Array.of_list !args in
      let name =  get_name () in
      let var = Llvm.build_call func args name builder in 
      assert_in_scope ();
      push_variable @@ new variable_c  (Value var) (create_parent ())
    | RET ->
        (* print_endline ">>> Return from function"; *)
        ignore @@ Llvm.build_ret (pop_variable ()) builder;  
    | DROP -> ()
          (* (as_scope !current_con "build_one/swap")#drop_from_stack    *)
    | DUP ->
      (* let _ = Llvm.build_load (Option.get stackptr) a builder in *)
      (* print_endline ">>> DUP";
      (match !current_con with 
      | Scope x -> x#dup
      | _ -> failwith "Not in scope to dup")  *)
      (as_scope !current_con "build_one/swap")#dup  
    | SWAP ->
      (as_scope !current_con "build_one/swap")#swap
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
        print_endline "Not implemented\n"
    | PUBLIC str ->
      print_endline (">>> PUBLIC " ^ str)
    | IMPORT str ->
        print_endline (">>> IMPORT " ^ str);
        print_endline "Not implemented\n"
    | LINE line ->
      print_endline (">>> Line info " ^ string_of_int line);
      latest_line_info := string_of_int line

let build (insns:SM.prg) =
  let rec print_list = function
  | [] -> ()
  | h :: t ->
    let () = build_one h insns in 
    print_list t
  in
  define_functions_and_labels insns;
  print_endline "functions defined";
  print_list insns;
  Llvm.dump_module main_module

let%test _ = true
