let () =
  Llvm.enable_pretty_stacktrace ()

type prog =
  (string list
  * ([ `Lefta | `Nona | `Righta ]
    * string
    * [ `After of string | `At of string | `Before of string ])
    list)
  * Language.Expr.t

type state = 
  | None
  | FUNCTION_DEFINTION
let string_of_state (s : state) =
  match s with 
  | None -> "None"
  | FUNCTION_DEFINTION -> "function definition"

let waiting_labels : string list ref = ref [] 

let cur_state = ref None
let context = Llvm.global_context ()
let builder = Llvm.builder context
let i64_type = Llvm.i64_type context
let i32_type = Llvm.i32_type context
let lama_int_type = i32_type
let lama_ptr_type = Llvm.pointer_type lama_int_type

(* let integer_type = Llvm.double_type context *)
let () = assert (Llvm_executionengine.initialize ())
let main_module = Llvm.create_module context "main"
let cur_module = ref main_module
let the_execution_engine = Llvm_executionengine.create main_module
let the_fpm = Llvm.PassManager.create_function main_module

module LL = (val LL.make builder context main_module)

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

let variables_stack = ref [] 

module BlockMap = Map.Make(String)
let lables_map = ref BlockMap.empty
let variables_map = ref BlockMap.empty
let counter = ref 1

let clean_stack () = 
  variables_stack := []

let get_name () = 
  let () = counter := !counter + 1 in 
  string_of_int @@ !counter - 1

let create_entry_block_alloca the_function var_name =
  let builder =
    Llvm.builder_at context (Llvm.instr_begin (Llvm.entry_block the_function))
  in
  Llvm.build_alloca lama_ptr_type var_name builder

  (* pop variable from stack *)
let pop_variable () =
  match !variables_stack with
  | hd :: tl -> 
    variables_stack := tl;
    hd
  | [] ->  failwith "No values at stack"

  (* push variable to stack *)
let push_variable value =
  variables_stack := value :: !variables_stack
   
  (* add variable to variables table *)
let add_variable (variable : Llvm.llvalue) variable_name = 
  let current_function = Llvm.block_parent (Llvm.insertion_block builder) in
  let current_function_name = Llvm.value_name current_function in
  let inner_map = BlockMap.find current_function_name !variables_map in
  let new_inner_map = BlockMap.add variable_name variable inner_map in
  variables_map := BlockMap.add current_function_name new_inner_map !variables_map

let create_argument_allocas the_function =
  let inner_counter = ref 0 in
  ArrayLabels.iteri (Llvm.params the_function) ~f:(fun i ai ->
    let var_name = get_name () in
    (* Create an alloca for this variable. *)
    let alloca = create_entry_block_alloca the_function var_name in
    (* Store the initial value into the alloca. *)
    let _ = Llvm.build_store ai alloca builder in
    (* Add arguments to variable symbol table. *)
    add_variable  alloca @@ string_of_int !inner_counter;
    inner_counter := !inner_counter + 1 
  )

let show_desig (desig : Language.Value.designation) =
  match desig with
  | Global s -> print_endline (">>> Global: " ^ s)
  | Local i -> print_endline (">>> Local: " ^ string_of_int i)
  | Arg i -> print_endline (">>> Arg: " ^ string_of_int i)
  | Access i -> print_endline (">>> Access: " ^ string_of_int i)
  | Fun s -> print_endline (">>> Fun: " ^ s)
  
  let show_desig_list (desig : Language.Value.designation list) = 
    List.iter show_desig desig 
  
  let rec print_names = function
    | [] -> ()
    | (name, value)::rest ->
        Printf.printf "%s: %d\n" name value;
        print_names rest
  
  type scope = SM.scope
  let rec print_scope (scope:scope) =
    Printf.printf "blab: %s\n" scope.blab;
    Printf.printf "elab: %s\n" scope.elab;
    print_names scope.names;
    List.iter print_scope scope.subs
  
  let print_module () = 
  let () = print_endline "" in
  let () = print_endline "Variables stack:" in
  let () =  List.iter (fun x -> Llvm.dump_value x; print_endline "") !variables_stack in 
  let () = print_endline "\n" in
  Llvm.dump_module main_module


let find_label s = 
  try
  let parent_function = LL.current_function () in
  let parent_function_name = Llvm.value_name parent_function in 
  let function_map = BlockMap.find parent_function_name !lables_map in
  let label = BlockMap.find s function_map in
  Some label
  with 
  | Not_found -> None

let find_variable s = 
  try
  let parent_function = LL.current_function () in
  let parent_function_name = Llvm.value_name parent_function in 
  let function_map = BlockMap.find parent_function_name !variables_map in
  let variable = BlockMap.find s function_map in
  Some variable
  with 
  | Not_found -> None
  

let declare_functions (insns:SM.prg) = 
  List.iter (function (x : SM.insn) -> 
    match x with
    | BEGIN (name, nargs, _, _, _, _) ->
       ignore (LL.declare_function name nargs)
      | _ -> ()
    ) insns

  let rec define_functions_and_labels (insns:SM.prg) = 
    List.iter (function (x : SM.insn) -> 
      match x with
      | BEGIN (name, nargs, _, _, _, _) -> 
        let () = print_endline "begin" in
        let () = cur_state := FUNCTION_DEFINTION in
        (* Create basic block and builder *)
        let arguments_array = Array.make nargs @@ Llvm.i32_type context in 
        let func = Llvm.define_function name (Llvm.function_type (Llvm.i32_type context) arguments_array) !cur_module in 
        
        (* Create inner map for labels*)
        let entry = Llvm.entry_block func in
        let inner_map = BlockMap.empty in
        let inner_map = BlockMap.add "entry" entry inner_map in
        let _ = (lables_map := BlockMap.add name inner_map  !lables_map) in
        let () = Llvm.position_at_end entry builder in
        (* Create inner map for variables*)
        let inner_map = BlockMap.empty in
        let _ = (variables_map := BlockMap.add name inner_map  !variables_map) in
        
        create_argument_allocas func
      | LABEL s | FLABEL s-> 
        let () = print_endline @@ "Label " ^ s in
        (* let () = print_endline @@ "Current state " ^ string_of_state !cur_state in *)
        if (!cur_state != FUNCTION_DEFINTION) then
          (* let () = print_endline "then branch" in *)
          waiting_labels := s :: !waiting_labels 
        else 
          (* let () = print_endline "else branch" in  *)
          let current_function = Llvm.block_parent (Llvm.insertion_block builder) in
          let current_function_name = Llvm.value_name current_function in
          let () = print_endline current_function_name in
          let block =  Llvm.append_block context s current_function in
          let () = Llvm.position_at_end block builder in
          let inner_map = BlockMap.find current_function_name !lables_map in
          let new_inner_map = BlockMap.add s block inner_map in
          lables_map := BlockMap.add current_function_name new_inner_map !lables_map
      | END -> 
        let () = print_endline "end" in
        cur_state := None 
      | _ -> ()
      ) insns



let print_basic_block_name bb =
  let name = Llvm.value_name bb in
  print_endline ("Basic block name: " ^ name)
  
let print_basic_blocks func =
  Llvm.fold_left_blocks (fun () bb -> print_basic_block_name @@ Llvm.value_of_block bb) () func
  
let build_one (insn : SM.insn) = 
  match insn with
  | BINOP op ->
    let () = print_endline (">>> Binary operator: " ^ op) in
    (* let () = print_module () in *)
    let a_value  = pop_variable
   () in
    let _ = print_endline @@ "a_value:  " ^ Llvm.string_of_llvalue a_value in
    let b_value  = pop_variable
   () in
    let _ = print_endline @@ "b_value:  " ^ Llvm.string_of_llvalue a_value in
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
    let temp = operand a_value b_value  ~name:(op_name ^ "tmp") in
    push_variable @@ LL.build_inttoptr temp lama_ptr_type ~name:(op_name ^ "tmp1")
  | CONST i ->
    let () = print_endline @@ ">>> Constant: " ^ (string_of_int i) in
    let int_val = Llvm.const_int (Llvm.i32_type context) i in
    variables_stack := int_val :: !variables_stack;
    (* print_module () *)
  | STRING s ->
    let () = print_endline (">>> String: " ^ s) in
    let string_val = Llvm.const_string  context s in
    variables_stack := string_val :: !variables_stack
  | SEXP (tag, size) ->
      (* handle S-expression *)
      print_endline (">>> S-expression: " ^ tag ^ " " ^ string_of_int size);
      print_endline ">>> Not implemented\n"
  | LD desig ->
      print_endline @@ ">>> Load variable";
      show_desig desig;
      let current_function = Llvm.block_parent (Llvm.insertion_block builder) in
      (match desig with
      | Global s -> failwiths "globals load not implemented"
      | Local i -> failwiths "locals load not implemented"
      | Arg i -> push_variable @@ 
      (
        match (find_variable @@ string_of_int i) with 
        | Some k -> k
        | None -> failwith "No such arguments variable"
      )
      | Access i -> failwiths "access load not implemented"
      | Fun s -> failwiths "function load not implemented"
      )
  | LDA desig ->
      print_endline ">>> Load variable address";
      show_desig desig
      (* print_endline "Not implemented\n" *)
  | ST desig ->
      (* handle store variable *)
      print_endline ">>> Store variable";
      show_desig desig;
      (match desig with
      | Global s -> failwiths "globals load not implemented"
      | Local i -> add_variable (pop_variable ()) @@ string_of_int i
      | Arg i -> add_variable (pop_variable ()) @@ string_of_int i
      | Access i -> failwiths "access load not implemented"
      | Fun s -> failwiths "function load not implemented"
      )
  | STI ->
      (* handle store reference *)
      print_endline ">>> Store reference"
      (* print_endline "Not implemented\n" *)
  | STA ->
      (* handle store array/sexp/string *)
      print_endline ">>> Store array/sexp/string"
      (* print_endline "Not implemented\n" *)
  | ELEM ->
      (* handle array/string/sexp element *)
      print_endline ">>> Element"
      (* print_endline "Not implemented\n" *)
  | LABEL s -> 
    (match  (find_label s) with 
    | Some block -> 
      Llvm.position_at_end block builder
    | None -> ())
  | FLABEL s ->
      (* handle forwarded label *)
      print_endline (">>> Forwarded label: " ^ s);
      (match  (find_label s) with 
      | Some block -> 
        Llvm.position_at_end block builder
      | None -> ()) | SLABEL s ->
      (* handle scope label *)
      print_endline (">>> Scope label: " ^ s)
      (* print_endline "Not implemented\n" *)
  | JMP s ->
    let () = print_endline (">>> Unconditional jump: " ^ s) in
    ( match find_label s with 
    | Some dest_block ->
    let _ =  Llvm.build_br dest_block builder in
      print_endline ">>> Done\n"
    | None -> failwiths "No such label"
    )
  | CJMP (cond, s) ->
      let () = print_endline (">>> Conditional jump: " ^ cond ^ " " ^ s) in
      let current_function = LL.current_function () in
      let after_bb = Llvm.append_block context ("else" ^ get_name ()) current_function in
      let dest_block = find_label s in
      let cond_ = pop_variable
     () in
      ( match find_label s with 
      Some dest_block ->
      ignore (match cond with
        | "z" ->  
          Llvm.build_cond_br cond_  after_bb dest_block builder 
        | "nz" -> 
          Llvm.build_cond_br cond_  dest_block after_bb builder
        | _ -> failwiths "wtf is this cjmp"
        )
        | None -> failwiths "No such label")
  | BEGIN (name, _, _, _, _, _) ->
    print_endline @@ "BEGIN" ^ name ; 
    clean_stack ();
    let func = LL.lookup_func_exn name in
    let entry = Llvm.entry_block func in 
    Llvm.position_at_end entry builder
  | END ->
      (* handle end procedure definition *)
      cur_state := None;
      print_endline ">>> End procedure definition";
      (* print_endline "Not implemented\n" *)
  | CLOSURE (name, args) ->
      (* handle closure *)
      print_endline (">>> Closure: " ^ name)
      (* print_endline "Not implemented\n" *)
  | PROTO (name, ret) ->
      (* handle proto closure *)
      print_endline (">>> Proto closure: " ^ name ^ " " ^ ret)
      (* print_endline "Not implemented\n" *)
  | PPROTO (name, ret) ->
      (* handle proto closure to possible constant *)
      print_endline (">>> Proto closure to possible constant: " ^ name ^ " " ^ ret)
      (* print_endline "Not implemented\n" *)
  | PCALLC (nargs, has_closure) ->
      (* handle proto call *)
      print_endline "pcalc"
      (* print_endline "Not implemented\n" *)
  | CALLC (arity, is_tail_call) ->
    print_endline (">>> Call closure with arity " ^ string_of_int arity ^ " (tail call: " ^ string_of_bool is_tail_call ^ ")")
    (* print_endline "Not implemented\n"; *)
  | CALL (func_name, arity, is_tail_call) ->
      let () = print_endline (">>> Call function/procedure " ^ func_name ^ " with arity " ^ string_of_int arity ^ " (tail call: " ^ string_of_bool is_tail_call ^ ")")  in
      let func = LL.declare_function func_name arity in
      let args = ref [] in 
      let () =
        for _ = 1 to arity do
          match !variables_stack with
            | hd::tl -> 
              let () = variables_stack := tl in 
              args := hd :: !args
            | [] -> Llvm.dump_module main_module;
              Format.kasprintf failwith "No arguments for function %s in variable stack" func_name
        done in 
      let args = Array.of_list !args in
      let name =  get_name () in
      let var = Llvm.build_call func args name builder in 
      variables_stack := var :: !variables_stack
    | RET ->
        print_endline ">>> Return from function"
        (* print_endline "Not implemented\n" *)
    | DROP ->
        print_endline ">>> Drop top element from stack"
        (* print_endline "Not implemented\n" *)
    | DUP ->
      (* let _ = Llvm.build_load (Option.get stackptr) a builder in *)
      print_endline ">>> Duplicate top element on stack"
    | SWAP ->
        print_endline ">>> Swap two top elements on stack"
        (* print_endline "Not implemented\n" *)
    | TAG (tag_name, arity) ->
        print_endline (">>> Check tag " ^ tag_name ^ " with arity " ^ string_of_int arity)
        (* print_endline "Not implemented\n" *)
    | ARRAY size ->
        print_endline (">>> Check array with size " ^ string_of_int size)
        (* print_endline "Not implemented\n" *)
    | PATT patt ->
        print_endline (">>> Check pattern " ^ SM.show_patt patt)
        (* print_endline "Not implemented\n" *)
    | FAIL (loc, leave_value) ->
        (* let leave_val_str = if leave_value then "leave a value" else "terminate program" in *)
        print_endline (">>> Match failure ");
        (* print_endline ("Match failure at location " ^ Loc.to_string loc ^ " (" ^ leave_val_str ^ ")"); *)
        (* print_endline "Not implemented\n" *)
    | EXTERN str ->
        print_endline (">>> External definition " ^ str)
        (* print_endline "Not implemented\n" *)
    | PUBLIC str ->
      let () = cur_state := FUNCTION_DEFINTION in
      print_endline (">>> Public definition " ^ str)
    | IMPORT str ->
        print_endline (">>> Import clause " ^ str)

    | LINE line ->
      print_endline (">>> Line info " ^ string_of_int line)
      (* print_endline "Not implemented\n" *)


let build (insns:SM.prg) =
  let rec print_list = function
  | [] -> ()
  | h :: t ->
    let () = build_one h in 
    print_list t
  in
  (* declare_functions insns; *)
  (* print_endline "functions declared"; *)
  Llvm.dump_module main_module;
  define_functions_and_labels insns;
  print_endline "functions defined";
  Llvm.dump_module main_module;
  print_list insns;
  Llvm.dump_module main_module

let%test _ = true
