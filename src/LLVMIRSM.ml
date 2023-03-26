
type prog =
  (string list
  * ([ `Lefta | `Nona | `Righta ]
    * string
    * [ `After of string | `At of string | `Before of string ])
    list)
  * Language.Expr.t

type state = 
  | None
  | IN_MAIN
  | FUNCTION_DEFINTION
  | IN_FUNCTION

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
let function_module = Llvm.create_module context "functions"
let main_module = Llvm.create_module context "main"
let cur_module = ref main_module
let the_execution_engine = Llvm_executionengine.create main_module
let the_fpm = Llvm.PassManager.create_function main_module

module LL_MAIN = (val LL.make builder main_module)
module LL_FUNCTIONS = (val LL.make builder main_module)

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
let my_map = ref BlockMap.empty
let counter = ref 1

let clean_stack () = 
  variables_stack := []

let get_name = 
  let () = counter := !counter + 1 in 
  string_of_int @@ !counter - 1

let create_entry_block_alloca the_function var_name =
  let builder =
    Llvm.builder_at context (Llvm.instr_begin (Llvm.entry_block the_function))
  in
  Llvm.build_alloca lama_ptr_type var_name builder

let create_argument_allocas the_function =
  ArrayLabels.iteri (Llvm.params the_function) ~f:(fun i ai ->
      let var_name = get_name in
      (* Create an alloca for this variable. *)
      let alloca = create_entry_block_alloca the_function var_name in
      (* Store the initial value into the alloca. *)
      let _ = Llvm.build_store ai alloca builder in
      (* Add arguments to variable symbol table. *)
      variables_stack := alloca :: !variables_stack)
  
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

let build_one (insn : SM.insn) = 
  match insn with
  | BINOP op ->
    let () = print_endline (">>> Binary operator: " ^ op) in
    let () = print_module () in
    let a_value  = List.hd !variables_stack in
    let _ = (variables_stack := List.tl !variables_stack) in
    let _ = print_endline @@ "a_value:  " ^ Llvm.string_of_llvalue a_value in
    let b_value  = List.hd !variables_stack in
    let _ = (variables_stack := List.tl !variables_stack) in
    let _ = print_endline @@ "b_value:  " ^ Llvm.string_of_llvalue a_value in
    let operand, op_name =
    match op with
      | "+" -> (LL_MAIN.build_add, "add")
      | "-" -> (LL_MAIN.build_sub, "sub")
      | "/" -> (LL_MAIN.build_sdiv, "div")
      | "*" -> (LL_MAIN.build_mul, "mul")
      | ">" -> (LL_MAIN.build_Sgt, "sgt")
      | "<" -> (LL_MAIN.build_Slt, "slt")
      | _ ->
          Format.kasprintf failwith
            "Only +,/,*,- are supported by now but %s appeared" op
    in
    (* TODO(for Danya): get rid of names *)
    let temp = operand a_value b_value  ~name:(op_name ^ "tmp") in
    ignore (LL_MAIN.build_inttoptr temp lama_ptr_type ~name:(op_name ^ "tmp1"))
  | CONST i ->
    let () = print_endline @@ ">>> Constant: " ^ (string_of_int i) in
    let int_val = Llvm.const_int (Llvm.i32_type context) i in
    variables_stack := int_val :: !variables_stack;
    print_module ()
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
      show_desig desig
      (* print_endline "Not implemented\n" *)
  | LDA desig ->
      print_endline ">>> Load variable address";
      show_desig desig
      (* print_endline "Not implemented\n" *)
  | ST desig ->
      (* handle store variable *)
      print_endline ">>> Store variable";
      show_desig desig
      (* print_endline "Not implemented\n" *)
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
    if (!cur_state == FUNCTION_DEFINTION) then
      let () = print_endline (">>> delayed label: " ^ s) in 
      waiting_labels := s :: !waiting_labels 
    else 
      let () = print_endline (">>> Label: " ^ s) in
      let current_function = Llvm.block_parent (Llvm.insertion_block builder) in
      let block =  Llvm.append_block context s current_function in
      my_map := BlockMap.add s block !my_map
      
    | FLABEL s ->
      (* handle forwarded label *)
      print_endline (">>> Forwarded label: " ^ s)
      (* print_endline "Not implemented\n" *)
  | SLABEL s ->
      (* handle scope label *)
      print_endline (">>> Scope label: " ^ s)
      (* print_endline "Not implemented\n" *)
  | JMP s ->
    let () = print_endline (">>> Unconditional jump: " ^ s) in
    (* let _ =  Llvm.build_br (BlockMap.find s !my_map) builder in *)
      print_endline ">>> Done\n"
  | CJMP (cond, s) ->
      (* handle conditional jump
      let current_function = Llvm.block_parent (Llvm.insertion_block builder) in
      let before_bb = Llvm.append_block context ("before" ^ (string_of_int !counter)) current_function in
      let after_bb = Llvm.append_block context ("after" ^ (string_of_int !counter)) current_function in
      let _ = (counter := !counter + 1) in 
      let _ = Llvm.position_at_end before_bb builder in
      let branch_instr = Llvm.build_cond_br (int_of_string cond) (BlockMap.find s !my_map) after_bb builder in *)

      print_endline (">>> Conditional jump: " ^ cond ^ " " ^ s)
      (* print_endline "Not implemented\n" *)
  | BEGIN (name, nargs, nlocals, args, frees, scopes) ->
    clean_stack ();
    (match name with
    | "main" ->
      (* handle procedure definition *)
      let () = cur_module := main_module in 
      let func = Llvm.define_function name (Llvm.function_type (Llvm.void_type context) [| |]) !cur_module in 
      
      (* Create basic block and builder *)
      let entry = Llvm.append_block context "entry" func in
      let _ = (my_map := BlockMap.add name entry  !my_map) in
      let () = Llvm.position_at_end entry builder in 
      let () = print_endline (">>> Begin procedure definition: " ^ name ^ " " ^ string_of_int nargs ^ " " ^ string_of_int nlocals ) in
      show_desig_list args;
      print_endline ">>> frees";
      List.iter print_endline frees;
      List.iter print_scope scopes
      (* print_endline "Not implemented\n" *)
      | _ ->
      let arguments_array = Array.make nargs @@ Llvm.i32_type context in 
      let func = Llvm.define_function name (Llvm.function_type (Llvm.void_type context) arguments_array) !cur_module in 
      (* Create basic block and builder *)
      let () = create_argument_allocas func in
      let entry = Llvm.append_block context "entry" func in
      let _ = (my_map := BlockMap.add name entry  !my_map) in
      let () = Llvm.position_at_end entry builder in 
      let () = print_endline (">>> Begin procedure definition: " ^ name ^ " " ^ string_of_int nargs ^ " " ^ string_of_int nlocals ) in
      show_desig_list args;
      print_endline ">>> frees";
      List.iter print_endline frees;
      List.iter print_scope scopes);
      print_module ()
  | END ->
      (* handle end procedure definition *)
      cur_state := None;
      print_endline ">>> End procedure definition";
      print_module ()
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
    let () = print_endline (">>> Call function/procedure " ^ func_name ^ " with arity " ^ string_of_int arity ^ " (tail call: " ^ string_of_bool is_tail_call ^ ")") in    
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
          let args_type = List.map (function x -> Llvm.type_of x) !args in
          let args = Array.of_list !args in
          let args_type = Array.of_list args_type in
          let func = Llvm.declare_function func_name (Llvm.function_type i32_type args_type) main_module in
          let var = Llvm.build_call func args (string_of_int !counter) builder in 
          let () = counter := !counter + 1 in
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
        (* print_endline "Not implemented\n" *)
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
  print_list insns;
  Llvm.dump_module main_module;
  Llvm.dump_module function_module

let%test _ = true
