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

type scope = SM.scope

module BlockMap = Map.Make(String)

let rec print_names = function
| [] -> ()
| (name, value)::rest ->
    Printf.printf "%s: %d\n" name value;
    print_names rest


let rec print_scope (scope:scope) =
  Printf.printf "blab: %s\n" scope.blab;
  Printf.printf "elab: %s\n" scope.elab;
  print_names scope.names;
  List.iter print_scope scope.subs

  type variable_type = 
  | Local
  | Global
  | Function 
  | Arg 
  | Label


class variable_class (name : string) (value : Llvm.llvalue) (variable_type : variable_type) (parent: Llvm.llvalue) = 
  object
    method get_name = name
    method get_type = variable_type
    method get_value = value  
    method get_parent = parent
  end

type namespace_type = 
  | Scope
  | Function
  | None

type scope_t =
  | Global
  | Local

class type common_class_t = object
  val mutable functions_map : function_class_t BlockMap.t
  val mutable inner_scopes : scope_class_t BlockMap.t
  val mutable labels_map : Llvm.llbasicblock BlockMap.t
  val mutable locals_map : variable_class BlockMap.t
  method is_scope : bool 
  method add_variable : variable_class -> unit
  method add_function : function_class_t -> unit
  method add_label : Llvm.llbasicblock -> string -> unit
  method add_scope : scope_class_t -> unit
  method get_all_functions : function_class_t BlockMap.t
  method get_all_labels :  Llvm.llbasicblock BlockMap.t
  method get_all_locals :  variable_class BlockMap.t
  method get_all_scopes : scope_class_t BlockMap.t
  method get_label : string -> Llvm.llbasicblock
  method get_parent : parent_class_t
  method get_variable : string -> variable_class
  method get_scope : string -> scope_class_t
  method has_label : string -> bool
  method has_scope : string -> bool
  method has_variable : string -> bool
  method is_function : bool
  method is_global : bool
  method is_scope : bool
  method set_inner_scopes : scope_class_t list -> unit    
  method find_function : string -> function_class_t

end
and scope_class_t = object
  method get_blab : string
  method get_elab : string
  val mutable functions_map : function_class_t BlockMap.t
  val mutable inner_scopes : scope_class_t BlockMap.t
  val mutable labels_map : Llvm.llbasicblock BlockMap.t
  val mutable locals_map : variable_class BlockMap.t
  method add_label : Llvm.llbasicblock -> string -> unit
  method add_variable : variable_class -> unit
  method add_function : function_class_t -> unit
  method add_scope : scope_class_t -> unit
  method add_function : function_class_t -> unit
  method get_all_functions : function_class_t BlockMap.t
  method get_all_labels :  Llvm.llbasicblock BlockMap.t
  method get_all_locals :  variable_class BlockMap.t
  method get_all_scopes : scope_class_t BlockMap.t
  method get_label : string -> Llvm.llbasicblock
  method get_scope : string -> scope_class_t
  method get_parent : parent_class_t
  method get_variable : string -> variable_class
  method has_label : string -> bool
  method has_scope : string -> bool
  method has_variable : string -> bool
  method is_function : bool
  method is_global : bool
  method is_scope : bool
  method set_inner_scopes : scope_class_t list -> unit 
  method find_function : string -> function_class_t
end
and function_class_t = object 
  val mutable functions_map : function_class_t BlockMap.t
  val mutable inner_scopes : scope_class_t BlockMap.t
  val mutable labels_map : Llvm.llbasicblock BlockMap.t
  val mutable locals_map : variable_class BlockMap.t
  method add_label : Llvm.llbasicblock -> string -> unit
  method add_scope : scope_class_t -> unit
  method add_function : function_class_t -> unit
  method add_variable : variable_class -> unit
  method get_all_functions : function_class_t BlockMap.t
  method get_all_labels :  Llvm.llbasicblock BlockMap.t
  method get_all_locals :  variable_class BlockMap.t
  method get_all_scopes : scope_class_t BlockMap.t
  method get_function : Llvm.llvalue
  method get_label : string -> Llvm.llbasicblock
  method get_name : string
  method get_parent : parent_class_t
  method get_variable : string -> variable_class
  method get_scope : string -> scope_class_t
  method has_label : string -> bool
  method has_variable : string -> bool
  method has_scope : string -> bool
  method is_function : bool
  method is_global : bool
  method is_scope : bool
  method set_inner_scopes : scope_class_t list -> unit
  method find_function : string -> function_class_t
end 
and parent_class_t = object
  method get_common_class : common_class_t
  method get_function : function_class_t
  method get_parent_class : namespace_type
  method get_scope : scope_class_t
 end

class common_class (parent : parent_class option) ?(scope_t= Local) (t : namespace_type) : common_class_t  = 
object (self)
  method get_parent = 
    match parent with
    | None -> failwith "No parent"
    | Some x -> x
  val mutable inner_scopes : scope_class_t BlockMap.t = BlockMap.empty 
  val mutable functions_map : function_class_t BlockMap.t = BlockMap.empty  
  val mutable locals_map =  BlockMap.empty
  val mutable labels_map = BlockMap.empty
  method is_scope : bool =
    match t with 
    | Scope -> true
    | Function -> false
    | _ -> failwith "common class without a type"
  method is_function : bool =
    match t with 
    | Scope -> false
    | Function -> true
    | _ -> failwith "common class without a type"

  method set_inner_scopes (array : scope_class_t list)  = 
    List.iter (function x -> self#add_scope x) array 
 
  method is_global = 
    match scope_t with 
    | Global -> true
    | Local -> false

    method add_scope (scope : scope_class_t) = 
      inner_scopes <- BlockMap.add scope#get_blab scope inner_scopes

    method add_function (func : function_class_t) = 
      functions_map <- BlockMap.add func#get_name func functions_map
    
    method get_all_labels = labels_map
    method get_all_locals = locals_map
    method get_all_scopes = inner_scopes
    method get_all_functions = functions_map

    method has_variable (name : string) = 
      BlockMap.mem name locals_map  
  
    method get_variable (name : string) = 
      BlockMap.find name locals_map
      
    method add_variable (var : variable_class) = 
      locals_map <- BlockMap.add var#get_name var locals_map

    method add_label (var: Llvm.llbasicblock) (name:string) =
      labels_map <- BlockMap.add name var labels_map
  
    method has_label (name : string) = 
      BlockMap.mem name labels_map
  
    method get_label (name : string) = 
      BlockMap.find name labels_map 

    method has_scope (name: string) = 
      BlockMap.mem name inner_scopes

    method find_function (name : string) = 
      BlockMap.find name functions_map 

    method get_scope (blab : string) = 
      BlockMap.find blab inner_scopes 
end 
and
scope_class  (s : scope) ?(scope_t= Local) (parent : parent_class option) : scope_class_t =
object (self)
  inherit common_class parent Scope ~scope_t:scope_t
  method get_elab = s.elab 
  method get_blab = s.blab
end and 
function_class (name:string) (parent : parent_class option) (func : Llvm.llvalue) : function_class_t = object (self)
  inherit common_class parent Function 
  method get_name = name
  method get_function = func
end 
and parent_class (x : function_class option) (y : scope_class option) : parent_class_t = 
object 
  method get_parent_class = 
    match x with
    | Some func -> Function 
    | None -> 
      (match y with 
      | Some scope -> Scope
      | None -> None)
  method get_function = 
    match x with
    | Some x -> x
    | None -> failwith "Parent class is not a function"
  method get_scope = 
    match y with 
    | Some y -> y
    | None -> failwith "Parent class is not a scope"
  method get_common_class : common_class =
    match x with
    | Some func -> (func :> common_class_t)
    | None ->   (match y with 
    | Some scope -> (scope :> common_class_t)
    | None -> failwith "No type in parent class")
  end
let global_scope : scope = {
  blab = "___start";
  elab = "___end";
  names = [];
  subs = [];
}


  let create_function_parent_class (func : function_class) = new parent_class (Some func) None 
let create_scope_parent_class (scope : scope_class) = new parent_class None (Some scope) 

type container_t = 
  | Scope of scope_class
  | Function of function_class

let parent_class_to_contatiner (p : parent_class) = 
  match p#get_parent_class with
  | Scope -> Scope p#get_scope
  | Function -> Function p#get_function
  | None -> failwith "No parent class"

let get_global_scope (con: common_class) = 
  let rec go_up scope = 
    if (scope#is_global) then scope else go_up @@ (scope#get_parent#get_common_class)
  in go_up (con)

let to_common (con : container_t) = 
  match con with 
  Scope x -> (x :> common_class_t) | 
  Function x -> (x :> common_class_t)

let add_variable (var: variable_class) (con : container_t)=
  match var#get_type with
  | Local | Arg -> (match con with 
  Scope x -> x#add_variable var | Function x -> x#add_variable var)
  | Global ->
      (get_global_scope (to_common con))#add_variable var
  | _ -> ()


let get_parent (s : common_class) = s#get_parent
let global_scope = new scope_class global_scope None ~scope_t:Global 

let rec create_scope (s : scope) (parent:parent_class option) = 
  let sclass = new scope_class s parent in 
  let array = ref [] in
  let parent_self = create_scope_parent_class sclass in
  List.iter (function x -> array := (create_scope x @@ Some parent_self) :: !array) s.subs;
  sclass#set_inner_scopes !array;
  sclass

let create_function (name : string) (parent : parent_class option) (func : Llvm.llvalue) (s : scope list) =
  let sclass = new function_class  name parent func in 
  let array = ref [] in
  let parent_self = create_function_parent_class sclass in
  List.iter (function x -> array := (create_scope x @@ Some parent_self) :: !array) s;
  sclass#set_inner_scopes !array;
  sclass
  
let current_con = ref @@ Scope global_scope 
let current_function () = 
  let rec go_up (con : container_t) : function_class= 
    match con with 
    | Function x -> x 
    | Scope x -> (
      match x#get_parent#get_parent_class with 
      | Scope -> go_up @@ Scope x#get_parent#get_scope 
      | Function -> x#get_parent#get_function
      | None -> failwith "Not in function"
    ) in 
    go_up !current_con

let go_up () = 
  let common = to_common !current_con in
  let parent = common#get_parent in
  match parent#get_parent_class with 
    | Scope -> current_con := Scope parent#get_scope
    | Function -> current_con := Function parent#get_function
    | None -> failwith "No parent"

let create_parent_class () = 
  match !current_con with 
    | Scope x -> create_scope_parent_class x 
    | Function y -> create_function_parent_class y

let scope_stack = [] 
let waiting_labels : string list ref = ref [] 
let latest_line_info = ref ""
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

let variables_stack :  variable_class list ref = ref [] 

let counter = ref 1

let clean_stack () = variables_stack := []

  (* method contains_function name = 
  self#get_name == name || List.exists (function x -> x#is_function && x#get_name == name) inner_scopes 
method contains_function_deep name = 
  self#get_name == name || 
  List.exists (
    function x -> x#is_function && x#get_name == name ||
      x#contains_function_deep name
      ) inner_scopes  *)
(*  
  (* Not the best way *)
  method get_function_deep (name : string) = 
    let find (scope: scope_class) (name: string) =
      if (scope#get_name == name) then scope 
      else 
        let in_scope = List.find (function x -> x#contains_function_deep name) inner_scopes in 
        in_scope#get_function_deep name in
    find (self :> scope_class) name    

  method get_current_func = 
    match t with 
    | Scope -> self#get_parent#get_current_func 
    | Function f -> f 

  method get_func (name: string) = 
  let rec find (scope: scope_class) (name: string) = 
    if (scope#get_name == name) then scope 
    else let in_scope = List.find (function x -> x#is_function && x#get_name == name) inner_scopes in 
    find in_scope name in
  find (self :> scope_class) *)
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

let push_variable value =
  variables_stack := value :: !variables_stack
   
let add_variable (variable : variable_class) = 
  match !current_con with 
    | Scope x -> x#add_variable variable
    | Function x -> x#add_variable variable

(* let create_argument_allocas the_function =
  ArrayLabels.iteri (Llvm.params the_function) ~f:(fun i ai ->
    let var_name = get_name () in
    (* Create an alloca for this variable. *)
    let alloca = create_entry_block_alloca the_function var_name in
    (* Store the initial value into the alloca. *)
    let _ = Llvm.build_store ai alloca builder in
    (* Add arguments to variable symbol table. *)
    let variable = new variable_class (get_name ()) alloca Arg (LL.current_function ()) in 
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
  let () =  List.iter (fun x -> print_endline x#get_name) !variables_stack in 
  let () = print_endline "\n" in
  Llvm.dump_module main_module

  (* fails if label not found *)
let find_label s =
  let rec go_up (scope : common_class_t) s = 
    match scope#has_label s with 
    | true -> scope#get_label s 
    | false -> go_up (scope#get_parent#get_common_class) s in
  go_up (to_common !current_con) s

let rec _find_variable variable_name (scope: common_class_t) = 
  match scope#has_variable variable_name with 
   | true -> scope#get_variable variable_name
   | false -> _find_variable variable_name scope#get_parent#get_common_class 

let is_in_function () = (to_common !current_con)#is_global 

let get_global_scope_as_scope () = 
  let rec  matching (x : common_class) = 
    (match x#get_parent#get_parent_class with 
      | Scope ->  go_up @@ Scope x#get_parent#get_scope 
      | Function ->  go_up @@ Function x#get_parent#get_function
      | None -> failwith "Not in global function"
    ) and
    go_up (con : container_t) : scope_class = 
      match con with 
        | Function x ->  matching (x :> common_class) 
        | Scope x -> if (x#is_global) then x else matching (x:>common_class) in 
  go_up !current_con

  let rec print_common_class (com : common_class) = 
    let print_key key _ = 
      print_endline key in
    print_endline "Variables: ";
    BlockMap.iter print_key com#get_all_locals;
    print_endline "Labels: ";
    BlockMap.iter print_key com#get_all_labels;
    print_endline "Functions: ";
    BlockMap.iter (function _ -> function value ->  print_function_class value) com#get_all_functions;
    print_endline "Scopes: ";
    BlockMap.iter (function _ -> function value ->  print_scope_class value) com#get_all_scopes;
  and
    print_function_class (func : function_class) =
      print_endline @@ "function: " ^ func#get_name;
      print_common_class (func :> common_class)
  and
    print_scope_class (scope : scope_class) =
      print_string "Scope \nelab:";
      print_endline scope#get_elab;
      print_string "blab:";
      print_endline scope#get_blab;
      print_common_class (scope :> common_class)
  and
    print_global_scope () = print_scope_class @@ get_global_scope_as_scope ()

let find_variable variable_name =
  let scope = !current_con  in
  _find_variable variable_name @@ to_common scope
 
let rec current_elab (con : container_t) = 
  match con with
  | Scope x -> x#get_elab
  | Function x -> current_elab @@ parent_class_to_contatiner x#get_parent

(* let next_scope = match 
  !current_con with 
    | Scope x -> 
      let blab = x#get_blab in
      let parent = x#get_parent#get_common_class in 
      let t = ref 0 in
      let found = ref false in
      List.iter (function x ->
        if (x#get_blab == blab) then found := true;
        if (not !found && x#get_blab != blab) then t := !t + 1 else ()) parent#get_all_scopes;
      List.nth parent#get_all_scopes !t
      | Function x -> List.hd x#get_all_scopes *)

let define_functions_and_labels (insns:SM.prg) = 
  List.iter (function (x : SM.insn) -> 
    match x with
    | BEGIN (name, nargs, nlocals, closure, args, scopes) ->
      while (nargs >= !counter) do 
          counter := !counter + 1
      done;
      let () = print_endline "begin" in
      print_endline @@ "name: " ^ name ^ string_of_int nargs ^ string_of_int nlocals;
      show_desig_list closure;
      List.iter (function x -> print_string x) args;
      List.iter (function x -> print_scope x) scopes;
      (* Create basic block and builder *)
      let arguments_array = Array.make nargs @@ Llvm.i32_type context in 
      let llvm_func = Llvm.define_function name (Llvm.function_type (Llvm.i32_type context) arguments_array) !cur_module in 
      let entry = Llvm.entry_block llvm_func in
      let () = Llvm.position_at_end entry builder in
      (* Create inner map for variables*)
      let new_function = create_function name (Some (create_parent_class ())) llvm_func scopes in
      print_function_class new_function;
      (to_common !current_con)#add_function new_function;
      current_con := Function new_function;
      (to_common !current_con)#add_label entry "entry";
      (* create_argument_allocas llvm_func *)
    | LABEL s | FLABEL s -> 
      let () = print_endline @@ ">>> Label " ^ s in
      if ((to_common !current_con)#is_global ) then () else
      (* let () = (match !current_scope with
      | Scope x -> print_scope_class x
      | Function x -> print_function_class x) in *)
      let llvm_function = Llvm.block_parent (Llvm.insertion_block builder) in
      let function_name = Llvm.value_name llvm_function in
      let () = print_endline function_name in
      let block =  Llvm.append_block context s llvm_function in
      let () = Llvm.position_at_end block builder in
      (to_common !current_con)#add_label block s
    | SLABEL s -> 
      let () = print_endline @@ ">>> scope label " ^ s in
      if ((to_common !current_con)#has_label s) then (
        (match  !current_con with
        | Scope x -> if (s == x#get_elab) then go_up () else (
          current_con := Scope ( (to_common !current_con)#get_scope s )
        )
        | Function x -> current_con  := Scope (x#get_scope s)))
    | END -> 
      let () = print_endline "end" in
      current_con :=  Scope ( get_global_scope_as_scope ())
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
    let a_value  = pop_variable
   () in
    let _ = print_endline @@ "a_value:  " ^  (a_value#get_name) in
    let b_value  = pop_variable
   () in
    let _ = print_endline @@ "b_value:  " ^ a_value#get_name in
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
    let a_value = a_value#get_value in 
    let b_value = b_value#get_value in
    let name = get_name () in
    let temp = operand a_value b_value  ~name:(name) in
    push_variable @@ new variable_class name temp Local (LL.current_function ())
    (* push_variable @@ LL.build_inttoptr temp lama_ptr_type ~name:(op_name ^ "tmp1") *)
  | CONST i ->
    let () = print_endline @@ ">>> Constant: " ^ (string_of_int i) in
    let int_val = Llvm.const_int (Llvm.i32_type context) i in
    push_variable @@ new variable_class "Const" int_val Local (LL.current_function ())
    | STRING s ->
    let () = print_endline (">>> String: " ^ s) in
    let string_val = Llvm.const_string  context s in
    push_variable @@ new variable_class "String" string_val Local (LL.current_function ())
  | SEXP (tag, size) ->
      (* handle S-expression *)
      print_endline (">>> S-expression: " ^ tag ^ " " ^ string_of_int size);
      print_endline ">>> Not implemented\n"
  | LD desig ->
      print_endline @@ ">>> Load variable";
      show_desig desig;
      (* let current_function = Llvm.block_parent (Llvm.insertion_block builder) in *)
      let current_function = current_function () in
      let llvm_function = current_function # get_function in
      (match desig with
      | Global s -> failwiths "globals load not implemented"
      | Local i -> push_variable @@ find_variable @@ string_of_int i
      | Arg i -> push_variable @@ new variable_class (string_of_int i)  ( Llvm.param llvm_function i) Arg llvm_function  
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
      | Global s  -> failwiths "globals load not implemented"
      | Local i   -> add_variable (pop_variable ()) 
      | Arg i     -> () 
      | Access i  -> failwiths "access load not implemented"
      | Fun s     -> failwiths "function load not implemented"
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
    if ((to_common !current_con) # is_global ) then () else
      let block = find_label s in
      Llvm.position_at_end block builder
  |  SLABEL s ->
    print_endline (">>> scope label: " ^ s);
    let () = print_endline @@ ">>> scope label " ^ s in
    if ((to_common !current_con)#has_label s) then (
      (match  !current_con with
      | Scope x -> if (s == x#get_elab) then (
        ignore @@ Llvm.build_ret ((pop_variable ())#get_value) builder;  
        go_up ())
       else (
        current_con := Scope ( (to_common !current_con)#get_scope s )
      )
      | Function x -> current_con  := Scope (x#get_scope s)))
    | JMP s ->
    let () = print_endline (">>> Unconditional jump: " ^ s) in
    let block = find_label s in 
    ignore @@ Llvm.build_br block builder 
  | CJMP (cond, s) ->
    let () = print_endline (">>> Conditional jump: " ^ cond ^ " " ^ s) in
    let current_function = LL.current_function () in
    let after_bb = Llvm.append_block context ("else" ^ get_name ()) current_function in
    let dest_block = find_label s in
    let cond_ = (pop_variable ())#get_value in
    let dest_block = find_label s in
    ignore (match cond with
      | "z" ->  
        Llvm.build_cond_br cond_  after_bb dest_block builder 
      | "nz" -> 
        Llvm.build_cond_br cond_  dest_block after_bb builder
      | _ -> failwiths "wtf is this cjmp"
      )
  | BEGIN (name, _, _, _, _, _) ->
    print_endline @@ "BEGIN" ^ name ; 
    (* clean_stack (); *)
    let llvm_func = LL.lookup_func_exn name in
    let entry = Llvm.entry_block llvm_func in 
    Llvm.position_at_end entry builder;
    let func =  (to_common !current_con)#find_function name in
    (* print_function_class func; *)
    current_con := Function func
  | END ->
    current_con := Scope (get_global_scope_as_scope ());
      (* handle end procedure definition *)
      print_endline ">>> End procedure definition";
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
          match !variables_stack with
            | hd::tl -> 
              let () = variables_stack := tl in 
              args := hd#get_value :: !args
            | [] -> Llvm.dump_module main_module;
              Format.kasprintf failwith "No arguments for function %s in variable stack" func_name
        done in 
      let args = Array.of_list !args in
      let name =  get_name () in
      let var = Llvm.build_call func args name builder in 
      push_variable @@ new variable_class name var Local (LL.current_function ())
    | RET ->
        print_endline ">>> Return from function";
        print_endline "Not implemented\n"
    | DROP ->
        print_endline ">>> Drop top element from stack";
        ( match !variables_stack with 
          | _ :: tl -> variables_stack := tl
          | [] -> failwith "Stack is empty"
        )    
    | DUP ->
      (* let _ = Llvm.build_load (Option.get stackptr) a builder in *)
      print_endline ">>> DUP";
      ( match !variables_stack with 
      | hd:: _ -> variables_stack := hd :: !variables_stack
      | [] -> failwith "Stack is empty"
      )
    | SWAP ->
      print_endline ">>> Swap";
      (match !variables_stack with 
      | hd:: mid :: tl -> variables_stack := mid :: hd :: tl
      | _ -> failwith "Not enough values in stack"
      )
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
    let () = build_one h in 
    print_list t
  in
  define_functions_and_labels insns;
  print_endline "functions defined";
  print_list insns;
  Llvm.dump_module main_module

let%test _ = true
