type scope = SM.scope
open Constants
let scope_stack = [] 
let waiting_labels : string list ref = ref [] 
let latest_line_info = ref ""
let builder = Llvm.builder context
(* let integer_type = Llvm.double_type context *)
let () = assert (Llvm_executionengine.initialize ())
let main_module = Llvm.create_module context "main"
let the_execution_engine = Llvm_executionengine.create main_module
let the_fpm = Llvm.PassManager.create_function main_module

module LL = (val LL.make builder context main_module)

module StringMap = Map.Make(String)
module IntMap = Map.Make(Int)
module StringSet = Set.Make(String)

let () =
  Llvm.enable_pretty_stacktrace ()
  let the_fpm = Llvm.PassManager.create_function main_module

  let dump_to_object ~the_fpm =
    Llvm_all_backends.initialize ();
    (* "x86_64-pc-linux-gnu" *)
    let target_triple = Llvm_target.Target.default_triple () in
    let target = Llvm_target.Target.by_triple target_triple in
    let cpu = "generic" in
    let reloc_mode = Llvm_target.RelocMode.Default in
    let machine =
      Llvm_target.TargetMachine.create ~triple:target_triple ~cpu ~reloc_mode
        target
    in
    (* Printf.printf "%s %d\n%!" __FILE__ __LINE__; *)
    let data_layout =
      Llvm_target.TargetMachine.data_layout machine
      |> Llvm_target.DataLayout.as_string
    in
    Llvm.set_target_triple target_triple main_module;
    Llvm.set_data_layout data_layout main_module;
    let filename = "output.o" in
    Llvm_target.TargetMachine.add_analysis_passes the_fpm machine;
    let file_type = Llvm_target.CodeGenFileType.ObjectFile in
    (* Printf.printf "%s %d\n%!" __FILE__ __LINE__; *)
    Llvm_target.TargetMachine.emit_to_file main_module file_type filename machine;

    let filename = "output.ll" in 
    Llvm.print_module filename main_module;
    print_endline ("LLVM module written to: " ^ filename)
    
    (* printf "Wrote %s\n" filename; *)
  
type variable = 
    | Ptr of variable 
    | Int
    | String of Llvm.lltype
    | Sexp of variable list * int
    | Array of variable list * int
    | List of variable list * int
    | Closure
(* module SignatureMap = Map.Make(struct type t = (string * variable list) let compare = compare end) *)

(* 
let rec create_lltype (var_type : variable) = 
  match var_type with
| Ptr x -> Llvm.pointer_type @@ create_lltype x
| Int -> i32_type 
| String x -> x
| Array (x, y) -> Llvm.array_type i8_ptr_type y
| Sexp (y, z) -> Llvm.array_type i8_ptr_type (z + 1)
| _ -> failwith "Closure/List not implemented"


let create_variable_ptr (var_type : variable) =
  Llvm.pointer_type @@ create_lltype var_type *)

(* let create_function_signature (ret : variable) (vars : variable list) = 
  let ar = ref [] in 
  List.iter (function x -> ar := create_lltype x :: !ar) vars;
  Llvm.function_type (create_lltype ret) @@ Array.of_list !ar *)

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
  | Ptr of Llvm.llvalue
  | Value of Llvm.llvalue

let print_variable_type (var_t :variable_type) = 
  match var_t with 
  | Ptr x -> print_endline @@ "Ptr " ^ Llvm.value_name x
  | Value x -> print_endline @@ "Value " ^ Llvm.value_name x

type namespace_type = 
  | Scope
  | Function
  | Global

type scope_t =
  | Global
  | Local

let counter = ref 1


let create_entry_block_alloca var_name func =
  let builder =
    Llvm.builder_at context (Llvm.instr_begin (Llvm.entry_block func))
  in
  Llvm.build_alloca int_type var_name builder

let get_name () = 
  let () = counter := !counter + 1 in 
  string_of_int @@ !counter - 1
  
class label_c (label : string) (block : Llvm.llbasicblock)  = object 
  val mutable stack_fullness : int option = None
  val mutable  option = None 
  method set_depth (n: int) = 
    stack_fullness <- Some n

  method get_depth = 
    match stack_fullness with 
    | Some x -> x 
    | None -> failwith "Unreachable code"
  method get_name = label
  method assert_depth c =
    match stack_fullness with 
    |  Some x -> if x != c then failwith "Different depths" else ()
    | None -> stack_fullness <- Some c 
  method get_block = block
  method is_set = not (stack_fullness = None)
end

class function_c (name:string)  (func : Llvm.llvalue) (args : int) = object (self)
  val mutable was_opcode = false
  (* val mutable free_ptr : Llvm.llvalue list = [] *)
  (* val mutable stack : Llvm.llvalue list = [] *)
  val mutable rstack : Llvm.llvalue = zero
  val mutable stack_fullness = 0
  (* val mutable type_stack : variable list = [] *) 
  val mutable locals = IntMap.empty
  val mutable locals_type = IntMap.empty 
  val mutable nargs = 0
  val mutable labels_map = StringMap.empty

  (* val mutable depth = 0 *)
  val mutable reachable = true

  method set_nargs n =
    nargs <- n 

  method get_closure_variables = 
    Llvm.param func 0 
  
  method get_param i = 
    Llvm.param func @@ i + 1 

  method set_reachable s = 
    reachable <- s

  method get_args_number = args
  method is_reachable = reachable
  method get_opcode = was_opcode
  method set_opcode s = 
    was_opcode <- s
  method get_name = name
  method get_function = func
  method set_depth n =
    while (stack_fullness < n) do 
      ignore @@ self#get_free
    done;
    while (stack_fullness > n) do 
      ignore @@ self#drop
    done

  method get_free = 
    print_endline "get_free called";
    stack_fullness <- stack_fullness + 1;
    Llvm.build_gep rstack [| Llvm.const_int int_type @@ stack_fullness - 1 |] (get_name()) builder
    (* depth <- depth +  1;
    match free_ptr with 
    hd :: tl -> 
      free_ptr <- tl;
      stack <- hd :: stack;
      hd
    | [] -> 
      let alloca = create_entry_block_alloca (get_name()) func in
      stack <- alloca :: stack;
      alloca *)

  (* method get_taken = 
    print_endline "get_taken_ptr called";
    match stack with 
    | hd :: _ ->
      self#drop;
      hd
    | [] -> failwith "No values on ptr stack (get)" *)

  method get_front = 
    print_endline "get_front called";
    Llvm.build_gep rstack [| Llvm.const_int int_type (stack_fullness - 1) |] (get_name()) builder
    (* match stack with 
    | hd :: _ ->
      hd
    | [] -> failwith "No values on ptr stack (get)" *)


  method drop = 
    (* match stack with 
    | hd :: tl ->
      stack <- tl;
      free_ptr <- hd :: free_ptr;
      depth <- depth - 1
    | [] -> failwith "No values on ptr stack (get)" *)
    stack_fullness <- stack_fullness - 1;
    assert (stack_fullness >= 0)

  method create_stack size = 
    let builder = Llvm.builder_at context (Llvm.instr_begin (Llvm.entry_block func)) in
    rstack <- Llvm.build_array_alloca int_type (Llvm.const_int int_type size) (get_name()) builder 

  method load (value : Llvm.llvalue)  = 
    let ptr = self#get_free in 
    Llvm.build_store value ptr builder

  method load_from_ptr (value : Llvm.llvalue) =
    let value = Llvm.build_load value (get_name ()) builder in 
    Llvm.build_store value (self#get_free) builder

  method store = 
    let ptr = self#get_front in 
    let stored = Llvm.build_load ptr (get_name ()) builder in 
    stored

  method store_and_drop_n  n = 
    stack_fullness <- stack_fullness - n;
    Llvm.build_gep rstack [| Llvm.const_int int_type @@ stack_fullness |] (get_name()) builder


  method get_local_ptr name = 
    if not @@ IntMap.mem name locals then (
    let builder = Llvm.builder_at context (Llvm.instr_begin (Llvm.entry_block func)) in
    let value = Llvm.build_alloca int_type (get_name ()) builder in 
    locals <- IntMap.add name value locals );
      IntMap.find name locals
    
  method load_local (name : int) (value : Llvm.llvalue)   =
    (* (val_type : variable)  *)
    let ptr = self#get_local_ptr name in 
    (* locals_type <- IntMap.add name val_type locals_type; *)
    Llvm.build_store value ptr builder
  
  method store_local (name : int) = 
    let ptr = self#get_local_ptr name in
    let value = Llvm.build_load ptr (get_name ()) builder in 
    value 

  method dup = 
    let value = self#store in 
    ignore @@ self#load value

  method swap = 
      let a = self#store in 
      self#drop;
      let b = self#store in 
      self#drop;
      ignore @@ self#load a;
      ignore @@ self#load b
      
  method get_depth = stack_fullness

  method update_label_depth (name:string) (depth: int) = 
    let label = StringMap.find name labels_map in 
    let () = label#set_depth stack_fullness in
    labels_map <- StringMap.update name  (fun _ -> Some label) labels_map

  method add_label (label : label_c) =
    labels_map <- StringMap.add label#get_name label labels_map
  method get_all_labels = labels_map
  method has_label (name : string) =
    StringMap.mem name labels_map
  method get_label (name : string) =
    StringMap.find name labels_map 


(* 
  method get_argument_type (n : int) =
    List.nth signature n  *)
  
end 

(* and parent_c (x : function_c option) (y : scope_c option) = 
object 
  method get_parent_c = 
    match x with
    | Some func -> Function 
    | None -> 
      (match y with 
      | Some scope -> Scope
      | None -> Global)
  method get_function = 
    match x with
    | Some x -> x
    | None -> failwith "Parent class is not a function"
  method get_scope = 
    match y with 
    | Some y -> y
    | None -> failwith "Parent class is not a scope"
  end *)

(* let global_scope = new global_c
let create_function_parent (func : function_c) = new parent_c (Some func) None 
let create_scope_parent (scope : scope_c) = new parent_c None (Some scope) 
let create_global_parent () = new parent_c None None *)

(* type container_t = 
  | Scope of scope_c
  | Function of function_c
  | Global of global_c *)
(* 
let parent_to_con (p : parent_c) = 
  match p#get_parent_c with
  | Scope -> Scope p#get_scope
  | Function -> Function p#get_function
  | Global -> Global global_scope *)

(* let add_variable (var: variable_c) (con : container_t) =
  match var#get_type with
  | Local | Arg -> (match con with 
  Scope x -> x#add_variable var | Function x -> x#add_variable var)
  | Global ->
      (get_global_scope (to_common con))#add_variable var
  | _ -> ()
 *)


(* let current_con = ref @@ Global global_scope *)

(* let current_function () = 
  let rec go_up (con : container_t) : function_c= 
    match con with 
    | Function x -> x 
    | Scope x -> (
      match x#get_parent#get_parent_c with 
      | Scope -> go_up @@ Scope x#get_parent#get_scope 
      | Function -> x#get_parent#get_function
      | Global  -> failwith "Not in function"
    )
    | Global _ -> failwith "Came to global while looking for function"
   in 
    go_up !current_con
  
let create_parent_c () = 
  match !current_con with 
    | Scope x -> create_scope_parent x 
    | Function y -> create_function_parent y
    | Global x -> create_global_parent ()

let get_scope x scope = 
  match x with 
  | Global _ -> failwith "Looking for scope in global scope"
  | Function x -> x#get_scope scope 
  | Scope x -> x#get_scope scope

let as_scope x msg= 
  match x with 
  | Scope x -> x 
  | Function _ -> failwith @@ "Trying to cast function to scope in " ^ msg
  | Global _  -> failwith @@ "Trying to cast global to scope in " ^ msg

let as_function x msg = 
  match x with 
  | Scope _ -> failwith @@ "Trying to cast scope to function in " ^ msg
  | Function x -> x
  | Global _  -> failwith @@ "Trying to cast global to function in " ^ msg


let as_global x msg = 
  match x with 
  | Scope _ -> failwith @@ "Trying to cast scope to global in " ^ msg
  | Function _ -> failwith @@ "Trying to cast function to global in " ^ msg
  | Global x  -> x  *)

(* let find_label s =
  let rec go_up scope s = 
    match scope with 
    | Function x -> failwith "Looking for a label in a function"
    | Scope x -> 
      (match x#has_label s with 
        | true -> x#get_label s 
        | false -> go_up (parent_to_con x#get_parent) s)
    | Global _  -> failwith "Looking for a label in Global" in
  go_up !current_con s
 *)

 (* let rec _find_variable variable_name (scope: scope_c) = 
  match scope#has_variable variable_name with 
    | true -> scope#get_variable variable_name
    | false ->  let parent = scope#get_parent in 
                _find_variable variable_name (match parent#get_parent_c with 
                                              | Scope -> parent#get_scope 
                                              | Function -> failwith "Can not find variable"
                                              | Global -> failwith "This message should not exist")  *)

(* let is_in_function () = 
  match !current_con with
  | Global _ -> true
  | _ -> false

let rec print_function_c (func : function_c) =
    print_endline @@ "function: " ^ func#get_name;
    StringMap.iter (function _ -> function value ->  print_scope_c value) func#get_all_scopes;
and
  print_scope_c (scope : scope_c) =
  let print_key key _ = 
    print_endline key in
  (* print_endline "Variables: ";
  StringMap.iter print_key scope#get_all_locals; *)
  print_string "Scope \nblab:/";
  print_endline @@ scope#get_blab ^ "/";
  (* print_endline @@ "Scope is if: " ^ string_of_bool scope#is_if;  *)
  print_string "elab:";
  print_endline scope#get_elab;
  print_endline "Labels: ";
  StringMap.iter print_key scope#get_all_labels;
  print_endline "Scopes: ";
  StringMap.iter (function _ -> function value ->  print_scope_c value) scope#get_all_scopes;
 
and
  print_global_scope () = StringMap.iter (function _ -> function value -> print_function_c value ) global_scope#get_all_functions *)

    (* called only on scopes *)
(* let go_up () = 
  let scope = (as_scope !current_con "Scope/go_up") in
  let as_value value = 
    match value with 
      | Ptr _ -> failwith "casting Ptr to Value"
      | Value x -> x in 
  let parent = scope#get_parent in
                (match parent#get_parent_c with 
                  | Scope -> 
                    let parent_scope = parent#get_scope in
                    let values = ref [] in
                    while (not scope#is_empty) do
                      let value = ref scope#get_from_stack_without_cast in
                      (match !value#get_value with 
                      Value x ->
                      if (!value#get_parent#get_scope#get_elab = scope#get_elab) then
                        let ptr = Llvm.build_store (as_value !value#get_value) ((current_function ())#get_free_ptr) builder in
                        value := new variable_c (Ptr ptr) (create_function_parent (current_function ()))                    
                      | Ptr _ -> ());
                      values := !value :: !values
                    done;
(* 
                    if (List.length !values > 1) then ( 
                      List.iter (function x -> print_variable_type x#get_value) !values;
                      failwith "More than 1 value in closing scope stack"); *)
                    current_con := Scope parent_scope;
                    if not (parent_scope#is_if) then 
                    List.iter (function x ->  parent_scope#add_to_stack x) !values
                    else 
                      (values := List.rev !values;
                      match (List.hd !values)#get_value with 
                      Ptr x -> 
                      let value =   (match parent_scope#get_return_ptr#get_value with 
                      | Ptr x -> Llvm.build_load x (get_name ()) builder
                      | Value x -> x  ) in
                      ignore @@ Llvm.build_store  x  value builder
                      | Value _ -> failwith "Not a ptr in stack in in scope") 
                  | Function -> 
                    let parent_function = parent#get_function in
                    let values = ref [] in
                    while (not scope#is_empty) do
                      let value = ref scope#get_from_stack_without_cast in
                      (match !value#get_value with
                      | Ptr x -> ()
                      | Value x -> 
                        if not ( !value#get_parent#get_scope#get_elab = (as_scope !current_con "Scope/go_up")#get_elab) then 
                          failwith "Value in last scope, whose parent is not this scope"
                        else 
                          let ptr = Llvm.build_store (as_value !value#get_value) ((current_function ())#get_free_ptr) builder in
                          value := new variable_c (Ptr ptr) (create_function_parent (current_function ())));
                          values := !value :: !values   
                    done;
                    if (List.length !values < 1) then failwith "No values in function scope stack";
                    current_con := Function parent_function
                  | Global -> current_con := Global global_scope) *)
  

(* let find_local name =
  let rec _find_local name con = 
  let scope = (as_scope con "Scope/find_local") in
  if scope#has_local name then 
    scope#get_local name 
  else 
    _find_local name (parent_to_con scope#get_parent) in 
  _find_local name !current_con *)

(* let rec current_elab (con : container_t) = 
  match con with
  | Scope x -> x#get_elab
  | Function _ -> failwith "Not in scope"
  | Global _  -> failwith "Not in scope" *)

(* let store_in_local name = 
  let func = current_function() in
  if not @@ func#has_local name then 
    func#add_local name (create_entry_block_alloca (get_name()) func#get_function);
  let value = func#store in
  func#store_in_local name value *)

(* let load_local name = 
  let func = current_function() in
  let temp = func#get_local_value name in
  func#load temp *)

  
(* and variable_c  (value : variable_type)  (parent: parent_c)  = 
object
  method get_value = value  
  method get_parent = parent
end  *)

(* and common_c (parent : parent_c) ?(scope_t=Local) (t : namespace_type)  = 
object (self)

  val mutable labels_map = StringMap.empty
  val mutable inner_scopes : scope_c StringMap.t = StringMap.empty 
  val mutable locals = IntMap.empty
  val mutable reachable = true

  method set_reachable s = 
    reachable <- s

  method is_reachable = reachable
  
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
  
  method add_local name ptr = 
    locals <- IntMap.add name ptr locals

  method get_local_ptr name = 
    IntMap.find name locals

  method has_local (name : int) = 
    IntMap.mem name locals  

  (* не проверяет на наличие и, в случае чего, просто падает *)
  method store_in_local (name : int) (value : Llvm.llvalue) = 
    Llvm.build_store value (self#get_local_ptr name) builder 
  
  method get_local_value name = 
    Llvm.build_load (self#get_local_ptr name) (get_name ()) builder

  method set_inner_scopes (array : scope_c list)  = 
    List.iter (function x -> self#add_scope x) array 

  method get_parent = parent
  method add_scope (scope : scope_c) =
    print_endline @@ "adding blab " ^ scope#get_blab; 
    inner_scopes <- StringMap.add scope#get_blab scope inner_scopes
  method add_label (label : label_c) =
    labels_map <- StringMap.add label#get_name label labels_map

  method get_all_labels = labels_map

  method get_all_scopes = inner_scopes
  method has_label (name : string) =
    StringMap.mem name labels_map
  method has_scope (name: string) =
    StringMap.mem name inner_scopes  
  method get_label (name : string) =
    StringMap.find name labels_map 

  method get_scope (blab : string) =
    StringMap.find blab inner_scopes
end

and scope_c  (s : scope) ?(scope_t= Local) (parent : parent_c) =
object (self)
  inherit common_c parent Scope ~scope_t:scope_t
  (* val mutable locals_ptr_map =  IntMap.empty
  val mutable locals_map = IntMap.empty *)
  val mutable stack : variable_c list = []
  val mutable stack_set = false
  (* method get_all_locals = locals_map *)

  method add_to_stack (value : variable_c) = 
    stack <- value :: stack
  
  method print_stack =
    let rec print_stack_ stack =
    match stack with 
    | hd :: tl -> print_variable_type hd#get_value 
     | [] -> () 
    in 
    print_endline @@ "Stack of " ^ s.blab;
    print_stack_ stack
  
  method front  = 
    match stack with 
    | hd :: tl -> hd 
     | [] -> failwith "No values on stack for front"
  
  method get_from_stack = 
    match stack with 
    | hd :: tl -> 
      stack <- tl;
      (match hd#get_value with 
      |Ptr x -> 
        Llvm.build_load x (get_name ()) builder 
      | Value x -> x
      )
    | [] -> failwith "No values on stack to get"
  
  method get_from_stack_without_cast = 
    match stack with 
    | hd :: tl -> 
      stack <- tl;
      hd
    | [] -> failwith "No values on stack to get"
    

  method is_empty : bool =
    match stack with 
    | [] -> true
    | _ -> false

  method get_elab = s.elab 
  method get_blab = s.blab
end  *)