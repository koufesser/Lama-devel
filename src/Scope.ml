  type scope = SM.scope
let context = Llvm.global_context ()
let i64_type = Llvm.i64_type context
let i32_type = Llvm.i32_type context
let lama_int_type = i32_type
let lama_ptr_type = Llvm.pointer_type lama_int_type
let () =
  Llvm.enable_pretty_stacktrace ()

type prog =
  (string list
  * ([ `Lefta | `Nona | `Righta ]
    * string
    * [ `After of string | `At of string | `Before of string ])
    list)
  * Language.Expr.t

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

module BlockMap = Map.Make(String)
module LocalsMap = Map.Make(Int)

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
  Llvm.build_alloca lama_int_type var_name builder

let get_name () = 
  let () = counter := !counter + 1 in 
  string_of_int @@ !counter - 1
  
class global_c = object
  val mutable functions = BlockMap.empty
  method add_function (func : function_c) = 
    functions <- BlockMap.add func#get_name func functions
  method has_function (name : string) = 
    BlockMap.mem name functions
  method get_function (name : string) = 
    BlockMap.find name functions
  method get_all_functions = functions
      
end
and variable_c  (value : variable_type)  (parent: parent_c)  = 
object
  method get_value = value  
  method get_parent = parent
end and common_c (parent : parent_c) ?(scope_t= Local) (t : namespace_type)  = 
object (self)

  val mutable labels_map = BlockMap.empty
  val mutable inner_scopes : scope_c BlockMap.t = BlockMap.empty 
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

  method set_inner_scopes (array : scope_c list)  = 
    List.iter (function x -> self#add_scope x) array 
  
  method get_parent = parent
  method add_scope (scope : scope_c) =
    print_endline @@ "adding blab " ^ scope#get_blab; 
    inner_scopes <- BlockMap.add scope#get_blab scope inner_scopes
  method add_label (var: Llvm.llbasicblock) (name:string) =
      labels_map <- BlockMap.add name var labels_map
    
  method get_all_labels = labels_map
  method get_all_scopes = inner_scopes
  method has_label (name : string) = 
    BlockMap.mem name labels_map

  method has_scope (name: string) = 
      BlockMap.mem name inner_scopes
      
  method get_label (name : string) = 
    BlockMap.find name labels_map 
  method get_scope (blab : string) = 
      BlockMap.find blab inner_scopes 

end 
and
scope_c  (s : scope) ?(scope_t= Local) (parent : parent_c) =
object (self)
  inherit common_c parent Scope ~scope_t:scope_t

  val mutable locals_ptr_map =  LocalsMap.empty
  val mutable locals_map = LocalsMap.empty
  val mutable stack : variable_c list = []
  val mutable if_scope = false
  val mutable stack_set = false
  val mutable return_ptr : variable_c option = None
  method get_all_locals = locals_map

  method has_local (number : int) = 
    LocalsMap.mem number locals_ptr_map  
  method set_return_ptr ptr = 
    return_ptr <- ptr
  method get_return_ptr = 
    match return_ptr 
  with 
  None -> failwith " "
  | Some  x -> x 
  method is_return_ptr_set = return_ptr = None
  method is_stack_set =
    stack_set
  method set_stack = 
    stack_set <- true
  method set_if =
    if_scope <- true
  method is_if = if_scope
  method add_local (num : int )(var : Llvm.llvalue) = 
    locals_ptr_map <- LocalsMap.add num var locals_ptr_map

  method get_local (name : int) = 
    if (LocalsMap.mem name locals_map) then LocalsMap.find name locals_map 
    else Llvm.build_load (LocalsMap.find name locals_ptr_map)  (get_name ()) builder

  method store_local (value : Llvm.llvalue) (num : int) : unit = 
    ignore @@ Llvm.build_store value (LocalsMap.find num locals_ptr_map) builder;
    if (LocalsMap.mem num locals_map) then 
      locals_map <- LocalsMap.remove num locals_map 
    else ()

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
    
  method drop_from_stack =
    match stack with 
    | hd :: tl -> (stack <- tl)
    | [] -> failwith "No values on stack to drop"

  method is_empty : bool =
    match stack with 
    | [] -> true
    | _ -> false
       
  method dup =
    match stack with 
    | hd :: tl -> (stack <- hd::hd::tl)
    | [] -> failwith "No values on stack to dup"
  
  method swap = 
    match stack with 
    | hd:: mid :: tl -> stack <- mid :: hd :: tl
    | _ -> failwith "Not enough values in stack to swap"

  method get_elab = s.elab 
  method get_blab = s.blab
end 
and function_c (name:string) (parent : parent_c) (func : Llvm.llvalue) = object (self)
  inherit common_c parent Function 
  val mutable free_ptr : Llvm.llvalue list = []
  val mutable ptr_stack : Llvm.llvalue list = []
  method get_name = name
  method get_function = func

  method get_free_ptr = 
    print_endline "get_free_ptr called";
    match free_ptr with 
    hd :: tl -> 
      free_ptr <- tl;
      ptr_stack <- hd :: ptr_stack;
      hd
    | [] -> 
      let alloca = create_entry_block_alloca (get_name()) func in
      ptr_stack <- alloca :: ptr_stack;
      alloca

  method get_taken_ptr = 
    print_endline "get_taken_ptr called";
    match ptr_stack with 
      | hd :: _ ->
        self#drop;
        hd
      | [] -> failwith "No values on ptr stack (get)"
  method drop_ptr = 
    print_endline "drop_ptr_called";
    match ptr_stack with 
    | hd :: tl -> 
      ptr_stack <- tl;
      free_ptr <- hd :: free_ptr
    | [] -> failwith "No values on ptr stack (drop)"

  method load (value : Llvm.llvalue) builder= 
    Llvm.build_store value (self#get_free_ptr) builder
  
  
  method drop = 
    match ptr_stack with
    | hd::tl -> 
      ptr_stack <- tl;
      free_ptr <- hd :: free_ptr
    | [] -> failwith "No values on ptr stack (drop)"
  method dup = 
    match ptr_stack with
    | hd::tl -> 
      ptr_stack <- hd :: ptr_stack;
    | [] -> failwith "No values on ptr stack (dup)"
  method swap = 
    match ptr_stack with
    | hd::tl -> 
      ptr_stack <- hd :: ptr_stack;
    | [] -> failwith "No values on ptr stack (dup)"
end 

and parent_c (x : function_c option) (y : scope_c option) = 
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
  end

let global_scope = new global_c 

let create_function_parent (func : function_c) = new parent_c (Some func) None 
let create_scope_parent (scope : scope_c) = new parent_c None (Some scope) 
let create_global_parent () = new parent_c None None

type container_t = 
  | Scope of scope_c
  | Function of function_c
  | Global of global_c

let parent_to_con (p : parent_c) = 
  match p#get_parent_c with
  | Scope -> Scope p#get_scope
  | Function -> Function p#get_function
  | Global -> Global global_scope

(* let add_variable (var: variable_c) (con : container_t) =
  match var#get_type with
  | Local | Arg -> (match con with 
  Scope x -> x#add_variable var | Function x -> x#add_variable var)
  | Global ->
      (get_global_scope (to_common con))#add_variable var
  | _ -> ()
 *)


let current_con = ref @@ Global global_scope

let current_function () = 
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
  | Global x  -> x 

let find_label s =
  let rec go_up scope s = 
    match scope with 
    | Function x -> failwith "Looking for a label in a function"
    | Scope x -> 
      (match x#has_label s with 
        | true -> x#get_label s 
        | false -> go_up (parent_to_con x#get_parent) s)
    | Global _  -> failwith "Looking for a label in Global" in
  go_up !current_con s
    
(* let rec _find_variable variable_name (scope: scope_c) = 
  match scope#has_variable variable_name with 
    | true -> scope#get_variable variable_name
    | false ->  let parent = scope#get_parent in 
                _find_variable variable_name (match parent#get_parent_c with 
                                              | Scope -> parent#get_scope 
                                              | Function -> failwith "Can not find variable"
                                              | Global -> failwith "This message should not exist")  *)

let is_in_function () = 
  match !current_con with
  | Global _ -> true
  | _ -> false

let rec print_function_c (func : function_c) =
    print_endline @@ "function: " ^ func#get_name;
    BlockMap.iter (function _ -> function value ->  print_scope_c value) func#get_all_scopes;
and
  print_scope_c (scope : scope_c) =
  let print_key key _ = 
    print_endline key in
  (* print_endline "Variables: ";
  BlockMap.iter print_key scope#get_all_locals; *)
  print_string "Scope \nblab:/";
  print_endline @@ scope#get_blab ^ "/";
  print_endline @@ "Scope is if: " ^ string_of_bool scope#is_if; 
  print_string "elab:";
  print_endline scope#get_elab;
  print_endline "Labels: ";
  BlockMap.iter print_key scope#get_all_labels;
  print_endline "Scopes: ";
  BlockMap.iter (function _ -> function value ->  print_scope_c value) scope#get_all_scopes;
 
and
  print_global_scope () = BlockMap.iter (function _ -> function value -> print_function_c value ) global_scope#get_all_functions

    (* called only on scopes *)
let go_up () = 
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
                  | Global -> current_con := Global global_scope)
  

let find_local name =
  let rec _find_local name con = 
  let scope = (as_scope con "Scope/find_local") in
  if scope#has_local name then 
    scope#get_local name 
  else 
    _find_local name (parent_to_con scope#get_parent) in 
  _find_local name !current_con

let rec current_elab (con : container_t) = 
  match con with
  | Scope x -> x#get_elab
  | Function _ -> failwith "Not in scope"
  | Global _  -> failwith "Not in scope"

let store_local num value = 
  (as_scope !current_con "Scope/ store_local")#store_local num value