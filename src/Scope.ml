type state = 
  | None
  | FUNCTION_DEFINTION
  
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
  val mutable stack : variable_class list
  method add_function : function_class_t -> unit
  method add_label : Llvm.llbasicblock -> string -> unit
  method add_scope : scope_class_t -> unit
  method add_to_stack : variable_class -> unit
  method add_variable : variable_class -> unit
  method drop_from_stack : unit
  method find_function : string -> function_class_t
  method get_all_functions : function_class_t BlockMap.t
  method get_all_labels : Llvm.llbasicblock BlockMap.t
  method get_all_locals : variable_class BlockMap.t
  method get_all_scopes : scope_class_t BlockMap.t
  method get_from_stack : variable_class
  method get_label : string -> Llvm.llbasicblock
  method get_parent : parent_class_t
  method get_scope : string -> scope_class_t
  method get_variable : string -> variable_class
  method has_label : string -> bool
  method has_scope : string -> bool
  method has_variable : string -> bool
  method is_function : bool
  method is_global : bool
  method is_scope : bool
  method set_inner_scopes : scope_class_t list -> unit
  method shove_up : unit
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
  val mutable blab : string
  val mutable elab : string
  val mutable finished : bool
  val mutable functions_map : function_class_t BlockMap.t
  val mutable inner_scopes : scope_class_t BlockMap.t
  val mutable labels_map : Llvm.llbasicblock BlockMap.t
  val mutable locals_map : variable_class BlockMap.t
  val mutable started : bool
  val mutable hard_stack : variable_class list
  val mutable free_ptr : variable_class list
  val mutable hrdst_builder : Llvm.llvalue
  method set_hrdst_builder : Llvm.llvalue -> unit
  method add_to_hrdst : variable_class -> unit
  method add_function : function_class_t -> unit
  method add_label : Llvm.llbasicblock -> string -> unit
  method add_scope : scope_class_t -> unit
  method add_variable : variable_class -> unit
  method find_function : string -> function_class_t
  method finish : unit
  method get_all_functions : function_class_t BlockMap.t
  method get_all_labels : Llvm.llbasicblock BlockMap.t
  method get_all_locals : variable_class BlockMap.t
  method get_all_scopes : scope_class_t BlockMap.t
  method get_function : Llvm.llvalue
  method get_label : string -> Llvm.llbasicblock
  method get_name : string
  method get_parent : parent_class_t
  method get_scope : string -> scope_class_t
  method get_variable : string -> variable_class
  method get_blab : string
  method get_elab : string
  method has_label : string -> bool
  method has_scope : string -> bool
  method has_variable : string -> bool
  method is_finished : bool
  method is_function : bool
  method is_global : bool
  method is_scope : bool
  method is_started : bool
  method set_blab : string -> unit
  method set_elab : string -> unit
  method set_inner_scopes : scope_class_t list -> unit
  method start : unit
  method reset : unit
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
  val mutable stack : variable_class list = []
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
    method add_variable (var : variable_class) = 
        locals_map <- BlockMap.add var#get_name var locals_map
    method add_label (var: Llvm.llbasicblock) (name:string) =
        labels_map <- BlockMap.add name var labels_map
      
    method get_all_labels = labels_map
    method get_all_locals = locals_map
    method get_all_scopes = inner_scopes
    method get_all_functions = functions_map

    method has_variable (name : string) = 
      BlockMap.mem name locals_map  
    method has_label (name : string) = 
      BlockMap.mem name labels_map
    method has_scope (name: string) = 
        BlockMap.mem name inner_scopes
        
    method get_variable (name : string) = 
      BlockMap.find name locals_map
    method get_label (name : string) = 
      BlockMap.find name labels_map 
    method get_scope (blab : string) = 
        BlockMap.find blab inner_scopes 
  
    method find_function (name : string) = 
      BlockMap.find name functions_map 

    method add_to_stack (value : variable_class) = 
      stack <- value :: stack

    method get_from_stack = 
      match stack with 
      | hd :: tl -> hd
      | [] -> failwith "No values on stack"
    
    method drop_from_stack =
      match stack with 
      | hd :: tl -> (stack <- tl)
      | [] -> failwith "No values on stack"
    
    method shove_up = 
      let shove_up_one v = 
       

end 
and
scope_class  (s : scope) ?(scope_t= Local) (parent : parent_class option) : scope_class_t =
object (self)
  inherit common_class parent Scope ~scope_t:scope_t
  method get_elab = s.elab 
  method get_blab = s.blab
end 
and function_class (name:string) (parent : parent_class option) (func : Llvm.llvalue) : function_class_t = object (self)
  inherit common_class parent Function 
  val mutable blab : string  = ""
  val mutable elab : string  = ""
  val mutable started = false
  val mutable finished = false
  method get_name = name
  method get_function = func
  method is_started = started
  method set_blab s = 
    blab <- s
  method set_elab s = 
    elab <- s 
  method start = 
    started <- true
  method is_finished =
    finished == true 
  method finish = 
    finished <- true 
  method get_elab = elab
  method get_blab = blab
  method reset = 
    started <- false;
    finished <- false
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
    
let rec reset_functions con = 
  match con with 
    | Scope s ->  BlockMap.iter (function _ -> function value ->  reset_functions @@ Scope value) s#get_all_scopes;
                  BlockMap.iter (function _ -> function value ->  reset_functions @@ Function value) s#get_all_functions
    | Function f ->
                  f#reset; 
                  BlockMap.iter (function _ -> function value ->  reset_functions @@ Scope value) f#get_all_scopes;
                  BlockMap.iter (function _ -> function value ->  reset_functions @@ Function value) f#get_all_functions