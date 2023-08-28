type scope = SM.scope
open Constants
let scope_stack = [] 
let waiting_labels : string list ref = ref [] 
let latest_line_info = ref ""
let builder = Llvm.builder context
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
    
    
type variable = 
    | Ptr of variable 
    | Int
    | String of Llvm.lltype
    | Sexp of variable list * int
    | Array of variable list * int
    | List of variable list * int
    | Closure

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
  val mutable rstack : Llvm.llvalue = zero
  val mutable stack_fullness = 0
  val mutable locals = IntMap.empty
  val mutable labels_map = StringMap.empty
  val mutable reachable = true
  val mutable params : Llvm.llvalue = zero

  method mark_int (value : Llvm.llvalue) = 
    LL.build_add ~name:(get_name()) one (LL.build_mul ~name:(get_name()) two value) 

  method unmark_int (value : Llvm.llvalue) = 
    LL.build_sdiv ~name:(get_name()) (LL.build_sub ~name:(get_name()) value one) two 

  method get_closure_variables = 
    Llvm.param func 0 

  method get_param i = 
    let ptr = Llvm.build_gep params [| Llvm.const_int int_type @@ i + 1 |] (get_name() ^ "_param_loading") builder in 
    Llvm.build_load ptr (get_name() ^ "_loaded_param") builder  

  method set_param i value = 
    let ptr = Llvm.build_gep params [| Llvm.const_int int_type @@ i + 1 |] (get_name()^ "_param_setting") builder in 
    Llvm.build_store value ptr builder 

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
    stack_fullness <- n;


  method get_free = 
    stack_fullness <- stack_fullness + 1;
    Llvm.build_gep rstack [| Llvm.const_int int_type @@ stack_fullness - 1 |] (get_name() ^ "_free_ptr_") builder

  method get_front = 
    Llvm.build_gep rstack [| Llvm.const_int int_type (stack_fullness - 1) |] (get_name() ^ "_front_ptr") builder

  method drop = 
    stack_fullness <- stack_fullness - 1;
    assert (stack_fullness >= 0)

  method create_stack size = 
    let builder = Llvm.builder_at context (Llvm.instr_begin (Llvm.entry_block func)) in
    rstack <- Llvm.build_array_alloca int_type (Llvm.const_int int_type size) (get_name()) builder;
    params <- Llvm.build_array_alloca int_type (Llvm.const_int int_type args) (get_name()) builder;
    if args > 0 then 
      let value = Llvm.param func 0 in    
    ignore @@ Llvm.build_store value params builder ;
    for i = 1 to args - 1 do
      let ptr = Llvm.build_gep params [| Llvm.const_int int_type i |] (get_name()) builder in
      let value = Llvm.param func i in    
      ignore @@ Llvm.build_store value ptr builder 
    done 

  method load (value : Llvm.llvalue)  = 
    let ptr = self#get_free in 
    Llvm.build_store value ptr builder

  method load_int (value : Llvm.llvalue)  = 
    let ptr = self#get_free in
    let ld_int = self#mark_int value in 
    Llvm.build_store ld_int ptr builder
  
  method load_from_ptr (value : Llvm.llvalue) =
    let value = Llvm.build_load value (get_name ()) builder in 
    Llvm.build_store value (self#get_free) builder

  method store = 
    let ptr = self#get_front in 
    let stored = Llvm.build_load ptr (get_name () ^ "_store") builder in 
    stored

  method store_int =
  let ptr = self#get_front in 
  let stored = Llvm.build_load ptr (get_name () ^ "_store") builder in 
  let st_int = self#unmark_int stored in
  st_int

  method store_and_drop_n  n = 
    stack_fullness <- stack_fullness - n;
    Llvm.build_gep rstack [| Llvm.const_int int_type @@ stack_fullness |] (get_name() ^ "store_and_drop") builder


  method get_local_ptr name = 
    if not @@ IntMap.mem name locals then (
    let builder = Llvm.builder_at context (Llvm.instr_begin (Llvm.entry_block func)) in
    let value = Llvm.build_alloca int_type (get_name ()) builder in 
    locals <- IntMap.add name value locals );
      IntMap.find name locals
    
  method load_local (name : int) (value : Llvm.llvalue)   =
    let ptr = self#get_local_ptr name in 
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
end 

