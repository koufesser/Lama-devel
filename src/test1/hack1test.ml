let context = Llvm.global_context ()
let the_module = Llvm.create_module context "main"
let builder = Llvm.builder context
let i64_type = Llvm.i64_type context
let i32_type = Llvm.i32_type context
let lama_int_type = i32_type
let lama_ptr_type = Llvm.pointer_type lama_int_type
let () = assert (Llvm_executionengine.initialize ())
let the_module = Llvm.create_module context "main"
let the_execution_engine = Llvm_executionengine.create the_module
let the_fpm = Llvm.PassManager.create_function the_module
let failwiths fmt = Format.kasprintf failwith fmt

let () =
  (* Promote allocas to registers. *)
  Llvm_scalar_opts.add_memory_to_register_promotion the_fpm;
  (* Do simple "peephole" optimizations and bit-twiddling optzn. *)
  Llvm_scalar_opts.add_instruction_combination the_fpm;
  (* reassociate expressions. *)
  Llvm_scalar_opts.add_reassociation the_fpm;
  (* Eliminate Common SubExpressions. *)
  Llvm_scalar_opts.add_gvn the_fpm;
  (* Simplify the control flow graph (deleting unreachable blocks, etc). *)
  Llvm_scalar_opts.add_cfg_simplification the_fpm;
  Llvm.PassManager.initialize the_fpm |> ignore

let log fmt = Format.kasprintf (fun s -> Format.printf "%s\n%!" s) fmt

let create_entry_block_alloca the_function var_name =
  let builder =
    Llvm.builder_at context (Llvm.instr_begin (Llvm.entry_block the_function))
  in
  Llvm.build_alloca lama_ptr_type var_name builder

let lamaint_to_ptr llv = Llvm.const_inttoptr llv lama_ptr_type
let lamaptr_to_int llv = Llvm.const_ptrtoint llv lama_int_type

let named_values : (string, Llvm.llvalue) Base.Hashtbl.t =
  Base.Hashtbl.create (module Base.String)

let create_argument_allocas the_function args =
  ArrayLabels.iteri (Llvm.params the_function) ~f:(fun i ai ->
      let var_name = List.nth args i in
      (* Create an alloca for this variable. *)
      let alloca = create_entry_block_alloca the_function var_name in
      (* Store the initial value into the alloca. *)
      let _ = Llvm.build_store ai alloca builder in
      (* Add arguments to variable symbol table. *)
      Base.Hashtbl.set named_values ~key:var_name ~data:alloca)

let rec codegen_expr () =
  let lhs = Llvm.const_int lama_int_type 52 in
  let rhs =
    let v = Base.Hashtbl.find_exn named_values "x" in
    lamaptr_to_int (Llvm.build_load v "x" builder)
  in

  (* let rhs = Llvm.const_int lama_int_type 52 in *)
  log "adding '%s' and '%s'"
    (Llvm.string_of_llvalue lhs)
    (Llvm.string_of_llvalue rhs);
  log " creating binop %s %d" __FILE__ __LINE__;

  lamaint_to_ptr (Llvm.build_add lhs rhs "addtmp" builder)

let prepare_main codegen_expr args body =
  let ft =
    let args = Array.make (List.length args) lama_ptr_type in
    Llvm.function_type lama_ptr_type args
  in
  let the_function = Llvm.declare_function "main" ft the_module in
  Base.Array.iteri (Llvm.params the_function) ~f:(fun i a ->
      let name = List.nth args i in
      Llvm.set_value_name name a;
      Base.Hashtbl.add_exn named_values ~key:name ~data:a);
  (* Create a new basic block to start insertion into. *)
  let bb = Llvm.append_block context "entry" the_function in
  Llvm.position_at_end bb builder;
  (* Add all arguments to the symbol table and create their allocas. *)
  (* Finish off the function. *)
  let return_val = codegen_expr body in
  log "%s %d" __FILE__ __LINE__;
  let (_ : Llvm.llvalue) = Llvm.build_ret return_val builder in

  log "%s %d" __FILE__ __LINE__;
  Llvm.dump_module the_module;
  (match Llvm_analysis.verify_function the_function with
  | true -> ()
  | false ->
      Caml.Format.printf "invalid function generated\n%s\n"
        (Llvm.string_of_llvalue the_function);
      Llvm_analysis.assert_valid_function the_function);
  log "%s %d" __FILE__ __LINE__;
  let (_ : bool) = Llvm.PassManager.run_function the_function the_fpm in
  Llvm.dump_value the_function

let () = prepare_main codegen_expr [ "x" ] ()
