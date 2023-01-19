type prog =
  (string list
  * ([ `Lefta | `Nona | `Righta ]
    * string
    * [ `After of string | `At of string | `Before of string ])
    list)
  * Language.Expr.t

let context = Llvm.global_context ()
let the_module = Llvm.create_module context "main"
let builder = Llvm.builder context
let i64_type = Llvm.i64_type context
let i32_type = Llvm.i32_type context
let lama_int_type = i32_type
let lama_ptr_type = Llvm.pointer_type lama_int_type

(* let integer_type = Llvm.double_type context *)
let () = assert (Llvm_executionengine.initialize ())
let the_module = Llvm.create_module context "main"
let the_execution_engine = Llvm_executionengine.create the_module
let the_fpm = Llvm.PassManager.create_function the_module

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
  Llvm.set_target_triple target_triple the_module;
  Llvm.set_data_layout data_layout the_module;
  let filename = "output.o" in
  Llvm_target.TargetMachine.add_analysis_passes the_fpm machine;
  let file_type = Llvm_target.CodeGenFileType.ObjectFile in
  (* Printf.printf "%s %d\n%!" __FILE__ __LINE__; *)
  Llvm_target.TargetMachine.emit_to_file the_module file_type filename machine;
  (* printf "Wrote %s\n" filename; *)
  ()

let () =
  (* Promote allocas to registers. *)
  (* Llvm_scalar_opts.add_memory_to_register_promotion the_fpm;
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

let create_entry_block_alloca the_function var_name =
  let builder =
    Llvm.builder_at context (Llvm.instr_begin (Llvm.entry_block the_function))
  in
  Llvm.build_alloca lama_ptr_type var_name builder

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

let lamaint_to_ptr llv = Llvm.const_inttoptr llv lama_ptr_type
let lamaptr_to_int llv = Llvm.const_ptrtoint llv lama_int_type

let prepare_main codegen_expr body =
  let ft =
    (* TODO main has special args *)
    let args = Array.make 0 lama_ptr_type in
    (* Llvm.function_type lama_ptr_type args *)
    Llvm.function_type lama_int_type args
  in
  let the_function = Llvm.declare_function "main" ft the_module in
  (* Set names for all arguments. *)
  (* Base.Array.iteri (Llvm.params the_function) ~f:(fun i a ->
      let name = List.nth args i in
      Llvm.set_value_name name a;
      Base.Hashtbl.add_exn named_values ~key:name ~data:a); *)
  (* Create a new basic block to start insertion into. *)
  let bb = Llvm.append_block context "entry" the_function in
  Llvm.position_at_end bb builder;
  (* Add all arguments to the symbol table and create their allocas. *)
  (* Finish off the function. *)
  let return_val =
    let temp = Llvm.build_alloca lama_ptr_type "a" builder in
    let body = codegen_expr body in
    (* let body = Llvm.build_inttoptr body lama_ptr_type "aa" builder in *)
    let ans = Llvm.build_store body temp builder in
    let tt = Llvm.build_load temp "b" builder in
    Llvm.build_ptrtoint tt lama_int_type "c" builder
  in
  (* let return_val =
       let body = codegen_expr body in
       let ans = Llvm.build_store (lamaptr_to_int body) temp builder in
       Llvm.build_load temp (Llvm.value_name temp) builder
     in *)
  (* let return_val =
       let body = codegen_expr body in
       lamaptr_to_int body
     in *)
  let (_ : Llvm.llvalue) = Llvm.build_ret return_val builder in
  (* Validate the generated code, checking for consistency. *)
  (* Llvm.dump_module the_module; *)
  (* log "%s %d" __FILE__ __LINE__; *)
  (match Llvm_analysis.verify_function the_function with
  | true -> ()
  | false ->
      Caml.Format.printf "invalid function generated\n%s\n"
        (Llvm.string_of_llvalue the_function);
      Llvm_analysis.assert_valid_function the_function);
  (* Optimize the function. *)
  let (_ : bool) = Llvm.PassManager.run_function the_function the_fpm in
  (* Llvm.dump_value the_function; *)
  ()

let build _cmd (prog : prog) =
  print_endline (GT.show Language.Expr.t (snd prog));
  let gen_func codegen_expr name args body =
    let ft =
      let args = Array.make (List.length args) lama_ptr_type in
      Llvm.function_type lama_ptr_type args
    in
    let the_function = Llvm.declare_function name ft the_module in
    (* Set names for all arguments. *)
    Base.Array.iteri (Llvm.params the_function) ~f:(fun i a ->
        let name = List.nth args i in
        Llvm.set_value_name name a;
        Base.Hashtbl.add_exn named_values ~key:name ~data:a);
    (* Create a new basic block to start insertion into. *)
    let bb = Llvm.append_block context "entry" the_function in
    Llvm.position_at_end bb builder;
    (* Add all arguments to the symbol table and create their allocas. *)
    (* create_argument_allocas the_function args; *)
    let return_val = codegen_expr body in
    (* Finish off the function. *)
    let (_ : Llvm.llvalue) = Llvm.build_ret return_val builder in

    (* Llvm.dump_module the_module; *)
    (* log "%s %d" __FILE__ __LINE__; *)

    (* Validate the generated code, checking for consistency. *)
    (match Llvm_analysis.verify_function the_function with
    | true -> ()
    | false ->
        Caml.Format.printf "invalid function generated\n%s\n"
          (Llvm.string_of_llvalue the_function);
        Llvm_analysis.assert_valid_function the_function);
    (* Optimize the function. *)
    let (_ : bool) = Llvm.PassManager.run_function the_function the_fpm in
    (* Llvm.dump_value the_function; *)
    ()
  in

  let rec codegen_expr = function
    | Language.Expr.Binop ("+", lhs, rhs) ->
        let lhs_val = lamaptr_to_int (codegen_expr lhs) in
        let rhs_val = lamaptr_to_int (codegen_expr rhs) in
        lamaint_to_ptr (Llvm.build_add lhs_val rhs_val "addtmp" builder)
    | Language.Expr.Const n -> lamaint_to_ptr (Llvm.const_int lama_int_type n)
    | Var name -> (
        match Base.Hashtbl.find named_values name with
        | None -> failwiths "unkown variable name %s" name
        | Some v -> v (* Llvm.build_load v name builder *))
    | Language.Expr.Binop ("-", lhs, rhs) ->
        let lhs_val = lamaptr_to_int (codegen_expr lhs) in
        let rhs_val = lamaptr_to_int (codegen_expr rhs) in
        lamaint_to_ptr (Llvm.build_sub lhs_val rhs_val "subtmp" builder)
    | Language.Expr.Binop ("*", lhs, rhs) ->
        let lhs_val = lamaptr_to_int (codegen_expr lhs) in
        let rhs_val = lamaptr_to_int (codegen_expr rhs) in
        lamaint_to_ptr (Llvm.build_mul lhs_val rhs_val "multmp" builder)
    | Scope (decls, body) ->
        let _ =
          List.iter
            (function
              | name, (_, `Fun (args, body)) ->
                  gen_func codegen_expr name args body
              | _name, (_, `Variable _) -> assert false)
            decls
        in
        codegen_expr body
    | Call (Var callee_name, args) ->
        (* Look up the name in the module table. *)
        let callee =
          match Llvm.lookup_function callee_name the_module with
          | Some callee -> callee
          | None -> failwiths "undefined function %s" (callee_name : string)
        in
        (* If argument mismatch error. *)
        if Array.length (Llvm.params callee) = List.length args then ()
        else failwiths "incorrect number of arguments %s" (callee_name : string);
        let args = Array.map codegen_expr (Array.of_list args) in
        (* Printf.printf "Preparing a call %s %d\n%!" __FILE__ __LINE__; *)
        Llvm.build_call callee args "calltmp" builder
    | Skip ->
        Printf.printf "%s %d\n%!" __FILE__ __LINE__;
        assert false
    | xxx -> failwiths "Unsupported: %s" (GT.show Language.Expr.t xxx)
  in

  match snd prog with
  | Scope (decls, body) ->
      let _ =
        List.iter
          (function
            | name, (_, `Fun (args, body)) ->
                gen_func codegen_expr name args body
            | _name, (_, `Variable _) -> assert false)
          decls
      in

      (* Llvm.const_float double_type 0.0 *)
      (* let lv = Llvm.const_float double_type 0.0 in *)
      (* Printf.printf "Preparing main %s %d\n%!" __FILE__ __LINE__; *)
      prepare_main codegen_expr body;
      (* gen_func codegen_expr "main" [] (Llvm.const_float double_type 0.0); *)
      (* Llvm.dump_module the_module; *)
      Llvm.print_module "pizda.ll" the_module;
      (* Llvm.dump_value lv; *)
      dump_to_object ~the_fpm;
      ()
  | _ -> assert false

let%test _ = true
