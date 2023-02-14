type prog =
  (string list
  * ([ `Lefta | `Nona | `Righta ]
    * string
    * [ `After of string | `At of string | `Before of string ])
    list)
  * Language.Expr.t

let context = Llvm.global_context ()
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

module LL = (val LL.make builder the_module)

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
    let _ = Llvm.build_store body temp builder in
    let tt = Llvm.build_load temp "b" builder in

    let c = Llvm.build_ptrtoint tt lama_int_type "c" builder in
    let __ () =
      Llvm.(
        build_call
          (lookup_function "printf" the_module |> Option.get)
          [| const_stringz context "%d"; c |])
        "" builder
    in
    let _ = LL.build_call LL.(lookup_func_exn "myputc") [ c ] in

    Llvm.const_int lama_int_type 0
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
        (* log "Adding to named_values %s" name; *)
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
    ();
    ()
  in

  (* let _ =
       Llvm.declare_function "printf"
         (Llvm.var_arg_function_type lama_int_type
            [| Llvm.array_type (Llvm.i8_type context) 3 |])
         the_module
     in *)
  let _ =
    (* void* lama_applyN(void* f, int32_t, ...) *)
    Llvm.declare_function "lama_applyN"
      (Llvm.var_arg_function_type lama_ptr_type
         [| lama_ptr_type; lama_int_type |])
      the_module
  in
  let _ =
    Llvm.declare_function "lama_apply0"
      (Llvm.function_type lama_ptr_type [| lama_ptr_type |])
      the_module
  in
  let _ =
    Llvm.declare_function "lama_apply1"
      (Llvm.function_type lama_ptr_type [| lama_ptr_type; lama_ptr_type |])
      the_module
  in
  let _ =
    Llvm.declare_function "lama_alloc_closure"
      (Llvm.function_type lama_ptr_type [| lama_ptr_type; lama_int_type |])
      the_module
  in
  let _ =
    Llvm.declare_function "myputc"
      (Llvm.function_type (Llvm.void_type context) [| lama_int_type |])
      the_module
  in
  let rec codegen_expr = function
    | Language.Expr.Binop (op, lhs, rhs) ->
        let lhs_val = LL.build_ptrtoint (codegen_expr lhs) lama_int_type in
        let rhs_val = LL.build_ptrtoint (codegen_expr rhs) lama_int_type in
        let op, op_name =
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
        (* TODO(for Danya): get rid of names *)
        let temp = op lhs_val rhs_val ~name:(op_name ^ "tmp") in
        LL.build_inttoptr temp lama_ptr_type ~name:(op_name ^ "tmp1")
    | Language.Expr.Const n ->
        let temp = Llvm.const_int lama_int_type n in
        LL.build_inttoptr temp lama_ptr_type
    | Var name -> (
        match Base.Hashtbl.find named_values name with
        | Some v -> (* an argument of current function *) v
        | None ->
            let f = LL.lookup_func_exn name in
            (* a global function *)
            let lama_alloc_closure = LL.lookup_func_exn "lama_alloc_closure" in
            let ptr = LL.build_pointercast f lama_ptr_type in
            let argscount = LL.params f |> Array.length in
            (* Llvm.dump_module the_module; *)
            LL.build_call lama_alloc_closure
              [ ptr; Llvm.const_int lama_int_type argscount ])
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
    | Call (Var callee_name, args)
      when Option.is_some (Llvm.lookup_function callee_name the_module)
           && Option.is_none (Base.Hashtbl.find named_values callee_name) -> (
        (* we may do a direct call *)
        let args = List.map codegen_expr args in
        let func = LL.lookup_func_exn callee_name in
        let real_args_count = List.length args in
        let formal_args_count = Array.length (LL.params func) in
        match Int.compare real_args_count formal_args_count with
        | 0 -> LL.build_call func args
        | 1 -> failwiths "Should not happen because we don't have currying?"
        | _ ->
            (* partial application *)
            let callee =
              let lama_alloc_closure =
                LL.lookup_func_exn "lama_alloc_closure"
              in
              let ptr = LL.build_pointercast func lama_ptr_type in
              LL.build_call lama_alloc_closure
                [ ptr; LL.const_int lama_int_type formal_args_count ]
            in
            let lama_apply = LL.lookup_func_exn "lama_applyN" in
            let final_args =
              callee :: LL.const_int lama_int_type real_args_count :: args
            in
            LL.build_call lama_apply final_args)
    | Call (callee_expr, args) ->
        let args = List.map codegen_expr args in
        let arg_number = List.length args in
        let lama_apply = LL.lookup_func_exn "lama_applyN" in
        let final_args =
          codegen_expr callee_expr
          :: LL.const_int lama_int_type arg_number
          :: args
        in
        LL.build_call lama_apply final_args
    | Skip ->
        Printf.printf "%s %d\n%!" __FILE__ __LINE__;
        assert false
    | If (cond, then_, else_) ->
        (* condition *)
        let cond =
          Llvm.build_icmp Llvm.Icmp.Ne
            (LL.build_ptrtoint (codegen_expr cond) lama_int_type)
            (Llvm.const_int lama_int_type 0)
            "ifcond" builder
        in
        (* get current function since basic blocks have to be inserted into a function *)
        let func = Llvm.block_parent (Llvm.insertion_block builder) in
        (* Following the LLVM tutorial: https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl05.html *)
        (* create conditional branch *)
        let then_bb, else_bb, merge_bb =
          let f x = Llvm.append_block context (Llvm.value_name func ^ x) func in
          (f "_then", f "_else", f "_cont")
        in
        ignore @@ Llvm.build_cond_br cond then_bb else_bb builder;
        (* then branch *)
        let t =
          Llvm.position_at_end then_bb builder;
          codegen_expr then_
        in
        Llvm.build_br merge_bb builder |> ignore;
        let then_bb = Llvm.insertion_block builder in
        (* else branch *)
        let e =
          Llvm.position_at_end else_bb builder;
          codegen_expr else_
        in
        let else_bb = Llvm.insertion_block builder in
        Llvm.build_br merge_bb builder |> ignore;
        (* merge point *)
        Llvm.position_at_end merge_bb builder;
        Llvm.build_phi [ (t, then_bb); (e, else_bb) ] "phi_result" builder
    | xxx -> failwiths "Unsupported: %s" (GT.show Language.Expr.t xxx)
  in
  let conv = CConv.run (snd prog) in
  (* Format.printf "Rewritten:@,@[%s@]\n%!" (GT.show Language.Expr.t conv); *)
  Format.printf "%a\n%!" Pprinter.pp conv;
  match conv with
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

      (* let _ =
           Llvm.set_global_constant true
             (Llvm.const_array (Llvm.pointer_type (Llvm.i8_type context)) [||])
         in *)
      prepare_main codegen_expr body;
      (* gen_func codegen_expr "main" [] (Llvm.const_float double_type 0.0); *)
      (* Llvm.dump_module the_module; *)
      Llvm.print_module "pizda.ll" the_module;
      (* Llvm.dump_value lv; *)
      dump_to_object ~the_fpm;
      ()
  | _ -> assert false

let%test _ = true
