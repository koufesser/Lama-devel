open Llvm
open Printf
open Constants

module type S = sig
  val build_store : Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue
  val build_call : ?name:string -> llvalue -> llvalue list -> llvalue
  val lookup_func_exn : string -> llvalue
  val build_add : ?name:string -> llvalue -> llvalue -> llvalue
  val build_sub : ?name:string -> llvalue -> llvalue -> llvalue
  val build_mul : ?name:string -> llvalue -> llvalue -> llvalue
  val build_sdiv : ?name:string -> llvalue -> llvalue -> llvalue [@@inline]
  val build_urem : ?name:string -> llvalue -> llvalue -> llvalue
  val build_eq : string -> ?name:string -> llvalue -> llvalue -> llvalue
  val build_ne : string  -> ?name:string -> llvalue -> llvalue -> llvalue
  val build_Sgt : string -> ?name:string -> llvalue -> llvalue -> llvalue
  val build_Sge : string -> ?name:string -> llvalue -> llvalue -> llvalue
  val build_Slt : string -> ?name:string  -> llvalue -> llvalue -> llvalue
  val build_Sle : string -> ?name:string -> llvalue -> llvalue -> llvalue
  val build_and : ?name:string -> llvalue -> llvalue -> llvalue

  (* ?? *)

  val build_ptrtoint : ?name:string -> llvalue -> lltype -> llvalue
  val build_inttoptr : ?name:string -> llvalue -> lltype -> llvalue
  val build_pointercast : ?name:string -> llvalue -> lltype -> llvalue

  val declare_function : string -> int -> llvalue 
  val define_function : string -> int -> llvalue 

  (** Just aliases *)

  val current_function : unit -> Llvm.llvalue
  val const_int : Llvm.lltype -> int -> Llvm.llvalue
  val params : Llvm.llvalue -> Llvm.llvalue array
end

let make builder context module_ =
  let module L : S = struct
    let build_store a b = Llvm.build_store a b builder

    let build_call ?(name = "") f args =
      build_call f (Array.of_list args) name builder

    let lookup_func_exn fname =
      match lookup_function fname module_ with
      | Some f -> f
      | None -> failwith (sprintf "Function '%s' not found" fname)

    let build_add ?(name = "") l r = build_add l r name builder
    let build_sub ?(name = "") l r = build_sub l r name builder
    let build_mul ?(name = "") l r = build_mul l r name builder
    let build_sdiv ?(name = "") l r = build_sdiv l r name builder
    let build_urem ?(name = "")  l r = build_urem l r name builder 
    let build_eq name1 ?(name = "") l r = (build_zext (build_icmp Icmp.Eq l r name1 builder) int_type name builder) 
    let build_ne name1 ?(name = "") l r = (build_zext (build_icmp Icmp.Ne l r name1 builder) int_type name builder)
    let build_Sgt name1 ?(name = "") l r =  (build_zext (build_icmp Icmp.Sgt l r name1 builder) int_type name builder)
    let build_Slt name1 ?(name = "") l r = (build_zext (build_icmp Icmp.Slt l r name1 builder) int_type name builder)
    let build_Sge name1 ?(name = "") l r = (build_zext (build_icmp Icmp.Sge l r name1 builder) int_type name builder)
    let build_Sle name1 ?(name = "") l r = (build_zext (build_icmp Icmp.Sle l r name1 builder) int_type name builder)
    let build_and ?(name = "") l r = build_and l r name builder 
    let build_ptrtoint ?(name = "") e typ =
      Llvm.build_ptrtoint e typ name builder

    let build_inttoptr ?(name = "") e typ =
      Llvm.build_inttoptr e typ name builder

    let build_pointercast ?(name = "") f typ =
      Llvm.build_pointercast f typ name builder


    let declare_function name nargs =
      let params = Array.make nargs int_type in
      let function_type = Llvm.function_type int_type params in 
      Llvm.declare_function name  function_type module_ 
  
    let define_function name nargs =
      let params = Array.make nargs int_type in
      let function_type = Llvm.function_type int_type params in 
      Llvm.define_function name  function_type module_

    let current_function () = Llvm.block_parent (Llvm.insertion_block builder)
    
    (* Aliases *)
    let const_int = Llvm.const_int
    let params = Llvm.params
     
  end in
  (module L : S)

