open Llvm
open Printf

module type S = sig
  val build_store : Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue
  val build_call : ?name:string -> llvalue -> llvalue list -> llvalue
  val lookup_func_exn : string -> llvalue
  val build_sub : ?name:string -> llvalue -> llvalue -> llvalue [@@inline]
  val build_ptrtoint : llvalue -> lltype -> llvalue
end

let make builder module_ =
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
    let build_ptrtoint e typ = Llvm.build_ptrtoint e typ "" builder
  end in
  (module L : S)
