open Llvm

module type S = sig
  val build_store : Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue
  val build_call : ?name:string -> llvalue -> llvalue list -> llvalue
  val lookup_func_exn : string -> llvalue
end

let make builder module_ =
  let module L : S = struct
    let build_store a b = Llvm.build_store a b builder

    let build_call ?(name = "") f args =
      build_call f (Array.of_list args) name builder

    let lookup_func_exn fname =
      match lookup_function fname module_ with
      | Some f -> f
      | None -> raise Not_found
  end in
  (module L : S)
