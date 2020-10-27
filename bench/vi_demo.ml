module V = VisitorsWrapper

module Pattern =
  struct

    (* The type for patterns *)
    type t =
    (* wildcard "-"     *) | Wildcard
    (* S-expression     *) | Sexp   of string * t list
    (* array            *) | Array  of t list
    (* identifier       *) | Named  of string * t
    (* ground integer   *) | Const  of int
    (* ground string    *) | String of string
    (* boxed value      *) | Boxed
    (* unboxed value    *) | UnBoxed
    (* any string value *) | StringTag
    (* any sexp value   *) | SexpTag
    (* any array value  *) | ArrayTag
    (* any closure      *) | ClosureTag
    [@@deriving visitors { polymorphic = false;  variety = "fold" }]

end
