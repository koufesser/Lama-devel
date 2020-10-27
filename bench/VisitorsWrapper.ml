module Pattern = struct

  type t = Language.Pattern.t =
    (* wildcard "-"     *) | Wildcard
    (* S-expression     *) | Sexp   of (GT.string [@name "real"]) * t list
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
    [@@deriving visitors { polymorphic = false;  variety = "iter" }]


  class ['self] pp_pattern = object(self: 'self)
    inherit ['self] iter
    inherit[_] VisitorsRuntime.iter
    method build_Wildcard ppf = Format.fprintf ppf "_"
    method build_Const ppf = Format.fprintf ppf "%d"
    method build_Named ppf name _ =
      Format.fprintf ppf "%s" name
    method build_Sexp ppf name xs =
      match name,xs with
      | "cons", [l; r] ->
          Format.fprintf ppf "%a@ :@ %a" self#visit_t l self#visit_t r
      | _ ->
          Format.fprintf ppf "@[%s@ (" name;
          xs |> List.iter (Format.fprintf ppf "%a@ " self#visit_t);
          Format.fprintf ppf ")@] "
    method build_Array ppf xs =
      Format.fprintf ppf "@[{ ";
      Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
        self#visit_t ppf xs;
      Format.fprintf ppf " }@]"
    method build_String ppf = Format.fprintf ppf "%S"

    method build_ArrayTag ppf = Format.fprintf ppf "#array"
    method build_SexpTag ppf = Format.fprintf ppf "#sexp"
    method build_ClosureTag ppf = Format.fprintf ppf "#fun"
    method build_UnBoxed ppf = Format.fprintf ppf "#UNBOXED?"
    method build_Boxed ppf = Format.fprintf ppf "#BOXED?"
    method build_StringTag ppf = Format.fprintf ppf "#string"
    (* should be in VisitorsRuntime *)
    method visit_real _ _ = assert false
  end
end

module Loc = struct
  type t = int * int
  [@@deriving visitors { polymorphic = false;  variety = "iter" }]
end

module Expr = struct

  type 'expr hack_polyvar =
    [`Fun of string list * 'expr | `Variable of 'expr option ]


  type atr = Language.Expr.atr = Reff | Void | Val | Weak
  class ['self] config = object(self:'self) end

  class qualifier = object(self:'self) end
  class ['a,'b] arrow = object(self:'self) end

  type t = Language.Expr.t =
    (* integer constant           *) | Const     of int
    (* array                      *) | Array     of t list
    (* string                     *) | String    of string
    (* S-expressions              *) | Sexp      of string * t list
    (* variable                   *) | Var       of string
    (* reference (aka "lvalue")   *) | Ref       of string
    (* binary operator            *) | Binop     of string * t * t
    (* element extraction         *) | Elem      of t * t
    (* reference to an element    *) | ElemRef   of t * t
    (* length                     *) | Length    of t
    (* string conversion          *) | StringVal of t
    (* function call              *) | Call      of t * t list
    (* assignment                 *) | Assign    of t * t
    (* composition                *) | Seq       of t * t
    (* empty statement            *) | Skip
    (* conditional                *) | If        of t * t * t
    (* loop with a pre-condition  *) | While     of t * t
    (* loop with a post-condition *) | Repeat    of t * t
    (* pattern-matching           *) | Case      of t * ((Pattern.t [@name "pattern"]) * t) list * (Loc.t[@opaque]) * atr
    (* return statement           *) | Return    of t option
    (* ignore a value             *) | Ignore    of t
    (* unit value                 *) | Unit
    (* entering the scope         *) | Scope     of (string * Language.Expr.decl) list * Language.Expr.t
    (* lambda expression          *) | Lambda    of string list * t
    (* leave a scope              *) | Leave
    (* intrinsic (for evaluation) *) | Intrinsic of (Language.Expr.t Language.Expr.config, Language.Expr.t Language.Expr.config) GT.arrow
    | Control   of  ( Language.Expr.t Language.Expr.config
                    , Language.Expr.t * Language.Expr.t Language.Expr.config
                    ) GT.arrow

    and decl = qualifier * t hack_polyvar
    (* [@@deriving visitors { polymorphic = false;  variety = "iter" }] *)

    (* type decl = qualifier * t hack_polyvar *)
    (* [@@deriving visitors { polymorphic = false;  variety = "iter" }] *)

        class virtual ['self] iter =
          object (self : 'self)
            inherit  [_] VisitorsRuntime.iter
            method visit_Const env _visitors_c0 =
              let _visitors_r0 = self#visit_int env _visitors_c0 in ()
            method visit_Array env _visitors_c0 =
              let _visitors_r0 =
                self#visit_list self#visit_t env _visitors_c0 in
              ()
            method visit_String env _visitors_c0 =
              let _visitors_r0 = self#visit_string env _visitors_c0 in ()
            method visit_Sexp env _visitors_c0 _visitors_c1 =
              let _visitors_r0 = self#visit_string env _visitors_c0 in
              let _visitors_r1 =
                self#visit_list self#visit_t env _visitors_c1 in
              ()
            method visit_Var env _visitors_c0 =
              let _visitors_r0 = self#visit_string env _visitors_c0 in ()
            method visit_Ref env _visitors_c0 =
              let _visitors_r0 = self#visit_string env _visitors_c0 in ()
            method visit_Binop env _visitors_c0 _visitors_c1 _visitors_c2 =
              let _visitors_r0 = self#visit_string env _visitors_c0 in
              let _visitors_r1 = self#visit_t env _visitors_c1 in
              let _visitors_r2 = self#visit_t env _visitors_c2 in ()
            method visit_Elem env _visitors_c0 _visitors_c1 =
              let _visitors_r0 = self#visit_t env _visitors_c0 in
              let _visitors_r1 = self#visit_t env _visitors_c1 in ()
            method visit_ElemRef env _visitors_c0 _visitors_c1 =
              let _visitors_r0 = self#visit_t env _visitors_c0 in
              let _visitors_r1 = self#visit_t env _visitors_c1 in ()
            method visit_Length env _visitors_c0 =
              let _visitors_r0 = self#visit_t env _visitors_c0 in ()
            method visit_StringVal env _visitors_c0 =
              let _visitors_r0 = self#visit_t env _visitors_c0 in ()
            method visit_Call env _visitors_c0 _visitors_c1 =
              let _visitors_r0 = self#visit_t env _visitors_c0 in
              let _visitors_r1 =
                self#visit_list self#visit_t env _visitors_c1 in
              ()
            method visit_Assign env _visitors_c0 _visitors_c1 =
              let _visitors_r0 = self#visit_t env _visitors_c0 in
              let _visitors_r1 = self#visit_t env _visitors_c1 in ()
            method visit_Seq env _visitors_c0 _visitors_c1 =
              let _visitors_r0 = self#visit_t env _visitors_c0 in
              let _visitors_r1 = self#visit_t env _visitors_c1 in ()
            method visit_Skip env = ()
            method visit_If env _visitors_c0 _visitors_c1 _visitors_c2 =
              let _visitors_r0 = self#visit_t env _visitors_c0 in
              let _visitors_r1 = self#visit_t env _visitors_c1 in
              let _visitors_r2 = self#visit_t env _visitors_c2 in ()
            method visit_While env _visitors_c0 _visitors_c1 =
              let _visitors_r0 = self#visit_t env _visitors_c0 in
              let _visitors_r1 = self#visit_t env _visitors_c1 in ()
            method visit_Repeat env _visitors_c0 _visitors_c1 =
              let _visitors_r0 = self#visit_t env _visitors_c0 in
              let _visitors_r1 = self#visit_t env _visitors_c1 in ()
            method visit_Case env _visitors_c0 _visitors_c1 _visitors_c2
              _visitors_c3 =
              let _visitors_r0 = self#visit_t env _visitors_c0 in
              let _visitors_r1 =
                self#visit_list
                  (fun env ->
                     fun (_visitors_c0, _visitors_c1) ->
                       let _visitors_r0 = self#visit_pattern env _visitors_c0 in
                       let _visitors_r1 = self#visit_t env _visitors_c1 in ())
                  env _visitors_c1 in
              let _visitors_r2 = (fun _visitors_this -> ()) _visitors_c2 in
              let _visitors_r3 = self#visit_atr env _visitors_c3 in ()
            method visit_Return env _visitors_c0 =
              let _visitors_r0 =
                self#visit_option self#visit_t env _visitors_c0 in
              ()
            method visit_Ignore env _visitors_c0 =
              let _visitors_r0 = self#visit_t env _visitors_c0 in ()
            method visit_Unit env = ()
            method visit_Scope env _visitors_c0 _visitors_c1 =
              let _visitors_r0 =
                self#visit_list
                  (fun env ->
                     fun (_visitors_c0, _visitors_c1) ->
                       let _visitors_r0 = self#visit_string env _visitors_c0 in
                       let _visitors_r1 = self#visit_decl env _visitors_c1 in
                       ()) env _visitors_c0 in
              let _visitors_r1 = self#visit_t env _visitors_c1 in ()
            method visit_Lambda env _visitors_c0 _visitors_c1 =
              let _visitors_r0 =
                self#visit_list self#visit_string env _visitors_c0 in
              let _visitors_r1 = self#visit_t env _visitors_c1 in ()
            method visit_Leave env = ()
            method visit_Intrinsic env _visitors_c0 =
              let _visitors_r0 =
                self#visit_arrow (self#visit_config self#visit_t)
                  (self#visit_config self#visit_t) env _visitors_c0 in
              ()
            method visit_Control env _visitors_c0 =
              let _visitors_r0 =
                self#visit_arrow (self#visit_config self#visit_t)
                  (fun env ->
                     fun (_visitors_c0, _visitors_c1) ->
                       let _visitors_r0 = self#visit_t env _visitors_c0 in
                       let _visitors_r1 =
                         self#visit_config self#visit_t env _visitors_c1 in
                       ()) env _visitors_c0 in
              ()
            method visit_t env _visitors_this =
              match _visitors_this with
              | Const _visitors_c0 -> self#visit_Const env _visitors_c0
              | Array _visitors_c0 -> self#visit_Array env _visitors_c0
              | String _visitors_c0 -> self#visit_String env _visitors_c0
              | Sexp (_visitors_c0, _visitors_c1) ->
                  self#visit_Sexp env _visitors_c0 _visitors_c1
              | Var _visitors_c0 -> self#visit_Var env _visitors_c0
              | Ref _visitors_c0 -> self#visit_Ref env _visitors_c0
              | Binop (_visitors_c0, _visitors_c1, _visitors_c2) ->
                  self#visit_Binop env _visitors_c0 _visitors_c1 _visitors_c2
              | Elem (_visitors_c0, _visitors_c1) ->
                  self#visit_Elem env _visitors_c0 _visitors_c1
              | ElemRef (_visitors_c0, _visitors_c1) ->
                  self#visit_ElemRef env _visitors_c0 _visitors_c1
              | Length _visitors_c0 -> self#visit_Length env _visitors_c0
              | StringVal _visitors_c0 ->
                  self#visit_StringVal env _visitors_c0
              | Call (_visitors_c0, _visitors_c1) ->
                  self#visit_Call env _visitors_c0 _visitors_c1
              | Assign (_visitors_c0, _visitors_c1) ->
                  self#visit_Assign env _visitors_c0 _visitors_c1
              | Seq (_visitors_c0, _visitors_c1) ->
                  self#visit_Seq env _visitors_c0 _visitors_c1
              | Skip -> self#visit_Skip env
              | If (_visitors_c0, _visitors_c1, _visitors_c2) ->
                  self#visit_If env _visitors_c0 _visitors_c1 _visitors_c2
              | While (_visitors_c0, _visitors_c1) ->
                  self#visit_While env _visitors_c0 _visitors_c1
              | Repeat (_visitors_c0, _visitors_c1) ->
                  self#visit_Repeat env _visitors_c0 _visitors_c1
              | Case (_visitors_c0, _visitors_c1, _visitors_c2, _visitors_c3)
                  ->
                  self#visit_Case env _visitors_c0 _visitors_c1 _visitors_c2
                    _visitors_c3
              | Return _visitors_c0 -> self#visit_Return env _visitors_c0
              | Ignore _visitors_c0 -> self#visit_Ignore env _visitors_c0
              | Unit -> self#visit_Unit env
              | Scope (_visitors_c0, _visitors_c1) ->
                  self#visit_Scope env _visitors_c0 _visitors_c1
              | Lambda (_visitors_c0, _visitors_c1) ->
                  self#visit_Lambda env _visitors_c0 _visitors_c1
              | Leave -> self#visit_Leave env
              | Intrinsic _visitors_c0 ->
                  self#visit_Intrinsic env _visitors_c0
              | Control _visitors_c0 -> self#visit_Control env _visitors_c0
            method visit_decl env (_visitors_c0, _visitors_c1) =
              let _visitors_r0 = self#visit_qualifier env _visitors_c0 in
              let _visitors_r1 =
                self#visit_hack_polyvar self#visit_t env _visitors_c1 in
              ()
          end


end
