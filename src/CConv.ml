let failwithf fmt = Format.kasprintf failwith fmt

module String_set = struct
  include Set.Make (String)

  let pp ppf set =
    let open Format in
    fprintf ppf "{set| ";
    iter (fprintf ppf "%s, ") set;
    fprintf ppf "|set}"

  let to_list set = to_seq set |> List.of_seq
end

module SS = String_set

(** [without set ~other] removes from [set] everything in [other] *)
let without set ~other = SS.diff set other
(* String_set.fold (fun x acc -> String_set.remove x acc) other set *)

let free_vars_of_expr =
  let open Language.Expr in
  let rec helper acc = function
    | Const _ -> acc
    | Ref s -> acc (* TODO: report error *)
    | Var s -> String_set.add s acc
    | Call (f, args) -> List.fold_left helper (helper acc f) args
    | Lambda (args, body) ->
        helper SS.empty body |> without ~other:(SS.of_list args) |> SS.union acc
    | Scope (_, expr) -> helper acc expr
    | Binop (_, l, r) -> helper (helper acc l) r
    | If (e, l, r) -> List.fold_left helper acc [ e; l; r ]
    | rest -> failwithf "PIZDA : %s" (GT.show t rest)
  in
  helper String_set.empty

let%expect_test " " =
  let left = String_set.of_list [ "a"; "b"; "c" ] in
  let other = String_set.of_list [ "d"; "b"; "c" ] in
  let set = without ~other left in
  Format.printf "%a\n" String_set.pp set;
  [%expect {|{set| a, |set} |}]

let gensym =
  let last = ref 0 in
  fun ?(prefix = "fresh") () ->
    incr last;
    Format.sprintf "%s_%d" prefix !last

(* Predefined *)
let standard_globals = SS.of_list [ "+"; "-" ]

type expr = Language.Expr.t

let show_expr = GT.show Language.Expr.t

let run : Language.Expr.t -> Language.Expr.t =
 fun root ->
  let classify globals params expr =
    let fvs = free_vars_of_expr expr in
    Format.printf "free vars inside `%s` are:\n%a\n%!" (show_expr expr) SS.pp
      fvs;
    (* let vars = fvs |> without ~other:params |> without ~other:globals in *)
    let vars = fvs |> without ~other:params in
    if String_set.cardinal vars = 0 then None else Some vars
  in

  (* TODO: Do we need nultiset to support shadowing ? *)
  let open Language.Expr in
  let scope names rhs =
    match names with [] -> rhs | _ -> Scope (names, rhs)
  in
  (* acc --  accumulator for bubbled functions
      globals -- variables, introduced in scopes above

      Returns accumulator with answers and new transformed expression
  *)
  let rec helper acc globals :
      expr -> (string * [ `Fun of string list * expr ]) list * expr = function
    | Scope (defs, body) ->
        let globals =
          (* add local functions and vars from the scope to globals *)
          List.fold_left (fun gls (name, _) -> SS.add name gls) globals defs
        in
        let acc, defs =
          List.fold_left_map
            (fun acc -> function
              | _, (`Extern, _) ->
                  failwith "External functions are not supported"
              | _, (`PublicExtern, _) ->
                  failwith "PublicExtern functions are not supported"
              | f_name, (q, `Fun (f_args, f_body)) ->
                  let acc, f_body =
                    helper acc (SS.union globals (SS.of_list f_args)) f_body
                  in
                  (acc, (f_name, (q, `Fun (f_args, f_body))))
              | v_name, (q, `Variable (Some v_body)) ->
                  let acc, v_body = helper acc globals v_body in
                  (acc, (v_name, (q, `Variable (Some v_body))))
              | (_, (_, `Variable None)) as v -> (acc, v)
              | _ -> failwith "This pattern-mathing supposed to be exhaustive")
            acc defs
        in
        let acc, body = helper acc globals body in
        (acc, Scope (defs, body))
    | Lambda (args, rhs) as root -> (
        match classify globals (SS.of_list args) rhs with
        | None ->
            Format.printf "Classify says None for '%s'\n%!" (show_expr root);
            let new_f = gensym () in
            let acc, rhs =
              helper acc (SS.union globals (SS.of_list args)) rhs
            in
            ((new_f, `Fun (args, rhs)) :: acc, Call (Var new_f, []))
        | Some closure_vars ->
            let new_f = gensym () in
            let acc, rhs =
              helper acc (SS.union globals (SS.of_list args)) rhs
            in
            let enhanced_args = SS.to_list closure_vars @ args in
            let ans = (new_f, `Fun (enhanced_args, rhs)) in
            ( ans :: acc,
              Call (Var new_f, enhanced_args |> List.map (fun x -> Var x)) ))
    | If (cond, th, el) ->
        let acc, cond = helper acc globals cond in
        let acc, th = helper acc globals th in
        let acc, el = helper acc globals el in
        (acc, If (cond, th, el))
    | (Const _ | Seq _ | Var _ | Binop _) as r -> (acc, r)
    | Call (expr, args) ->
        let acc, expr = helper acc globals expr in
        let acc, args =
          List.fold_left_map (fun acc x -> helper acc globals x) acc args
        in
        (acc, Call (expr, args))
    | rest ->
        failwithf "CConv: not implemented: %s" (GT.show Language.Expr.t rest)
  in
  let topdefs, expr = helper [] standard_globals root in
  match topdefs with
  | [] -> expr
  | _ ->
      scope
        (topdefs |> List.rev
        |> List.map (fun (name, body) ->
               let body =
                 (body
                   :> [ `Fun of string list * expr | `Variable of t option ])
               in
               (name, (`Public, body))))
        expr
(*
let parse_exn str =
  let cmd =
    object
      method is_workaround = true
      method get_include_paths = []
    end
  in
  match Language.run_parser_string cmd str with
  | `OK ast -> ast
  | `Fail s -> failwith s

let%expect_test " _ " =
  print_endline "Hello";
  let ast = parse_exn {|fun f (x) { x }; 42 |} in
  print_endline (GT.show Language.Expr.t ast);
  [%expect {| |}]
 *)
