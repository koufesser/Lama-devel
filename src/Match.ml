open Scope

let match_cjmp (insn : SM.insn) =
  match  insn with
  | CJMP (x, y) -> Some y
  | _ -> None

let match_scope (insn : SM.insn) = 
  match  insn with
  | SLABEL y -> Some y
  | _ -> None

let match_label (insn : SM.insn) = 
  match  insn with
  | LABEL y -> Some y
  | _ -> None

let rec find_label insns = 
  match !insns with 
  hd :: tl ->
    insns := tl;
    (match match_label hd with 
    | Some y -> Some y 
    | None -> find_label insns) 
  | [] -> None

let rec find_cjmp insns = 
  match !insns with 
  hd :: tl ->
    insns := tl;
    (match match_cjmp hd with 
    | Some y -> Some y 
    | None -> find_cjmp insns) 
  | [] -> None

let rec find_scope_elab elab insns =  
  match !insns with 
  hd :: tl ->
    insns := tl;
    ( match match_scope hd with 
    | Some y -> if (y = elab) then true else  find_scope_elab elab insns
    | None -> find_scope_elab elab insns)
  | [] -> false 

let rec find_scope outer_scope insns = 
  match !insns with 
  | hd :: tl ->
    insns := tl;
    (match match_scope hd with 
    | Some y -> find_scope_elab (outer_scope#get_scope y)#get_elab insns 
    | None -> find_scope outer_scope insns) 
  | [] -> false

let match_if_pattern (insns:SM.prg) (scope : scope_c) =
  print_endline @@ "Matching scope: " ^ scope#get_blab;
  if (BlockMap.cardinal scope#get_all_scopes != 2) then (print_endline @@ "Not 2 subscopes " ^ string_of_int (BlockMap.cardinal scope#get_all_scopes); false)
  else (
    let insns = ref insns in
      match find_cjmp insns with 
      | None ->   print_endline @@ "No cjmp";
                  false
      | Some lab -> 
        (match find_scope scope insns with 
        true -> (match !insns with 
        hd :: tl -> 
        insns := tl; 
        (match find_label insns with 
          | None -> print_endline @@ "No label scope"; false
          | Some label -> 
            if lab = label then find_scope scope insns else  (print_endline @@ "Labels are not matching " ^ lab ^ " " ^ label; false)
      )
      | [] -> false
      )
      | false -> print_endline @@ "No first scope"; false
    )
  )
    
