open SM
open Language

let parse_designation (line : string list) : Value.designation =
  match List.hd line with
  | "Global" -> Global (List.nth line 1)
  | "Local"  -> 
    (* List.iter (function x -> print_endline @@ "var: " ^ x) line; *)
    (* print_endline @@ "size: " ^ string_of_int (List.length line);  *)
    Local (int_of_string (List.nth line 1))
  | "Arg"    -> Arg (int_of_string (List.nth line 1))
  | "Access" -> Access (int_of_string (List.nth line 1))
  | "Fun"    -> Fun (List.nth line 1)
  | _        -> raise (Invalid_argument "Invalid designation")

let parse_designation_list line = 
  let vars = Str.split (Str.regexp ";") line in 
  List.map (function x -> parse_designation @@ List.filter (fun str -> str <> "") @@ Str.split (Str.regexp "[ ()]") x) vars  

let print_list l = 
  let rec print_list_ l = 
    match l with 
     hd :: tl -> print_endline @@ hd ^ " ";
     print_list_ tl
    | [] -> () in 
  print_endline "line was parsed in:";
  print_list_ l;
  print_endline ""

(* let parse_designation_list desigs () *)
let parse_insn (line : string list) (wline : string) =
  (* print_list line; *)
  match List.hd line with
  | "BINOP"  -> BINOP (List.nth line 1)
  | "CONST"  -> CONST (int_of_string (List.nth line 1))
  | "STRING" -> STRING (List.nth (Str.split (Str.regexp "[()\"]") wline) 2)
  | "SEXP"   -> SEXP (List.nth line 1, (int_of_string (List.nth line 2)))
  | "LD"     -> LD (parse_designation @@ List.tl line) 
  | "LDA"    -> LDA (parse_designation @@ List.tl line)
  | "ST"     -> ST (parse_designation @@ List.tl line)
  | "STA"    -> STA
  | "STI"    -> STI
  | "ELEM"   -> ELEM                                                
  | "LABEL"  -> LABEL (List.nth line 1)
  | "FLABEL" -> FLABEL (List.nth line 1)
  | "SLABEL" -> SLABEL (List.nth line 1)
  | "JMP"    -> JMP (List.nth line 1)
  | "CJMP"   -> CJMP (List.nth line 1, List.nth line 2)
  | "BEGIN"  -> BEGIN (List.nth line 1, int_of_string (List.nth line 2), int_of_string (List.nth line 3), [], [], [])
  | "END"    -> END
  | "CLOSURE" -> let vars =  List.nth (Str.split (Str.regexp "\]") @@ List.nth (Str.split (Str.regexp "\[") wline) 1) 0 in 
   CLOSURE (List.nth line 1, parse_designation_list vars)
  | "PROTO"  -> PROTO (List.nth line 1, List.nth line 2)
  | "PPROTO" -> PPROTO (List.nth line 1, List.nth line 2)
  | "PCALLC" -> PCALLC (int_of_string (List.nth line 1), bool_of_string (List.nth line 2))
  | "CALLC"  -> CALLC (int_of_string (List.nth line 1), bool_of_string (List.nth line 2))
  | "CALL"   -> CALL (List.nth line 1, int_of_string (List.nth line 2), bool_of_string (List.nth line 3))
  | "RET"    -> RET
  | "DROP"   -> DROP
  | "DUP"    -> DUP
  | "SWAP"   -> SWAP
  | "TAG"    -> TAG (List.nth line 1, int_of_string (List.nth line 2))
  | "ARRAY"  -> ARRAY (int_of_string (List.nth line 1))
  (* | "PATT"   -> PATT (parse_patt @@ List.nth line 1) *)
  (* | "FAIL"   -> FAIL (parse_loc @@ List.nth line 1, bool_of_string (List.nth line 2)) *)
  | "EXTERN" -> EXTERN (List.nth line 1)
  | "PUBLIC" -> PUBLIC (List.nth line 1)
  | "IMPORT" -> IMPORT (List.nth line 1)
  | "LINE"   -> LINE (int_of_string (List.nth line 1))
  | "FAIL"   -> FAIL ((1,2), false)
  | _   -> failwith @@ "instruction not parsed : " ^ List.hd line 

let run_sm_parser file = 
  let insns : insn list ref = ref [] in
  let channel = open_in file in
  (try
    while true do
      let line = input_line channel in
      (* print_endline line; *)
      let words =   List.filter (fun str -> str <> "") @@
      Str.split (Str.regexp "[\" (),]") line in 
      insns := (parse_insn words line) :: !insns 
    done
  with End_of_file ->
    close_in channel);
  List.rev !insns
  