open Lang
open Util
open Print

(* Reapair template = expression template * required functions definition *)
type exp_template = 
  | ModifyExp of label * lexp (* Modify (l, E) -> (l' E') *)
  | InsertBranch of label * branch (* Insert branch to label l *)
  | DeleteBranch of label * branch (* Delete branch b at label l *)
  | Explore of label (* Explore label l *)
and exp_templates = exp_template BatSet.t

type required_function = (id, bool * arg list * typ * lexp * id BatSet.t) BatMap.t (* function id -> func info * caller functions *)

type repair_template = exp_template * required_function
type call_templates = (id, lexp BatSet.t) BatMap.t

let merge_templates : repair_template BatSet.t -> exp_templates * required_function
= fun temps -> 
	BatSet.fold (fun (e_temp, d_temp) (e_temps, d_temps) ->
	  (BatSet.add e_temp e_temps, BatMap.union d_temp d_temps)
	) temps (BatSet.empty, BatMap.empty)

(* To string *)
let string_of_exp_template : exp_template -> string
= fun temp ->
  match temp with
  | ModifyExp (l, e) -> "Modify (" ^ string_of_int l ^ " : " ^ exp_to_string e ^ ")" 
  | InsertBranch (l, (p, e)) -> "Insert (" ^ pat_to_string p ^ " -> " ^ exp_to_string e ^ " At label " ^ string_of_int l
	| DeleteBranch (l, (p, e)) -> "Delete (" ^ pat_to_string p ^ " -> " ^ exp_to_string e ^ " At label " ^ string_of_int l
	| Explore l -> "Explore label " ^ string_of_int l

let string_of_required_function : required_function -> string 
= fun d_temp -> 
  BatMap.foldi (fun f (is_rec, args, typ, body, callers) acc ->
    acc ^ (decl_to_string (DLet (BindOne f, is_rec, args, typ, body)) "") ^ "\n" ^
    "Callers : " ^ string_of_set id callers ^ "\n"
  ) d_temp ""
  
let string_of_template : repair_template -> string
= fun (e_temp, d_temp) -> 
  "Exp : " ^ string_of_exp_template e_temp ^ "\n\n" ^
  "Decls : \n" ^ string_of_required_function d_temp
  
let string_of_templates : repair_template BatSet.t -> string 
= fun temp ->
	let (e_temps, d_temp) = merge_templates temp in 
  "Exp : " ^ string_of_set string_of_exp_template e_temps ^ "\n\n" ^
  "Decls : \n" ^ string_of_required_function d_temp

(******************)
(* Template apply *)
(******************)
let normalize_template : exp_template -> exp_template
= fun e_temp ->
	match e_temp with
	| ModifyExp (l, e) -> ModifyExp (l, Normalize.normalize_exp e)
  | InsertBranch (l, (p, e)) -> InsertBranch (l, (p, Normalize.normalize_exp e))
  | DeleteBranch (l, (p, e)) -> DeleteBranch (l, (p, Normalize.normalize_exp e))
  | _ -> e_temp
  
(* Modification *)
let rec modify_exp : lexp -> label * lexp -> lexp
= fun (l, exp) (l', exp') ->
	if l = l' then exp' else 
	let exp = 
	  match exp with
	  | Raise e -> Raise (modify_exp e (l', exp'))
	  | EFun (arg, e) -> EFun (arg, (modify_exp e (l', exp')))
	  | MINUS e -> MINUS (modify_exp e (l', exp'))
	  | NOT e -> NOT (modify_exp e (l', exp'))
	  | ADD (e1, e2) -> ADD (modify_exp e1 (l', exp'), modify_exp e2 (l', exp'))
	  | SUB (e1, e2) -> SUB (modify_exp e1 (l', exp'), modify_exp e2 (l', exp'))
	  | MUL (e1, e2) -> MUL (modify_exp e1 (l', exp'), modify_exp e2 (l', exp'))
	  | DIV (e1, e2) -> DIV (modify_exp e1 (l', exp'), modify_exp e2 (l', exp'))
	  | MOD (e1, e2) -> MOD (modify_exp e1 (l', exp'), modify_exp e2 (l', exp'))
	  | OR (e1, e2) -> OR (modify_exp e1 (l', exp'), modify_exp e2 (l', exp'))
	  | AND (e1, e2) -> AND (modify_exp e1 (l', exp'), modify_exp e2 (l', exp'))
	  | LESS (e1, e2) -> LESS (modify_exp e1 (l', exp'), modify_exp e2 (l', exp'))
	  | LESSEQ (e1, e2) -> LESSEQ (modify_exp e1 (l', exp'), modify_exp e2 (l', exp'))
	  | LARGER (e1, e2) -> LARGER (modify_exp e1 (l', exp'), modify_exp e2 (l', exp'))
	  | LARGEREQ (e1, e2) -> LARGEREQ (modify_exp e1 (l', exp'), modify_exp e2 (l', exp'))
	  | EQUAL (e1, e2) -> EQUAL (modify_exp e1 (l', exp'), modify_exp e2 (l', exp'))
	  | NOTEQ (e1, e2) -> NOTEQ (modify_exp e1 (l', exp'), modify_exp e2 (l', exp'))
	  | DOUBLECOLON (e1, e2) -> DOUBLECOLON (modify_exp e1 (l', exp'), modify_exp e2 (l', exp'))
	  | AT (e1, e2) -> AT (modify_exp e1 (l', exp'), modify_exp e2 (l', exp'))
	  | STRCON (e1, e2) -> STRCON (modify_exp e1 (l', exp'), modify_exp e2 (l', exp'))
	  | EApp (e1, e2) -> EApp (modify_exp e1 (l', exp'), modify_exp e2 (l', exp'))
	  | EList es -> EList (List.map (fun e -> modify_exp e (l', exp')) es)
	  | ETuple es -> ETuple (List.map (fun e -> modify_exp e (l', exp')) es)
	  | ECtor (x, es) -> ECtor (x, List.map (fun e -> modify_exp e (l', exp')) es)
	  | IF (e1, e2, e3) -> IF (modify_exp e1 (l', exp'), modify_exp e2 (l', exp'), modify_exp e3 (l', exp'))
	  | EMatch (e, bs) -> EMatch (modify_exp e (l', exp'), List.map (fun (p, e) -> (p, modify_exp e (l', exp'))) bs)
	  | ELet (f, is_rec, args, typ, e1, e2) -> ELet (f, is_rec, args, typ, modify_exp e1 (l', exp'), modify_exp e2 (l', exp'))
	  | EBlock (is_rec, bindings, e2) -> EBlock (is_rec, List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, args, typ, modify_exp e (l', exp'))) bindings, modify_exp e2 (l', exp'))
	  | _ -> exp
	in
	(l, exp)

let rec modify_decl : decl -> label * lexp -> decl
= fun decl temp ->
	match decl with
	| DLet (f, is_rec, args, typ, e) -> DLet (f, is_rec, args, typ, modify_exp e temp)
	| DBlock (is_rec, bindings) ->
	  let bindings = List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, args, typ, modify_exp e temp)) bindings in
	  DBlock (is_rec, bindings) 
	| _ -> decl

let rec modify_pgm : prog -> label * lexp -> prog
= fun pgm temp -> List.map (fun decl -> modify_decl decl temp) pgm

(* Insertion *)
let rec compare_pat : pat -> pat -> int
= fun p1 p2 ->
	match (p1, p2) with
	| _, PVar _ | _, PUnder -> -1
	| PInt n1, PInt n2 -> if n1 < n2 then -1 else 1
  | PList ps1, PList ps2 | PTuple ps1, PTuple ps2 -> compare_pat_list ps1 ps2
  | PCtor (x, ps1), PCtor (y, ps2) -> if x = y then compare_pat_list ps1 ps2 else 1
  | PCons (phd1, ptl1), PCons (phd2, ptl2) -> if (compare_pat phd1 phd2) < 0 then -1 else compare_pat ptl1 ptl2
  | PList _, PCons _ -> -1
  | _ -> 1

and compare_pat_list : pat list -> pat list -> int
= fun ps1 ps2 ->
	match (ps1, ps2) with
	| [], [] -> 1
	| [], ps2 -> -1 (* if ps1 is shorter *)
	| ps1, [] -> 1 (* if ps1 is longer *)
	| phd1::ptl1, phd2::ptl2 -> if (compare_pat phd1 phd2) < 0 then -1 else compare_pat_list ptl1 ptl2

let rec insert_branch : branch -> branch list -> branch list
= fun b bs ->
	match bs with
	| [] -> [b]
	| hd::tl -> 
		(* Insert branch before wild card (default case) *)
		if compare_pat (fst b) (fst hd) < 0 then b::bs else hd::(insert_branch b tl) 

let rec insert_branch_exp : lexp -> label * branch -> lexp
= fun (l, exp) (l', b) ->
	let exp =
	  match exp with
	  | EMatch (e, bs) -> if l = l' then EMatch (e, insert_branch b bs) else EMatch (insert_branch_exp e (l', b), List.map (fun (p, e) -> (p, insert_branch_exp e (l', b))) bs)
	  | Raise e -> Raise (insert_branch_exp e (l', b))
	  | EFun (arg, e) -> EFun (arg, (insert_branch_exp e (l', b)))
	  | MINUS e -> MINUS (insert_branch_exp e (l', b))
	  | NOT e -> NOT (insert_branch_exp e (l', b))
	  | ADD (e1, e2) -> ADD (insert_branch_exp e1 (l', b), insert_branch_exp e2 (l', b))
	  | SUB (e1, e2) -> SUB (insert_branch_exp e1 (l', b), insert_branch_exp e2 (l', b))
	  | MUL (e1, e2) -> MUL (insert_branch_exp e1 (l', b), insert_branch_exp e2 (l', b))
	  | DIV (e1, e2) -> DIV (insert_branch_exp e1 (l', b), insert_branch_exp e2 (l', b))
	  | MOD (e1, e2) -> MOD (insert_branch_exp e1 (l', b), insert_branch_exp e2 (l', b))
	  | OR (e1, e2) -> OR (insert_branch_exp e1 (l', b), insert_branch_exp e2 (l', b))
	  | AND (e1, e2) -> AND (insert_branch_exp e1 (l', b), insert_branch_exp e2 (l', b))
	  | LESS (e1, e2) -> LESS (insert_branch_exp e1 (l', b), insert_branch_exp e2 (l', b))
	  | LESSEQ (e1, e2) -> LESSEQ (insert_branch_exp e1 (l', b), insert_branch_exp e2 (l', b))
	  | LARGER (e1, e2) -> LARGER (insert_branch_exp e1 (l', b), insert_branch_exp e2 (l', b))
	  | LARGEREQ (e1, e2) -> LARGEREQ (insert_branch_exp e1 (l', b), insert_branch_exp e2 (l', b))
	  | EQUAL (e1, e2) -> EQUAL (insert_branch_exp e1 (l', b), insert_branch_exp e2 (l', b))
	  | NOTEQ (e1, e2) -> NOTEQ (insert_branch_exp e1 (l', b), insert_branch_exp e2 (l', b))
	  | DOUBLECOLON (e1, e2) -> DOUBLECOLON (insert_branch_exp e1 (l', b), insert_branch_exp e2 (l', b))
	  | AT (e1, e2) -> AT (insert_branch_exp e1 (l', b), insert_branch_exp e2 (l', b))
	  | STRCON (e1, e2) -> STRCON (insert_branch_exp e1 (l', b), insert_branch_exp e2 (l', b))
	  | EApp (e1, e2) -> EApp (insert_branch_exp e1 (l', b), insert_branch_exp e2 (l', b))
	  | EList es -> EList (List.map (fun e -> insert_branch_exp e (l', b)) es)
	  | ETuple es -> ETuple (List.map (fun e -> insert_branch_exp e (l', b)) es)
	  | ECtor (x, es) -> ECtor (x, List.map (fun e -> insert_branch_exp e (l', b)) es)
	  | IF (e1, e2, e3) -> IF (insert_branch_exp e1 (l', b), insert_branch_exp e2 (l', b), insert_branch_exp e3 (l', b))
	  | ELet (f, is_rec, args, typ, e1, e2) -> ELet (f, is_rec, args, typ, insert_branch_exp e1 (l', b), insert_branch_exp e2 (l', b))
	  | EBlock (is_rec, bindings, e2) -> EBlock (is_rec, List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, args, typ, insert_branch_exp e (l', b))) bindings, insert_branch_exp e2 (l', b))
	  | _ -> exp
	in
	(l, exp)

let rec insert_branch_decl : decl -> label * branch -> decl
= fun decl temp ->
	match decl with
	| DLet (f, is_rec, args, typ, e) -> DLet (f, is_rec, args, typ, insert_branch_exp e temp)
	| DBlock (is_rec, bindings) ->
	  let bindings = List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, args, typ, insert_branch_exp e temp)) bindings in
	  DBlock (is_rec, bindings) 
	| _ -> decl

let rec insert_branch_pgm : prog -> label * branch -> prog
= fun pgm temp -> List.map (fun decl -> insert_branch_decl decl temp) pgm 

(* Deletion *)
let rec delete_branch : branch -> branch list -> branch list
= fun b bs ->
	match bs with
	| [] -> []
	| hd::tl -> 
		(* Insert branch before wild card (default case) *)
		if b = hd then tl else hd::(delete_branch b tl) 

let rec delete_branch_exp : lexp -> label * branch -> lexp
= fun (l, exp) (l', b) ->
	let exp =
	  match exp with
	  | EMatch (e, bs) -> if l = l' then EMatch (e, delete_branch b bs) else EMatch (delete_branch_exp e (l', b), List.map (fun (p, e) -> (p, delete_branch_exp e (l', b))) bs)
	  | Raise e -> Raise (delete_branch_exp e (l', b))
	  | EFun (arg, e) -> EFun (arg, (delete_branch_exp e (l', b)))
	  | MINUS e -> MINUS (delete_branch_exp e (l', b))
	  | NOT e -> NOT (delete_branch_exp e (l', b))
	  | ADD (e1, e2) -> ADD (delete_branch_exp e1 (l', b), delete_branch_exp e2 (l', b))
	  | SUB (e1, e2) -> SUB (delete_branch_exp e1 (l', b), delete_branch_exp e2 (l', b))
	  | MUL (e1, e2) -> MUL (delete_branch_exp e1 (l', b), delete_branch_exp e2 (l', b))
	  | DIV (e1, e2) -> DIV (delete_branch_exp e1 (l', b), delete_branch_exp e2 (l', b))
	  | MOD (e1, e2) -> MOD (delete_branch_exp e1 (l', b), delete_branch_exp e2 (l', b))
	  | OR (e1, e2) -> OR (delete_branch_exp e1 (l', b), delete_branch_exp e2 (l', b))
	  | AND (e1, e2) -> AND (delete_branch_exp e1 (l', b), delete_branch_exp e2 (l', b))
	  | LESS (e1, e2) -> LESS (delete_branch_exp e1 (l', b), delete_branch_exp e2 (l', b))
	  | LESSEQ (e1, e2) -> LESSEQ (delete_branch_exp e1 (l', b), delete_branch_exp e2 (l', b))
	  | LARGER (e1, e2) -> LARGER (delete_branch_exp e1 (l', b), delete_branch_exp e2 (l', b))
	  | LARGEREQ (e1, e2) -> LARGEREQ (delete_branch_exp e1 (l', b), delete_branch_exp e2 (l', b))
	  | EQUAL (e1, e2) -> EQUAL (delete_branch_exp e1 (l', b), delete_branch_exp e2 (l', b))
	  | NOTEQ (e1, e2) -> NOTEQ (delete_branch_exp e1 (l', b), delete_branch_exp e2 (l', b))
	  | DOUBLECOLON (e1, e2) -> DOUBLECOLON (delete_branch_exp e1 (l', b), delete_branch_exp e2 (l', b))
	  | AT (e1, e2) -> AT (delete_branch_exp e1 (l', b), delete_branch_exp e2 (l', b))
	  | STRCON (e1, e2) -> STRCON (delete_branch_exp e1 (l', b), delete_branch_exp e2 (l', b))
	  | EApp (e1, e2) -> EApp (delete_branch_exp e1 (l', b), delete_branch_exp e2 (l', b))
	  | EList es -> EList (List.map (fun e -> delete_branch_exp e (l', b)) es)
	  | ETuple es -> ETuple (List.map (fun e -> delete_branch_exp e (l', b)) es)
	  | ECtor (x, es) -> ECtor (x, List.map (fun e -> delete_branch_exp e (l', b)) es)
	  | IF (e1, e2, e3) -> IF (delete_branch_exp e1 (l', b), delete_branch_exp e2 (l', b), delete_branch_exp e3 (l', b))
	  | ELet (f, is_rec, args, typ, e1, e2) -> ELet (f, is_rec, args, typ, delete_branch_exp e1 (l', b), delete_branch_exp e2 (l', b))
	  | EBlock (is_rec, bindings, e2) -> EBlock (is_rec, List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, args, typ, delete_branch_exp e (l', b))) bindings, delete_branch_exp e2 (l', b))
	  | _ -> exp
	in
	(l, exp)

let rec delete_branch_decl : decl -> label * branch -> decl
= fun decl temp ->
	match decl with
	| DLet (f, is_rec, args, typ, e) -> DLet (f, is_rec, args, typ, delete_branch_exp e temp)
	| DBlock (is_rec, bindings) ->
	  let bindings = List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, args, typ, delete_branch_exp e temp)) bindings in
	  DBlock (is_rec, bindings) 
	| _ -> decl

let rec delete_branch_pgm : prog -> label * branch -> prog
= fun pgm temp -> List.map (fun decl -> delete_branch_decl decl temp) pgm 

let rec apply_exp_temp : prog -> exp_template -> prog
= fun pgm temp ->
	match temp with
	| ModifyExp (l, e) -> modify_pgm pgm (l, e)
	| InsertBranch (l, b) -> insert_branch_pgm pgm (l, b)
	| DeleteBranch (l, b) -> delete_branch_pgm pgm (l, b)
	| _ -> pgm

(* Insert new function *)
let rec is_defined_decl : id -> decl -> bool
= fun f decl -> 
	match decl with
	| DLet (f', _, _, _, _) ->
	  begin match f' with
	  | BindOne f' -> f = f'
	  | _ -> false
	  end
	| DBlock (_, bindings) -> List.exists (fun (f', _, _, _, _) ->
	    match f' with
	    | BindOne f' -> f = f'
	    | _ -> false
	  ) bindings
	| _ -> false

let rec is_defined : id -> prog -> bool
= fun f decls -> List.exists (fun decl -> is_defined_decl f decl) decls

let rec insert_function : prog -> id * bool * arg list * typ * lexp * id BatSet.t -> prog
= fun pgm (f, is_rec, args, typ, e, caller) -> 
	match pgm with
	| [] -> []
	| hd::tl ->
	  if BatSet.exists (fun f' -> is_defined_decl f' hd) caller then
	    (DLet (BindOne f, is_rec, args, typ, e))::(pgm)
	  else hd::(insert_function tl (f, is_rec, args, typ, e, caller))

let rec apply_required_function : prog -> required_function -> prog
= fun pgm d_temp -> 
	let (pgm', d_temp') = BatMap.foldi (fun f (is_rec, args, typ, e, caller) (pgm', d_temp') ->
	  if BatSet.for_all (fun id -> is_defined id pgm) caller then 
	    (insert_function pgm' (f, is_rec, args, typ, e, caller), d_temp')
	  else
	    (pgm', BatMap.add f (is_rec, args, typ, e, caller) d_temp')
	) d_temp (pgm, BatMap.empty) in
	if BatMap.is_empty (BatMap.diff d_temp d_temp') then pgm' else apply_required_function pgm' d_temp'

let rec apply_template : prog -> repair_template -> prog
= fun pgm (e_temp, d_temp) ->
	let pgm = apply_required_function pgm d_temp in
	apply_exp_temp pgm e_temp

let rec apply_templates : prog -> repair_template BatSet.t -> prog 
= fun pgm temps ->
	let (e_temps, d_temps) = merge_templates temps in
	let pgm' = apply_required_function pgm d_temps in
	BatSet.fold (fun e_temp pgm -> apply_exp_temp pgm e_temp) e_temps pgm'

(* Redundant check *)
let check_redundant_template : prog -> repair_template -> bool
= fun pgm temp ->
  let pgm' = apply_template pgm temp in
  (program_to_string pgm) = (program_to_string pgm')