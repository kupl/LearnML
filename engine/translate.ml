open Lang
open Util

let is_hole : lexp -> bool
= fun (_, exp) ->
	match exp with
	| Hole _ -> true
	| _ -> false

let rec exp_is_closed : lexp -> bool
= fun (l, e) ->
  match e with
  | EList es | ECtor (_, es) | ETuple es -> List.for_all exp_is_closed es
  | MINUS e | NOT e | EFun (_, e) | Raise e -> exp_is_closed e
  | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2)
  | OR (e1, e2) | AND (e1, e2) | LESS (e1, e2) | LARGER (e1, e2) | EQUAL (e1, e2) | NOTEQ (e1, e2)
  | LESSEQ (e1, e2) | LARGEREQ (e1, e2) | AT (e1, e2) | DOUBLECOLON (e1, e2) | STRCON (e1, e2)
  | EApp (e1, e2) | ELet (_, _, _, _, e1, e2) -> exp_is_closed e1 && exp_is_closed e2
  | EBlock (_, ds, e2) -> 
    let es = List.map (fun (f, is_rec, args, typ, e) -> e) ds in
    (exp_is_closed e2) && (List.for_all exp_is_closed es)
  | EMatch (e, bs) ->
    let (ps, es) = List.split bs in
    exp_is_closed e && (List.for_all exp_is_closed es)
  | IF (e1, e2, e3) -> exp_is_closed e1 && exp_is_closed e2 && exp_is_closed e3
  | Hole n -> false
  | _ -> true

(* Translation Rule *)
let rec translate_exp : lexp -> lexp
= fun (l,exp) ->
  let exp = 
  	begin match exp with
    | EList es ->  EList (List.map translate_exp es)
    | ETuple es -> ETuple (List.map translate_exp es)
    | ECtor (x, es) -> ECtor (x, List.map translate_exp es)
    | ADD (e1, e2) ->
    	let (e1, e2) = (translate_exp e1, translate_exp e2) in
    	if ((is_hole e1) || (is_hole e2)) then gen_hole () else ADD (e1, e2)
    | SUB (e1, e2) ->
    	let (e1, e2) = (translate_exp e1, translate_exp e2) in
    	if ((is_hole e1) || (is_hole e2)) then gen_hole () else SUB (e1, e2)
    | MUL (e1, e2) ->
    	let (e1, e2) = (translate_exp e1, translate_exp e2) in
    	if ((is_hole e1) && (is_hole e2)) then gen_hole () else MUL (e1, e2)
    | DIV (e1, e2) ->
    	let (e1, e2) = (translate_exp e1, translate_exp e2) in
    	if ((is_hole e1) && (is_hole e2)) then gen_hole () else DIV (e1, e2)
    | MOD (e1, e2) ->
    	let (e1, e2) = (translate_exp e1, translate_exp e2) in
    	if ((is_hole e1) && (is_hole e2)) then gen_hole () else MOD (e1, e2)
    | MINUS e ->
    	let e = translate_exp e in
    	if (is_hole e) then gen_hole () else MINUS e
    | NOT e ->
    	let e = translate_exp e in
    	if (is_hole e) then gen_hole () else NOT e
    | OR (e1, e2) ->
    	let (e1, e2) = (translate_exp e1, translate_exp e2) in
    	if ((is_hole e1) && (is_hole e2)) then gen_hole () else OR (e1, e2)
    | AND (e1, e2) ->
    	let (e1, e2) = (translate_exp e1, translate_exp e2) in
    	if ((is_hole e1) && (is_hole e2)) then gen_hole () else AND (e1, e2)
    | LESS (e1, e2) ->
    	let (e1, e2) = (translate_exp e1, translate_exp e2) in
    	if ((is_hole e1) || (is_hole e2)) then gen_hole () else LESS (e1, e2)
    | LARGER (e1, e2) ->
    	let (e1, e2) = (translate_exp e1, translate_exp e2) in
    	if ((is_hole e1) || (is_hole e2)) then gen_hole () else LARGER (e1, e2)
    | EQUAL (e1, e2) ->
    	let (e1, e2) = (translate_exp e1, translate_exp e2) in
    	if ((is_hole e1) || (is_hole e2)) then gen_hole () else EQUAL (e1, e2)
    | NOTEQ (e1, e2) ->
    	let (e1, e2) = (translate_exp e1, translate_exp e2) in
    	if ((is_hole e1) || (is_hole e2)) then gen_hole () else NOTEQ (e1, e2)
    | LESSEQ (e1, e2) ->
    	let (e1, e2) = (translate_exp e1, translate_exp e2) in
    	if ((is_hole e1) || (is_hole e2)) then gen_hole () else LESSEQ (e1, e2)
    | LARGEREQ (e1, e2) ->
    	let (e1, e2) = (translate_exp e1, translate_exp e2) in
    	if ((is_hole e1) || (is_hole e2)) then gen_hole () else LARGEREQ (e1, e2)
    | EApp (e1, e2) ->
    	let (e1, e2) = (translate_exp e1, translate_exp e2) in
    	if is_hole e1 then gen_hole () else EApp (e1, e2)
    | EFun (args, e) -> 
    	let e = translate_exp e in
    	if is_hole e then gen_hole () else EFun (args, e)
    | ELet (f, is_rec, args, typ, e1, e2) -> 
    	let (e1, e2) = (translate_exp e1, translate_exp e2) in
    	if is_hole e2 then gen_hole () else  ELet (f, is_rec, args, typ, e1, e2)
    | EMatch (e, bs) ->
    	let (ps, es) = List.split bs in
    	let es = List.map translate_exp es in
    	if (exp_is_closed e) then EMatch (e, List.combine ps es) else gen_hole () 
    | IF (e1, e2, e3) ->
    	if (exp_is_closed e1) then IF (translate_exp e1, translate_exp e2, translate_exp e3) else gen_hole() 
    | AT (e1, e2) ->
    	let (e1, e2) = (translate_exp e1, translate_exp e2) in
    	if ((is_hole e1) && (is_hole e2)) then gen_hole () else AT (e1, e2)
    | DOUBLECOLON (e1, e2) ->
    	let (e1, e2) = (translate_exp e1, translate_exp e2) in
    	if ((is_hole e1) && (is_hole e2)) then gen_hole () else DOUBLECOLON (e1, e2)
    | _ -> exp
  end in
  (l, exp)

let translate_decl : decl -> decl
= fun decl ->
	match decl with
  | DLet (f, is_rec, args, typ, exp) ->
    DLet (f, is_rec, args, typ, translate_exp exp) 
    
  | DBlock (is_rec, bindings) ->
    let bindings = List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, args, typ, translate_exp e)) bindings in
    DBlock (is_rec, bindings)
  | _ -> decl
 

let translate : prog -> prog
= fun decls -> List.map translate_decl decls