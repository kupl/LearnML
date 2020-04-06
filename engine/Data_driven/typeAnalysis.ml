open Lang
open Util
open Print 

module T = Type_annotate

(* Compute type and type environment of each label *)
module LabelType = Type.HoleType (* label -> typ *)
module VarType = Type.VariableType (* label -> var -> typ*)

type t = LabelType.t * VarType.t

let print (l_t, v_t) = LabelType.print l_t; VarType.print v_t

let rec gen_partial_exp : label -> lexp -> lexp
= fun label (l, exp) ->
  if l =label then (l, gen_hole ())
  else 
    let exp = 
      begin match exp with
      | NOT e -> NOT (gen_partial_exp label e)
      | MINUS e -> MINUS (gen_partial_exp label e)
      | EFun (arg, e) -> EFun (arg, gen_partial_exp label e)
      | Raise e -> Raise (gen_partial_exp label e)
      | ADD (e1, e2) -> ADD (gen_partial_exp label e1, gen_partial_exp label e2)
      | SUB (e1, e2) -> SUB (gen_partial_exp label e1, gen_partial_exp label e2)
      | MUL (e1, e2) -> MUL (gen_partial_exp label e1, gen_partial_exp label e2)
      | DIV (e1, e2) -> DIV (gen_partial_exp label e1, gen_partial_exp label e2)
      | MOD (e1, e2) -> MOD (gen_partial_exp label e1, gen_partial_exp label e2)
      | OR (e1, e2) -> OR (gen_partial_exp label e1, gen_partial_exp label e2)
      | AND (e1, e2) -> AND (gen_partial_exp label e1, gen_partial_exp label e2)
      | LESS (e1, e2) -> LESS (gen_partial_exp label e1, gen_partial_exp label e2)
      | LARGER (e1, e2) -> LARGER (gen_partial_exp label e1, gen_partial_exp label e2)
      | EQUAL (e1, e2) -> EQUAL (gen_partial_exp label e1, gen_partial_exp label e2)
      | NOTEQ (e1, e2) -> NOTEQ (gen_partial_exp label e1, gen_partial_exp label e2)
      | LESSEQ (e1, e2) -> LESSEQ (gen_partial_exp label e1, gen_partial_exp label e2)
      | LARGEREQ (e1, e2) -> LARGEREQ (gen_partial_exp label e1, gen_partial_exp label e2)
      | AT (e1, e2) -> AT (gen_partial_exp label e1, gen_partial_exp label e2)
      | DOUBLECOLON (e1, e2) -> DOUBLECOLON (gen_partial_exp label e1, gen_partial_exp label e2)
      | STRCON (e1, e2) -> STRCON (gen_partial_exp label e1, gen_partial_exp label e2)
      | EApp (e1, e2) -> EApp (gen_partial_exp label e1, gen_partial_exp label e2)
      | ELet (f, is_rec, args, t, e1, e2) -> ELet (f, is_rec, args, t, gen_partial_exp label e1, gen_partial_exp label e2)
      | IF (e1, e2, e3) -> IF (gen_partial_exp label e1, gen_partial_exp label e2, gen_partial_exp label e3)
      | ECtor (x, elst) -> ECtor (x, List.map (gen_partial_exp label) elst)
      | EList elst -> EList (List.map (gen_partial_exp label) elst)
      | ETuple elst -> ETuple (List.map (gen_partial_exp label) elst)
      | EBlock (is_rec, bindings, e) ->
        let bindings = List.map (fun (f, is_rec, args, t, e) -> (f, is_rec, args, t, gen_partial_exp label e)) bindings in
        EBlock (is_rec, bindings, gen_partial_exp label e)
      | EMatch (e, blst) -> EMatch (gen_partial_exp label e, List.map (fun (p, e) -> (p,gen_partial_exp label e)) blst)
      | _ -> exp
      end in
    (l,exp) 

let gen_partial_decl : label -> decl -> decl
= fun label decl ->
  match decl with
  | DLet (f, is_rec, args, t, e) -> DLet (f, is_rec, args, t, gen_partial_exp label e)
  | DBlock (is_rec, bindings) ->
    let bindings = List.map (fun (f, is_rec, args, t, e) -> (f, is_rec, args, t, gen_partial_exp label e)) bindings in
    DBlock (is_rec, bindings) 
  | _ -> decl

let gen_partial_pgm : label -> prog -> prog
= fun label pgm -> List.map (fun decl -> gen_partial_decl label decl) pgm

let rec get_labels_exp : lexp -> label BatSet.t
= fun (l, exp) ->
	match exp with
	| EList es | ECtor (_, es) | ETuple es -> 
		List.fold_left (fun labels e -> BatSet.union labels (get_labels_exp e)) BatSet.empty es
  | EFun (_, e) | MINUS e | NOT e -> BatSet.union (get_labels_exp e) (BatSet.singleton l)
  | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2) 
  | OR (e1, e2) | AND (e1, e2) | LESS (e1, e2) | LARGER (e1, e2) | LESSEQ (e1, e2) | LARGEREQ (e1, e2) 
  | EQUAL (e1, e2) | NOTEQ (e1, e2) | AT (e1, e2) | DOUBLECOLON (e1, e2) | STRCON (e1, e2) | EApp (e1, e2) 
  | ELet (_, _, _, _, e1, e2) ->
  	BatSet.singleton l
  	|> BatSet.union (get_labels_exp e1)
  	|> BatSet.union (get_labels_exp e2)
  | EBlock (_, ds, e) ->
  	let es = e::(List.map (fun (_, _, _, _, e) -> e) ds) in
		List.fold_left (fun labels e -> BatSet.union labels (get_labels_exp e)) BatSet.empty es
  | EMatch (e, bs) ->
    let es = e::(List.map (fun (p, e) -> e) bs) in
		List.fold_left (fun labels e -> BatSet.union labels (get_labels_exp e)) BatSet.empty es
  | IF (e1, e2, e3) ->
  	BatSet.singleton l
  	|> BatSet.union (get_labels_exp e1)
  	|> BatSet.union (get_labels_exp e2)
  	|> BatSet.union (get_labels_exp e3)
  | _ -> BatSet.singleton l

let get_labels_decl : decl -> label BatSet.t 
= fun decl ->
	match decl with
  | DLet (_, _, _, _, e) -> get_labels_exp e
  | DBlock (_, ds) -> List.fold_left (fun labels (_, _, _, _, e) -> BatSet.union labels (get_labels_exp e)) BatSet.empty ds 
  | _ -> BatSet.empty

let get_labels : prog -> label BatSet.t
= fun pgm -> List.fold_left (fun labels decl -> BatSet.union labels (get_labels_decl decl)) BatSet.empty pgm

let run : prog -> t
= fun pgm ->
	let labels = get_labels pgm in
	BatSet.fold (fun l (l_t, v_t) ->
		let pgm' = gen_partial_pgm l pgm in
		let (_, l_t', v_t', subst) = Type.run pgm' in
		(BatMap.union l_t l_t', BatMap.union v_t v_t')
	) labels (LabelType.empty, VarType.empty)

