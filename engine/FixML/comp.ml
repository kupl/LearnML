open Lang
open Util

(********************************************************)
(* naive components, including all syntactic components *)
(********************************************************)

let all_component () =
	let e_t = BatSet.empty in
	let e_t = BatSet.add (0,Const 0) e_t in
	let e_t = BatSet.add (0,Const 1) e_t in
	let e_t = BatSet.add (0,TRUE) e_t in
	let e_t = BatSet.add (0,FALSE) e_t in
	let e_t = BatSet.add (0,EList []) e_t in
	let e_t = BatSet.add (0,(ADD (dummy_hole (),dummy_hole ()))) e_t in
	let e_t = BatSet.add (0,(SUB (dummy_hole (),dummy_hole ()))) e_t in
	let e_t = BatSet.add (0,(MUL (dummy_hole (),dummy_hole ()))) e_t in
	let e_t = BatSet.add (0,(DIV (dummy_hole (),dummy_hole ()))) e_t in
	let e_t = BatSet.add (0,(MOD (dummy_hole (),dummy_hole ()))) e_t in
	let e_t = BatSet.add (0,(MINUS (dummy_hole ()))) e_t in
	let e_t = BatSet.add (0,(NOT (dummy_hole ()))) e_t in
	let e_t = BatSet.add (0,(OR (dummy_hole (),dummy_hole ()))) e_t in
	let e_t = BatSet.add (0,(AND (dummy_hole (),dummy_hole ()))) e_t in
	let e_t = BatSet.add (0,(LESS (dummy_hole (),dummy_hole ()))) e_t in
	let e_t = BatSet.add (0,(LARGER (dummy_hole (),dummy_hole ()))) e_t in
	let e_t = BatSet.add (0,(EQUAL (dummy_hole (),dummy_hole ()))) e_t in
	let e_t = BatSet.add (0,(NOTEQ (dummy_hole (),dummy_hole ()))) e_t in
	let e_t = BatSet.add (0,(LESSEQ (dummy_hole (),dummy_hole ()))) e_t in
	let e_t = BatSet.add (0,(LARGEREQ (dummy_hole (),dummy_hole ()))) e_t in
	let e_t = BatSet.add (0,(EApp (dummy_hole (),dummy_hole ()))) e_t in
	let e_t = BatSet.add (0,(AT (dummy_hole (),dummy_hole ()))) e_t in
	let e_t = BatSet.add (0,(DOUBLECOLON (dummy_hole (),dummy_hole ()))) e_t in
	let e_t = BatSet.add (0,(IF (dummy_hole (),dummy_hole (),dummy_hole ()))) e_t in
	(* let e_t = BatSet.add EUnit e_t in *)
	(* let e_t = BatSet.add (STRCON (Hole 0, Hole 0)) e_t in *)
	(* let e_t = BatSet.add (Raise (Hole 0)) e_t in *)
	e_t

(******************************************************)
(* 	 Code for extract components from correct code	  *)
(******************************************************)
let rec find_component : lexp -> components -> components
= fun (_, e) comps->
	match e with
	| Const _ | TRUE | FALSE | String _ -> BatSet.add (0, e) comps
	| ADD (e1,e2) ->
		let comps = find_component e1 comps in
		let comps = find_component e2 comps in
		(BatSet.add (0, (ADD (dummy_hole(),dummy_hole()))) comps)
	| SUB (e1,e2) ->
		let comps = find_component e1 comps in
		let comps = find_component e2 comps in
		(BatSet.add (0, (SUB (dummy_hole(),dummy_hole()))) comps)
	| MUL (e1,e2) ->
		let comps = find_component e1 comps in
		let comps = find_component e2 comps in
		(BatSet.add (0, (MUL (dummy_hole(),dummy_hole()))) comps)
	| DIV (e1,e2) ->
		let comps = find_component e1 comps in
		let comps = find_component e2 comps in
		(BatSet.add (0, (DIV (dummy_hole(),dummy_hole()))) comps)
	| MOD (e1,e2) ->
		let comps = find_component e1 comps in
		let comps = find_component e2 comps in
		(BatSet.add (0, (MOD (dummy_hole(),dummy_hole()))) comps)
	| NOT e1 -> 
		let comps = find_component e1 comps in
		(BatSet.add (0, (NOT (dummy_hole()))) comps)
	| MINUS e1 -> 
		let comps = find_component e1 comps in
		(BatSet.add (0, (MINUS (dummy_hole()))) comps)
	| OR (e1,e2) ->
		let comps = find_component e1 comps in
		let comps = find_component e2 comps in
		(BatSet.add (0, (OR (dummy_hole(),dummy_hole()))) comps)
	| AND (e1,e2) ->
		let comps = find_component e1 comps in
		let comps = find_component e2 comps in
		(BatSet.add (0, (AND (dummy_hole(),dummy_hole()))) comps)
	| LESS (e1,e2) ->
		let comps = find_component e1 comps in
		let comps = find_component e2 comps in
		(BatSet.add (0, (LESS (dummy_hole(),dummy_hole()))) comps)
	| LARGER (e1,e2) ->
		let comps = find_component e1 comps in
		let comps = find_component e2 comps in
		(BatSet.add (0, (LESS (dummy_hole(),dummy_hole()))) comps)
	| EQUAL (e1,e2) ->
		let comps = find_component e1 comps in
		let comps = find_component e2 comps in
		(BatSet.add (0, (EQUAL (dummy_hole(),dummy_hole()))) comps)
	| NOTEQ (e1,e2) ->
		let comps = find_component e1 comps in
		let comps = find_component e2 comps in
		(BatSet.add (0, (NOTEQ (dummy_hole(),dummy_hole()))) comps)
	| LESSEQ (e1,e2) ->
		let comps = find_component e1 comps in
		let comps = find_component e2 comps in
		(BatSet.add (0, (LESSEQ (dummy_hole(),dummy_hole()))) comps)
	| LARGEREQ (e1,e2) ->
		let comps = find_component e1 comps in
		let comps = find_component e2 comps in
		(BatSet.add (0, (LESSEQ (dummy_hole(),dummy_hole()))) comps)
	| EApp (e1,e2) ->
		let comps = find_component e1 comps in
		let comps = find_component e2 comps in
		(BatSet.add (0, (EApp (dummy_hole(),dummy_hole()))) comps)
	| IF (e1,e2,e3) ->
		let comps = find_component e1 comps in
		let comps = find_component e2 comps in
		let comps = find_component e3 comps in
		(BatSet.add (0, (IF (dummy_hole(),dummy_hole(),dummy_hole()))) comps)
	| AT (e1,e2) ->
		let comps = find_component e1 comps in
		let comps = find_component e2 comps in
		(BatSet.add (0, (AT (dummy_hole(),dummy_hole()))) comps)
	| DOUBLECOLON (e1,e2) ->
		let comps = find_component e1 comps in
		let comps = find_component e2 comps in
		(BatSet.add (0, (DOUBLECOLON (dummy_hole(),dummy_hole()))) comps)
	| EList lst ->
		let comps = list_fold find_component lst comps in
		BatSet.add (0, (EList (list_map (fun e -> dummy_hole()) lst))) comps
	| ETuple lst ->
		let comps = list_fold find_component lst comps in
		BatSet.add (0, (ETuple (list_map (fun e -> dummy_hole()) lst))) comps
	| ELet (f,is_rec,xs,t,e1,e2) ->
		let comps = find_component e1 comps in
		let comps = find_component e2 comps in
		if (is_rec) then (BatSet.add (0, (ELet (BindOne("__f__"),is_rec,xs,t,dummy_hole(),dummy_hole()))) comps)
		else comps
	| EFun (arg,e1) -> 
		let comps = find_component e1 comps in
		(BatSet.add (0, (EFun (ArgOne("__x__",TVar "comp"),dummy_hole()))) comps)
	| ECtor (x,lst) -> list_fold find_component lst comps
	| EMatch (e,lst) ->
		let comps = find_component e comps in
	 	let (pl,el) = List.split lst in
	 	let comps = list_fold find_component el comps in
	 	let el = list_map (fun e -> dummy_hole()) el in
	 	BatSet.add (0, (EMatch (dummy_hole(),list_combine pl el))) comps
	|_ -> comps

let extracts : decl -> components -> components
= fun decl comps ->
	match decl with
	| DLet (_, _, _, _, exp) ->
		find_component exp comps
	| DBlock (_, bindings) ->
		list_fold (fun (_, _, _, _, exp) comps -> find_component exp comps) bindings comps
	| _ -> comps

let extract_component : prog -> components
= fun decls -> (list_fold extracts decls BatSet.empty)
