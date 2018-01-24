open Lang
open Util
(*
 ******************************************************
 	Code for extract components from correct code
 ******************************************************
*)

let rec find_component : exp-> components -> components
= fun e comps->
	match e with
	| ADD (e1,e2) ->
		let comps = find_component e1 comps in
		let comps = find_component e2 comps in
		(BatSet.add (ADD (Hole (0),Hole (0))) comps)
	| SUB (e1,e2) ->
		let comps = find_component e1 comps in
		let comps = find_component e2 comps in
		(BatSet.add (SUB (Hole (0),Hole (0))) comps)
	| MUL (e1,e2) ->
		let comps = find_component e1 comps in
		let comps = find_component e2 comps in
		(BatSet.add (MUL (Hole (0),Hole (0))) comps)
	| DIV (e1,e2) ->
		let comps = find_component e1 comps in
		let comps = find_component e2 comps in
		(BatSet.add (DIV (Hole (0),Hole (0))) comps)
	| MOD (e1,e2) ->
		let comps = find_component e1 comps in
		let comps = find_component e2 comps in
		(BatSet.add (MOD (Hole (0),Hole (0))) comps)
	| NOT e1 -> 
		let comps = find_component e1 comps in
		(BatSet.add (NOT (Hole (0))) comps)
	| MINUS e1 -> 
		let comps = find_component e1 comps in
		(BatSet.add (MINUS (Hole (0))) comps)
	| OR (e1,e2) ->
		let comps = find_component e1 comps in
		let comps = find_component e2 comps in
		(BatSet.add (OR (Hole (0),Hole (0))) comps)
	| AND (e1,e2) ->
		let comps = find_component e1 comps in
		let comps = find_component e2 comps in
		(BatSet.add (AND (Hole (0),Hole (0))) comps)
	| LESS (e1,e2) ->
		let comps = find_component e1 comps in
		let comps = find_component e2 comps in
		(BatSet.add (LESS (Hole (0),Hole (0))) comps)
	| LARGER (e1,e2) ->
		let comps = find_component e1 comps in
		let comps = find_component e2 comps in
		(BatSet.add (LESS (Hole (0),Hole (0))) comps)
	| EQUAL (e1,e2) ->
		let comps = find_component e1 comps in
		let comps = find_component e2 comps in
		(BatSet.add (EQUAL (Hole (0),Hole (0))) comps)
	| NOTEQ (e1,e2) ->
		let comps = find_component e1 comps in
		let comps = find_component e2 comps in
		(BatSet.add (NOTEQ (Hole (0),Hole (0))) comps)
	| LESSEQ (e1,e2) ->
		let comps = find_component e1 comps in
		let comps = find_component e2 comps in
		(BatSet.add (LESSEQ (Hole (0),Hole (0))) comps)
	| LARGEREQ (e1,e2) ->
		let comps = find_component e1 comps in
		let comps = find_component e2 comps in
		(BatSet.add (LESSEQ (Hole (0),Hole (0))) comps)
	| EVar _ -> comps
	| EApp (e1,e2) ->
		let comps = find_component e1 comps in
		let comps = find_component e2 comps in
		(BatSet.add (EApp (Hole (0),Hole (0))) comps)
	| IF (e1,e2,e3) ->
		let comps = find_component e1 comps in
		let comps = find_component e2 comps in
		let comps = find_component e3 comps in
		(BatSet.add (IF (Hole (0),Hole (0),Hole (0))) comps)
	| AT (e1,e2) ->
		let comps = find_component e1 comps in
		let comps = find_component e2 comps in
		(BatSet.add (AT (Hole (0),Hole (0))) comps)
	| DOUBLECOLON (e1,e2) ->
		let comps = find_component e1 comps in
		let comps = find_component e2 comps in
		(BatSet.add (DOUBLECOLON (Hole (0),Hole (0))) comps)
	| EList lst ->
		let comps = list_fold find_component lst comps in
		BatSet.add (EList (list_map (fun e -> Hole (0)) lst)) comps
	| ETuple lst ->
		let comps = list_fold find_component lst comps in
		BatSet.add (ETuple (list_map (fun e -> Hole (0)) lst)) comps
	| ELet (f,is_rec,xs,t,e1,e2) ->
		let comps = find_component e1 comps in
		let comps = find_component e2 comps in
    if (is_rec) then (BatSet.add (ELet (BindOne("__f__"),is_rec,xs,t,Hole (0),Hole (0))) comps)
    else comps
	| EFun (arg,e1) -> 
		let comps = find_component e1 comps in
		(BatSet.add (EFun (ArgOne("__x__",TVar "comp"),Hole (0))) comps)
	| ECtor (x,lst) -> list_fold find_component lst comps
	| EMatch (e,lst) ->
		let comps = find_component e comps in
	 	let (pl,el) = List.split lst in
	 	let comps = list_fold find_component el comps in
	 	let el = list_map (fun e -> Hole (0)) el in
	 	BatSet.add (EMatch (Hole (0),list_combine pl el)) comps
	|_ -> BatSet.add e comps

let extracts : decl -> components -> components
= fun decl comps ->
	match decl with
	| DLet (x,is_rec,args,typ,exp) ->
		find_component exp comps
	| _ -> comps

let extract_component : prog -> components
= fun decls -> (list_fold extracts decls BatSet.empty)
