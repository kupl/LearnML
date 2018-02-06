open Lang
open Util

module type S = sig
	type aliasPair
	type equivSet
	type aliasinfo
	type env

	val closure : equivSet -> equivSet

	val empty_set : equivSet
	val kill : equivSet -> id -> equivSet
	val gen : equivSet -> (pat * exp) -> equivSet 

	val empty_alias : aliasinfo
	val update_alias : aliasinfo -> (int * equivSet) -> aliasinfo
	
	val run : prog -> aliasinfo
end


module Sem : S = struct	
	
	module Set = BatSet
	module Map = BatMap

	type aliasPair = exp * exp
	type equivSet = aliasPair Set.t
	type aliasinfo = (int, equivSet) Map.t
	type env = equivSet * aliasinfo

	let closure : equivSet -> equivSet
	= fun set -> Set.empty (*TODO*)

	let empty_set = Set.empty

	let kill : equivSet -> id -> equivSet
	= fun set var -> Set.empty (*TODO*)

	let gen : equivSet -> (pat * exp) -> equivSet
	= fun set (pattern,exp) -> Set.empty (*TODO*)

	let empty_alias = Map.empty

	let update_alias : aliasinfo -> (int * equivSet) -> aliasinfo
	= fun map (hole_num,set) -> Map.add hole_num set map

	let empty_env = (empty_set,empty_alias)

	let rec kill_pat : pat -> equivSet -> equivSet
	= fun pat s ->
		match pat with
		| PVar x -> kill s x
		| PList l | PTuple l | PCtor (_,l)  | PCons l | Pats l -> list_fold kill_pat l s
		| _ -> s

	let rec kill_arg : arg -> equivSet -> equivSet
	= fun arg s ->
		match arg with
		| ArgOne (x, _) -> kill s x
		| ArgTuple l -> list_fold kill_arg l s
		| _ -> s

	let rec arg_to_pat : arg -> pat
	= function
		| ArgUnder _ -> PUnder
		| ArgOne (x, _) -> PVar x
		| ArgTuple l -> PTuple (list_map arg_to_pat l)
	
	let rec let_bind_to_pat : let_bind -> pat
	= function
		| BindUner -> PUnder
		| BindOne x -> PVar x
		| BindTuple l -> PTuple (list_map let_bind_to_pat l)

	let rec kill_let_bind : let_bind -> equivSet -> equivSet
	= fun letbind s ->
		match letbind with
		| BindOne x -> kill s x
		| BindTuple l -> list_fold kill_let_bind l s
		| _ -> s

	let rec analysis_exp : equivSet -> exp -> aliasinfo
	= fun s exp ->
		match exp with
		| Hole n -> update_alias empty_alias (n,s)
		| EUnit | Const _ | TRUE | FALSE | String _ | EVar _ | -> empty_alias
		| NOT e | MINUS e |Raise e -> analysis_exp s e
		| ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2) | OR (e1, e2) | AND (e1, e2)
		| LESS (e1, e2) | LARGER (e1, e2) | EQUAL (e1, e2) | NOTEQ (e1, e2) | LESSEQ (e1, e2) | LARGEREQ (e1, e2)
		| AT (e1, e2) | DOUBLECOLON (e1, e2) | STRCON (e1, e2) | EApp (e1, e2) -> Map.union (analysis_exp s e1) (analysis_exp s e2)
		| EList l | ETuple l | ECtor (_, l) -> list_fold (fun e acc -> Map.union (analysis_exp s e) acc) l empty_alias
		| EFun (a, e) ->
			let s = kill_arg a s in
			let s = gen s (arg_to_pat a,e) in
			let s = closure s in
			analysis_exp s e
		| ELet (f, is_rec, args, typ, e1, e2) -> (* TODO *)
			Map.union (analysis_exp s e1)
			(
				if (args = []) then empty_alias 
				else 
					let s = kill_let_bind f s in
					let s = gen s (let_bind_to_pat f,e1) in
					let s = closure s in
					analysis_exp s e2
			)
		| EBlock (is_rec, bindings, e2) -> (* TODO *)
			let (alias_info, s) = list_fold( fun (f, is_rec, args, typ, e) (map, s) ->
				if (args = []) then (map,s) 
				else
					let alias_e1 = Map.union (analysis_exp s e) map in
					let s = kill_let_bind f s in
					let s = gen s (let_bind_to_pat f,e1) in
					let s = closure s in
					(alias_e1,s)
			) bindings (empty_alias,s) in
		| EMatch (e, bs) ->
			let alias_e = analysis_exp s e in
			Map.union alias_e (list_fold (fun (p,e) acc ->
					let s = kill_pat p s in
					let s = gen s (p,e) in
					let s = closure s in
					Map.union (analysis_exp s e) acc
				) bs empty_alias
			)


	let analysis_decl : decl -> env -> env
	= fun decl (s,map) ->
		match decl with
		| DLet (x, is_rec, args, typ, exp) ->
		| DBlock (is_rec, bindings)->
		| _ -> s

	let run : prog -> aliasinfo
	= fun pgm -> 
		let env = list_fold analysis_decl pgm empty_env in
		snd env
end
