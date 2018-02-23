open Lang
open Util

module type S = sig
	type aliasPair = pat * pat
	type equivSet = aliasPair BatSet.t
	type aliasinfo = (int, equivSet) BatMap.t
	type env = equivSet * aliasinfo

	val print : aliasinfo -> unit

	val closure : equivSet -> equivSet

	val empty_set : equivSet
	val kill : equivSet -> id -> equivSet
	val gen : equivSet -> (pat * exp) -> equivSet 

	val empty_alias : aliasinfo
	val update_alias : aliasinfo -> (int * equivSet) -> aliasinfo
	val get_aliasSet : aliasinfo -> int -> equivSet

	val run : prog -> aliasinfo
end


module Sem : S = struct	
	
	module Set = BatSet
	module Map = BatMap

	type aliasPair = pat * pat
	type equivSet = aliasPair Set.t
	type aliasinfo = (int, equivSet) Map.t
	type env = equivSet * aliasinfo

	let print : aliasinfo -> unit
	= fun map -> Map.iter (fun n set ->
			print_endline (string_of_int n ^ " -> " ^ "{");
			Set.iter (fun (p1, p2) ->
				print_endline (Print.pat_to_string p1 ^ " = " ^ Print.pat_to_string p2)
			) set;
			print_endline ("}")
		) map

	let get_aliasSet : aliasinfo -> int -> equivSet
	= fun map n -> BatMap.find n map

	let rec exp_to_pat : exp -> pat
	= function
		| EUnit -> PUnit
		| Const n -> PInt n
		| TRUE -> PBool true
		| FALSE -> PBool false
		| EVar x -> PVar x
		| EList l -> PList (list_map exp_to_pat l)
		| ETuple l -> PTuple (list_map exp_to_pat l)
		| ECtor (x, l) -> PCtor (x, list_map exp_to_pat l)
		| DOUBLECOLON (e1, e2) -> PCons [exp_to_pat e1;exp_to_pat e2] (*TODO*)
		| _ -> raise (Failure "Must alias analysis : expression type error")

	let rec check_pat : exp -> bool
	= function
		| EUnit | Const _ | TRUE | FALSE | String _ | EVar _ -> true
		| ETuple l | ECtor(_, l) | EList l -> List.for_all check_pat l
		| DOUBLECOLON (e1,e2) -> (check_pat e1) && (check_pat e2)
		| _ -> false

	let rec is_exist : id -> pat -> bool
	= fun var pat ->
		match pat with
		| PVar x -> var=x
		| PUnit | PInt _ | PBool _ | PUnder -> false
		| PCtor (_, l) | PCons l | PList l | PTuple l | Pats l -> List.for_all (is_exist var) l

	let rec inverse : pat * pat -> (pat * pat) Set.t
	= fun (p1,p2) ->
		match (p1,p2) with
		| PCtor (x1, l1), PCtor (x2, l2) -> if (x1=x2) then List.fold_left2 (fun acc p1 p2 ->  
					Set.union acc (inverse (p1,p2))
				) Set.empty l1 l2 else Set.singleton (p1,p2)
		| PTuple l1, PTuple l2 | PList l1, PList l2 | PCons l1, PCons l2 -> List.fold_left2 (fun acc p1 p2 ->
					Set.union acc (inverse (p1,p2))
				) Set.empty l1 l2
		| PUnder, _ -> Set.empty
		| _ -> Set.singleton (p1,p2)

	let closure : equivSet -> equivSet
	= fun set -> set (*TODO*)

	let empty_set = Set.empty

	let kill : equivSet -> id -> equivSet
	= fun set var -> Set.fold ( fun (p1,p2) acc ->
			if (is_exist var p1 || is_exist var p2) then acc
			else Set.add (p1,p2) acc
		) set empty_set

	let gen : equivSet -> (pat * exp) -> equivSet
	= fun set (pattern,exp) ->
		if (check_pat exp) then Set.union (inverse (pattern,exp_to_pat exp)) set
		else set

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
		| BindUnder -> PUnder
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
		| EUnit | Const _ | TRUE | FALSE | String _ | EVar _ -> empty_alias
		| NOT e | MINUS e | Raise e -> analysis_exp s e
		| ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2) | OR (e1, e2) | AND (e1, e2)
		| LESS (e1, e2) | LARGER (e1, e2) | EQUAL (e1, e2) | NOTEQ (e1, e2) | LESSEQ (e1, e2) | LARGEREQ (e1, e2)
		| AT (e1, e2) | DOUBLECOLON (e1, e2) | STRCON (e1, e2) | EApp (e1, e2) -> Map.union (analysis_exp s e1) (analysis_exp s e2)
		| IF (e1,e2,e3) -> Map.union (Map.union (analysis_exp s e1) (analysis_exp s e2)) (analysis_exp s e3)
		| EList l | ETuple l | ECtor (_, l) -> list_fold (fun e acc -> Map.union (analysis_exp s e) acc) l empty_alias
		| EFun (a, e) ->
			let s = kill_arg a s in
			analysis_exp s e
		| ELet (f, is_rec, args, typ, e1, e2) -> (* TODO *)
			Map.union (analysis_exp s e1)
			(
				if (args != []) then analysis_exp s e2 
				else 
					let s = kill_let_bind f s in
					let s = gen s (let_bind_to_pat f,e1) in
					let s = closure s in
					analysis_exp s e2
			)
		| EBlock (is_rec, bindings, e2) -> (* TODO *)
			let (alias_info, s) = list_fold( fun (f, is_rec, args, typ, e) (map, s) ->
				if (args != []) then (map,s) 
				else
					let alias_e1 = Map.union (analysis_exp s e) map in
					let s = kill_let_bind f s in
					let s = gen s (let_bind_to_pat f,e) in
					let s = closure s in
					(alias_e1,s)
			) bindings (empty_alias,s) in
			Map.union alias_info (analysis_exp s e2)
		| EMatch (e, bs) ->
			let alias_e = analysis_exp s e in
			Map.union alias_e (list_fold (fun (p,exp) acc ->
					let s = kill_pat p s in
					let s = gen s (p,e) in
					let s = closure s in
					Map.union (analysis_exp s exp) acc
				) bs empty_alias
			)

	let rec analysis_decl : decl -> env -> env
	= fun decl (s,map) ->
		match decl with
		| DLet (x, is_rec, args, typ, exp) ->
			let alias = 
				begin match x with
				| BindUnder -> analysis_exp s exp
				| _ -> analysis_exp s (ELet (x, is_rec, args, typ, exp, let_to_exp x))
				end in
			if (args != []) then (s,Map.union map alias)
			else
				let s = kill_let_bind x s in
				let s = gen s (let_bind_to_pat x, exp) in
				let s = closure s in
				(s,Map.union map alias)
		| DBlock (is_rec, bindings)-> (* TODO *)
			list_fold (
				fun (f, is_rec, args, typ, e) (s, map) ->
					analysis_decl (DLet (f, is_rec, args, typ, e)) (s, map)
			) bindings (s, map)
		| _ -> (s,map)

	let run : prog -> aliasinfo
	= fun pgm -> 
		let env = list_fold analysis_decl pgm empty_env in
		(*Print.print_header "alias information";
		Print.print_pgm pgm;
		(print (snd env));*)
		snd env
end
