open Lang
open Util

exception MatchError
exception DeclError

type repair_cand = (label * lexp) (* error label, expected patch *)

module PreAnalysis = struct
  
  (* Compute a mapping which stores type of each program points *)
	type t = (int, typ) BatMap.t

	let print : t -> unit
  = fun t -> BatMap.iter (fun num typ -> print_endline(string_of_int num ^ " -> " ^ Print.type_to_string typ)) t

  let rec get_all_labels_exp : int BatSet.t -> lexp -> int BatSet.t
  = fun labels (l, exp) ->
  	match exp with
		| MINUS e | NOT e | EFun (_, e) | Raise e -> get_all_labels_exp (BatSet.add l labels) e
		| EList es | ECtor (_, es) | ETuple es -> get_all_labels_exp_list (BatSet.add l labels) es
		| ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2)
		| OR (e1, e2) | AND (e1, e2) | EQUAL (e1, e2) | NOTEQ (e1, e2)
	  | LESS (e1, e2) | LARGER (e1, e2)| LESSEQ (e1, e2) | LARGEREQ (e1, e2)
	  | AT (e1, e2) | DOUBLECOLON (e1, e2) | STRCON (e1, e2) | EApp (e1, e2) 
		| ELet (_, _, _, _, e1, e2) -> get_all_labels_exp_list (BatSet.add l labels) [e1; e2]
		| IF (e1, e2, e3) -> get_all_labels_exp_list (BatSet.add l labels) [e1; e2; e3]
		| EMatch (e, bs) -> 
			let es = e::(List.map (fun (p, e) -> e) bs) in
			get_all_labels_exp_list (BatSet.add l labels) es
		| EBlock (_, bs, e) -> 
			let es = e::(List.map (fun (_, _, _, _, e) -> e) bs) in
			get_all_labels_exp_list (BatSet.add l labels) es
		| _ -> BatSet.add l labels
  
  and get_all_labels_exp_list : int BatSet.t -> lexp list -> int BatSet.t
  = fun labels es -> List.fold_left (fun labels e -> get_all_labels_exp labels e) labels es

  let rec get_all_labels_decl : int BatSet.t -> decl -> int BatSet.t
  = fun labels decl ->
  	match decl with
  	| DLet (_, _, _, _, e) -> get_all_labels_exp labels e
  	| DBlock (_, ds) -> List.fold_left (fun labels (_, _, _, _, e) -> get_all_labels_exp labels e) labels ds
  	| _ -> labels

  let rec get_all_labels : prog -> int BatSet.t
  = fun pgm -> List.fold_left (fun labels decl -> get_all_labels_decl labels decl) BatSet.empty pgm

  let run : prog -> t 
  = fun pgm -> 
  	let lables = get_all_labels pgm in
  	BatSet.fold (fun l t -> 
			let pgm = Localize.gen_partial_pgm l pgm in
			let hole = !hole_count in
			let (_, h_t, _, _) = Type.run pgm in
			let typ = BatMap.find hole h_t in
			BatMap.add l typ t
  	) lables BatMap.empty
end

(* Get repair candidate *)
let rec match_pat : pat -> pat -> bool
= fun p1 p2 ->
  match (p1, p2) with
  | PUnit, PUnit | PUnder, PUnder | PVar _, PVar _ -> true
  | PInt n1, PInt n2 -> n1 = n2
  | PBool b1, PBool b2 -> b1 = b2
  | PList ps1, PList ps2 | PTuple ps1, PTuple ps2 | PCons ps1, PCons ps2 -> (try List.for_all2 match_pat ps1 ps2 with _ -> false)
  | PCtor (x, ps1), PCtor (y, ps2) -> (x = y) && (try List.for_all2 match_pat ps1 ps2 with _ -> false)
  | Pats ps1, Pats ps2 -> raise (Failure "Invalid pattern mathcing of two cfgs")
  | _ -> false

let rec get_repair_candidate_exp : lexp -> lexp -> repair_cand BatSet.t
= fun (l1, exp1) (l2, exp2) ->
	match (exp1, exp2) with
	(* Const and exceptional expresions *)
	| SInt _, SInt _ | SStr _, SStr _ | EUnit, EUnit | Hole _, Hole _ | TRUE, TRUE | FALSE, FALSE | Raise _, Raise _ -> BatSet.empty
	| Const n1, Const n2 when n1 = n2 -> BatSet.empty
	| String s1, String s2 when s1 = s2 -> BatSet.empty
	(* List *)
	| EList es1, EList es2 | ETuple es1, ETuple es2 -> 
		(try List.fold_left2 (fun acc e1 e2 -> BatSet.union acc (get_repair_candidate_exp e1 e2)) BatSet.empty es1 es2 with _ -> BatSet.singleton (l1, (l2, exp2)))
	| ECtor (x1, es1), ECtor (x2, es2) -> 
		if x1 = x2 then 
			(try List.fold_left2 (fun acc e1 e2 -> BatSet.union acc (get_repair_candidate_exp e1 e2)) BatSet.empty es1 es2 with _ -> BatSet.singleton (l1, (l2, exp2))) 
		else BatSet.singleton (l1, (l2, exp2))
	(* unary *)
	| MINUS e1, MINUS e2 | NOT e1, NOT e2 | EFun (_, e1), EFun (_, e2) -> get_repair_candidate_exp e1 e2
	(* non-sequntial binop -> refine to find minimal set *)
	| ADD (e1, e2), ADD (e1', e2') | MUL (e1, e2), MUL (e1', e2') | OR (e1, e2), OR (e1', e2') | AND (e1, e2), AND (e1', e2') 
	| EQUAL (e1, e2), EQUAL (e1', e2') | NOTEQ(e1, e2), NOTEQ (e1', e2') -> 
		BatSet.union (get_repair_candidate_exp e1 e1') (get_repair_candidate_exp e2 e2')
 	(* sequntial binop *)
 	| DIV (e1, e2), DIV (e1', e2') | MOD (e1, e2), MOD (e1', e2') | SUB (e1, e2), SUB (e1', e2') 
 	| LESS (e1, e2), LESS (e1', e2') | LARGER (e1, e2), LARGER (e1', e2') | LESSEQ (e1, e2), LESSEQ (e1', e2') | LARGEREQ (e1, e2), LARGEREQ (e1', e2') 
 	| AT (e1, e2), AT (e1', e2') | STRCON (e1, e2), STRCON (e1', e2')
 	| DOUBLECOLON (e1, e2), DOUBLECOLON (e1', e2') -> 
 		BatSet.union (get_repair_candidate_exp e1 e1') (get_repair_candidate_exp e2 e2')
 	(* binding *)
 	| ELet (_, _, _, _, e1, e2), ELet (_, _, _, _, e1', e2') -> BatSet.empty (* TODO *)
 	| EBlock (_, bs1, e1), EBlock (_, bs2, e2) -> BatSet.empty (* TODO *)
 	(* conditional *)
 	| EMatch (e1, bs1), EMatch (e2, bs2) ->
		let rec get_repair_candidate_match : branch list -> branch list  -> repair_cand BatSet.t
		= fun bs1 bs2 ->
			match bs1, bs2 with
			| [], [] -> BatSet.empty
			| (p1, e1)::tl1, (p2, e2)::tl2 ->
				begin
		      try
		        let (p2, e2) = List.find (fun (p2, e2) -> match_pat p1 p2) bs2 in
		        BatSet.union (get_repair_candidate_exp e1 e2) (get_repair_candidate_match tl1 (List.remove_assoc p2 bs2))
		      with Not_found -> raise MatchError
		    end
			| _ -> raise MatchError
		in
		(try BatSet.union (get_repair_candidate_match bs1 bs2) (get_repair_candidate_exp e1 e2) with MatchError -> BatSet.singleton (l1, (l2, exp2)))
 	| IF (e1, e2, e3), IF (e1', e2', e3') -> 
 		BatSet.union (get_repair_candidate_exp e1 e1') (get_repair_candidate_exp e2 e2') 
 		|> BatSet.union (get_repair_candidate_exp e3 e3')
 	| _, EApp (e1', e2') -> 
 		let e2 = EApp (gen_labeled_hole (), gen_labeled_hole ()) in
 		BatSet.singleton (l1, (l2, e2))
 	| _ -> BatSet.singleton (l1, (l2, exp2))

(*
let rec get_repair_candidate_decl : decl -> decl -> repair_cand BatSet.t
= fun decl1 decl2 ->
	match decl1, decl2 with
	| DLet (_, _, _, _, e1), DLet (_, _, _, _, e2) -> get_repair_candidate_exp e1 e2
	| _ -> BatSet.empty

let rec find_matched_decl : Cfg.S.cfg -> (prog * Cfg.S.t) -> decl 
= fun cfg1 (decls, t) ->
	match decls with
	| DLet (f, is_rec, args, typ, e)::decls -> 
    begin match f with
    | BindOne f -> 
    	let cfg2 = List.assoc f t in
    	if (Cfg.match_cfg cfg1 cfg2) then DLet (BindOne f, is_rec, args, typ, e) else find_matched_decl cfg1 (decls, t)
    | _ -> raise DeclError
    end
  | DBlock (is_rec, bindings)::decls -> 
  	let decls = List.fold_left (fun decls binding -> (DLet binding)::decls) decls bindings in
  	find_matched_decl cfg1 (decls, t)
  | _::decls -> find_matched_decl cfg1 (decls, t)
  | _ -> raise DeclError
	
let rec get_repair_candidate : PreAnalysis.t -> (prog * Cfg.S.t) -> (prog * Cfg.S.t) -> repair_cand list
= fun t (decls1, t1) (decls2, t2) ->
	match decls1 with
	| DLet (f, is_rec, args, typ, e)::decls -> 
    begin match f with
    | BindOne f -> 
    	let cfg1 = List.assoc f t1 in
    	begin 
    		try 
	    		let matched_decl = find_matched_decl cfg1 (decls2, t2) in
	    		get_repair_candidate_decl t (DLet (BindOne f, is_rec, args, typ, e)) matched_decl
	    	with DeclError -> []
	    end
    | _ -> []
    end
  | DBlock (is_rec, bindings)::decls -> 
  	let decls1 = List.fold_left (fun decls binding -> (DLet binding)::decls) decls bindings in
  	get_repair_candidate t (decls1, t1) (decls2, t2)
  | _::decls -> get_repair_candidate t (decls, t1) (decls2, t2)
  | _ -> []
*)

(* Translate function call in repair candidate to hole *)
let rec func_temp_exp : lexp -> lexp
= fun (l, exp) ->
	let exp = 
		match exp with
		| Raise e -> Raise (func_temp_exp e)
		| EFun (arg, e) -> EFun (arg, func_temp_exp e)
		| MINUS e -> MINUS (func_temp_exp e)
		| NOT e -> NOT (func_temp_exp e)
		| ADD (e1, e2) -> ADD (func_temp_exp e1, func_temp_exp e2)
		| SUB (e1, e2) -> SUB (func_temp_exp e1, func_temp_exp e2)
		| MUL (e1, e2) -> MUL (func_temp_exp e1, func_temp_exp e2)
		| DIV (e1, e2) -> DIV (func_temp_exp e1, func_temp_exp e2)
		| MOD (e1, e2) -> MOD (func_temp_exp e1, func_temp_exp e2)
		| OR (e1, e2) -> OR (func_temp_exp e1, func_temp_exp e2)
		| AND (e1, e2) -> AND (func_temp_exp e1, func_temp_exp e2)
		| LESS (e1, e2) -> LESS (func_temp_exp e1, func_temp_exp e2)
		| LESSEQ (e1, e2) -> LESSEQ (func_temp_exp e1, func_temp_exp e2)
		| LARGER (e1, e2) -> LARGER (func_temp_exp e1, func_temp_exp e2)
		| LARGEREQ (e1, e2) -> LARGEREQ (func_temp_exp e1, func_temp_exp e2)
		| EQUAL (e1, e2) -> EQUAL (func_temp_exp e1, func_temp_exp e2)
		| NOTEQ (e1, e2) -> NOTEQ (func_temp_exp e1, func_temp_exp e2)
		| DOUBLECOLON (e1, e2) -> DOUBLECOLON (func_temp_exp e1, func_temp_exp e2)
		| AT (e1, e2) -> AT (func_temp_exp e1, func_temp_exp e2)
		| STRCON (e1, e2) -> STRCON (func_temp_exp e1, func_temp_exp e2)
		| EApp (e1, e2) -> EApp (gen_labeled_hole (), gen_labeled_hole ())
		| EList es -> EList (List.map func_temp_exp es)
		| ETuple es -> ETuple (List.map func_temp_exp es)
		| ECtor (x, es) -> ECtor (x, List.map func_temp_exp es)
		| IF (e1, e2, e3) -> IF (func_temp_exp e1, func_temp_exp e2, func_temp_exp e3)
		| EMatch (e, bs) -> EMatch (func_temp_exp e, List.map (fun (p, e) -> (p, func_temp_exp e)) bs)
		| ELet (f, is_rec, args, typ, e1, e2) -> ELet (f, is_rec, args, typ, func_temp_exp e1, func_temp_exp e2)
		| EBlock (is_rec, bindings, e2) -> EBlock (is_rec, List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, args, typ, func_temp_exp e)) bindings, func_temp_exp e2)
		| _ -> exp
	in
	(l, exp)

let rec gen_func_temp : repair_cand BatSet.t -> repair_cand BatSet.t
= fun candidates -> BatSet.map (fun (l, e) -> (l, func_temp_exp e)) candidates

(* Variable Unification *)
let rec subst_var_comp : PreAnalysis.t -> lexp -> (id * typ) list -> lexp BatSet.t
= fun tenv (l, exp) vars ->
	let result = 
		match exp with
		| Raise e -> BatSet.map (fun e -> Raise e) (subst_var_comp tenv e vars)
		| EFun (arg, e) -> BatSet.map (fun e -> EFun (arg, e)) (subst_var_comp tenv e vars)
		| MINUS e -> BatSet.map (fun e -> MINUS e) (subst_var_comp tenv e vars)
		| NOT e -> BatSet.map (fun e -> NOT e) (subst_var_comp tenv e vars)
		| ADD (e1, e2) -> BatSet.fold (fun e1 acc ->
				BatSet.fold (fun e2 acc -> BatSet.add (ADD (e1, e2)) acc) (subst_var_comp tenv e2 vars) acc
			) (subst_var_comp tenv e1 vars) BatSet.empty
		| SUB (e1, e2) -> BatSet.fold (fun e1 acc ->
				BatSet.fold (fun e2 acc -> BatSet.add (SUB (e1, e2)) acc) (subst_var_comp tenv e2 vars) acc
			) (subst_var_comp tenv e1 vars) BatSet.empty
		| MUL (e1, e2) -> BatSet.fold (fun e1 acc ->
				BatSet.fold (fun e2 acc -> BatSet.add (MUL (e1, e2)) acc) (subst_var_comp tenv e2 vars) acc
			) (subst_var_comp tenv e1 vars) BatSet.empty
		| DIV (e1, e2) -> BatSet.fold (fun e1 acc ->
				BatSet.fold (fun e2 acc -> BatSet.add (DIV (e1, e2)) acc) (subst_var_comp tenv e2 vars) acc
			) (subst_var_comp tenv e1 vars) BatSet.empty
		| MOD (e1, e2) -> BatSet.fold (fun e1 acc ->
				BatSet.fold (fun e2 acc -> BatSet.add (MOD (e1, e2)) acc) (subst_var_comp tenv e2 vars) acc
			) (subst_var_comp tenv e1 vars) BatSet.empty
		| OR (e1, e2) -> BatSet.fold (fun e1 acc ->
				BatSet.fold (fun e2 acc -> BatSet.add (OR (e1, e2)) acc) (subst_var_comp tenv e2 vars) acc
			) (subst_var_comp tenv e1 vars) BatSet.empty
		| AND (e1, e2) -> BatSet.fold (fun e1 acc ->
				BatSet.fold (fun e2 acc -> BatSet.add (AND (e1, e2)) acc) (subst_var_comp tenv e2 vars) acc
			) (subst_var_comp tenv e1 vars) BatSet.empty
		| LESS (e1, e2) -> BatSet.fold (fun e1 acc ->
				BatSet.fold (fun e2 acc -> BatSet.add (LESS (e1, e2)) acc) (subst_var_comp tenv e2 vars) acc
			) (subst_var_comp tenv e1 vars) BatSet.empty
		| LESSEQ (e1, e2) -> BatSet.fold (fun e1 acc ->
				BatSet.fold (fun e2 acc -> BatSet.add (LESSEQ (e1, e2)) acc) (subst_var_comp tenv e2 vars) acc
			) (subst_var_comp tenv e1 vars) BatSet.empty
		| LARGER (e1, e2) -> BatSet.fold (fun e1 acc ->
				BatSet.fold (fun e2 acc -> BatSet.add (LARGER (e1, e2)) acc) (subst_var_comp tenv e2 vars) acc
			) (subst_var_comp tenv e1 vars) BatSet.empty
		| LARGEREQ (e1, e2) -> BatSet.fold (fun e1 acc ->
				BatSet.fold (fun e2 acc -> BatSet.add (LARGEREQ (e1, e2)) acc) (subst_var_comp tenv e2 vars) acc
			) (subst_var_comp tenv e1 vars) BatSet.empty
		| EQUAL (e1, e2) -> BatSet.fold (fun e1 acc ->
				BatSet.fold (fun e2 acc -> BatSet.add (EQUAL (e1, e2)) acc) (subst_var_comp tenv e2 vars) acc
			) (subst_var_comp tenv e1 vars) BatSet.empty
		| NOTEQ (e1, e2) -> BatSet.fold (fun e1 acc ->
				BatSet.fold (fun e2 acc -> BatSet.add (NOTEQ (e1, e2)) acc) (subst_var_comp tenv e2 vars) acc
			) (subst_var_comp tenv e1 vars) BatSet.empty
		| DOUBLECOLON (e1, e2) -> BatSet.fold (fun e1 acc ->
				BatSet.fold (fun e2 acc -> BatSet.add (DOUBLECOLON (e1, e2)) acc) (subst_var_comp tenv e2 vars) acc
			) (subst_var_comp tenv e1 vars) BatSet.empty
		| AT (e1, e2) -> BatSet.fold (fun e1 acc ->
				BatSet.fold (fun e2 acc -> BatSet.add (AT (e1, e2)) acc) (subst_var_comp tenv e2 vars) acc
			) (subst_var_comp tenv e1 vars) BatSet.empty
		| STRCON (e1, e2) -> BatSet.fold (fun e1 acc ->
				BatSet.fold (fun e2 acc -> BatSet.add (STRCON (e1, e2)) acc) (subst_var_comp tenv e2 vars) acc
			) (subst_var_comp tenv e1 vars) BatSet.empty
		(* Call *)
		| EApp (e1, e2) -> BatSet.fold (fun e2 acc ->
				BatSet.add (EApp (e1, e2)) acc
			) (subst_var_comp tenv e2 vars) BatSet.empty
		| EList es -> 
			let es' = subst_var_comp_list tenv es vars in
			BatSet.map (fun es -> EList es) es'
		| ETuple es ->
			let es' = subst_var_comp_list tenv es vars in
			BatSet.map (fun es -> ETuple es) es'
		| ECtor (x, es) ->
			let es' = subst_var_comp_list tenv es vars in
			BatSet.map (fun es -> ECtor (x, es)) es'
		| EVar y -> List.fold_left (fun acc (x, t2) -> 
				let t1 = BatMap.find l tenv in
				try 
					let _ = Type.unify Type.Subst.empty (t1, t2) in
					BatSet.add (EVar x) acc
				with Type.TypeError -> acc
			) BatSet.empty vars
		| IF (e1, e2, e3) -> 
			(* TODO *)
			let _ =
				PreAnalysis.print tenv;
				List.iter (fun (id, typ) -> 
					print_endline (id ^ " : " ^ Print.type_to_string typ);
				) vars
			in
			BatSet.singleton exp
		(*
		| EMatch (e, bs) -> EMatch (subst_var_comp e x, List.map (fun (p, e) -> (p, subst_var_comp e x)) bs)
		| ELet (f, is_rec, args, typ, e1, e2) -> ELet (f, is_rec, args, typ, subst_var_comp e1 x, subst_var_comp e2 x)
		| EBlock (is_rec, bindings, e2) -> EBlock (is_rec, List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, args, typ, subst_var_comp e x)) bindings, subst_var_comp e2 x)
		*)
		| _ -> BatSet.singleton exp
	in
	BatSet.map (fun e -> (l, e)) result

and subst_var_comp_list : PreAnalysis.t -> lexp list -> (id * typ) list -> ((label * exp) list) BatSet.t
= fun tenv es vars ->
	match es with
	| [] -> BatSet.singleton []
	| hd::tl -> 
		let tl_result = subst_var_comp_list tenv tl vars in
		BatSet.fold (fun e acc -> 
			BatSet.union acc (BatSet.map (fun es -> e::es) tl_result)
		) (subst_var_comp tenv hd vars) BatSet.empty

let rec update_var_comp : PreAnalysis.t -> prog -> repair_cand BatSet.t -> repair_cand BatSet.t
= fun t pgm candidates ->
	BatSet.fold (fun (l, lexp) acc -> 
		let pgm = Localize.gen_partial_pgm l pgm in
		let hole = !hole_count in
		let (_, _, v_t, _) = Type.run pgm in
		let vars = BatMap.foldi (fun x typ vars ->
			match typ with
			| TCtor _ -> vars
			| typ -> (x, typ)::vars
		) (BatMap.find hole v_t) [] in
		let result = BatSet.map (fun lexp -> (l, lexp)) (subst_var_comp t lexp vars) in
		BatSet.union result acc
	) candidates BatSet.empty 

let rec subst_exp : lexp -> repair_cand -> lexp
= fun (l1, exp1) (l2, exp2) ->
	if l1 = l2 then exp2 else 
	let exp = 
		match exp1 with
		| Raise e -> Raise (subst_exp e (l2, exp2))
		| EFun (arg, e) -> EFun (arg, (subst_exp e (l2, exp2)))
		| MINUS e -> MINUS (subst_exp e (l2, exp2))
		| NOT e -> NOT (subst_exp e (l2, exp2))
		| ADD (e1, e2) -> ADD (subst_exp e1 (l2, exp2), subst_exp e2 (l2, exp2))
		| SUB (e1, e2) -> SUB (subst_exp e1 (l2, exp2), subst_exp e2 (l2, exp2))
		| MUL (e1, e2) -> MUL (subst_exp e1 (l2, exp2), subst_exp e2 (l2, exp2))
		| DIV (e1, e2) -> DIV (subst_exp e1 (l2, exp2), subst_exp e2 (l2, exp2))
		| MOD (e1, e2) -> MOD (subst_exp e1 (l2, exp2), subst_exp e2 (l2, exp2))
		| OR (e1, e2) -> OR (subst_exp e1 (l2, exp2), subst_exp e2 (l2, exp2))
		| AND (e1, e2) -> AND (subst_exp e1 (l2, exp2), subst_exp e2 (l2, exp2))
		| LESS (e1, e2) -> LESS (subst_exp e1 (l2, exp2), subst_exp e2 (l2, exp2))
		| LESSEQ (e1, e2) -> LESSEQ (subst_exp e1 (l2, exp2), subst_exp e2 (l2, exp2))
		| LARGER (e1, e2) -> LARGER (subst_exp e1 (l2, exp2), subst_exp e2 (l2, exp2))
		| LARGEREQ (e1, e2) -> LARGEREQ (subst_exp e1 (l2, exp2), subst_exp e2 (l2, exp2))
		| EQUAL (e1, e2) -> EQUAL (subst_exp e1 (l2, exp2), subst_exp e2 (l2, exp2))
		| NOTEQ (e1, e2) -> NOTEQ (subst_exp e1 (l2, exp2), subst_exp e2 (l2, exp2))
		| DOUBLECOLON (e1, e2) -> DOUBLECOLON (subst_exp e1 (l2, exp2), subst_exp e2 (l2, exp2))
		| AT (e1, e2) -> AT (subst_exp e1 (l2, exp2), subst_exp e2 (l2, exp2))
		| STRCON (e1, e2) -> STRCON (subst_exp e1 (l2, exp2), subst_exp e2 (l2, exp2))
		| EApp (e1, e2) -> EApp (subst_exp e1 (l2, exp2), subst_exp e2 (l2, exp2))
		| EList es -> EList (List.map (fun e -> subst_exp e (l2, exp2)) es)
		| ETuple es -> ETuple (List.map (fun e -> subst_exp e (l2, exp2)) es)
		| ECtor (x, es) -> ECtor (x, List.map (fun e -> subst_exp e (l2, exp2)) es)
		| IF (e1, e2, e3) -> IF (subst_exp e1 (l2, exp2), subst_exp e2 (l2, exp2), subst_exp e3 (l2, exp2))
		| EMatch (e, bs) -> EMatch (subst_exp e (l2, exp2), List.map (fun (p, e) -> (p, subst_exp e (l2, exp2))) bs)
		| ELet (f, is_rec, args, typ, e1, e2) -> ELet (f, is_rec, args, typ, subst_exp e1 (l2, exp2), subst_exp e2 (l2, exp2))
		| EBlock (is_rec, bindings, e2) -> EBlock (is_rec, List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, args, typ, subst_exp e (l2, exp2))) bindings, subst_exp e2 (l2, exp2))
		| _ -> exp1
	in
	(l1, exp)

let rec subst_decl : decl -> repair_cand -> decl
= fun decl candidate ->
	match decl with
  | DLet (f, is_rec, args, typ, e) -> DLet (f, is_rec, args, typ, subst_exp e candidate)
  | DBlock (is_rec, bindings) ->
    let bindings = List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, args, typ, subst_exp e candidate)) bindings in
    DBlock (is_rec, bindings) 
  | _ -> decl

(*
	Input : An incorrect program pgm, a correct program cpgm, and a set of testcases testcases
	Output : A repaired program pgm' satisfying all testcases
*)
let run : prog -> prog -> examples -> prog option
= fun pgm cpgm testcases -> 
	(*
	(* Repair Candidates *)
	let start_time = Unix.gettimeofday () in
	let (t1, t2) = (Cfg.S.run pgm, Cfg.S.run cpgm) in
  (*
	Print.print_header "Analysis1"; print_endline (Cfg.S.string_of_t t1);
  Print.print_header "Analysis2"; print_endline (Cfg.S.string_of_t t2);
  *)
	let repair_cand = get_repair_candidate (Type.VariableType.empty) (pgm, t1) (cpgm, t2) in
    (*
	Print.print_header "Repair candidates";
	(*
	List.iter (fun (l, e) -> 
		print_endline ("label : " ^ string_of_int l);
		print_endline (Print.exp_to_string (l, e))
	) repair_cand;
	*)
    *)
	(* Update Candidate *)
	let repair_cand = update_var_comp pgm repair_cand in
	Print.print_header "Updated Repair candidates";
	(*
	List.iter (fun (l, e) -> 
		print_endline ("label : " ^ string_of_int l);
		print_endline (Print.exp_to_string (l, e))
	) repair_cand;
    *)
	(* Replace & Checking *)
	print_endline ("Size of repair Cand : " ^ string_of_int (BatSet.cardinal repair_cand));
	let repair = List.find_opt (fun (l, e) -> 
		let pgm' = List.map (fun decl -> subst_decl decl (l, e)) pgm in
		print_endline ("label : " ^ string_of_int l);
        print_endline (Print.exp_to_string (l, e));
		(*Print.print_header "Repair Candidate"; Print.print_pgm pgm';*)
		Eval.is_solution pgm' testcases
	) (BatSet.to_list repair_cand) 
	in
	match repair with
	| Some (l, e) -> 
		let pgm' = List.map (fun decl -> subst_decl decl (l, e)) pgm in
		Print.print_header "Repair result"; Print.print_pgm pgm';
		print_endline ("Time : " ^ string_of_float (Unix.gettimeofday() -. start_time));
		Some pgm'
	| None -> print_endline ("Fail to Repair"); None
	*)
	None