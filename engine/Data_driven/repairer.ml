open Lang
open Util

exception MatchError
exception DeclError

(* Get repair candidate : set of (label * exp) *)
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

let rec get_repair_candidate_exp : lexp -> lexp -> (label * exp) list
= fun (l1, exp1) (l2, exp2) ->
	match (exp1, exp2) with
	(* Const and exceptional expresions *)
	| SInt _, SInt _ | SStr _, SStr _ | EUnit, EUnit | Hole _, Hole _ | TRUE, TRUE | FALSE, FALSE | Raise _, Raise _ -> []
	| Const n1, Const n2 -> if (n1 = n2) then [] else [(l1, exp2)]
	| String s1, String s2 -> if (s1 = s2) then [] else [(l1, exp2)]
	(* List *)
	| EList es1, EList es2 | ETuple es1, ETuple es2 -> (try List.fold_left2 (fun acc e1 e2 -> (get_repair_candidate_exp e1 e2)@acc) [] es1 es2 with _ -> [(l1, exp2)])
	| ECtor (x1, es1), ECtor (x2, es2) -> if x1 = x2 then (try List.fold_left2 (fun acc e1 e2 -> (get_repair_candidate_exp e1 e2)@acc) [] es1 es2 with _ -> [(l1, exp2)]) else [(l1, exp2)]
	(* unary *)
	| MINUS e1, MINUS e2 | NOT e1, NOT e2 | EFun (_, e1), EFun (_, e2) -> get_repair_candidate_exp e1 e2
	(* non-sequntial binop *)
	| ADD (e1, e2), ADD (e1', e2') | SUB (e1, e2), SUB (e1', e2') | MUL (e1, e2), MUL (e1', e2')| DIV (e1, e2), DIV (e1', e2') | MOD (e1, e2), MOD (e1', e2') 
	| OR (e1, e2), OR (e1', e2') | AND (e1, e2), AND (e1', e2') | LESS (e1, e2), LESS (e1', e2') | LARGER (e1, e2), LARGER (e1', e2')
	| LESSEQ (e1, e2), LESSEQ (e1', e2') | LARGEREQ (e1, e2), LARGEREQ (e1', e2') | EQUAL (e1, e2), EQUAL (e1', e2') | NOTEQ(e1, e2), NOTEQ (e1', e2') 
	| AT (e1, e2), AT (e1', e2') | STRCON (e1, e2), STRCON (e1', e2') ->
		(* Find minimul fix *)
		let result1 = (get_repair_candidate_exp e1 e1')@(get_repair_candidate_exp e2 e2') in
		let result2 = (get_repair_candidate_exp e1 e2')@(get_repair_candidate_exp e2 e1') in
		if List.length result1 < List.length result2 then result1 else result2
 	(* sequntial binop *)
 	| DOUBLECOLON (e1, e2), DOUBLECOLON (e1', e2') | EApp (e1, e2), EApp (e1', e2') -> (get_repair_candidate_exp e1 e1')@(get_repair_candidate_exp e2 e2')
 	(* binding *)
 	| ELet (_, _, _, _, e1, e2), ELet (_, _, _, _, e1', e2') -> [] (* TODO *)
 	| EBlock (_, bs1, e1), EBlock (_, bs2, e2) -> [] (* TODO *)
 	(* conditional *)
 	| EMatch (e1, bs1), EMatch (e2, bs2) ->
		let rec get_repair_candidate_match : branch list -> branch list  -> (label * exp) list
		= fun bs1 bs2 ->
			match bs1, bs2 with
			| [], [] -> []
			| (p1, e1)::tl1, (p2, e2)::tl2 ->
				begin
		      try
		        let (p2, e2) = List.find (fun (p2, e2) -> match_pat p1 p2) bs2 in
		        (get_repair_candidate_exp e1 e2)@(get_repair_candidate_match tl1 (List.remove_assoc p2 bs2))
		      with Not_found -> raise MatchError
		    end
			| _ -> raise MatchError
		in
		(try (get_repair_candidate_match bs1 bs2)@(get_repair_candidate_exp e1 e2) with MatchError -> [(l1, exp2)])
 	| IF (e1, e2, e3), IF (e1', e2', e3') ->
 		let result1 = (get_repair_candidate_exp e2 e2')@(get_repair_candidate_exp e3 e3') in
		let result2 = (get_repair_candidate_exp e2 e3')@(get_repair_candidate_exp e3 e2') in
		(get_repair_candidate_exp e1 e1')@(if List.length result1 < List.length result2 then result1 else result2)
 	| _ -> [(l1, exp2)]

let rec get_repair_candidate_decl : decl -> decl -> (label * exp) list
= fun decl1 decl2 ->
	match decl1, decl2 with
	| DLet (_, _, _, _, e1), DLet (_, _, _, _, e2) -> get_repair_candidate_exp e1 e2
	| _ -> []

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
	
let rec get_repair_candidate : (prog * Cfg.S.t) -> (prog * Cfg.S.t) -> (label * exp) list
= fun (decls1, t1) (decls2, t2) ->
	match decls1 with
	| DLet (f, is_rec, args, typ, e)::decls -> 
    begin match f with
    | BindOne f -> 
    	let cfg1 = List.assoc f t1 in
    	begin 
    		try 
	    		let matched_decl = find_matched_decl cfg1 (decls2, t2) in
	    		get_repair_candidate_decl (DLet (BindOne f, is_rec, args, typ, e)) matched_decl
	    	with DeclError -> []
	    end
    | _ -> []
    end
  | DBlock (is_rec, bindings)::decls -> 
  	let decls1 = List.fold_left (fun decls binding -> (DLet binding)::decls) decls bindings in
  	get_repair_candidate (decls1, t1) (decls2, t2)
  | _::decls -> get_repair_candidate (decls, t1) (decls2, t2)
  | _ -> []

(* Repair *)
let rec subst_var_comp : lexp -> id list -> (label * exp) BatSet.t
= fun (l, exp) vars ->
	let result = 
		match exp with
		| Raise e -> BatSet.map (fun e -> Raise e) (subst_var_comp e vars)
		| EFun (arg, e) -> BatSet.map (fun e -> EFun (arg, e)) (subst_var_comp e vars)
		| MINUS e -> BatSet.map (fun e -> MINUS e) (subst_var_comp e vars)
		| NOT e -> BatSet.map (fun e -> NOT e) (subst_var_comp e vars)
		| ADD (e1, e2) -> BatSet.fold (fun e1 acc ->
				BatSet.fold (fun e2 acc -> BatSet.add (ADD (e1, e2)) acc) (subst_var_comp e2 vars) acc
			) (subst_var_comp e1 vars) BatSet.empty
		| SUB (e1, e2) -> BatSet.fold (fun e1 acc ->
				BatSet.fold (fun e2 acc -> BatSet.add (SUB (e1, e2)) acc) (subst_var_comp e2 vars) acc
			) (subst_var_comp e1 vars) BatSet.empty
		| MUL (e1, e2) -> BatSet.fold (fun e1 acc ->
				BatSet.fold (fun e2 acc -> BatSet.add (MUL (e1, e2)) acc) (subst_var_comp e2 vars) acc
			) (subst_var_comp e1 vars) BatSet.empty
		| DIV (e1, e2) -> BatSet.fold (fun e1 acc ->
				BatSet.fold (fun e2 acc -> BatSet.add (DIV (e1, e2)) acc) (subst_var_comp e2 vars) acc
			) (subst_var_comp e1 vars) BatSet.empty
		| MOD (e1, e2) -> BatSet.fold (fun e1 acc ->
				BatSet.fold (fun e2 acc -> BatSet.add (MOD (e1, e2)) acc) (subst_var_comp e2 vars) acc
			) (subst_var_comp e1 vars) BatSet.empty
		| OR (e1, e2) -> BatSet.fold (fun e1 acc ->
				BatSet.fold (fun e2 acc -> BatSet.add (OR (e1, e2)) acc) (subst_var_comp e2 vars) acc
			) (subst_var_comp e1 vars) BatSet.empty
		| AND (e1, e2) -> BatSet.fold (fun e1 acc ->
				BatSet.fold (fun e2 acc -> BatSet.add (AND (e1, e2)) acc) (subst_var_comp e2 vars) acc
			) (subst_var_comp e1 vars) BatSet.empty
		| LESS (e1, e2) -> BatSet.fold (fun e1 acc ->
				BatSet.fold (fun e2 acc -> BatSet.add (LESS (e1, e2)) acc) (subst_var_comp e2 vars) acc
			) (subst_var_comp e1 vars) BatSet.empty
		| LESSEQ (e1, e2) -> BatSet.fold (fun e1 acc ->
				BatSet.fold (fun e2 acc -> BatSet.add (LESSEQ (e1, e2)) acc) (subst_var_comp e2 vars) acc
			) (subst_var_comp e1 vars) BatSet.empty
		| LARGER (e1, e2) -> BatSet.fold (fun e1 acc ->
				BatSet.fold (fun e2 acc -> BatSet.add (LARGER (e1, e2)) acc) (subst_var_comp e2 vars) acc
			) (subst_var_comp e1 vars) BatSet.empty
		| LARGEREQ (e1, e2) -> BatSet.fold (fun e1 acc ->
				BatSet.fold (fun e2 acc -> BatSet.add (LARGEREQ (e1, e2)) acc) (subst_var_comp e2 vars) acc
			) (subst_var_comp e1 vars) BatSet.empty
		| EQUAL (e1, e2) -> BatSet.fold (fun e1 acc ->
				BatSet.fold (fun e2 acc -> BatSet.add (EQUAL (e1, e2)) acc) (subst_var_comp e2 vars) acc
			) (subst_var_comp e1 vars) BatSet.empty
		| NOTEQ (e1, e2) -> BatSet.fold (fun e1 acc ->
				BatSet.fold (fun e2 acc -> BatSet.add (NOTEQ (e1, e2)) acc) (subst_var_comp e2 vars) acc
			) (subst_var_comp e1 vars) BatSet.empty
		| DOUBLECOLON (e1, e2) -> BatSet.fold (fun e1 acc ->
				BatSet.fold (fun e2 acc -> BatSet.add (DOUBLECOLON (e1, e2)) acc) (subst_var_comp e2 vars) acc
			) (subst_var_comp e1 vars) BatSet.empty
		| AT (e1, e2) -> BatSet.fold (fun e1 acc ->
				BatSet.fold (fun e2 acc -> BatSet.add (AT (e1, e2)) acc) (subst_var_comp e2 vars) acc
			) (subst_var_comp e1 vars) BatSet.empty
		| STRCON (e1, e2) -> BatSet.fold (fun e1 acc ->
				BatSet.fold (fun e2 acc -> BatSet.add (STRCON (e1, e2)) acc) (subst_var_comp e2 vars) acc
			) (subst_var_comp e1 vars) BatSet.empty
		(* Call *)
		| EApp (e1, e2) -> BatSet.fold (fun e2 acc ->
				BatSet.add (EApp (e1, e2)) acc
			) (subst_var_comp e2 vars) BatSet.empty
		| EList es -> 
			let es' = subst_var_comp_list es vars in
			BatSet.map (fun es -> EList es) es'
		| ETuple es ->
			let es' = subst_var_comp_list es vars in
			BatSet.map (fun es -> ETuple es) es'
		| ECtor (x, es) ->
			let es' = subst_var_comp_list es vars in
			BatSet.map (fun es -> ECtor (x, es)) es'
		| EVar _ -> List.fold_left (fun acc x -> BatSet.add (EVar x) acc) BatSet.empty vars
		(*
		| IF (e1, e2, e3) -> IF (subst_var_comp e1 x, subst_var_comp e2 x, subst_var_comp e3 x)
		| EMatch (e, bs) -> EMatch (subst_var_comp e x, List.map (fun (p, e) -> (p, subst_var_comp e x)) bs)
		| ELet (f, is_rec, args, typ, e1, e2) -> ELet (f, is_rec, args, typ, subst_var_comp e1 x, subst_var_comp e2 x)
		| EBlock (is_rec, bindings, e2) -> EBlock (is_rec, List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, args, typ, subst_var_comp e x)) bindings, subst_var_comp e2 x)
		*)
		| _ -> BatSet.singleton exp
	in
	BatSet.map (fun e -> (l, e)) result

and subst_var_comp_list : lexp list -> id list -> ((label * exp) list) BatSet.t
= fun es vars ->
	match es with
	| [] -> BatSet.singleton []
	| hd::tl -> 
		let tl_result = subst_var_comp_list tl vars in
		BatSet.fold (fun e acc -> 
			BatSet.union acc (BatSet.map (fun es -> e::es) tl_result)
		) (subst_var_comp hd vars) BatSet.empty

let rec update_var_comp : prog -> (label * exp) list -> (label * exp) BatSet.t
= fun pgm candidates ->
	List.fold_left (fun acc (l, e) -> 
		let pgm = Localize.gen_partial_pgm l pgm in
		let hole = !hole_count in
		let (_, _, v_t, _) = Type.run pgm in
		let vars = BatMap.foldi (fun x typ vars ->
			match typ with
            | TCtor _ -> vars
			(*| TCtor _| TArr _-> vars*)
			| _ -> x::vars
		) (BatMap.find hole v_t) [] in
		let result = subst_var_comp (l, e) vars in
		BatSet.union result acc
	) BatSet.empty candidates

let rec subst_exp : lexp -> (label * exp) -> lexp
= fun (l1, exp1) (l2, exp2) ->
	let exp = if l1 = l2 then exp2 else 
		begin match exp1 with
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
		end
	in
	(l1, exp)

let rec subst_decl : decl -> (label * exp) -> decl
= fun decl candidate ->
	match decl with
  | DLet (f, is_rec, args, typ, e) -> DLet (f, is_rec, args, typ, subst_exp e candidate)
  | DBlock (is_rec, bindings) ->
    let bindings = List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, args, typ, subst_exp e candidate)) bindings in
    DBlock (is_rec, bindings) 
  |_ -> decl

(*
	Input : An incorrect program pgm, a correct program cpgm, and a set of testcases testcases
	Output : A repaired program pgm' satisfying all testcases
*)
let run : prog -> prog -> examples -> prog option
= fun pgm cpgm testcases -> 
	(* Repair Candidates *)
	let start_time = Unix.gettimeofday () in
	let (t1, t2) = (Cfg.S.run pgm, Cfg.S.run cpgm) in
    (*
	Print.print_header "Analysis1"; print_endline (Cfg.S.string_of_t t1);
  Print.print_header "Analysis2"; print_endline (Cfg.S.string_of_t t2);
  *)
	let repair_cand = get_repair_candidate (pgm, t1) (cpgm, t2) in
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
