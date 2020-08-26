open Lang
open Util
open Symbol_lang2
open Thread
open Event

module Executor = struct
	(* Symbolic execution module *)
	let k_bound = ref 6 (* Loop bound *)
	let start_time = ref 0.0 (* Time Out *)

	let empty_env = BatMap.empty
	let extend_env (x,t) env = BatMap.add x t env
	let find_env x env = BatMap.find x env

	let gen_sym_state (pc, sv) = BatSet.singleton (pc, sv)

	exception Invalid_Arg (* Argument is not feasible *)

	(* Helper functions for pattern matching *)
	let rec flatten_branch : branch list -> branch list
	= fun bs ->
		match bs with
		| [] -> []
		| (p, e)::bs -> 
			begin match p with
			| Pats ps -> 
				let flat_bs = (List.map (fun p -> (p, e)) ps) in
				(flatten_branch flat_bs)@(flatten_branch bs)
			| _ -> (p, e)::(flatten_branch bs)
			end

	let rec pattern_match : symbolic_value -> pat -> bool
	= fun sv p ->
	  match (sv, p) with
	  | Int n1, PInt n2 -> n1 = n2
	  | Bool b1, PBool b2 -> b1 = b2
	  | List l1, PList l2 | Tuple l1, PTuple l2 -> pattern_match_list l1 l2
	  | Ctor (x1, l1), PCtor (x2, l2) -> (x1 = x2) && pattern_match_list l1 l2
	  | List (vhd::vtl), PCons (phd, ptl) -> (pattern_match vhd phd) && (pattern_match (List vtl) ptl)
	  | ASymbol _, PInt _ | Aop _, PInt _ | Minus _, PInt _ | Not _, PBool _ | Bop _, PBool _ | ABop _, PBool _ | EQop _, PBool _ -> true
	  | _, PVar _ | _, PUnder -> true
	  | _, Pats pl -> raise (Failure "Invalid pattern type")
	  | _ -> false
	
	and pattern_match_list : symbolic_value list -> pat list -> bool
	= fun svs ps -> try List.for_all2 pattern_match svs ps with _ -> false
  
  let rec has_symbol : symbolic_value -> bool
 	= fun sv ->
 		match sv with
	  | SSymbol _ | ASymbol _ -> true
	  | List svs | Tuple svs | Ctor (_, svs) -> List.exists has_symbol svs
	  | Minus sv | Not sv -> has_symbol sv
	  | Aop (_, sv1, sv2) | Bop (_, sv1, sv2) | ABop (_, sv1, sv2) | EQop (_, sv1, sv2) 
	  | Cons (sv1, sv2) | Append (sv1, sv2) | Strcon (sv1, sv2) -> has_symbol sv1 || has_symbol sv2
	  | _ -> false

  let rec pat_has_int : pat -> bool
  = fun p -> 
  	match p with
  	| PInt _ -> true
  	| PList ps | PTuple ps | PCtor (_, ps) | Pats ps -> List.exists pat_has_int ps
  	| PCons (phd, ptl) -> pat_has_int phd || pat_has_int ptl
  	| _ -> false

  let rec gen_matched_value : symbolic_value -> pat -> symbolic_value
  = fun sv p ->
  	match (sv, p) with
	  | Int _, PInt n2 | ASymbol _, PInt n2 | Aop _, PInt n2 | Minus _, PInt n2 -> Int n2
	  | Bool _, PBool b2 | Not _, PBool b2 | Bop _, PBool b2 | ABop _, PBool b2 | EQop _, PBool b2-> Bool b2
	  | List svs, PList ps -> List (List.map2 gen_matched_value svs ps)
	  | Tuple svs, PTuple ps -> Tuple (List.map2 gen_matched_value svs ps)
	  | Ctor (x1, svs), PCtor (x2, ps) -> Ctor (x2 , List.map2 gen_matched_value svs ps)
	  | List (vhd::vtl), PCons (phd, ptl) -> Cons (gen_matched_value vhd phd, (gen_matched_value (List vtl) ptl))
	  | _, PVar _ | _, PUnder -> sv
	  | _ -> raise (Failure "Invalid pattern type")

  (* Environment update *)
	let rec func_binding : arg list -> lexp -> lexp 
  = fun xs e -> 
    match xs with
    | [] -> e
    | hd::tl -> (gen_label (), EFun (hd, func_binding tl e))  

  let flat_sym_formula : sym_formula -> sym_formula list
	= fun psi ->
		let flat_list = BatSet.fold (fun (pc, sv) psis ->
			match sv with
			| Tuple svs -> (List.map (fun sv -> (pc, sv)) svs)::psis
			| _ -> raise (Failure "argument binding failure - tuples should be bound with tuple")
		) psi []
		in
		let init_psis = List.map (fun (pc, sv) -> BatSet.empty) (List.hd flat_list) in
		let psi_list = List.fold_left (fun acc psi_list ->
			List.rev (List.fold_left2 (fun acc (pc, sv) psi -> (BatSet.add (pc, sv) psi::acc)) [] psi_list acc) 
		) init_psis flat_list 
		in
		psi_list

	let rec let_binding : symbolic_env -> let_bind -> sym_formula -> symbolic_env
	= fun env x psi ->
		match x with
		| BindUnder -> env
		| BindOne x -> extend_env (x, psi) env
		| BindTuple xs ->
			if BatSet.is_empty psi then
				raise Invalid_Arg
			else
				let valid = BatSet.for_all (fun (pc, sv) -> 
					match sv with 
					| Tuple svs -> (List.length svs = List.length xs) 
					| _ -> false 
				) psi
				in
				if valid then
					let psis = flat_sym_formula psi in
					List.fold_left2 let_binding env xs psis
				else raise (Failure "argument binding failure - tuples are not compatible")

	let rec arg_binding : symbolic_env -> arg -> sym_formula -> symbolic_env
	= fun env arg psi ->
		let rec arg_to_binding : arg -> let_bind
		= fun arg ->
			match arg with
			| ArgUnder _ -> BindUnder
			| ArgOne (x, t) -> BindOne x
			| ArgTuple xs -> BindTuple (List.map arg_to_binding xs)
		in
		let_binding env (arg_to_binding arg) psi
	
	let rec pat_binding : symbolic_env -> path_cond -> pat -> symbolic_value -> symbolic_env
	= fun env pc p sv ->
		(*print_endline (symbol_to_string sv ^ " " ^ Print.pat_to_string p);*)
		match (sv, p) with
		| _, PUnit | _, PInt _ | _, PBool _ | _, PUnder -> env
		| _, PVar x -> extend_env (x, gen_sym_state (pc, sv)) env
		| List svs, PList ps | Tuple svs, PTuple ps | Ctor (_, svs), PCtor (_, ps) -> pat_list_binding env pc ps svs
	  | List (vhd::vtl), PCons (phd, ptl) -> pat_binding (pat_binding env pc phd vhd) pc ptl (List vtl)
	  | Cons (vhd, vtl), PCons (phd, ptl) -> pat_binding (pat_binding env pc phd vhd) pc ptl vtl
		| _, Pats ps -> pat_binding env pc (List.hd ps) sv (* TODO => validitiy of patterns *)
		| _ -> raise (Failure ("Pattern binding failure " ^ Print.pat_to_string p ^ ", " ^ symbol_to_string sv))

	and pat_list_binding : symbolic_env -> path_cond -> pat list -> symbolic_value list -> symbolic_env
	= fun env pc ps svs-> List.fold_left2 (fun env p sv-> pat_binding env pc p sv) env ps svs

	(* Helper functions for mutually recursive call *)
	let rec is_fun : sym_formula -> bool
	= fun psi ->
		BatSet.exists (fun (pc, sv) -> 
			let rec symval_is_fun : symbolic_value -> bool
			= fun sv ->
				match sv with
			  | Minus sv | Not sv -> symval_is_fun sv 
			  | Aop (_, sv1, sv2) | Bop (_, sv1, sv2) | ABop (_, sv1, sv2) | EQop (_, sv1, sv2)
			  | Cons (sv1, sv2) | Append (sv1, sv2) | Strcon (sv1, sv2) -> symval_is_fun sv1 || symval_is_fun sv2
			  | List vs | Tuple vs | Ctor (_, vs) -> List.exists symval_is_fun vs
			  | Fun _ | FunRec _ | FunBlock _ -> true
			  | _ -> false
			in symval_is_fun sv
		) psi
	  
	let rec update_func_env : symbolic_env -> sym_formula -> sym_formula
	= fun env psi ->
		BatSet.map (fun (pc, sv) -> 
			let rec update_symval_func_env : symbolic_value -> symbolic_value
			= fun sv ->
				match sv with
				| Minus sv -> Minus (update_symval_func_env sv)
				| Not sv -> Not (update_symval_func_env sv)
			  | Aop (op, sv1, sv2) -> Aop (op, update_symval_func_env sv1, update_symval_func_env sv2)
			  | Bop (op, sv1, sv2) -> Bop (op, update_symval_func_env sv1, update_symval_func_env sv2)
				| ABop (op, sv1, sv2) -> ABop (op, update_symval_func_env sv1, update_symval_func_env sv2)
				| EQop (op, sv1, sv2) -> EQop (op, update_symval_func_env sv1, update_symval_func_env sv2)
			  | Cons (sv1, sv2) -> Cons (update_symval_func_env sv1, update_symval_func_env sv2)
			  | Append (sv1, sv2) -> Append (update_symval_func_env sv1, update_symval_func_env sv2)
			  | Strcon (sv1, sv2) -> Strcon (update_symval_func_env sv1, update_symval_func_env sv2)
			  | List svs -> List (List.map update_symval_func_env svs)
			  | Tuple svs -> Tuple (List.map update_symval_func_env svs)
			  | Ctor (x, svs) -> Ctor (x, List.map update_symval_func_env svs)
			  | Fun (x, e, env') -> Fun (x, e, env)
			  | FunRec (f, x, e, env', depth) -> FunRec (f, x, e, env, depth)
			  | FunBlock (f, mappings) -> 
			    let (xs, psis) = List.split mappings in
			    let psis = List.map (fun psi -> update_func_env env psi) psis in
			    FunBlock (f, List.combine xs psis)
			  | _ -> sv
			in
			(pc, update_symval_func_env sv)
		) psi

	let rec decrease_func_depth : sym_formula -> sym_formula
	= fun psi ->
		BatSet.map (fun (pc, sv) ->
			let rec decrease_symval_func_depth : symbolic_value -> symbolic_value
			= fun sv ->
			match sv with
			| Minus sv -> Minus (decrease_symval_func_depth sv)
			| Not sv -> Not (decrease_symval_func_depth sv)
		  | Aop (op, sv1, sv2) -> Aop (op, decrease_symval_func_depth sv1, decrease_symval_func_depth sv2)
		  | Bop (op, sv1, sv2) -> Bop (op, decrease_symval_func_depth sv1, decrease_symval_func_depth sv2)
			| ABop (op, sv1, sv2) -> ABop (op, decrease_symval_func_depth sv1, decrease_symval_func_depth sv2)
			| EQop (op, sv1, sv2) -> EQop (op, decrease_symval_func_depth sv1, decrease_symval_func_depth sv2)
		  | Cons (sv1, sv2) -> Cons (decrease_symval_func_depth sv1, decrease_symval_func_depth sv2)
		  | Append (sv1, sv2) -> Append (decrease_symval_func_depth sv1, decrease_symval_func_depth sv2)
		  | Strcon (sv1, sv2) -> Strcon (decrease_symval_func_depth sv1, decrease_symval_func_depth sv2)
		  | List svs -> List (List.map decrease_symval_func_depth svs)
		  | Tuple svs -> Tuple (List.map decrease_symval_func_depth svs)
		  | Ctor (x, svs) -> Ctor (x, List.map decrease_symval_func_depth svs)
		  | FunRec (f, x, e, env', depth) -> FunRec (f, x, e, env', depth - 1)
		  | _ -> sv
			in
			(pc, decrease_symval_func_depth sv)
		) psi
		
	let bind_block : symbolic_env -> path_cond -> (id * sym_formula) list -> symbolic_env
	= fun env pc mappings ->
	  let (xs, _) = List.split mappings in
	  List.fold_left (fun env x -> extend_env (x, gen_sym_state (pc, FunBlock (x, mappings))) env) env xs

	(* Execution rules *)
	let rec sym_eval_exp : symbolic_env -> path_cond -> bool -> lexp -> sym_formula
	= fun env pc mode (l, exp) ->
		(*
		let _ =
			print_endline (string_of_pc pc);
			print_endline ("Exp : " ^ Print.exp_to_string (l, exp));
			print_endline (if mode then "Dynamic" else "Static");
			print_endline ("Time : " ^ string_of_float (Unix.gettimeofday () -. !start_time))
		in
		*)
		let psi =
			try 
				if Unix.gettimeofday () -. !start_time > 0.25 then
					BatSet.singleton (pc, Exn) 
				else 
				match exp with 
			  (* Const *)
			  | SStr n -> BatSet.singleton (pc, SSymbol n)
			 	| SInt n -> BatSet.singleton (pc, ASymbol n)
			  | EUnit -> BatSet.singleton (pc, Unit)
			  | Const n -> BatSet.singleton (pc, Int n)
			  | TRUE -> BatSet.singleton (pc, Bool true)
			  | FALSE -> BatSet.singleton (pc, Bool false)
			  | String str -> BatSet.singleton (pc, Str str)
			  | EVar x -> BatSet.map (fun (pc', sv') -> (extend_pc pc pc', sv')) (find_env x env)
			  | EList es -> 
			  	let psis = sym_eval_exp_list env pc mode es in
			  	BatSet.map (fun (pc, svs) -> (pc, List svs)) psis
			  | ETuple es -> 
			  	let psis = sym_eval_exp_list env pc mode es in
			  	BatSet.map (fun (pc, svs) -> (pc, Tuple svs)) psis
			  | ECtor (x, es) -> 
			  	let psis = sym_eval_exp_list env pc mode es in
			  	BatSet.map (fun (pc, svs) -> (pc, Ctor (x, svs))) psis
			  | Raise e -> BatSet.singleton (pc, Exn)
			  | EFun (arg, e) -> BatSet.singleton (pc, Fun (arg, e, env))
			  (* Unary operation *)
			  | MINUS e -> 
			  	let psi1 = sym_eval_exp env pc mode e in
			  	BatSet.map (fun (pc, sv) -> (pc, Minus sv)) psi1
			  | NOT e -> 
			  	let psi1 = sym_eval_exp env pc mode e in
			  	BatSet.map (fun (pc, sv) -> (pc, Not sv)) psi1
			  (* Binary operation *)
			  | ADD (e1, e2) -> 
			  	let (psi1, psi2) = (sym_eval_exp env pc mode e1, sym_eval_exp env pc mode e2) in
			  	sym_eval_aop pc psi1 psi2 Add
			  | SUB (e1, e2) -> 
			  	let (psi1, psi2) = (sym_eval_exp env pc mode e1, sym_eval_exp env pc mode e2) in
			  	sym_eval_aop pc psi1 psi2 Sub
			  | MUL (e1, e2) -> 
			  	let (psi1, psi2) = (sym_eval_exp env pc mode e1, sym_eval_exp env pc mode e2) in
			  	sym_eval_aop pc psi1 psi2 Mul
			  | DIV (e1, e2) -> 
			  	let (psi1, psi2) = (sym_eval_exp env pc mode e1, sym_eval_exp env pc mode e2) in
			  	sym_eval_nonzero_aop pc psi1 psi2 Div
			  | MOD (e1, e2) -> 
			  	let (psi1, psi2) = (sym_eval_exp env pc mode e1, sym_eval_exp env pc mode e2) in
			  	sym_eval_nonzero_aop pc psi1 psi2 Mod
			  | OR (e1, e2) -> 
			  	let (psi1, psi2) = (sym_eval_exp env pc mode e1, sym_eval_exp env pc mode e2) in
			  	sym_eval_bop pc psi1 psi2 Or
			  | AND (e1, e2) -> 
			  	let (psi1, psi2) = (sym_eval_exp env pc mode e1, sym_eval_exp env pc mode e2) in
			  	sym_eval_bop pc psi1 psi2 And
			  | LESS (e1, e2) -> 
			  	let (psi1, psi2) = (sym_eval_exp env pc mode e1, sym_eval_exp env pc mode e2) in
			  	sym_eval_abop pc psi1 psi2 Lt
			  | LESSEQ (e1, e2) -> 
			  	let (psi1, psi2) = (sym_eval_exp env pc mode e1, sym_eval_exp env pc mode e2) in
			  	sym_eval_abop pc psi1 psi2 Le
			  | LARGER (e1, e2) -> 
			  	let (psi1, psi2) = (sym_eval_exp env pc mode e1, sym_eval_exp env pc mode e2) in
			  	sym_eval_abop pc psi1 psi2 Gt
			  | LARGEREQ (e1, e2) -> 
			  	let (psi1, psi2) = (sym_eval_exp env pc mode e1, sym_eval_exp env pc mode e2) in
			  	sym_eval_abop pc psi1 psi2 Ge
			  | EQUAL (e1, e2) ->
			  	let (psi1, psi2) = (sym_eval_exp env pc mode e1, sym_eval_exp env pc mode e2) in
			  	sym_eval_eqop pc psi1 psi2 Eq
			  | NOTEQ (e1, e2) -> 
			  	let (psi1, psi2) = (sym_eval_exp env pc mode e1, sym_eval_exp env pc mode e2) in
			  	sym_eval_eqop pc psi1 psi2 NEq
			  | DOUBLECOLON (e1, e2) -> 
			  	let (psi1, psi2) = (sym_eval_exp env pc mode e1, sym_eval_exp env pc mode e2) in
			  	BatSet.fold (fun (pc1, sv1) acc ->
						let psi = BatSet.fold (fun (pc2, sv2) acc2 ->
							BatSet.add (extend_pc pc (extend_pc pc1 pc2), Cons (sv1, sv2)) acc2
						) psi2 BatSet.empty 
						in 
						BatSet.union psi acc
					) psi1 BatSet.empty
			  | AT (e1, e2) -> 
			  	let (psi1, psi2) = (sym_eval_exp env pc mode e1, sym_eval_exp env pc mode e2) in
			  	BatSet.fold (fun (pc1, sv1) acc ->
						let psi = BatSet.fold (fun (pc2, sv2) acc2 ->
							BatSet.add (extend_pc pc (extend_pc pc1 pc2), Append (sv1, sv2)) acc2
						) psi2 BatSet.empty 
						in 
						BatSet.union psi acc
					) psi1 BatSet.empty
			  | STRCON (e1,e2) -> 
			  	let (psi1, psi2) = (sym_eval_exp env pc mode e1, sym_eval_exp env pc mode e2) in
			  	BatSet.fold (fun (pc1, sv1) acc ->
						let psi = BatSet.fold (fun (pc2, sv2) acc2 ->
							BatSet.add (extend_pc pc (extend_pc pc1 pc2), Strcon (sv1, sv2)) acc2
						) psi2 BatSet.empty 
						in 
						BatSet.union psi acc
					) psi1 BatSet.empty
			  | IF (e1, e2, e3) ->
			  	let psi1 = sym_eval_exp env pc mode e1 in
			  	BatSet.fold (fun (pc1, sv1) psi ->
			  		let psi' =
			  			(* If branch is deterministic execute one of them *)
				  		if sv1 = Bool true then
			    			let pc = extend_pc pc (extend_pc pc1 (gen_pc sv1)) in
			    			sym_eval_exp env pc mode e2
				  		else if sv1 = Bool false then
			    			let pc = extend_pc pc (extend_pc pc1 (gen_pc (Not sv1))) in
			    			sym_eval_exp env pc mode e3
				  		else 
				  			(* Execute both branches with bounded loop boundary *)
				  			let pc2 = extend_pc pc (extend_pc pc1 (gen_pc sv1)) in
			    			let psi2 = sym_eval_exp env pc2 false e2 in
			    			let pc3 = extend_pc pc (extend_pc pc1 (gen_pc (Not sv1))) in
			    			let psi3 = sym_eval_exp env pc3 false e3 in
			    			BatSet.union psi2 psi3
				  	in
				  	BatSet.union psi' psi
			  	) psi1 BatSet.empty
			  | EMatch (e, bs) -> 
			  	let psi1 = sym_eval_exp env pc mode e in
			  	BatSet.fold (fun (pc, sv) psi ->
			  		let bs = List.filter (fun (p, e') -> pattern_match sv p) (flatten_branch bs) in	
			  		let psi' =
			  			if List.length bs = 0 then 
			  				(* Pattern Match Fail *)
			  				(*raise (Failure ("Symbol : " ^ symbol_to_string sv ^ " Pattern matching failure"))*)
			  				BatSet.singleton (pc, Exn)
				  		else if not (has_symbol sv) || List.for_all (fun (p, e') -> not (pat_has_int p)) bs then
			  				(* If match with only one branch or only matched with wildcards => concrete execution *)
				  			let (p, e') = List.hd bs in
				  			let sv' = gen_matched_value sv p in
				  			let pc = extend_pc pc (gen_pc (EQop (Eq, sv, sv'))) in
				  			sym_eval_exp (pat_binding env pc p sv') pc mode e'
				  		else
				  			(* If not execute all matched branchese with bounded loop boundary *)
				  			let (psi, _) = List.fold_left (fun (psis, ps) (p, e') ->
				  				let pc = List.fold_left (fun pc p -> 
				  					let sv' = gen_matched_value sv p in
				  					extend_pc pc (gen_pc (EQop (NEq, sv, sv')))
				  				) pc ps 
				  				in
				  				let sv' = gen_matched_value sv p in
				  				let pc = extend_pc pc (gen_pc (EQop (Eq, sv, sv'))) in
				  				let psi = sym_eval_exp (pat_binding env pc p sv') pc false e' in
				  				(BatSet.union psi psis, p::ps)
				  			) (BatSet.empty, []) bs
				  			in
				  			psi 
				  	in
				  	BatSet.union psi' psi
			  	) psi1 BatSet.empty
			  | ELet (f, is_rec, args, typ, e1, e2) -> 
			  	begin match args with
			  	| [] ->
			  		let psi1 = sym_eval_exp env pc mode e1 in
			  		if is_rec then
		          begin match f with
		          | BindOne f ->
		          	let psi1 = BatSet.map (fun (pc1, sv1) -> 
		          		match sv1 with
		          		| Fun (x, e, env') -> (pc1, FunRec (f, x, e, env', !k_bound))
		          		| _ -> (pc1, sv1)
		          	) psi1 
		          	in
		          	sym_eval_exp (extend_env (f, psi1) env) pc mode e2
		          | _ -> raise (Failure "Only variables are allowed as left-hand side of `let rec'")
		          end
		        else 
		        	let pc = if BatSet.cardinal psi1 = 1 then BatSet.fold (fun (pc1, sv1) pc -> extend_pc pc1 pc) psi1 pc else pc in
		        	sym_eval_exp (let_binding env f psi1) pc mode e2
			  	| _ ->
				  	let x = List.hd args in
				  	let vfunc = 
						  if is_rec then
					      begin match f with
					      | BindOne f -> BatSet.singleton (pc, FunRec (f, x, (func_binding (List.tl args) e1), env, !k_bound))
					      | _ -> raise (Failure "Only variables are allowed as left-hand side of `let rec'")
					      end
					    else BatSet.singleton (pc, Fun (x, (func_binding (List.tl args) e1), env))  
					   in
					   sym_eval_exp (let_binding env f vfunc) pc mode e2
					 end
			  | EBlock (is_rec, bindings, e2) -> 
			  	let env = 
			      begin match is_rec with
			      | true ->
			        let (func_map, const_map) = List.fold_left (
			          fun (func_map, const_map) (f, is_rec, args, typ, exp) ->
			          begin match f with 
			          | BindOne x ->
			            let psi = sym_eval_exp env pc mode (gen_label(), ELet (BindOne x, is_rec, args, typ, exp, (gen_label (), EVar x))) in
			            if is_fun psi then ((x, psi)::func_map, const_map) else (func_map, (x, psi)::const_map)
			          | _ -> raise (Failure "Only variables are allowed as left-hand side of `let rec'")
			          end
			        ) ([], []) bindings
			        in
			        (* constant mapping *)
			        let init_env = List.fold_left (fun env (x, psi) -> extend_env (x, psi) env) env const_map in
			        (* update each function's closure *)
			        let func_map = List.map (fun (x, psi) -> (x, update_func_env init_env psi)) func_map in
			        (* block mapping *)
			        List.fold_left (fun env (x, psi) -> extend_env (x, (gen_sym_state (pc, FunBlock (x, func_map)))) env) init_env func_map
			      | false ->
			        let psis = List.map (fun (f, is_rec, args, typ, e) -> sym_eval_exp env pc mode (gen_label (), ELet (f, is_rec, args, typ, e, let_to_exp f))) bindings in
			        List.fold_left2 (fun env (f, is_rec, args, typ, e) psi -> let_binding env f psi) env bindings psis
			      end
			    in sym_eval_exp env pc mode e2
			  | EApp (e1, e2) ->
			  	let (psi1, psi2) = (sym_eval_exp env pc mode e1, sym_eval_exp env pc mode e2) in
			  	(* If e1 or e2 are evaluated statically then change execution mode *)
			  	let mode = if (BatSet.cardinal psi1) > 1 || (BatSet.cardinal psi2) > 1 then false else mode in
			    BatSet.fold (fun (pc1, sv1) psis ->
			    	begin match sv1 with
			    	| Fun (x, e, env') ->
			    		let psi = sym_eval_exp (arg_binding env' x psi2) (extend_pc pc pc1) mode e in
							BatSet.union psi psis
			    	| FunRec (f, x, e, env', k) ->
	    				(* Stop if it over the loop-bound *)
			    		let psi =
			    			(* Stop if it over the loop-bound *)
				    		if k <= 0 then BatSet.singleton (extend_pc pc pc1, Exn)
				    		else 
				    		(* If the mode is static execution, decrease the loop depth *)
				    			sym_eval_exp (extend_env (f, BatSet.singleton (pc1, FunRec (f, x, e, env', if mode then k else k-1))) (arg_binding env' x psi2)) (extend_pc pc pc1) mode e 
					    in
							BatSet.union psi psis
			    	| FunBlock (f, mappings) -> 
			    		let psi1 = snd (List.find (fun (f', psi) -> f = f') mappings) in
			    		BatSet.fold (fun (pc1, sv1) psis ->
			    			let psi = 
				    			begin match sv1 with
				    			| FunRec (f, x, e, env', k) ->
				    				(* Stop if it over the loop-bound *)
						    		if k <= 0 then BatSet.singleton (extend_pc pc pc1, Exn)
						    		else 
						    		(* If the mode is static execution, decrease the loop depth *)
						    			let mappings = if mode then mappings else List.map (fun (f', psi) -> if f = f' then (f', decrease_func_depth psi) else (f', psi)) mappings in
						    			let env' = bind_block env' pc mappings in
						    			sym_eval_exp (arg_binding env' x psi2) (extend_pc pc pc1) mode e 
				    			| _ -> raise (Failure "mutually recursive function call error")
				    			end
				    		in 
				    		BatSet.union psi psis
			    		)	psi1 BatSet.empty 
			    	| Exn -> BatSet.add (pc, Exn) psis
			    	| _ -> raise (Failure ("Symbol : " ^ symbol_to_string sv1 ^ " Function Call error"))
			    	end
					) psi1 BatSet.empty
				| _ -> raise (Failure "Unexpected Exp while sym_exec")
			with 
			| Invalid_Arg -> BatSet.singleton (pc, Exn)
			| e -> (*print_endline (Printexc.to_string e);*) BatSet.singleton (pc, Exn) (* Stack Overflow or TimeOut *)
		in 
		let psi = Formula_normalize.run psi in
		(*
		let _ = 
			print_endline ("*************");
			print_endline (string_of_pc pc);
			print_endline ("Exp : " ^ Print.exp_to_string (l, exp));
			print_endline ("Output : ");
			print psi;
		in
		*)
		(*print_endline ("Size : " ^ string_of_int (BatSet.cardinal psi));*)
		psi

	and sym_eval_exp_list : symbolic_env -> path_cond -> bool -> lexp list -> (path_cond * symbolic_value list) BatSet.t
	= fun env pc mode es ->
		match es with
		| [] -> (BatSet.singleton (pc, []))
		| hd::tl ->
			let psi1 = sym_eval_exp env pc mode hd in
			let psi2 = sym_eval_exp_list env pc mode tl in
			BatSet.fold (fun (pc1, sv1) acc ->
				let psi = BatSet.fold (fun (pc2, svs) acc2 ->
					BatSet.add (extend_pc pc (extend_pc pc1 pc2), sv1::svs) acc2
				) psi2 BatSet.empty
				in
				BatSet.union psi acc
			) psi1 BatSet.empty

	and sym_eval_aop : path_cond -> sym_formula -> sym_formula -> operator -> sym_formula
	= fun pc psi1 psi2 op ->
		BatSet.fold (fun (pc1, sv1) acc ->
			let psi = BatSet.fold (fun (pc2, sv2) acc2 ->
				BatSet.add (extend_pc pc (extend_pc pc1 pc2), Aop (op, sv1, sv2)) acc2
			) psi2 BatSet.empty
			in
			BatSet.union psi acc
		) psi1 BatSet.empty
	
	and sym_eval_nonzero_aop : path_cond -> sym_formula -> sym_formula -> operator -> sym_formula
	= fun pc psi1 psi2 op ->
		BatSet.fold (fun (pc1, sv1) acc ->
			let psi = BatSet.fold (fun (pc2, sv2) acc2 ->
				let pc2 = extend_pc (gen_pc (EQop (NEq, sv2, Int 0))) pc2 in
				BatSet.add (extend_pc pc (extend_pc pc1 pc2), Aop (op, sv1, sv2)) acc2
			) psi2 BatSet.empty
			in
			BatSet.union psi acc
		) psi1 BatSet.empty

	and sym_eval_bop : path_cond -> sym_formula -> sym_formula -> combinator -> sym_formula
	= fun pc psi1 psi2 op ->
		BatSet.fold (fun (pc1, sv1) acc ->
			let psi = BatSet.fold (fun (pc2, sv2) acc2 ->
				BatSet.add (extend_pc pc (extend_pc pc1 pc2), Bop (op, sv1, sv2)) acc2
			) psi2 BatSet.empty
			in
			BatSet.union psi acc
		) psi1 BatSet.empty

	and sym_eval_abop : path_cond -> sym_formula -> sym_formula -> comparator -> sym_formula
	= fun pc psi1 psi2 op ->
		BatSet.fold (fun (pc1, sv1) acc ->
			let psi = BatSet.fold (fun (pc2, sv2) acc2 ->
				BatSet.add (extend_pc pc (extend_pc pc1 pc2), ABop (op, sv1, sv2)) acc2
			) psi2 BatSet.empty
			in
			BatSet.union psi acc
		) psi1 BatSet.empty

	and sym_eval_eqop : path_cond -> sym_formula -> sym_formula -> eq_operator -> sym_formula
	= fun pc psi1 psi2 op ->
		BatSet.fold (fun (pc1, sv1) acc ->
			let psi = BatSet.fold (fun (pc2, sv2) acc2 ->
				BatSet.add (extend_pc pc (extend_pc pc1 pc2), EQop (op, sv1, sv2)) acc2
			) psi2 BatSet.empty
			in
			BatSet.union psi acc
		) psi1 BatSet.empty

	let rec sym_eval_decl : symbolic_env -> decl -> symbolic_env
	= fun env decl ->
		match decl with
	  | DLet (f, is_rec, args, typ, e) ->
	  	let exp = 
	      begin match f with 
	      | BindUnder -> e
	      | _ -> (gen_label(), ELet (f, is_rec, args, typ, e, let_to_exp f))
	      end
	    in
	    let psi = sym_eval_exp env init_pc true exp in
	    let_binding env f psi
	  | DBlock (is_rec, bindings) -> 
	  	begin match is_rec with
      | true ->
        let (func_map, const_map) = List.fold_left (
          fun (func_map, const_map) (f, is_rec, args, typ, exp) ->
          begin match f with 
          | BindOne x ->
            let psi = sym_eval_exp env init_pc true (gen_label(), ELet (BindOne x, is_rec, args, typ, exp, (gen_label (), EVar x))) in
            if is_fun psi then ((x, psi)::func_map, const_map) else (func_map, (x, psi)::const_map)
          | _ -> raise (Failure "Only variables are allowed as left-hand side of `let rec'")
          end
        ) ([], []) bindings
        in
        (* constant mapping *)
        let init_env = List.fold_left (fun env (x, psi) -> extend_env (x, psi) env) env const_map in
        (* update each function's closure *)
        let func_map = List.map (fun (x, psi) -> (x, update_func_env init_env psi)) func_map in
        (* block mapping *)
	      List.fold_left (fun env (x, psi) -> extend_env (x, (gen_sym_state (init_pc, FunBlock (x, func_map)))) env) init_env func_map
      | false ->
        let envs = List.map (fun binding -> sym_eval_decl env (DLet binding)) bindings in
        List.fold_left (fun env new_env -> BatMap.union env new_env) env envs
      end
	  | _ -> env

	let run : prog -> lexp list -> sym_formula
	= fun pgm input ->
  	let pgm = pgm@(!grading_pgm) in
  	let _ = start_time := Unix.gettimeofday () in
  	let init_env = List.fold_left (fun env decl -> sym_eval_decl env decl) empty_env (!library_pgm) in
  	let _ = start_time := Unix.gettimeofday () in
  	let env = List.fold_left (fun (env) decl -> sym_eval_decl env decl) init_env pgm in
  	let exp = appify (gen_label(), (EVar !Options.opt_entry_func)) input in
  	let _ = start_time := Unix.gettimeofday () in
  	Formula_normalize.run2 (sym_eval_exp env init_pc true exp)
end

module VC_generator = struct

	let rec is_valid_pc : path_cond -> bool
	= fun pc ->
		(Formula_normalize.is_sat_pc pc pc) && (Formula_normalize.is_sat_pc2 pc pc) && (Formula_normalize.filter_unsat_class pc) && (Formula_normalize.manual_filter pc)

	(* VC generation *)
	let run : Z3_solve.CtorTable.t -> sym_formula -> sym_formula -> vc
	= fun ctor_table psi_b psi_c -> 
		if BatSet.is_empty psi_b then
			[[(Bool true, Bool false)]]
		else
			BatSet.fold (fun (pc1, sv1) vc ->
				let pre = Formula_normalize.normalize_sym_val (flatten_pc pc1) in
				let formula = BatSet.fold (fun (pc2, sv2) formula ->
					(* If two pathes are not compitable => that formula is invalid (true => false) *)
					if not (is_valid_pc (extend_pc pc1 pc2)) then
						if (List.length formula > 0) then formula else (pre, Bool false)::formula
					(* else check satisfiablity *)
					else 
						let pc = Formula_normalize.normalize_sym_val (flatten_pc pc2) in
						let eq = Formula_normalize.normalize_sym_val (EQop (Eq, sv1, sv2)) in
						if eq = Bool false then
							if (List.length formula > 0) then formula else (pre, Bool false)::formula
						else 
							let post = Bop (And, pc, eq) in
							(pre, post)::formula
				) psi_b []
				in
				let formula' = List.filter (fun (pre, post) -> post <> Bool false) formula in
				if List.length formula' > 0 then formula'::vc else formula::vc
			) psi_c []

	let run2 : Z3_solve.CtorTable.t -> sym_formula -> sym_formula -> vc
	= fun ctor_table psi_b psi_c -> 
		if BatSet.is_empty psi_b then
			[[(Bool true, Bool false)]]
		else
			BatSet.fold (fun (pc1, sv1) vc ->
				let pre = Formula_normalize.normalize_sym_val (flatten_pc pc1) in
				let formula = BatSet.fold (fun (pc2, sv2) formula ->
					(* If two pathes are not compitable => that formula is invalid (true => false) *)
					let pc_formula = [(Bool true ,flatten_pc (extend_pc pc1 pc2))] in
					let t = (match Z3_solve.is_valid ctor_table pc_formula with
					| Some _ -> true
					| None -> false)
					in
					if not t then
						if (List.length formula > 0) then formula else (pre, Bool false)::formula
					(* else check satisfiablity *)
					else 
						let pc = Formula_normalize.normalize_sym_val (flatten_pc pc2) in
						let eq = Formula_normalize.normalize_sym_val (EQop (Eq, sv1, sv2)) in
						if eq = Bool false then
							if (List.length formula > 0) then formula else (pre, Bool false)::formula
						else 
							let post = Bop (And, pc, eq) in
							(pre, post)::formula
				) psi_b []
				in
				let formula' = List.filter (fun (pre, post) -> post <> Bool false) formula in
				if List.length formula' > 0 then formula'::vc else formula::vc
			) psi_c []

	let string_of_formula : vc_formula -> string
	= fun f -> List.fold_left (fun str (pc, sv) -> symbol_to_string pc ^ " => " ^ symbol_to_string sv ^ "\\/\n" ^ str) "" f

	let print_vc : vc -> unit
	= fun vc -> List.iter (fun f -> print_endline (string_of_formula f ^ "/\\")) vc
end

(* Check the semantic equivalence between two programs *)
let rec run : prog -> prog -> lexp list -> Z3.Model.model option
= fun pgm cpgm input -> 
	let ctor_table = Z3_solve.CtorTable.generation BatMap.empty pgm in
	let ctor_table = Z3_solve.CtorTable.generation ctor_table cpgm in
	(* Symbolic Formula Generation *)
	let psi_c = Executor.run cpgm input in
	if BatSet.is_empty psi_c then
		None
	else
		let psi_b = Executor.run pgm input in
		let vc = (VC_generator.run ctor_table psi_b psi_c) in
		Z3_solve.check2 ctor_table vc