open Lang
open Util

(**************************)
(* Program Simplification *)
(**************************)

module Simplification = struct
  
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

  let rec extract_variables : arg -> string list
	= fun arg ->
	  match arg with
		| ArgUnder _ -> []
		| ArgTuple args -> list_fold (fun arg acc-> extract_variables arg @ acc) args []
		| ArgOne (id, _) -> [id]

	let rec arg_equality : arg * lexp -> bool
	= fun (arg, (l, e)) ->
		match (arg, e) with
		| (ArgUnder _, _) -> false
		| (ArgTuple args, ETuple lst) -> 
		  if (List.length args = List.length lst) then 
			  List.for_all arg_equality (list_combine args lst)
			else false
		| (ArgOne (x1, _), EVar x2) -> if x1=x2 then true else false
		| _ -> false
	
  let rec var_in_arg : string -> arg -> bool
	= fun var arg ->
	  match arg with
		| ArgUnder _ -> false
		| ArgTuple args -> List.exists (var_in_arg var) args
		| ArgOne (x, _) -> x=var

	let rec var_in_bind : string -> let_bind -> bool
	= fun var bind ->
	  match bind with
		| BindUnder -> false
		| BindOne x -> x=var
		| BindTuple lst -> List.exists (var_in_bind var) lst

	let rec var_in_pat : string -> pat -> bool
	= fun var pat ->
	  match pat with
		| PList l | PTuple l | PCtor (_, l) |Pats l -> List.exists (var_in_pat var) l
    | PCons (p1, p2) -> var_in_pat var p1 || var_in_pat var p2
		| PVar x -> x=var
		| _ -> false

	let rec var_exist : string -> lexp -> bool
	= fun var (l, e) -> 
    match e with
    | EList es | ECtor (_, es) | ETuple es -> List.exists (var_exist var) es
    | MINUS e | NOT e | Raise e -> var_exist var e
    | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2)
    | OR (e1, e2) | AND (e1, e2) | LESS (e1, e2) | LARGER (e1, e2) | EQUAL (e1, e2) | NOTEQ (e1, e2)
    | LESSEQ (e1, e2) | LARGEREQ (e1, e2) | AT (e1, e2) | DOUBLECOLON (e1, e2) | STRCON (e1, e2)
    | EApp (e1, e2) -> var_exist var e1 || var_exist var e2
		| EFun (arg, e) -> if var_in_arg var arg then false else var_exist var e
		| ELet (f, is_rec, args, _, e1, e2) ->
			let e1_existence = 
			  if (List.exists (var_in_arg var) args) then false
			  else if (is_rec && var_in_bind var f) then false
				else var_exist var e1
			in
		  let e2_existence = 
			  if (var_in_bind var f) then false else var_exist var e2
		  in
			e1_existence || e2_existence
    | EBlock (is_rec, ds, e2) ->
		  if is_rec then 
			  if (List.exists (fun (bind, _, _, _, _) -> var_in_bind var bind) ds) then false
				else 
				  (
					  List.exists (fun (_, _, args, _, e) -> 
						  if (List.exists (var_in_arg var) args) then false
						  else var_exist var e
					  ) ds
					) || var_exist var e2 
		  else
				List.exists (fun (bind, _, args, _, e) ->
					if (List.exists (var_in_arg var) args || var_in_bind var bind) then false
					else var_exist var e
				) ds || (if List.exists (fun (bind, _, _, _, _) -> var_in_bind var bind) ds then false else var_exist var e2)


    | EMatch (e, bs) ->
		  var_exist var e || (List.exists (fun (p,e) -> if var_in_pat var p then false else var_exist var e) bs)
    | IF (e1, e2, e3) -> var_exist var e1 || var_exist var e2 || var_exist var e3
		| EVar x -> x=var
    | _ -> false

  let rec is_id : lexp -> bool
  = fun (l, e) ->
	  match e with
		| EFun (arg, e) -> arg_equality (arg, e)
		| _ -> false

	let rec is_const : lexp -> bool
  = fun (l, e) ->
	  match e with
		| EFun (arg, e) ->
		  let vars = extract_variables arg in
		  List.for_all (fun var -> not(var_exist var e)) vars
		| _ -> false

  let rec simplify_exp : lexp -> lexp
  = fun (l, exp) ->
    let exp = 
      begin match exp with
      | EList es -> EList (List.map simplify_exp es)
      | ETuple es -> ETuple (List.map simplify_exp es)
      | ECtor (x, es) -> ECtor (x, List.map simplify_exp es)
      (* aop *)
      | MINUS e ->
        let e = simplify_exp e in
        begin match snd e with
        | Const n -> Const (-n)
        | MINUS e2 -> snd e2
        | SInt n -> SInt n 
        | _ -> MINUS e
        end
      | ADD (e1, e2) ->
        let (e1, e2) = (simplify_exp e1, simplify_exp e2) in
        begin match (snd e1, snd e2) with
        | Const n1, Const n2 -> Const (n1 + n2)
        | Const 0, e | e, Const 0 -> e
				| x1, MINUS x2 | MINUS x2, x1 -> if(x1 = snd x2) then Const 0 else ADD (e1,e2)
        | SInt n1, SInt n2 -> SInt n1
        | _ -> ADD (e1, e2)
        end
      | SUB (e1, e2) ->
        let (e1, e2) = (simplify_exp e1, simplify_exp e2) in
        begin match (snd e1, snd e2) with
        | Const n1, Const n2 -> Const (n1 - n2)
        | Const 0, e -> MINUS e2
        | e, Const 0 -> e
        | SInt n1, SInt n2 -> SInt n1
        | _ -> if (snd e1 = snd e2) then Const 0 else SUB (e1, e2)
        end
      | MUL (e1, e2) ->
        let (e1, e2) = (simplify_exp e1, simplify_exp e2) in
        begin match (snd e1, snd e2) with
        | Const n1, Const n2 -> Const (n1 * n2)
        | Const 0, _ | _, Const 0 -> Const 0
        | Const 1, e -> e
        | e, Const 1 -> e
        | SInt n1, SInt n2 -> SInt n1
        | _ -> MUL (e1, e2)
        end
      | DIV (e1, e2) ->
        let (e1, e2) = (simplify_exp e1, simplify_exp e2) in
        begin match (snd e1, snd e2) with
        | _, Const 0 -> raise (Failure "Division_by_zero.")
        | Const n1, Const n2 -> Const (n1 / n2)
        | Const 0, _ -> Const 0
        | e, Const 1 -> e
        | SInt n1, SInt n2 -> SInt n1
        | _ -> if (snd e1 = snd e2) then Const 1 else DIV (e1, e2)
        end
      | MOD (e1, e2) ->
        let (e1, e2) = (simplify_exp e1, simplify_exp e2) in
        begin match (snd e1, snd e2) with
        | _, Const 0 -> raise (Failure "Division_by_zero.")
        | Const n1, Const n2 -> Const (n1 mod n2)
        | Const 0, _ -> Const 0
        | _, Const 1 -> Const 0
        | SInt n1, SInt n2 -> SInt n1
        | _ -> if (snd e1 = snd e2) then Const 0 else MOD (e1, e2)
        end
      (* bop *)
      | NOT e ->
        let e = simplify_exp e in
        begin match snd e with
        | TRUE -> FALSE
        | FALSE -> TRUE
				| NOT x -> snd x
        | _ -> NOT e
        end
      | OR (e1, e2) ->
        let (e1, e2) = (simplify_exp e1, simplify_exp e2) in
        begin match (snd e1, snd e2) with
        | TRUE, _ | _, TRUE -> TRUE
        | FALSE, e | e, FALSE -> e
				| x1, NOT x2 | NOT x2, x1 -> if (snd x2 = x1) then TRUE else OR (e1,e2)
        | x1, x2 -> if x1=x2 then x1 else OR (e1, e2)
        end
      | AND (e1, e2) ->
        let (e1, e2) = (simplify_exp e1, simplify_exp e2) in
        begin match (snd e1, snd e2) with
        | FALSE, _ | _, FALSE -> FALSE
        | TRUE, e | e, TRUE -> e
				| x1, NOT x2 | NOT x2, x1 -> if (snd x2 = x1) then FALSE else AND (e1,e2)
        | x1, x2 -> if x1=x2 then x1 else AND (e1, e2)
        end
      (* abop *)
      | LESS (e1, e2) ->
        let (e1, e2) = (simplify_exp e1, simplify_exp e2) in
        begin match (snd e1, snd e2) with
        | Const n1, Const n2 -> if (n1 < n2) then TRUE else FALSE
        | x1, x2 -> if (x1 = x2) then FALSE else LESS (e1, e2)
        end
      | LESSEQ (e1, e2) ->
        let (e1, e2) = (simplify_exp e1, simplify_exp e2) in
        begin match (snd e1, snd e2) with
        | Const n1, Const n2 -> if (n1 <= n2) then TRUE else FALSE
        | x1, x2 -> if (x1 = x2) then TRUE else LESSEQ (e1, e2)
        end
      | LARGER (e1, e2) -> snd (simplify_exp (l, LESS (e2, e1)))
      | LARGEREQ (e1, e2) -> snd (simplify_exp (l, (LESSEQ (e2, e1))))
      (* equality *)
      | EQUAL (e1, e2) ->
        let (e1, e2) = (simplify_exp e1, simplify_exp e2) in
				begin match (snd e1, snd e2) with
				| TRUE, x | x, TRUE -> x
				| FALSE, _ -> NOT e2
	      | _, FALSE -> NOT e1
				| x1, x2 -> if (x1 = x2) then TRUE else EQUAL (e1, e2)
	      end
      | NOTEQ (e1, e2) ->
        let (e1, e2) = (simplify_exp e1, simplify_exp e2) in
				begin match (snd e1, snd e2) with
				| FALSE, x | x, FALSE -> x
				| TRUE, _ -> NOT e2
				| _, TRUE -> NOT e1
				| x1, x2 -> if (x1 = x2) then FALSE else NOTEQ (e1, e2)
	      end
      (* lop *)
      | AT (e1, e2) -> 
        let (e1, e2) = (simplify_exp e1, simplify_exp e2) in
        begin match (snd e1, snd e2) with
        | EList es1, EList es2 -> EList (es1 @ es2)
        | _ -> AT (e1, e2)
        end
      | DOUBLECOLON (e1, e2) ->
        let (e1, e2) = (simplify_exp e1, simplify_exp e2) in
        begin match snd e2 with
        | EList es -> EList (e1 :: es)
        | _ -> DOUBLECOLON (e1, e2)
        end
      | STRCON (e1, e2) ->
        let (e1, e2) = (simplify_exp e1, simplify_exp e2) in
        begin match (snd e1, snd e2) with
        | String s1, String s2 -> String (s1 ^ s2)
        | SStr n, _ | _, SStr n -> SStr n
        | _ -> STRCON (e1, e2)
        end
      (* else *)
      | EApp (e1, e2) -> 
        begin match snd e1 with
				| EFun (arg, e) -> 
	        if (exp_is_closed e) then
					  if (is_id e1) then snd (simplify_exp e2)
	          else if (is_const e1) then snd (simplify_exp e)
	          else EApp (simplify_exp e1, simplify_exp e2)
	        else EApp (simplify_exp e1, simplify_exp e2)
				| _ -> EApp (simplify_exp e1, simplify_exp e2)
				end
      | EFun (arg, e) -> EFun (arg, simplify_exp e)
      | ELet (f, is_rec, args, typ , e1, e2) -> ELet (f, is_rec, args, typ, simplify_exp e1, simplify_exp e2)
      | EBlock (is_rec, bindings, e2) ->
        let bindings = List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, args, typ, simplify_exp e)) bindings in
        EBlock (is_rec, bindings, simplify_exp e2)
      | EMatch (e, bs) ->
        let e = simplify_exp e in
        let (ps, es) = List.split bs in
        EMatch (e, List.combine ps (List.map simplify_exp es))
      | IF (e1, e2, e3) ->
        let e1 = simplify_exp e1 in
        begin match snd e1 with
        | TRUE -> snd (simplify_exp e2)
        | FALSE -> snd (simplify_exp e3)
        | _ -> IF (e1, simplify_exp e2, simplify_exp e3)
        end
      | Raise e -> Raise (simplify_exp e)
      | _ -> exp
    end in
    (l, exp)

  let rec simplify_decl : decl -> decl
  = fun decl ->
    match decl with
    | DLet (f, is_rec, args, typ, e) -> DLet (f, is_rec, args, typ, simplify_exp e)
    | DBlock (is_rec, bindings) ->
      let bindings = List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, args, typ, simplify_exp e)) bindings in
      DBlock (is_rec, bindings)
    | _ -> decl

  let simplify_pgm : prog -> prog
  = fun pgm -> List.map simplify_decl pgm

end

(**********************)
(* Program Reordering *)
(**********************)

let rec arg_num : lexp -> int
= fun (_, e) ->
  match e with
  | ADD (e1,e2) | MUL (e1,e2) | OR (e1,e2) | AND (e1,e2) | EQUAL (e1,e2) | NOTEQ (e1,e2) 
  | SUB (e1,e2) | DIV (e1,e2) | LESS (e1,e2) | LARGER (e1,e2) | LESSEQ (e1,e2) 
  | LARGEREQ (e1,e2) | EApp (e1,e2) | MOD (e1,e2) | AT (e1,e2) 
  | DOUBLECOLON (e1,e2) | ELet (_,_,_,_,e1,e2) -> (arg_num e1) + (arg_num e2)
  | MINUS e | NOT e | EFun (_,e) -> (arg_num e)
  | IF (e1,e2,e3) -> (arg_num e1) + (arg_num e2) + (arg_num e3)
  | ECtor (_,exps) | EList (exps) | ETuple (exps) -> list_fold (fun e r -> (arg_num e) + r) exps 0
  | EMatch (exp,branches) -> (arg_num exp)+ (list_fold (fun (_,e) r -> (arg_num e) + r ) branches 0)
  |_ -> 1

let compare_exp : lexp -> lexp -> bool
= fun (l1, e1) (l2, e2) ->
  match (e1,e2) with
  | (EVar x1,EVar x2) -> x1 >= x2
  | (Const x1,Const x2) -> x1 >= x2
  | (EVar _,_) -> true
  | (_,EVar _) -> false
  | (Hole _,_) -> true
  | (_,Hole _) -> false
  | (SInt _,_) -> true
  | (_,SInt _) -> false
  | _ -> ((arg_num (l1, e1)) >= (arg_num (l2, e2)))

let rec reorder_exp : lexp -> lexp
= fun (l, exp) ->
  let exp = 
  begin match exp with  
    | ADD (e1,e2) -> if compare_exp e1 e2 then ADD (reorder_exp e1,reorder_exp e2) else ADD (reorder_exp e2,reorder_exp e1)
    | MUL (e1,e2) -> if compare_exp e1 e2 then MUL (reorder_exp e1,reorder_exp e2) else MUL (reorder_exp e2,reorder_exp e1)
    | OR (e1,e2) -> if compare_exp e1 e2 then OR (reorder_exp e1,reorder_exp e2) else OR (reorder_exp e2,reorder_exp e1)
    | AND (e1,e2) -> if compare_exp e1 e2 then AND (reorder_exp e1,reorder_exp e2) else AND (reorder_exp e2,reorder_exp e1)
    | EQUAL (e1,e2) -> if compare_exp e1 e2 then EQUAL (reorder_exp e1,reorder_exp e2) else EQUAL (reorder_exp e2,reorder_exp e1)
    | NOTEQ (e1,e2) -> if compare_exp e1 e2 then NOTEQ (reorder_exp e1,reorder_exp e2) else NOTEQ (reorder_exp e2,reorder_exp e1)
    | SUB (e1,e2) -> SUB (reorder_exp e1,reorder_exp e2)
    | DIV (e1,e2) -> DIV (reorder_exp e1,reorder_exp e2)
    | MOD (e1,e2) -> MOD (reorder_exp e1,reorder_exp e2)
    | MINUS e -> MINUS (reorder_exp e)
    | NOT e -> NOT (reorder_exp e)
    | LESS (e1,e2) -> LESS (reorder_exp e1,reorder_exp e2)   
    | LARGER (e1,e2) -> snd (reorder_exp (l, LESS (e2,e1)))
    | LESSEQ (e1,e2) -> LESSEQ (reorder_exp e1, reorder_exp e2) 
    | LARGEREQ (e1,e2) -> snd (reorder_exp (l, LESSEQ (e2,e1)))
    | EApp (e1,e2) -> EApp (reorder_exp e1,reorder_exp e2)
    | EFun (arg,e) -> EFun (arg, reorder_exp e)
    | ELet (x,is_rec,args,t,e1,e2) -> ELet (x,is_rec,args,t,reorder_exp e1,reorder_exp e2)
    | ECtor (id,exps) -> ECtor(id,(list_map reorder_exp exps))
    | EList (exps) -> EList (list_map reorder_exp exps)
    | ETuple (exps) -> ETuple (list_map reorder_exp exps)
    | EMatch (exp,branches) ->
      let (pat_list,exp_list) = list_split branches in
      let exp_list = list_map reorder_exp exp_list in
      let branches = list_combine pat_list exp_list in
      EMatch (reorder_exp exp,branches)
    | IF (e1,e2,e3) -> IF (reorder_exp e1,reorder_exp e2,reorder_exp e3)
    | AT (e1,e2) -> AT (reorder_exp e1,reorder_exp e2)
    | DOUBLECOLON (e1,e2) -> DOUBLECOLON (reorder_exp e1,reorder_exp e2)
    |_ -> exp
  end in
  (l, exp)

let reorder_decl : decl -> decl
= fun decl ->
  match decl with
  | DLet (x,is_rec,args,typ,exp) -> 
    let exp = reorder_exp exp in
    DLet (x,is_rec,args,typ,exp)
  | _ -> decl

let reorder_pgm : prog -> prog 
= fun pgm -> list_map reorder_decl pgm

let normalize_exp : lexp -> lexp
= fun e ->
  let constant_func = (fun x -> Simplification.simplify_exp x) in
  let normalize_func = (fun x -> reorder_exp x) in
  let e = fix constant_func e in
  fix normalize_func e

let normalize : prog -> prog
= fun pgm ->
  let constant_func = (fun x -> Simplification.simplify_pgm x) in
  let normalize_func = (fun x ->reorder_pgm x) in
  let pgm = fix constant_func pgm in
  fix normalize_func pgm
