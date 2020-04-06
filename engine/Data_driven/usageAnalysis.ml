open Lang
open Util

(********************************************************)
(* Compute usage of all variables at each program point *)
(********************************************************)

type t = (label, usage_map) BatMap.t
and usage_map = (id, usage) BatMap.t
and usage = 
	(* Parameter *)
	| Param of typ (* type of parameter *)
	(* Use other variable *)
	| Ctor of (id * usage list)
	| Tuple of usage list
	| List of usage list
	(* Decompose *)
	| CtorElem of (id * usage) (* elem of ctor *)
	| TupleElem of (int * usage) (* n-th tuple element *)
	| ListElem of (int * usage) (* n-th list element *)
	| Head of usage (* head of list *)
	| Tail of usage (* tail of list *)
	(* Constant?? *)
	| Unit
	| Int of int
	| Bool of bool
	| Str of string
	| Nil
	| Top

exception InvalidUse

let rec string_of_usage : usage -> string
= fun usage ->
	match usage with
	| Param typ -> "Param (" ^ Print.type_to_string typ ^ ")"
	(* Use other variable *)
	| Ctor (x, usages) -> x ^ " " ^ Print.pp_tuple string_of_usage usages
	| Tuple usages -> Print.pp_tuple string_of_usage usages
	| List usages -> Print.pp_list string_of_usage usages
	(* Decompose *)
	| CtorElem (x, usage) -> x ^ " of " ^ string_of_usage usage
	| TupleElem (idx, usage) -> string_of_int idx ^ "-th (" ^ string_of_usage usage ^ ")"
	| ListElem (idx, usage) -> string_of_int idx ^ "-th (" ^ string_of_usage usage ^ ")"
	| Head usage -> "Head (" ^ string_of_usage usage ^ ")"
	| Tail usage -> "Tail (" ^ string_of_usage usage ^ ")"

let print_map : usage_map -> unit
= fun map -> BatMap.iter (fun x usage -> print_endline (x ^ " |-> " ^ string_of_usage usage)) map

let print : t -> unit 
= fun t -> BatMap.iter (fun l map ->
		print_endline ("Label - " ^ string_of_int l);
		print_map map
	) t

(* Check uses of two vars are the same *)
let rec check_same : usage -> usage -> bool
= fun u1 u2 ->
	match (u1, u2) with
	| Param typ1, Param typ2 -> Type.check_typs typ1 typ2
	| Ctor (x1, us1), Ctor (x2, us2) -> (x1 = x2) && (try List.for_all2 check_same us1 us2 with _ -> false)
	| Tuple us1, Tuple us2 | List us1, List us2 -> (try List.for_all2 check_same us1 us2 with _ -> false)
	| CtorElem (x1, u1), CtorElem (x2, u2) -> x1 = x2 && check_same u1 u2
	| TupleElem (i1, u1), TupleElem (i2, u2) | ListElem (i1, u1), ListElem (i2, u2) -> i1 = i2 && check_same u1 u2
	| ListElem (1, ul), Head uhd | Head uhd, ListElem (1, ul) -> check_same uhd ul
	| Head u1, Head u2 | Tail u1, Tail u2 -> check_same u1 u2
	| _ -> false

let rec check_exp : lexp -> bool
= fun (l, exp) ->
	match exp with
	| ETuple es | EList es | ECtor (_, es) -> if es <> [] then List.for_all check_exp es else false
	| EVar x -> true
	| _ -> false

let rec gen_usage : usage_map -> lexp -> usage
= fun map (l, exp) ->
	match exp with
	(*
	| MINUS e | NOT e | Raise e -> BatMap.add l map (analysis_exp map e)
	| ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2)
	| OR (e1, e2) | AND (e1, e2) | LESS (e1, e2) | LARGER (e1, e2) | EQUAL (e1, e2) | NOTEQ (e1, e2)
	| LESSEQ (e1, e2) | LARGEREQ (e1, e2) | AT (e1, e2) | DOUBLECOLON (e1, e2) | STRCON (e1, e2) 
	| EApp (e1, e2) -> BatMap.add l map (BatMap.union (analysis_exp map e1) (analysis_exp map e2))
	| ELet (f, is_rec, args, typ, e1, e2) ->
		let map' = 
		BatMap.add l map (BatMap.union (analysis_exp (update_args args map) e1) (analysis_exp map e2))
	| EBlock (is_rec, ds, e2) -> 
		let t = List.fold_left (fun acc (f, is_rec, args, typ, e) -> BatMap.union acc (analysis_exp (update_args args map) e)) BatMap.empty ds in
		let t = BatMap.union t (analysis_exp map e2) in
		BatMap.add l map t
	| EMatch (e, bs) ->
		let t = analysis_exp map e in
		let t = List.fold_left (fun acc (p, e) -> BatMap.union acc (analysis_exp map e)) t bs in
		BatMap.add l map t
	| IF (e1, e2, e3) -> 
		let t = BatMap.union (analysis_exp map e1) (BatMap.union (analysis_exp map e2) (analysis_exp map e3)) in
		BatMap.add l map t
	| EFun (arg, e) -> BatMap.add l map (analysis_exp (update_arg arg map) e)
	*)
	| EList es -> List (List.map (fun e -> gen_usage map e) es)
	| ETuple es -> Tuple (List.map (fun e -> gen_usage map e) es)
	| ECtor (x, es) -> Ctor (x, (List.map (fun e -> gen_usage map e) es))
	| EVar x -> (try BatMap.find x map with _ -> raise InvalidUse)
	| _ -> raise InvalidUse

(* Usage from pat *)
let rec check_pat : pat -> bool 
= fun pat ->
	match pat with
	| PVar x -> true 
  | PList ps | PCons ps | PTuple ps | PCtor (_, ps) -> List.for_all check_pat ps 
  (*| PUnit | PUnder | PInt _ | PBool _ | Pats _ -> false *)
  | _ -> false
 
let rec update_map : usage_map -> (pat * usage) -> usage_map
= fun map (pat, usage) ->
	match pat with
	| PVar x -> BatMap.add x usage map
  | PList ps ->	
  	let (map, idx) = List.fold_left (fun (map, idx) p -> 
  		let idx = idx + 1 in
  		(update_map map (p, ListElem (idx, usage)), idx)
		) (map, 0) ps in
		map
  | PCons (hd::tl) -> 
		if tl = [] then (update_map map (hd, usage))
		else 
			let map = update_map map (hd, Head (usage)) in
			update_map map (PCons tl, Tail (usage))
  | PTuple ps -> 
  	let (map, idx) = List.fold_left (fun (map, idx) p -> 
  		let idx = idx + 1 in
  		(update_map map (p, TupleElem (idx, usage)), idx)
		) (map, 0) ps in 
		map
  | PCtor (x, ps) -> 
  	let (map, idx) = List.fold_left (fun (map, idx) p -> 
  		let idx = idx + 1 in
  		(update_map map (p, CtorElem (x, usage)), idx)
		) (map, 0) ps in
		map
  (*| PUnit | PUnder | PInt _ | PBool _ | Pats _ -> raise (Failure "Usage analysis : Invalid pat")*)
  | _ -> raise InvalidUse

let rec update_arg : arg -> usage_map -> usage_map
= fun arg map ->
	match arg with
	| ArgUnder typ -> map
	| ArgOne (x, typ) -> BatMap.add x (Param typ) map
  | ArgTuple args -> update_args args map

and update_args : arg list -> usage_map -> usage_map
= fun args map -> List.fold_left (fun map arg -> update_arg arg map) map args

let rec analysis_exp : usage_map -> lexp -> t
= fun map (l, exp) ->
	match exp with
	| EList es | ECtor (_, es) | ETuple es -> 
		let t = List.fold_left (fun acc e -> BatMap.union acc (analysis_exp map e)) BatMap.empty es in
		BatMap.add l map t
	| MINUS e | NOT e | Raise e -> BatMap.add l map (analysis_exp map e)
	| ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2)
	| OR (e1, e2) | AND (e1, e2) | LESS (e1, e2) | LARGER (e1, e2) | EQUAL (e1, e2) | NOTEQ (e1, e2)
	| LESSEQ (e1, e2) | LARGEREQ (e1, e2) | AT (e1, e2) | DOUBLECOLON (e1, e2) | STRCON (e1, e2) 
	| EApp (e1, e2) -> BatMap.add l map (BatMap.union (analysis_exp map e1) (analysis_exp map e2))
	| EFun (arg, e) -> BatMap.add l map (analysis_exp (update_arg arg map) e)
	| ELet (f, is_rec, args, typ, e1, e2) ->
		BatMap.add l map (BatMap.union (analysis_exp (update_args args map) e1) (analysis_exp map e2))
	| EBlock (is_rec, ds, e2) -> 
		let t = List.fold_left (fun acc (f, is_rec, args, typ, e) -> BatMap.union acc (analysis_exp (update_args args map) e)) BatMap.empty ds in
		let t = BatMap.union t (analysis_exp map e2) in
		BatMap.add l map t
	| EMatch (e', bs) ->
		let t = analysis_exp map e' in
		let t = List.fold_left (fun acc (p, e) -> 
			try 
				let map = update_map map (p, gen_usage map e') in
				BatMap.union acc (analysis_exp map e)
			with InvalidUse -> BatMap.union acc (analysis_exp map e)
		) t bs in
		BatMap.add l map t
	| IF (e1, e2, e3) -> 
		let t = BatMap.union (analysis_exp map e1) (BatMap.union (analysis_exp map e2) (analysis_exp map e3)) in
		BatMap.add l map t
	| _ -> BatMap.singleton l map 

let rec analysis_decl : decl -> t
= fun decl ->
	match decl with 
	| DLet (f, is_rec, args, typ, e) -> analysis_exp (update_args args BatMap.empty) e
	| DBlock (is_rec, bindings) -> 
		List.fold_left (fun acc (f, is_rec, args, typ, e) -> BatMap.union acc (analysis_exp (update_args args BatMap.empty) e)) BatMap.empty bindings
	| _ -> BatMap.empty

let run : prog -> t
= fun pgm ->
	let pgm' = Normalizer.T.run pgm in (* why it needed?? *)
	List.fold_left (fun acc decl -> BatMap.union acc (analysis_decl decl)) BatMap.empty pgm'
