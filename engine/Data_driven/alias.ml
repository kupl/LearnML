open Lang
open Util
open Print
open CallGraph

(* Data flow analysis of each function *)
type t = (label, analysis) BatMap.t
and analysis = (id, data) BatMap.t
and data = 
	(* Func *)
	| Func of typ
	(* Parameter *)
	| Param of typ (* parameter with type *)
	(* Decompose *)
	| CtorElem of (id * data) (* elem of ctor *)
	| TupleElem of (int * data) (* n-th tuple element *)
	| ListElem of (int * data) (* n-th list element *)
	| Head of data (* head of list *)
	| Tail of data (* tail of list *)
	(* Use other variable *)
	| Ctor of (id * data list)
	| Tuple of data list
	| List of data list
	| Unit
	| Int of int
	| Bool of bool
	| Str of string
	| Nil
	| Unknown (* Something complex *)

type alias_info = (label, alias_set) BatMap.t
and alias_set = (id * id) BatSet.t

(* PP *)
let rec string_of_data : data -> string
= fun data ->
	match data with
	| Func typ -> "Func (" ^ type_to_string typ ^ ")"
	| Param typ -> "Param (" ^ type_to_string typ ^ ")"
	(* Decompose *)
	| CtorElem (x, data) -> x ^ " of " ^ string_of_data data
	| TupleElem (idx, data) -> string_of_int idx ^ "-th (" ^ string_of_data data ^ ")"
	| ListElem (idx, data) -> string_of_int idx ^ "-th (" ^ string_of_data data ^ ")"
	| Head data -> "Head (" ^ string_of_data data ^ ")"
	| Tail data -> "Tail (" ^ string_of_data data ^ ")"
	(* Use other variable *)
	| Ctor (x, datas) -> x ^ " " ^ pp_tuple string_of_data datas
	| Tuple datas -> pp_tuple string_of_data datas
	| List datas -> pp_list string_of_data datas
	| Unit -> "Unit"
	| Int n -> string_of_int n
	| Bool b -> if b then "true" else "false"
	| Str str -> str
	| Nil -> "[]"
	| Unknown -> "T"

let print_map : analysis -> unit
= fun map -> BatMap.iter (fun x data -> print_endline (x ^ " |-> " ^ string_of_data data)) map

let print : t -> unit 
= fun t -> BatMap.iter (fun l map ->
		print_endline ("Label - " ^ string_of_int l);
		print_map map
	) t

(* Compare two data-flow *)
let rec check_euqal_data : data -> data -> bool
= fun d1 d2 ->
	match (d1, d2) with
	| Func t1, Func t2 | Param t1, Param t2 -> Type.check_typs t1 t2
	| CtorElem (c1, d1), CtorElem (c2, d2) -> (c1 = c2) && (check_euqal_data d1 d2)
	| TupleElem (i1, d1), TupleElem (i2, d2) | ListElem (i1, d1), ListElem (i2, d2) -> (i1 = i2) && (check_euqal_data d1 d2)
	| Head d1, Head d2 | Tail d1, Tail d2 -> check_euqal_data d1 d2
	| Ctor (c1, ds1), Ctor (c2, ds2) -> (c1 = c2) && (try List.for_all2 (fun d1 d2 -> check_euqal_data d1 d2) ds1 ds2 with _ -> false)
	| Tuple ds1, Tuple ds2 | List ds1, List ds2 -> (try List.for_all2 (fun d1 d2 -> check_euqal_data d1 d2) ds1 ds2 with _ -> false)
	| Unit, Unit | Nil, Nil -> true
	| Int n1, Int n2 -> n1 = n2
	| Bool b1, Bool b2 -> b1 = b2
	| Str s1, Str s2 -> s1 = s2 
	| Unknown, _ | _, Unknown -> false (* ?? => hole*)
	| _ -> false

(* Analysis data of given expression *)
let rec find_map : id -> analysis -> data
= fun x map -> (try BatMap.find x map with _ -> Unknown)

let rec gen_data : analysis -> lexp -> data
= fun map (l, exp) ->
	match exp with
	| EUnit -> Unit
	| Const n -> Int n
	| TRUE -> Bool true
	| FALSE -> Bool false 
	| String s -> Str s 
	| EVar x -> find_map x map
	| EFun (_, e) | ERef e | EDref e | Raise e -> Unknown
	| MINUS e ->
		begin match gen_data map e with
		| Int n -> Int (-n)
		| _ -> Unknown
		end
	| NOT e -> 
		begin match gen_data map e with
		| Bool b -> Bool (not b)
		| _ -> Unknown
		end
	| ADD (e1, e2) ->
		begin match (gen_data map e1, gen_data map e2) with
		| Int n1, Int n2 -> Int (n1 + n2)
		| _ -> Unknown
		end
	| SUB (e1, e2) ->
		begin match (gen_data map e1, gen_data map e2) with
		| Int n1, Int n2 -> Int (n1 - n2)
		| _ -> Unknown
		end
 	| MUL (e1, e2) ->
 		begin match (gen_data map e1, gen_data map e2) with
		| Int n1, Int n2 -> Int (n1 * n2)
		| _ -> Unknown
		end
	| DIV (e1, e2) ->
		begin match (gen_data map e1, gen_data map e2) with
		| Int n1, Int n2 -> Int (n1 / n2)
		| _ -> Unknown
		end
	| MOD (e1, e2) ->
		begin match (gen_data map e1, gen_data map e2) with
		| Int n1, Int n2 -> Int (n1 mod n2)
		| _ -> Unknown
		end
	| OR (e1, e2) ->
		begin match (gen_data map e1, gen_data map e2) with
		| Bool b1, Bool b2 -> Bool (b1 || b2)
		| _ -> Unknown
		end
	| AND (e1, e2) ->
		begin match (gen_data map e1, gen_data map e2) with
		| Bool b1, Bool b2 -> Bool (b1 && b2)
		| _ -> Unknown
		end
	| LESS (e1, e2) ->
		begin match (gen_data map e1, gen_data map e2) with
		| Int n1, Int n2 -> Bool (n1 < n2)
		| _ -> Unknown
		end
	| LESSEQ (e1, e2) ->
		begin match (gen_data map e1, gen_data map e2) with
		| Int n1, Int n2 -> Bool (n1 <= n2)
		| _ -> Unknown
		end
	| LARGER (e1, e2) ->
		begin match (gen_data map e1, gen_data map e2) with
		| Int n1, Int n2 -> Bool (n1 > n2)
		| _ -> Unknown
		end
	| LARGEREQ (e1, e2) ->
		begin match (gen_data map e1, gen_data map e2) with
		| Int n1, Int n2 -> Bool (n1 >= n2)
		| _ -> Unknown
		end
	| EQUAL (e1, e2) | NOTEQ (e1, e2) -> Unknown
	| DOUBLECOLON (e1, e2) | AT (e1, e2) | STRCON (e1, e2) -> Unknown
	| EApp (e1, e2) -> Unknown
	| EAssign (e1, e2) -> Unknown
	| EList es -> List (List.map (fun e -> gen_data map e) es)
	| ETuple es -> Tuple (List.map (fun e -> gen_data map e) es)
	| ECtor (c, es) -> Ctor (c, (List.map (fun e -> gen_data map e) es))
	| IF (e1, e2, e3) -> Unknown
	| EMatch (e, bs) -> Unknown
	| ELet (f, is_rec, args, typ, e1, e2) -> Unknown
	| EBlock (is_rec, ds, e) -> Unknown
	| _ -> raise (Failure ("Data-flow analysis : invalid exp (" ^ exp_to_string (l, exp)))

let rec update_arg : arg -> analysis -> analysis
= fun arg map ->
	match arg with
	| ArgUnder typ -> map
	| ArgOne (x, typ) -> BatMap.add x (Param typ) map
	| ArgTuple args -> update_args args map

and update_args : arg list -> analysis -> analysis
= fun args map -> 
	match args with
	| [] -> map 
	| hd::tl -> update_arg hd (update_args tl map)

let rec update_pat : analysis -> pat -> data -> analysis 
= fun map p d ->
	match p with
	| PVar x -> BatMap.add x d map
	| PList ps ->	
		let (map, idx) = List.fold_left (fun (map, idx) p -> 
			let idx = idx + 1 in
			(update_pat map p (ListElem (idx, d)), idx)
		) (map, 0) ps in
		map
	| PCons (phd, ptl) -> 
		let map = update_pat map phd (Head d) in
		update_pat map ptl (Tail d)
	| PTuple ps -> 
		let (map, idx) = List.fold_left (fun (map, idx) p -> 
			let idx = idx + 1 in
			(update_pat map p (TupleElem (idx, d)), idx)
		) (map, 0) ps in 
		map
	| PCtor (x, ps) -> 
		let (map, idx) = List.fold_left (fun (map, idx) p -> 
			let idx = idx + 1 in
			(update_pat map p (CtorElem (x, d)), idx)
		) (map, 0) ps in
		map
	| PUnit | PUnder | PInt _ | PBool _ -> map
	| Pats ps -> List.fold_left (fun map p -> update_pat map p d) map ps

let rec update_binding : analysis -> let_bind -> data -> analysis 
= fun map binding d ->
	match binding with
	| BindUnder -> map
	| BindOne x -> BatMap.add x d map
	| BindTuple bs ->
		begin match d with
		| Tuple ds -> List.fold_left2 (fun map b d -> update_binding map b d) map bs ds
		| _ -> 
			let (map, idx) = List.fold_left (fun (map, idx) b -> 
				let idx = idx + 1 in
				(update_binding map b (TupleElem (idx, d)), idx)
			) (map, 0) bs in 
			map
		end

let rec analysis_exp : analysis -> lexp -> t
= fun map (l, exp) ->	
	match exp with
	| EUnit | Const _ | TRUE | FALSE | String _ | EVar _ -> BatMap.singleton l map 
	(*
	| EFun (arg, e) -> 
		let t = analysis_fun map [arg] e in
		BatMap.add l map t
	*)
	| ERef e | EDref e | Raise e | MINUS e | NOT e | EFun (_, e) -> 
		let t = analysis_exp map e in
		BatMap.add l map t 
	| ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2) 
	| OR (e1, e2) | AND (e1, e2) | LESS (e1, e2) | LESSEQ (e1, e2) | LARGER (e1, e2) | LARGEREQ (e1, e2) 
	| EQUAL (e1, e2) | NOTEQ (e1, e2) | DOUBLECOLON (e1, e2) | AT (e1, e2) | STRCON (e1, e2) | EApp (e1, e2) 
	| EAssign (e1, e2) -> 
		let (t1, t2) = (analysis_exp map e1, analysis_exp map e2) in
		let t = BatMap.union t1 t2 in
		BatMap.add l map t 
	| EList es | ETuple es | ECtor (_, es) -> 
		let t = List.fold_left (fun t e -> BatMap.union t (analysis_exp map e)) BatMap.empty es in
		BatMap.add l map t
	| IF (e1, e2, e3) -> 
		let (t1, t2, t3) = (analysis_exp map e1, analysis_exp map e2, analysis_exp map e3) in
		let t = BatMap.union t1 (BatMap.union t2 t3) in
		BatMap.add l map t 
	| EMatch (e, bs) -> 
		let t = analysis_exp map e in
		let data = gen_data map e in
		let t = List.fold_left (fun t (pi, ei) -> 
			let map = update_pat map pi data in
			BatMap.union t (analysis_exp map ei)
		) t bs in
		BatMap.add l map t
	(* There are no function definitions in binding expression *)
	| ELet (f, is_rec, args, typ, e1, e2) ->
		let t1 = analysis_exp (update_args args map) e1 in 
		let t2 = analysis_exp (update_binding map f (gen_data map e1)) e2 in
		BatMap.add l map (BatMap.union t1 t2)
	| EBlock (is_rec, ds, e2) -> 
		let (t, map) = List.fold_left (fun (t, map) (f, is_rec, args, typ, e) -> 
			let t' = analysis_exp (update_args args map) e in
			(BatMap.union t t', update_binding map f (gen_data map e))
		) (BatMap.empty, map) ds in
		let t = BatMap.union t (analysis_exp map e2) in
		BatMap.add l map t
	| _ -> raise (Failure ("Data-flow analysis : invalid exp (" ^ exp_to_string (l, exp)))

and analysis_fun : analysis -> arg list -> lexp -> t
= fun map args (l, exp) ->
	match exp with
	| EFun (arg, e) -> analysis_fun map (args@[arg]) e
	| _ -> analysis_exp (update_args args map) (l, exp)

let rec analysis_node : node -> t
= fun node -> 
	let init_condition = if node.is_rec then (BatMap.singleton node.name (Func node.typ)) else BatMap.empty in
	analysis_exp (update_args node.args init_condition) node.body

(*
let rec analysis_decl : decl -> t
= fun decl ->
	match decls with
	| DLet (f, is_rec, args, typ, e) ->
		
		DLet (f, is_rec, annotate_args subst args, Type2.Subst.apply typ subst, annotate_exp subst e)
	| DBlock (is_rec, bindings) -> DBlock (is_rec, List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, annotate_args subst args, Type2.Subst.apply typ subst, annotate_exp subst e)) bindings)
	| _ -> decl

let rec analysis_pgm : prog -> t
= fun pgm ->
*)

let rec compute_alias_set : analysis -> analysis -> alias_set
= fun map1 map2 ->
	BatMap.foldi (fun x d1 acc ->
		let ys = keys (BatMap.filterv (fun d2 -> check_euqal_data d1 d2) map2) in
		BatSet.union acc (BatSet.map (fun y -> (x, y)) ys)
	) map1 BatSet.empty
	
let rec compute_alias_info : analysis -> t -> alias_info 
= fun map t -> BatMap.map (fun map' -> compute_alias_set map' map) t