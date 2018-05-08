open Lang
open Util
open Printf

let start_time = ref 0.0
let iter = ref 0
let count = ref 0

let tvar_num = ref 0
let var_num = ref 0

(*
 ******************************************************
 	Code for Synthesizing a Test-case
 ******************************************************
*)
(* helper functions *)
let fresh_tvar () = (tvar_num := !tvar_num + 1; (TVar ("s" ^ string_of_int !tvar_num)))

let fresh_var () = (var_num := !var_num +1; "x_" ^ string_of_int !var_num)

let cost_input : input -> int
= fun input ->
	List.fold_left (fun cost e -> cost + exp_cost e) 0 input

(* Components *)
let all_components : unit -> components
= fun () ->
	(* 
		except ctor, var (variable comp),
		condition let binding, match, exception, hole 
	*)
	BatSet.empty 
	|> BatSet.add (gen_label (), Const 1)
	|> BatSet.add (gen_label (), TRUE)
	|> BatSet.add (gen_label (), FALSE)
	|> BatSet.add (gen_label (), EList [])
	|> BatSet.add (gen_label (), ETuple [])
	|> BatSet.add (gen_label (), String "")
	|> BatSet.add (gen_label (), String "x")
	|> BatSet.add (gen_label (), ADD (gen_labeled_hole (), gen_labeled_hole ()))
	|> BatSet.add (gen_label (), SUB (gen_labeled_hole (), gen_labeled_hole ()))
	|> BatSet.add (gen_label (), MUL (gen_labeled_hole (), gen_labeled_hole ()))
	|> BatSet.add (gen_label (), DIV (gen_labeled_hole (), gen_labeled_hole ()))
	|> BatSet.add (gen_label (), MOD (gen_labeled_hole (), gen_labeled_hole ()))
	|> BatSet.add (gen_label (), MINUS (gen_labeled_hole ()))
	|> BatSet.add (gen_label (), NOT (gen_labeled_hole ()))
	|> BatSet.add (gen_label (), OR (gen_labeled_hole (), gen_labeled_hole ()))
	|> BatSet.add (gen_label (), AND (gen_labeled_hole (), gen_labeled_hole ()))
	|> BatSet.add (gen_label (), LESS (gen_labeled_hole (), gen_labeled_hole ()))
	|> BatSet.add (gen_label (), EQUAL (gen_labeled_hole (), gen_labeled_hole ()))
	|> BatSet.add (gen_label (), NOTEQ (gen_labeled_hole (), gen_labeled_hole ()))
	|> BatSet.add (gen_label (), LESSEQ (gen_labeled_hole (), gen_labeled_hole ()))
	|> BatSet.add (gen_label (), AT (gen_labeled_hole (), gen_labeled_hole ()))
	|> BatSet.add (gen_label (), DOUBLECOLON (gen_labeled_hole (), gen_labeled_hole ()))
	|> BatSet.add (gen_label (), STRCON (gen_labeled_hole (), gen_labeled_hole ()))
	|> BatSet.add (gen_label (), EFun (ArgUnder (fresh_tvar ()), gen_labeled_hole ()))

let get_var_components : Type.TEnv.t -> components
= fun tenv ->
	BatMap.foldi (fun var t set ->
		match t with
		| TCtor (name, ts) ->
			begin match ts with
			| [] -> BatSet.add (gen_label (), ECtor (var, [])) set
			| hd::tl -> BatSet.add (gen_label (), ECtor (var, [gen_labeled_hole ()])) set
			end
		| _ -> BatSet.add (gen_label (), EVar var) set
	) tenv BatSet.empty

let rec fresh_arg : typ -> arg
= fun typ ->
	match typ with
	| TTuple ts -> ArgTuple (List.map (fun t -> fresh_arg t) ts)
	| _ -> ArgOne (fresh_var (), typ)

(* type *)
type state = lexp * Type.HoleType.t * Type.VariableType.t

module Workset = struct
  type work = input * Type.HoleType.t * Type.VariableType.t 

  module OrderedType = struct
    type t = work
    let compare (input1, _, _) (input2, _, _) =
    let (c1, c2) = (cost_input input1, cost_input input2) in
      if c1=c2 then 0 else
      if c1>c2 then 1
      else -1
  end

  module Heap = BatHeap.Make (OrderedType)

  (* type of workset : heap * (string set) *)
  type t = Heap.t * string BatSet.t
  let empty = (Heap.empty, BatSet.empty)
 
  let explored : input -> t -> bool
  = fun input (_, sset) -> BatSet.mem (Print.input_to_string input) sset

  let add : work -> t -> t
  = fun (input, h_t, v_t) (heap, sset) ->
		try
    	if explored (List.map Normalize.normalize_exp input) (heap, sset) then (heap, sset)
    	else
      	(Heap.add (input, h_t, v_t) heap, BatSet.add (Print.input_to_string (List.map Normalize.normalize_exp input)) sset)
		with
			|_ -> (heap, sset)
  
  let choose : t -> (work * t) option
  = fun (heap, sset) ->
    try
      let elem = Heap.find_min heap in
      Some (elem, (Heap.del_min heap, sset))
    with
      | _ -> None

  let workset_info : t -> string
  = fun (heap, sset) ->
    "To explore : " ^ (string_of_int (Heap.size heap)) ^
    " Explored : " ^ (string_of_int (BatSet.cardinal sset))
end

(* Search a hole that appears at first *)
let extract_holenum : lexp -> int
= fun (l, e) ->
	match e with
	| Hole n -> n
	| _ -> raise (Failure "error during obtain hole number")

let rec exp_holes : lexp -> lexp BatSet.t
= fun (l, e) ->
	match e with
  | EList es | ECtor (_, es) | ETuple es -> exp_holes_list es
  | MINUS e | NOT e | EFun (_, e) | Raise e -> exp_holes e
  | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2)
  | OR (e1, e2) | AND (e1, e2) | LESS (e1, e2) | LARGER (e1, e2) | EQUAL (e1, e2) | NOTEQ (e1, e2)
  | LESSEQ (e1, e2) | LARGEREQ (e1, e2) | AT (e1, e2) | DOUBLECOLON (e1, e2) | STRCON (e1, e2)
  | EApp (e1, e2) | ELet (_, _, _, _, e1, e2) -> exp_holes_list [e1; e2]
	| EBlock (_, ds, e2) -> 
		let es = List.map (fun (f, is_rec, args, typ, e) -> e) ds in
		exp_holes_list (es@[e2])
	| EMatch (e, bs) ->
		let es = e :: (List.map (fun (p, e) -> e) bs) in
		exp_holes_list es
	| IF (e1, e2, e3) -> exp_holes_list [e1; e2; e3]
  | Hole n -> BatSet.singleton (l, e)
  | _ -> BatSet.empty

and exp_holes_list : lexp list -> lexp BatSet.t
= fun es ->
	match es with
	| [] -> BatSet.empty 
	| hd::tl -> 
		let set = exp_holes hd in
		if (BatSet.is_empty set) then exp_holes_list tl else set

let find_exp_holes : input -> lexp BatSet.t
= fun input -> exp_holes_list input

let is_closed : input -> bool
= fun input -> BatSet.is_empty (find_exp_holes input)

(* Type Checking *)
let get_ctor_type : typ -> (typ * typ list)
= fun typ ->
	match typ with
	| TCtor (tname, ts) -> (tname, ts)
	| _ -> raise (Failure "Invalid Constructor Type")

let rec update_type : (typ * typ) -> typ -> typ
= fun (t1, t2) typ ->
	match typ with
	| TVar x -> if typ = t1 then t2 else typ
	| TList typ -> TList (update_type (t1, t2) typ)
	| TTuple ts -> TTuple (List.map (update_type (t1, t2)) ts)
	| TCtor (tbase, ts) -> TCtor (tbase, List.map (update_type (t1, t2)) ts)
	| TArr (typ1, typ2) -> TArr (update_type (t1, t2) typ1, update_type (t1, t2) typ2)
	| _ -> typ

let rec update_polymorphic : (typ * typ) -> (Type.HoleType.t * Type.TEnv.t) -> (Type.HoleType.t * Type.TEnv.t)
= fun (t1, t2) (h_t, tenv) ->
	let h_t = BatMap.map (fun typ -> update_type (t1, t2) typ) h_t in
	let tenv = BatMap.map (fun typ -> update_type (t1, t2) typ) tenv in
	(h_t, tenv)

let rec check_typ : typ -> typ -> bool
= fun t1 t2 ->
	match (t1, t2) with
	| TList t1, TList t2 -> check_typ t1 t2
	| TTuple ts1, TTuple ts2 -> check_typ_list ts1 ts2
	| TCtor (x1, ts1), TCtor (x2, ts2) -> if x1 <> x2 then false else check_typ_list ts1 ts2
	| TArr (t1, t2), TArr (t1', t2') -> (check_typ t1 t1') && (check_typ t2 t2')
	| TVar x, _ ->
    (*if x occurs in t recursively, ill typed*)
    begin match t2 with
    | TList t -> not (Type.extract_tvar x t)
    | TTuple ts | TCtor (_, ts) -> not (Type.extract_tvar2 x ts)
    | TArr (t1, t2) -> not ((Type.extract_tvar x t1) || (Type.extract_tvar x t2))
    | _ -> true
    end
	| _, TVar x -> check_typ t2 t1
	| _ -> t1 = t2

and check_typ_list : typ list -> typ list -> bool
= fun ts1 ts2 ->
	try 
		List.for_all2 (fun t1 t2 -> check_typ t1 t2) ts1 ts2
	with _ -> false

(* One-step Transition *)
let rec update_state : (int * typ) list -> Type.TEnv.t -> state -> state
= fun ts tenv (e, h_t, v_t) ->
	List.fold_left (fun (e, h_t, v_t) (hole, typ) ->
		let h_t = BatMap.add hole typ h_t in
		let v_t = BatMap.add hole tenv v_t in
		(e, h_t, v_t)
	) (e, h_t, v_t) ts

let type_directed : (int * typ * Type.TEnv.t) -> state -> state option
= fun (hole, typ, tenv) (lexp, h_t, v_t) ->
	(* Transition from hole to exp *)
	match snd lexp with
	(* Const *)
	| Const _ ->
		begin match typ with
		| TInt -> Some (lexp, h_t, v_t) 
		| TVar _ ->	
			let (h_t, tenv) = update_polymorphic (typ, TInt) (h_t, tenv) in
			Some (lexp, h_t, v_t)
		| _ -> None
		end
	| TRUE | FALSE ->
		begin match typ with
		| TBool -> Some (lexp, h_t, v_t)
		| TVar _ ->
			let (h_t, tenv) = update_polymorphic (typ, TBool) (h_t, tenv) in
			Some (lexp, h_t, v_t)
		| _ -> None
		end
	| String _ ->
		begin match typ with
		| TString -> Some (lexp, h_t, v_t)
		| TVar _ ->
			let (h_t, tenv) = update_polymorphic (typ, TString) (h_t, tenv) in
			Some (lexp, h_t, v_t)
		| _ -> None
		end
	| EList [] ->
		begin match typ with
		| TList t -> Some (lexp, h_t, v_t)
		| TVar _ ->
			let (h_t, tenv) = update_polymorphic (typ, TList (fresh_tvar ())) (h_t, tenv) in
			Some (lexp, h_t, v_t)
		| _ -> None
		end
	| ETuple _ ->
		begin match typ with
		| TTuple ts ->
			let es = (List.map (fun _ -> gen_labeled_hole ()) ts) in
			let ts = List.map2 (fun e t -> (extract_holenum e, t)) es ts in
			Some (update_state ts tenv ((fst lexp, ETuple es), h_t, v_t))
		| _ -> None
		end
	(* aop *)
	| MINUS e1 -> 
		let n1 = extract_holenum e1 in
		begin match typ with
		| TInt -> Some (update_state [n1, TInt] tenv (lexp, h_t, v_t))
		| TVar _ ->
			let (h_t, tenv) = update_polymorphic (typ, TInt) (h_t, tenv) in
			Some (update_state [n1, TInt] tenv (lexp, h_t, v_t))
		| _ -> None
		end
	| ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2) -> 
		let (n1, n2) = (extract_holenum e1, extract_holenum e2) in 
		begin match typ with
		| TInt -> Some (update_state [(n1, TInt); (n2, TInt)] tenv (lexp, h_t, v_t))
		| TVar _ ->
			let (h_t, tenv) = update_polymorphic (typ, TInt) (h_t, tenv) in
			Some (update_state [(n1, TInt); (n2, TInt)] tenv (lexp, h_t, v_t))
		| _ -> None
		end
	(* bop *)
	| NOT e1 ->
		let n1 = extract_holenum e1 in
		begin match typ with
		| TBool -> Some (update_state [n1, TBool] tenv (lexp, h_t, v_t))
		| TVar _ ->
			let (h_t, tenv) = update_polymorphic (typ, TBool) (h_t, tenv) in
			Some (update_state [n1, TBool] tenv (lexp, h_t, v_t))
		| _ -> None
		end
	| OR (e1, e2) | AND (e1, e2) ->
		let (n1, n2) = (extract_holenum e1, extract_holenum e2) in 
		begin match typ with
		| TBool -> Some (update_state [(n1, TBool); (n2, TBool)] tenv (lexp, h_t, v_t))
		| TVar _ ->
			let (h_t, tenv) = update_polymorphic (typ, TBool) (h_t, tenv) in
			Some (update_state [(n1, TBool); (n2, TBool)] tenv (lexp, h_t, v_t))
		| _ -> None
		end
	| LESS (e1, e2) | LESSEQ (e1, e2) ->
		let (n1, n2) = (extract_holenum e1, extract_holenum e2) in 
		begin match typ with
		| TBool -> Some (update_state [(n1, TInt); (n2, TInt)] tenv (lexp, h_t, v_t))
		| TVar _ ->
			let (h_t, tenv) = update_polymorphic (typ, TBool) (h_t, tenv) in
			Some (update_state [(n1, TInt); (n2, TInt)] tenv (lexp, h_t, v_t))
		| _ -> None
		end
	| EQUAL (e1, e2) | NOTEQ (e1, e2) -> 
		let (n1, n2) = (extract_holenum e1, extract_holenum e2) in 
		let tv = fresh_tvar () in
		begin match typ with
		| TBool -> Some (update_state [(n1, tv); (n2, tv)] tenv (lexp, h_t, v_t))
		| TVar _ ->
			let (h_t, tenv) = update_polymorphic (typ, TBool) (h_t, tenv) in
			Some (update_state [(n1, tv); (n2, tv)] tenv (lexp, h_t, v_t))
		| _ -> None
		end
	(* list *)
	| AT (e1, e2) ->
		let (n1, n2) = (extract_holenum e1, extract_holenum e2) in 
		begin match typ with
		| TList t -> Some (update_state [(n1, TList t); (n2, TList t)] tenv (lexp, h_t, v_t))
		| TVar _ ->
			let tv = fresh_tvar () in
			let (h_t, tenv) = update_polymorphic (typ, TList tv) (h_t, tenv) in
			Some (update_state [(n1, TList tv); (n2, TList tv)] tenv (lexp, h_t, v_t))
		| _ -> None
		end
	| DOUBLECOLON (e1, e2) ->
		let (n1, n2) = (extract_holenum e1, extract_holenum e2) in 
		begin match typ with
		| TList t -> Some (update_state [(n1, t); (n2, TList t)] tenv (lexp, h_t, v_t))
		| TVar _ ->
			let tv = fresh_tvar () in
			let (h_t, tenv) = update_polymorphic (typ, TList tv) (h_t, tenv) in
			Some (update_state [(n1, tv); (n2, TList tv)] tenv (lexp, h_t, v_t))
		| _ -> None
		end
	(* string *)
	| STRCON (e1, e2) ->
		let (n1, n2) = (extract_holenum e1, extract_holenum e2) in 
		begin match typ with
		| TString -> Some (update_state [(n1, TString); (n2, TString)] tenv (lexp, h_t, v_t))
		| TVar _ ->
			let (h_t, tenv) = update_polymorphic (typ, TString) (h_t, tenv) in
			Some (update_state [(n1, TString); (n2, TString)] tenv (lexp, h_t, v_t))
		| _ -> None
		end
	(* else *)
	| EFun (_, e1) ->
		let n1 = extract_holenum e1 in
		begin match typ with
		| TArr (t1, t2) -> 
			let arg = fresh_arg t1 in
			let tenv = Type.bind_arg tenv arg in
			Some (update_state [(n1, t2)] tenv ((fst lexp, EFun (arg, e1)), h_t, v_t))
		| _ -> None
		end
	(* Var Comp *)
	| EVar x ->
		let var_typ = BatMap.find x tenv in
		if check_typ typ var_typ then
			begin match (typ, var_typ) with
			| TVar x, t | t, TVar x ->
				let (h_t, tenv) = update_polymorphic (TVar x, t) (h_t, tenv) in
				Some (lexp, h_t, v_t)
			| _ -> Some (lexp, h_t, v_t)
			end
		else
			None
	(* Ctor *)
	| ECtor (x, es) ->
		let ctor_typ = BatMap.find x tenv in
		let (tname, ts) = get_ctor_type ctor_typ in
		let holes_typ = List.map2 (fun e t -> (extract_holenum e, t)) es ts in
		begin match typ with
		| TBase _ when typ = tname -> Some (update_state holes_typ tenv (lexp, h_t, v_t))
		| TVar _ ->
			let (h_t, tenv) = update_polymorphic (typ, tname) (h_t, tenv) in
			Some (update_state holes_typ tenv (lexp, h_t, v_t))
		| _ -> None
		end
	| _ -> raise (Failure ("Invalid Syntax Components : " ^ Print.exp_to_string lexp))

(* Next State *)
let rec replace_exp : int -> lexp -> lexp -> lexp
= fun hole e' (l, e) ->
	match e with
	| EList es -> (l, EList (List.map (replace_exp hole e') es))
	| ECtor (x, es) -> (l, ECtor (x, List.map (replace_exp hole e') es))
	| ETuple es -> (l, ETuple (List.map (replace_exp hole e') es))
  | MINUS e -> (l, MINUS (replace_exp hole e' e))
  | NOT e -> (l, NOT (replace_exp hole e' e))
  | EFun (arg, e) -> (l, EFun (arg, (replace_exp hole e' e)))
  | Raise e -> (l, Raise (replace_exp hole e' e))
  | ADD (e1, e2) -> (l, ADD (replace_exp hole e' e1, replace_exp hole e' e2))
  | SUB (e1, e2) -> (l, SUB (replace_exp hole e' e1, replace_exp hole e' e2))
  | MUL (e1, e2) -> (l, MUL (replace_exp hole e' e1, replace_exp hole e' e2))
  | DIV (e1, e2) -> (l, DIV (replace_exp hole e' e1, replace_exp hole e' e2))
  | MOD (e1, e2) -> (l, MOD (replace_exp hole e' e1, replace_exp hole e' e2))
  | OR (e1, e2) -> (l, OR (replace_exp hole e' e1, replace_exp hole e' e2))
  | AND (e1, e2) -> (l, AND (replace_exp hole e' e1, replace_exp hole e' e2))
  | LESS (e1, e2) -> (l, LESS (replace_exp hole e' e1, replace_exp hole e' e2))
  | LARGER (e1, e2) -> (l, LARGER (replace_exp hole e' e1, replace_exp hole e' e2))
  | EQUAL (e1, e2) -> (l, EQUAL (replace_exp hole e' e1, replace_exp hole e' e2))
  | NOTEQ (e1, e2) -> (l, NOTEQ (replace_exp hole e' e1, replace_exp hole e' e2))
  | LESSEQ (e1, e2) -> (l, LESSEQ (replace_exp hole e' e1, replace_exp hole e' e2))
  | LARGEREQ (e1, e2) -> (l, LARGEREQ (replace_exp hole e' e1, replace_exp hole e' e2))
  | AT (e1, e2) -> (l, AT (replace_exp hole e' e1, replace_exp hole e' e2))
  | DOUBLECOLON (e1, e2) -> (l, DOUBLECOLON (replace_exp hole e' e1, replace_exp hole e' e2))
  | STRCON (e1, e2) -> (l, STRCON (replace_exp hole e' e1, replace_exp hole e' e2))
  | EApp (e1, e2) -> (l, EApp (replace_exp hole e' e1, replace_exp hole e' e2))
  | ELet (f, is_rec, args, typ, e1, e2) -> (l, ELet (f, is_rec, args, typ, replace_exp hole e' e1, replace_exp hole e' e2))
	| EBlock (is_rec, ds, e2) -> 
		let ds = List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, args, typ, replace_exp hole e' e)) ds in
		(l, EBlock (is_rec, ds, replace_exp hole e' e2))
	| EMatch (e, bs) ->
		let (ps, es) = List.split bs in
		(l, EMatch (replace_exp hole e' e, List.combine ps (List.map (replace_exp hole e') es)))
	| IF (e1, e2, e3) -> (l, IF (replace_exp hole e' e1, replace_exp hole e' e2, replace_exp hole e' e3))
  | Hole n -> if (n = hole) then e' else (l, e)
  | _ -> (l, e)

let replace_input : int -> lexp -> input -> input
= fun hole e' input -> List.map (fun e -> replace_exp hole e' e) input

let get_nextstates : Workset.work -> lexp -> Workset.work BatSet.t
= fun (input, h_t, v_t) hole ->
	let hole = extract_holenum hole in
	let hole_typ = BatMap.find hole h_t in
	let tenv = BatMap.find hole v_t in
	let components = BatSet.union (all_components ()) (get_var_components tenv) in
	(* transition *)
	let next_states = BatSet.fold (fun comp set ->
		let new_state = 
			try
				type_directed (hole, hole_typ, tenv) (comp, h_t, v_t)
			with _ -> None
		in
		match new_state with
		| None -> set
		| Some state -> BatSet.add state set
	) components BatSet.empty
	in
	(* replace *)
	BatSet.map (fun (e', h_t, v_t) ->
		(replace_input hole e' input, BatMap.remove hole h_t, BatMap.remove hole v_t)
	) next_states

let next : Workset.work -> Workset.work BatSet.t
= fun (input, h_t, v_t) ->
	let exp_holes = find_exp_holes input in
	let next_states = BatSet.fold (fun hole set -> 
		BatSet.union (get_nextstates (input, h_t, v_t) hole) set
	) exp_holes BatSet.empty in
	next_states

(* Sketch Generation *)
let rec type_to_sketch : typ -> (input * Type.HoleType.t)
= fun typ ->
	match typ with
	| TArr (t1, t2) -> 
		let (e, h_t1) = type_to_hole t1 in
		let (es, h_t2) = type_to_sketch t2 in
		(e::es, BatMap.union h_t1 h_t2)
	| _ -> ([], BatMap.empty)

and type_to_hole : typ -> (lexp * Type.HoleType.t)
= fun typ ->
	match typ with
	| TTuple ts -> 
		let (es, h_t) = List.fold_left (fun (es, h_t) t ->
			let (e, h_t') = type_to_hole t in
			(e::es, BatMap.union h_t' h_t)
		) ([], BatMap.empty) ts
		in
		((gen_label (), ETuple (List.rev es)), h_t)
	| t -> 
		let hole = gen_labeled_hole () in
		let n = extract_holenum hole in
		(hole, BatMap.singleton n t)

let rec get_sketch : prog -> Workset.work
= fun pgm ->
	let (tenv, _, _) = Type.run pgm in
	let func_typ = Type.TEnv.find tenv (!Options.opt_entry_func) in
	let (input, h_t) = type_to_sketch func_typ in
	let ctor_table = BatMap.filter (fun var typ ->
		match typ with
		| TCtor _ -> true
		| _ -> false
	) tenv in
	let v_t = BatMap.foldi (fun n typ v_t ->
		BatMap.add n ctor_table v_t
	) h_t BatMap.empty in
	(*
	let _ = print_endline (Print.type_to_string func_typ) in
	Print.print_header "Sketch";
	let _ = print_endline (Print.input_to_string input) in
	Print.print_header "Hole_type";
	let _ = Type.HoleType.print h_t in
	Print.print_header "Hole_env";
	let _ = Type.VariableType.print v_t in
	*)
	(input, h_t, v_t)


(* Main Synthesis Algorithm *)
let rec get_output : prog -> input -> value
= fun pgm input ->
	let res_var = "__res__" in
	let pgm = pgm@(External.grading_prog) in
	let pgm' = pgm @ [(DLet (BindOne res_var,false,[],fresh_tvar(),(appify (gen_label(), EVar !Options.opt_entry_func) input)))] in
	let env = Eval.run pgm' in
	lookup_env res_var env
	
let rec work : Workset.t -> prog -> prog -> example option
= fun workset pgm cpgm ->
	iter := !iter +1;
  if (Sys.time()) -. (!start_time) > 60.0 then None
  (*
  else if (!iter mod 10000 = 0)
	  then
		  begin
			  print_string("Test Generation Iter : " ^ (string_of_int !iter) ^ " ");
			  print_endline((Workset.workset_info workset) ^ (" Total elapsed : " ^ (string_of_float (Sys.time() -. !start_time))));
			  work workset pgm cpgm
		  end
	*)
	else
	match Workset.choose workset with
	| None -> None
	| Some ((input, h_t, v_t), remain) ->
		if is_closed input then
	  	let _ = count := !count +1 in
	  	try 
	  		let v2 = get_output cpgm input in
	  		try
	  			let v1 = get_output pgm input in
	  			if not (Eval.value_equality v1 v2) then Some (input, v2) else work remain pgm cpgm 
	  		with _ -> Some (input, v2)
	  	with _ -> work remain pgm cpgm
		else
			let nextstates = next (input, h_t, v_t) in
			let new_workset = BatSet.fold Workset.add nextstates remain in
			work new_workset pgm cpgm

let gen_counter_example : prog -> prog -> example option
= fun pgm cpgm ->
	let _ = start_time := 0.0 in
	let sketch = get_sketch cpgm in
	let initial_workset = Workset.add sketch Workset.empty in
	let result = work initial_workset pgm cpgm in
	result
