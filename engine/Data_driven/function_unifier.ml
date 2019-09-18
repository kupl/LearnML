open Lang
open Repairer
open Util

(* type *)
type state = lexp * Type.HoleType.t * Type.VariableType.t * Type.Subst.t * hole BatSet.t
and hole = int

module Workset = struct
  (* Workset *)
  type work = state

  module OrderedType = struct
    type t = work
    let compare (e1, _, _, _, _) (e2, _, _, _, _) =
    let (c1, c2) = (exp_cost e1, exp_cost e2) in
      if c1=c2 then 0 else
      if c1>c2 then 1
      else -1
  end

  module Heap = BatHeap.Make (OrderedType)

  (* type of workset : heap * (string set) *)
  type t = Heap.t * string BatSet.t
  let empty = (Heap.empty, BatSet.empty)

  let explored : lexp -> t -> bool
  = fun exp (_, sset) -> BatSet.mem (Print.exp_to_string exp) sset

  let add : work -> t -> t
  = fun (e, h_t, v_t, subst, init) (heap, sset) ->
    try
      let e = Normalize.normalize_exp e in
      if explored e (heap, sset) then (heap, sset)
      else (Heap.add (e, h_t, v_t, subst, init) heap, BatSet.add (Print.exp_to_string e) sset)
    with _ -> (heap, sset)

  let choose : t -> (work * t) option
  = fun (heap, sset) ->
    try
      let elem = Heap.find_min heap in
      Some (elem, (Heap.del_min heap, sset))
    with _ -> None

  let workset_info : t -> string
  = fun (heap, sset) ->
    "To explore : " ^ (string_of_int (Heap.size heap)) ^
    " Explored : " ^ (string_of_int (BatSet.cardinal sset))
end

module Comp = struct

	(* Update Components with fresh hole *)
	let rec update_components : lexp -> lexp
	= fun (l, exp)->
	  match exp with
  	| EUnit | Const _ | TRUE | FALSE | String _ | EList [] | EVar _ | Hole _ -> (gen_label (), exp)
    | EList es -> (gen_label (), EList (List.map update_components es))
    | ECtor (x, es) -> (gen_label (), ECtor (x, List.map update_components es))
    | ETuple es -> (gen_label (), ETuple (List.map update_components es))
	  | _ -> print_endline (Print.exp_to_string (l, exp)); raise (Failure "Unexpected components are updated")

	(* Extract Constant Components in submissions *)
	let rec get_const_comp_exp : components -> lexp -> components
	= fun comp (l, exp) ->
		match exp with 
  	| EUnit | Const _ | TRUE | FALSE | String _ | EList [] -> BatSet.add (0, exp) comp
		| EList es | ECtor (_, es) | ETuple es -> List.fold_left get_const_comp_exp comp es
	  | MINUS e | NOT e | EFun (_, e) -> get_const_comp_exp comp e
	  | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2)
	  | OR (e1, e2) | AND (e1, e2) | LESS (e1, e2) | LARGER (e1, e2) | EQUAL (e1, e2) | NOTEQ (e1, e2)
	  | LESSEQ (e1, e2) | LARGEREQ (e1, e2) | AT (e1, e2) | DOUBLECOLON (e1, e2) | STRCON (e1, e2)
	  | EApp (e1, e2) | ELet (_, _, _, _, e1, e2) -> List.fold_left get_const_comp_exp comp [e1; e2]
		| EBlock (_, ds, e2) -> 
			let es = e2::(List.map (fun (f, is_rec, args, typ, e) -> e) ds) in
			List.fold_left get_const_comp_exp comp es
		| EMatch (e, bs) ->
			let es = e::(List.map (fun (p, e) -> e) bs) in
			List.fold_left get_const_comp_exp comp es
		| IF (e1, e2, e3) -> List.fold_left get_const_comp_exp comp [e1; e2; e3]
		| Raise e -> comp
	  | _ -> comp

	let rec get_const_comp_decl : components -> decl -> components
	= fun comp decl ->
		match decl with
	  | DLet (f, is_rec, args, typ, exp) -> get_const_comp_exp comp exp
	  | DBlock (is_rec, bindings) -> List.fold_left (fun comp (f, is_rec, args, typ, exp) -> get_const_comp_exp comp exp) comp bindings
	  | _ -> comp

	let rec get_const_comp : prog -> components
	= fun pgm -> List.fold_left get_const_comp_decl BatSet.empty pgm

	(* Get Bounded Variable, User-defined Constructor *)
	let update_var_components : Type.TEnv.t -> components -> components
	= fun tenv comp ->
		BatMap.foldi (fun var t set ->
			match t with
			| TCtor _ -> set
			| _ -> BatSet.add (0, EVar var) set
		) tenv comp

	let update_ctor_components : Type.TEnv.t -> components -> components
	= fun tenv comp ->
		BatMap.foldi (fun var t set ->
			match t with
			| TCtor (name, ts) -> 
				begin match ts with
				| [] -> BatSet.add (0, ECtor (var, [])) set
				| (TTuple ts)::tl -> 
					let ctor_arg = (gen_label (), ETuple (List.map (fun t -> dummy_hole ()) ts)) in
					BatSet.add (0, ECtor (var, [ctor_arg])) set
				| _::tl -> BatSet.add (0, ECtor (var, [dummy_hole ()])) set
				end
			| _ -> set
		) tenv comp
end

(************************************)
(* Get next states of current state *)
(************************************)
let rec replace_exp : lexp -> (hole * lexp) -> lexp
= fun (l, e) (hole, e') ->
  match e with
  | EList es -> (l, EList (List.map (fun e -> replace_exp e (hole, e')) es))
  | ECtor (x, es) -> (l, ECtor (x, List.map (fun e -> replace_exp e (hole, e')) es))
  | ETuple es -> (l, ETuple (List.map (fun e -> replace_exp e (hole, e')) es))
  | MINUS e -> (l, MINUS (replace_exp e (hole, e')))
  | NOT e -> (l, NOT (replace_exp e (hole, e')))
  | EFun (arg, e) -> (l, EFun (arg, (replace_exp e (hole, e'))))
  | Raise e -> (l, Raise (replace_exp e (hole, e')))
  | ADD (e1, e2) -> (l, ADD (replace_exp e1 (hole, e'), replace_exp e2 (hole, e')))
  | SUB (e1, e2) -> (l, SUB (replace_exp e1 (hole, e'), replace_exp e2 (hole, e')))
  | MUL (e1, e2) -> (l, MUL (replace_exp e1 (hole, e'), replace_exp e2 (hole, e')))
  | DIV (e1, e2) -> (l, DIV (replace_exp e1 (hole, e'), replace_exp e2 (hole, e')))
  | MOD (e1, e2) -> (l, MOD (replace_exp e1 (hole, e'), replace_exp e2 (hole, e')))
  | OR (e1, e2) -> (l, OR (replace_exp e1 (hole, e'), replace_exp e2 (hole, e')))
  | AND (e1, e2) -> (l, AND (replace_exp e1 (hole, e'), replace_exp e2 (hole, e')))
  | LESS (e1, e2) -> (l, LESS (replace_exp e1 (hole, e'), replace_exp e2 (hole, e')))
  | LARGER (e1, e2) -> (l, LARGER (replace_exp e1 (hole, e'), replace_exp e2 (hole, e')))
  | EQUAL (e1, e2) -> (l, EQUAL (replace_exp e1 (hole, e'), replace_exp e2 (hole, e')))
  | NOTEQ (e1, e2) -> (l, NOTEQ (replace_exp e1 (hole, e'), replace_exp e2 (hole, e')))
  | LESSEQ (e1, e2) -> (l, LESSEQ (replace_exp e1 (hole, e'), replace_exp e2 (hole, e')))
  | LARGEREQ (e1, e2) -> (l, LARGEREQ (replace_exp e1 (hole, e'), replace_exp e2 (hole, e')))
  | AT (e1, e2) -> (l, AT (replace_exp e1 (hole, e'), replace_exp e2 (hole, e')))
  | DOUBLECOLON (e1, e2) -> (l, DOUBLECOLON (replace_exp e1 (hole, e'), replace_exp e2 (hole, e')))
  | STRCON (e1, e2) -> (l, STRCON (replace_exp e1 (hole, e'), replace_exp e2 (hole, e')))
  | EApp (e1, e2) -> (l, EApp (replace_exp e1 (hole, e'), replace_exp e2 (hole, e')))
  | ELet (f, is_rec, args, typ, e1, e2) -> (l, ELet (f, is_rec, args, typ, replace_exp e1 (hole, e'), replace_exp e2 (hole, e')))
  | EBlock (is_rec, ds, e2) -> 
    let ds = List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, args, typ, replace_exp e (hole, e'))) ds in
    (l, EBlock (is_rec, ds, replace_exp e2 (hole, e')))
  | EMatch (e, bs) ->
    let (ps, es) = List.split bs in
    (l, EMatch (replace_exp e (hole, e'), List.combine ps (List.map (fun e -> replace_exp e (hole, e')) es)))
  | IF (e1, e2, e3) -> (l, IF (replace_exp e1 (hole, e'), replace_exp e2 (hole, e'), replace_exp e3 (hole, e')))
  | Hole n -> if (n = hole) then e' else (l, e)
  | _ -> (l, e)

let extract_holenum : lexp -> hole
= fun e -> 
  match snd e with
  | Hole n -> n
  | _ -> raise (Failure "Error during obtain hole number")

let get_ctor_type : typ -> (typ * typ list)
= fun typ ->
  match typ with
  | TCtor (tname, ts) -> (tname, ts)
  | _ -> raise (Failure "Constructor type does not included at synthesizing")

(* Update polymorphic type variable t1 in Hole_Type, Var_Type, Subst to t2 *)
let rec update_polymorphic : (typ * typ) -> (Type.HoleType.t * Type.VariableType.t * Type.Subst.t) -> (Type.HoleType.t * Type.VariableType.t * Type.Subst.t)
= fun (t1, t2) (h_t, v_t, subst) ->
  let subst' = Type.unify subst (Type.Subst.apply t1 subst, Type.Subst.apply t2 subst) in
  let h_t' = Type.HoleType.update subst' h_t in
  let v_t' = Type.VariableType.update subst' v_t in
  (h_t', v_t', subst')

(* One-step Transition *)
let rec update_state : (hole * typ) list -> Type.TEnv.t -> state -> state
= fun ts hole_env (e, h_t, v_t, subst, init) ->
  List.fold_left (fun (e, h_t, v_t, subst, init) (hole, typ) ->
    let h_t = Type.HoleType.extend hole typ h_t in
    let v_t = Type.VariableType.extend hole hole_env v_t in
    (e, h_t, v_t, subst, init)
  ) (e, h_t, v_t, subst, init) ts

(* Type-directed Transition *)
let rec type_directed : (hole * typ * Type.TEnv.t) -> state -> state option
= fun (hole, hole_typ, hole_env) (lexp, h_t, v_t, subst, init) ->
	match snd lexp with
	(* const *)
	| Const _ -> 
		let (h_t, v_t, subst) = update_polymorphic (hole_typ, TInt) (h_t, v_t, subst) in
		Some (lexp, h_t, v_t, subst, init)
  | TRUE | FALSE -> 
		let (h_t, v_t, subst) = update_polymorphic (hole_typ, TBool) (h_t, v_t, subst) in
		Some (lexp, h_t, v_t, subst, init)
  | String _ ->
		let (h_t, v_t, subst) = update_polymorphic (hole_typ, TString) (h_t, v_t, subst) in
		Some (lexp, h_t, v_t, subst, init)
  (* var comp *)
  | EVar x ->
    let var_typ = BatMap.find x hole_env in
    let (h_t, v_t, subst) = update_polymorphic (hole_typ, var_typ) (h_t, v_t, subst) in
    Some (lexp, h_t, v_t, subst, init)
  (* list comp *)
  | EList es ->
  	let ns = List.map (fun e -> extract_holenum e) es in
    let tv = fresh_tvar() in
    let (h_t, v_t, subst) = update_polymorphic (hole_typ, TList tv) (h_t, v_t, subst) in
    Some (update_state (List.map (fun n -> (n, tv)) ns) hole_env (lexp, h_t, v_t, subst, init))
  (* tuple comp *)
  | ETuple es ->
    let ns = List.map (fun e -> extract_holenum e) es in
    let ts = List.map (fun _ -> fresh_tvar ()) ns in
    let (h_t, v_t, subst) = update_polymorphic (hole_typ, TTuple ts) (h_t, v_t, subst) in
    (*let init = List.fold_left (fun acc hole -> BatSet.add hole acc) init ns in *)
    Some (update_state (List.map2 (fun n typ -> (n, typ)) ns ts) hole_env (lexp, h_t, v_t, subst, init))
  (* Ctor comp *)
	| ECtor (x, es) ->
    let ctor_typ = BatMap.find x hole_env in
    let (tname, ts) = get_ctor_type ctor_typ in
    let holes_typ = List.map2 (fun e typ -> (extract_holenum e, typ)) es ts in
		let (h_t, v_t, subst) = update_polymorphic (hole_typ, tname) (h_t, v_t, subst) in
    Some (update_state holes_typ hole_env (lexp, h_t, v_t, subst, init))
  | _ -> raise (Failure (Print.exp_to_string lexp ^ " is included in Components set"))

let rec get_all_holes : lexp -> lexp BatSet.t
= fun (l, exp) -> 
	match exp with
  | EList es | ECtor (_, es) | ETuple es -> get_all_holes_list es
  | MINUS e | NOT e | EFun (_, e) | Raise e -> get_all_holes e
  | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2)
  | OR (e1, e2) | AND (e1, e2) | LESS (e1, e2) | LARGER (e1, e2) | EQUAL (e1, e2) | NOTEQ (e1, e2)
  | LESSEQ (e1, e2) | LARGEREQ (e1, e2) | AT (e1, e2) | DOUBLECOLON (e1, e2) | STRCON (e1, e2)
  | EApp (e1, e2) | ELet (_, _, _, _, e1, e2) -> get_all_holes_list [e1; e2]
  | EBlock (_, ds, e2) -> 
    let es = e2 :: (List.map (fun (f, is_rec, args, typ, e) -> e) ds) in
    get_all_holes_list es
  | EMatch (e, bs) ->
    let es = e :: (List.map (fun (p, e) -> e) bs) in
    get_all_holes_list es
  | IF (e1, e2, e3) -> get_all_holes_list [e1; e2; e3]
  | Hole n -> BatSet.singleton (l, exp)
  | _ -> BatSet.empty

and get_all_holes_list : lexp list -> lexp BatSet.t
= fun es -> List.fold_left (fun acc e -> BatSet.union (get_all_holes e) acc) BatSet.empty es

(* Get a hole that appears at first, if the expression is closed returns empty set *)
let rec find_first_hole : lexp -> lexp BatSet.t
= fun (l, exp) ->
  match exp with
  | EList es | ECtor (_, es) | ETuple es -> find_first_hole_list es
  | MINUS e | NOT e | EFun (_, e) | Raise e -> find_first_hole e
  | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2)
  | OR (e1, e2) | AND (e1, e2) | LESS (e1, e2) | LARGER (e1, e2) | EQUAL (e1, e2) | NOTEQ (e1, e2)
  | LESSEQ (e1, e2) | LARGEREQ (e1, e2) | AT (e1, e2) | DOUBLECOLON (e1, e2) | STRCON (e1, e2)
  | EApp (e1, e2) | ELet (_, _, _, _, e1, e2) -> find_first_hole_list [e1; e2]
  | EBlock (_, ds, e2) -> 
    let es = List.map (fun (f, is_rec, args, typ, e) -> e) ds in
    find_first_hole_list (es@[e2])
  | EMatch (e, bs) ->
    let es = e :: (List.map (fun (p, e) -> e) bs) in
    find_first_hole_list es
  | IF (e1, e2, e3) -> find_first_hole_list [e1; e2; e3]
  | Hole n -> BatSet.singleton (l, exp)
  | _ -> BatSet.empty

and find_first_hole_list : lexp list -> lexp BatSet.t
= fun es ->
  match es with
  | [] -> BatSet.empty 
  | hd::tl -> 
    let set = find_first_hole hd in
    if (BatSet.is_empty set) then find_first_hole_list tl else set

let get_next_states : components -> Workset.work -> lexp -> Workset.work BatSet.t
= fun comp (e, h_t, v_t, subst, init) hole ->
  let n = extract_holenum hole in
  let hole_typ = BatMap.find n h_t in
  let hole_env = BatMap.find n v_t in
  (* Component Extraction *)
  let comp =
    begin match hole_typ with
    | TTuple ts -> BatSet.add (gen_label(), ETuple (List.map (fun _ -> gen_labeled_hole()) ts)) comp
    | _ -> comp
    end 
  in
  let comp = Comp.update_var_components hole_env comp in
  let comp = if BatSet.mem n init then Comp.update_ctor_components hole_env comp else comp in
  let comp = BatSet.map Comp.update_components comp in
  (* Transition *)
  let next_states = BatSet.fold (fun comp set -> 
    let new_state = 
      try 
        type_directed (n, hole_typ, hole_env) (comp, h_t, v_t, subst, init)
      with _ -> None
    in
    match new_state with
    |Some state -> BatSet.add state set
    |None -> set
  ) comp BatSet.empty in
  (* Replace *)
  BatSet.map (fun (e', h_t, v_t, subst, init)-> (replace_exp e (n, e'), BatMap.remove n h_t, BatMap.remove n v_t, subst, init)) next_states 

let next : components -> Workset.work -> Workset.work BatSet.t
= fun comp (e, h_t, v_t, subst, init) ->
  let holes = find_first_hole e in
  BatSet.fold (fun hole set ->
    BatSet.union (get_next_states comp (e, h_t, v_t, subst, init) hole) set
  ) holes BatSet.empty

let is_closed : lexp -> bool
= fun e -> BatSet.is_empty (find_first_hole e)

let rec work : Workset.t -> components -> lexp BatSet.t
= fun workset comp ->
  match Workset.choose workset with
  | None -> BatSet.empty
  | Some ((e, h_t, v_t, subst, init), remain) ->
    if is_closed e then 
    	BatSet.add e (work remain comp)
    else
      let nextstates = next comp (e, h_t, v_t, subst, init) in
      let new_workset = BatSet.fold Workset.add nextstates remain in
      work new_workset comp

(* Generate templates including function call *)
let rec get_output_typ : typ -> typ
= fun typ ->
	match typ with
	| TArr (t1, t2) -> get_output_typ t2
	| _ -> typ

let rec get_input_typ : typ -> typ
= fun typ ->
	match typ with
	| TArr (t1, t2) -> 
		begin match t2 with
		| TArr (t1', t2') -> 
			let t1 = TArr (t1, t1') in
			get_input_typ (TArr (t1, t2'))
		| _ -> t1
		end
	| _ -> print_endline (Print.type_to_string typ); raise (Failure ("Type " ^ Print.type_to_string typ ^ " has no input"))

let rec refine_call : (id * typ) -> lexp
= fun (f, typ_i) ->
	match typ_i with
	| TArr (t1, t2) ->
		(*print_endline (f ^ " : " ^ Print.type_to_string typ_i ^ "out : " ^ Print.type_to_string typ_o);*)
		let typ_i = t1 in
		let e1 = refine_call (f, typ_i) in
		let e2 = 
			match t2 with
			| TTuple ts -> (gen_label (), ETuple (List.map (fun t -> gen_labeled_hole ()) ts))
			| _ -> gen_labeled_hole ()
		in
		(gen_label (), EApp (e1, e2))
	| t -> 
		let e1 = (gen_label (), EVar f) in
		let e2 =
			match t with
			| TTuple ts -> (gen_label (), ETuple (List.map (fun t -> gen_labeled_hole ()) ts))
			| _ -> gen_labeled_hole ()
		in
		(gen_label (), EApp (e1, e2))	
	
let rec refine_exp : PreAnalysis.t -> Type.TEnv.t -> lexp -> lexp BatSet.t
= fun pre func_map (l, exp) -> 
	match exp with 
	| EApp (e1, e2) -> 
		BatMap.foldi (fun f t candidates -> 
			let tout = BatMap.find l pre in
			match t with
			| TArr (t1, t2) ->
				(try
					let (t1, t2) = (get_input_typ t, get_output_typ t2) in
					let _ = Type.unify Type.Subst.empty (t2, tout) in
					BatSet.add (refine_call (f, t1)) candidates
				with _ -> candidates)
			| _ -> candidates
		) func_map BatSet.empty 
  | ADD (e1, e2) -> 
  	let (e1', e2') = (refine_exp pre func_map e1, refine_exp pre func_map e2) in 
  	BatSet.map (fun (e1, e2) -> (l, ADD (e1, e2))) (join_tuple e1' e2' )
  | SUB (e1, e2) -> 
  	let (e1', e2') = (refine_exp pre func_map e1, refine_exp pre func_map e2) in 
  	BatSet.map (fun (e1, e2) -> (l, SUB (e1, e2))) (join_tuple e1' e2' )
  | MUL (e1, e2) -> 
  	let (e1', e2') = (refine_exp pre func_map e1, refine_exp pre func_map e2) in 
  	BatSet.map (fun (e1, e2) -> (l, MUL (e1, e2))) (join_tuple e1' e2' )
  | DIV (e1, e2) -> 
  	let (e1', e2') = (refine_exp pre func_map e1, refine_exp pre func_map e2) in 
  	BatSet.map (fun (e1, e2) -> (l, DIV (e1, e2))) (join_tuple e1' e2' )
  | MOD (e1, e2) -> 
  	let (e1', e2') = (refine_exp pre func_map e1, refine_exp pre func_map e2) in 
  	BatSet.map (fun (e1, e2) -> (l, MOD (e1, e2))) (join_tuple e1' e2' )
  | OR (e1, e2) -> 
  	let (e1', e2') = (refine_exp pre func_map e1, refine_exp pre func_map e2) in 
  	BatSet.map (fun (e1, e2) -> (l, OR (e1, e2))) (join_tuple e1' e2' )
  | AND (e1, e2) -> 
  	let (e1', e2') = (refine_exp pre func_map e1, refine_exp pre func_map e2) in 
  	BatSet.map (fun (e1, e2) -> (l, AND (e1, e2))) (join_tuple e1' e2' )
	| EList es -> 
		let es' = List.map (fun e -> refine_exp pre func_map e) es in
		BatSet.map (fun es -> (l, EList es)) (join_list es')
  | ECtor (x, es) -> 
		let es' = List.map (fun e -> refine_exp pre func_map e) es in
		BatSet.map (fun es -> (l, ECtor (x, es))) (join_list es')
  | ETuple es -> 
		let es' = List.map (fun e -> refine_exp pre func_map e) es in
		BatSet.map (fun es -> (l, ETuple es)) (join_list es')
  | MINUS e -> BatSet.map (fun e -> (l, MINUS e)) (refine_exp pre func_map e)
  | NOT e -> BatSet.map (fun e -> (l, NOT e)) (refine_exp pre func_map e)
  | EFun (arg, e) -> BatSet.map (fun e -> (l, EFun (arg, e))) (refine_exp pre func_map e)
  | Raise e -> BatSet.map (fun e -> (l, Raise e)) (refine_exp pre func_map e)
  | LESS (e1, e2) -> 
  	let (e1', e2') = (refine_exp pre func_map e1, refine_exp pre func_map e2) in 
  	BatSet.map (fun (e1, e2) -> (l, LESS (e1, e2))) (join_tuple e1' e2' )
  | LARGER (e1, e2) -> 
  	let (e1', e2') = (refine_exp pre func_map e1, refine_exp pre func_map e2) in 
  	BatSet.map (fun (e1, e2) -> (l, LARGER (e1, e2))) (join_tuple e1' e2' )
  | EQUAL (e1, e2) -> 
  	let (e1', e2') = (refine_exp pre func_map e1, refine_exp pre func_map e2) in 
  	BatSet.map (fun (e1, e2) -> (l, EQUAL (e1, e2))) (join_tuple e1' e2' )
  | NOTEQ (e1, e2) -> 
  	let (e1', e2') = (refine_exp pre func_map e1, refine_exp pre func_map e2) in 
  	BatSet.map (fun (e1, e2) -> (l, NOTEQ (e1, e2))) (join_tuple e1' e2' )
  | LESSEQ (e1, e2) -> 
  	let (e1', e2') = (refine_exp pre func_map e1, refine_exp pre func_map e2) in 
  	BatSet.map (fun (e1, e2) -> (l, LESSEQ (e1, e2))) (join_tuple e1' e2' )
  | LARGEREQ (e1, e2) -> 
  	let (e1', e2') = (refine_exp pre func_map e1, refine_exp pre func_map e2) in 
  	BatSet.map (fun (e1, e2) -> (l, LARGEREQ (e1, e2))) (join_tuple e1' e2' )
  | AT (e1, e2) -> 
  	let (e1', e2') = (refine_exp pre func_map e1, refine_exp pre func_map e2) in 
  	BatSet.map (fun (e1, e2) -> (l, AT (e1, e2))) (join_tuple e1' e2' )
  | DOUBLECOLON (e1, e2) -> 
  	let (e1', e2') = (refine_exp pre func_map e1, refine_exp pre func_map e2) in 
  	BatSet.map (fun (e1, e2) -> (l, DOUBLECOLON (e1, e2))) (join_tuple e1' e2')
  | STRCON (e1, e2) -> 
  	let (e1', e2') = (refine_exp pre func_map e1, refine_exp pre func_map e2) in 
  	BatSet.map (fun (e1, e2) -> (l, STRCON (e1, e2))) (join_tuple e1' e2')
  | ELet (f, is_rec, args, typ, e1, e2) -> 
  	let (e1', e2') = (refine_exp pre func_map e1, refine_exp pre func_map e2) in 
  	BatSet.map (fun (e1, e2) -> (l, ELet (f, is_rec, args, typ, e1, e2))) (join_tuple e1' e2')
  (*
  | EBlock (is_rec, ds, e2) -> 
    let ds = List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, args, typ, replace_exp e (hole, e'))) ds in
    (l, EBlock (is_rec, ds, replace_exp e2 (hole, e')))
  | EMatch (e, bs) ->
    let (ps, es) = List.split bs in
    (l, EMatch (replace_exp e (hole, e'), List.combine ps (List.map (fun e -> replace_exp e (hole, e')) es)))
  *)
  | IF (e1, e2, e3) -> 
  	let (e1', e2', e3') = (refine_exp pre func_map e1, refine_exp pre func_map e2, refine_exp pre func_map e3) in 
  	BatSet.map (fun (e1, e2, e3) -> (l, IF (e1, e2, e3))) (join_triple e1' e2' e3')
  | _ -> BatSet.singleton (l, exp)

let compute_func_map : prog -> label -> Type.TEnv.t
= fun pgm l ->
	let pgm = Localize.gen_partial_pgm l pgm in
	let hole = !hole_count in
	let (_, _, v_t, _) = Type.run pgm in
	BatMap.foldi (fun x typ func_map ->
		match typ with
		| TArr _ -> BatMap.add x typ func_map
		| _ -> func_map
	) (BatMap.find hole v_t) BatMap.empty

let update_func_comp : PreAnalysis.t -> prog -> repair_cand BatSet.t -> repair_cand BatSet.t
= fun t pgm candidates ->
	let const_comp = Comp.get_const_comp pgm in
	let _ =
		print_endline ("Const Comp : ");
		Print.print_exp_set const_comp
	in
	let candidates = BatSet.fold (fun (l, exp) candidates ->
		(* Generate template *)
		let func_map = compute_func_map pgm l in
		Type.TEnv.print func_map;
		let result = 
			refine_exp t func_map exp 
			|> BatSet.map (fun exp -> (l, exp))
		in 
		(* Complete template *)
		let initial_workset = BatSet.fold (fun (l, e) workset -> 
			let pgm = List.map (fun decl -> subst_decl decl (l,e)) pgm in
			let (_, h_t, v_t, subst) = Type.run pgm in
			let init_holes = 
				get_all_holes e
				|> BatSet.map (fun hole -> extract_holenum hole) 
			in
			(*
			let _ =
				print_endline ("Exp : " ^ Print.exp_to_string e);
				print_endline ("Holes : ");
				BatSet.iter (fun hole -> print_endline (string_of_int hole)) init_holes
			in
			*)
			Workset.add (e, h_t, v_t, subst, init_holes) workset
		) result Workset.empty in
		let result = 
			work initial_workset const_comp
			|> BatSet.map (fun exp -> (l, exp))
		in
		BatSet.union result candidates
	) candidates BatSet.empty in
	candidates
