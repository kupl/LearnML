open Lang
open Util
open Extractor

(******************************)
(* Synthesize repair template *)
(******************************)

(*
	state = exp * h_t * v_t * subst * (initial) * except
*)
type state = lexp * Type.HoleType.t * Type.VariableType.t * Type.Subst.t * hole BatSet.t * hole BatSet.t
and hole = int

module Workset = struct
  (* Workset *)
  type work = state

  module OrderedType = struct
    type t = work
    let compare (e1, _, _, _, _, _) (e2, _, _, _, _, _) =
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
  = fun (e, h_t, v_t, subst, init, np) (heap, sset) ->
    try
      (* let e = Normalize.normalize_exp e in *)
      if explored e (heap, sset) then (heap, sset)
      else (Heap.add (e, h_t, v_t, subst, init, np) heap, BatSet.add (Print.exp_to_string e) sset)
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
    | EList es -> (gen_label (), EList (List.map update_components es))
    | ECtor (x, es) -> (gen_label (), ECtor (x, List.map update_components es))
    | ETuple es -> (gen_label (), ETuple (List.map update_components es))
    | MINUS e -> (gen_label (), MINUS (update_components e)) 
    | NOT e -> (gen_label (), NOT (update_components e)) 
	  | Raise e -> (gen_label (), Raise (update_components e))
    | EFun (arg, e) -> (gen_label (), EFun (arg, update_components e)) 
    | ADD (e1, e2) -> (gen_label (), ADD (update_components e1, update_components e2))
    | SUB (e1, e2) -> (gen_label (), SUB (update_components e1, update_components e2))
    | MUL (e1, e2) -> (gen_label (), MUL (update_components e1, update_components e2))
    | DIV (e1, e2) -> (gen_label (), DIV (update_components e1, update_components e2))
    | MOD (e1, e2) -> (gen_label (), MOD (update_components e1, update_components e2))
	  | OR (e1, e2) -> (gen_label (), OR (update_components e1, update_components e2))
	  | AND (e1, e2) -> (gen_label (), AND (update_components e1, update_components e2))
	  | LESS (e1, e2) -> (gen_label (), LESS (update_components e1, update_components e2))
	  | LARGER (e1, e2) -> (gen_label (), LARGER (update_components e1, update_components e2))
	  | EQUAL (e1, e2) -> (gen_label (), EQUAL (update_components e1, update_components e2))
	  | NOTEQ (e1, e2) -> (gen_label (), NOTEQ (update_components e1, update_components e2))
	  | LESSEQ (e1, e2) -> (gen_label (), LESSEQ (update_components e1, update_components e2))
	  | LARGEREQ (e1, e2) -> (gen_label (), LARGEREQ (update_components e1, update_components e2))
	  | AT (e1, e2) -> (gen_label (), AT (update_components e1, update_components e2))
	  | DOUBLECOLON (e1, e2) -> (gen_label (), DOUBLECOLON (update_components e1, update_components e2))
	  | STRCON (e1, e2) -> (gen_label (), STRCON (update_components e1, update_components e2))
	  | EApp (e1, e2) -> (gen_label (), EApp (update_components e1, update_components e2))
	  | ELet (f, is_rec, args, typ, e1, e2) -> (gen_label (), ELet (f, is_rec, args, typ, update_components e1, update_components e2))
		| EBlock (is_rec, ds, e2) -> 
			let ds = List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, args, typ, update_components e)) ds in
			(gen_label (), EBlock (is_rec, ds, update_components e2))
		| EMatch (e, bs) ->
			let bs = List.map (fun (p, e) -> (p, update_components e)) bs in
			(gen_label (), EMatch (update_components e, bs))
		| IF (e1, e2, e3) -> (gen_label (), IF (update_components e1, update_components e2, update_components e3))
		(*| Hole _ -> gen_labeled_hole ()*)
	  | _ -> (gen_label (), exp)

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

	(* Get operators *)
	let update_op_components : components -> components
	= fun comp ->
		BatSet.add (0, DOUBLECOLON (dummy_hole (), dummy_hole ())) comp
		|> BatSet.add (0, EFun (ArgUnder (fresh_tvar ()), dummy_hole ()))
		(* |> BatSet.add (0, STRCON (dummy_hole (), dummy_hole ())) *)
		|> BatSet.add (0, MINUS (dummy_hole ()))
		|> BatSet.add (0, ADD (dummy_hole (), dummy_hole ()))
		|> BatSet.add (0, SUB (dummy_hole (), dummy_hole ()))
		|> BatSet.add (0, MUL (dummy_hole (), dummy_hole ()))
		|> BatSet.add (0, DIV (dummy_hole (), dummy_hole ()))
		|> BatSet.add (0, MOD (dummy_hole (), dummy_hole ()))
		|> BatSet.add (0, NOT (dummy_hole ()))
		|> BatSet.add (0, OR (dummy_hole (), dummy_hole ()))
		|> BatSet.add (0, AND (dummy_hole (), dummy_hole ()))
		|> BatSet.add (0, LESS (dummy_hole (), dummy_hole ()))
		|> BatSet.add (0, LESSEQ (dummy_hole (), dummy_hole ()))
		|> BatSet.add (0, EQUAL (dummy_hole (), dummy_hole ()))
		|> BatSet.add (0, NOTEQ (dummy_hole (), dummy_hole ()))

	(* Get bounded variables *)
	let update_var_components : Type.TEnv.t -> components -> components
	= fun tenv comp ->
		BatMap.foldi (fun var t set ->
			match t with
			| TCtor _ -> set
			| _ -> BatSet.add (0, EVar var) set
		) tenv comp

	(* Get user-defined constructor s *)
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
= fun ts hole_env (e, h_t, v_t, subst, init, np) ->
  List.fold_left (fun (e, h_t, v_t, subst, init, np) (hole, typ) ->
    let h_t = Type.HoleType.extend hole typ h_t in
    let v_t = Type.VariableType.extend hole hole_env v_t in
    (e, h_t, v_t, subst, init, np)
  ) (e, h_t, v_t, subst, init, np) ts

(* Type-directed Transition *)
let rec type_directed : (hole * typ * Type.TEnv.t) -> state -> state option
= fun (hole, hole_typ, hole_env) (lexp, h_t, v_t, subst, init, np) ->
	match snd lexp with
	(* const *)
	| Const _ -> 
		let (h_t, v_t, subst) = update_polymorphic (hole_typ, TInt) (h_t, v_t, subst) in
		Some (lexp, h_t, v_t, subst, init, np)
  | TRUE | FALSE -> 
		let (h_t, v_t, subst) = update_polymorphic (hole_typ, TBool) (h_t, v_t, subst) in
		Some (lexp, h_t, v_t, subst, init, np)
  | String _ ->
		let (h_t, v_t, subst) = update_polymorphic (hole_typ, TString) (h_t, v_t, subst) in
		Some (lexp, h_t, v_t, subst, init, np)
  (* var comp *)
  | EVar x ->
    let var_typ = BatMap.find x hole_env in
    let (h_t, v_t, subst) = update_polymorphic (hole_typ, var_typ) (h_t, v_t, subst) in
    Some (lexp, h_t, v_t, subst, init, np)
  (* list comp *)
  | EList es ->
  	let ns = List.map (fun e -> extract_holenum e) es in
    let tv = fresh_tvar () in
    let (h_t, v_t, subst) = update_polymorphic (hole_typ, TList tv) (h_t, v_t, subst) in
    Some (update_state (List.map (fun n -> (n, tv)) ns) hole_env (lexp, h_t, v_t, subst, init, np))
  (* tuple comp *)
  | ETuple es ->
    let ns = List.map (fun e -> extract_holenum e) es in
    let ts = List.map (fun _ -> fresh_tvar ()) ns in
    let (h_t, v_t, subst) = update_polymorphic (hole_typ, TTuple ts) (h_t, v_t, subst) in
    (*let init = List.fold_left (fun acc hole -> BatSet.add hole acc) init ns in *)
    Some (update_state (List.map2 (fun n typ -> (n, typ)) ns ts) hole_env (lexp, h_t, v_t, subst, init, np))
  (* Ctor comp *)
	| ECtor (x, es) ->
    let ctor_typ = BatMap.find x hole_env in
    let (tname, ts) = get_ctor_type ctor_typ in
    let holes_typ = List.map2 (fun e typ -> (extract_holenum e, typ)) es ts in
		let (h_t, v_t, subst) = update_polymorphic (hole_typ, tname) (h_t, v_t, subst) in
    Some (update_state holes_typ hole_env (lexp, h_t, v_t, subst, init, np))
 	| EApp (e1, e2) -> 
 		begin match hole_typ with
 		| TArr _ ->
 			let (n1, n2) = (extract_holenum e1, extract_holenum e2) in
	    let tv = fresh_tvar () in
	    let init' = BatSet.add n2 (BatSet.add n1 init) in
	    Some (update_state [(n1, TArr (tv, hole_typ)); (n2, tv)] hole_env (lexp, h_t, v_t, subst, init', np))
 		| _ -> raise (Failure (Print.type_to_string hole_typ ^ " cannot be a function application"))
 		end
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
let rec find_first_hole : lexp -> hole BatSet.t -> lexp BatSet.t
= fun (l, exp) np ->
  match exp with
  | EList es | ECtor (_, es) | ETuple es -> find_first_hole_list es np
  | MINUS e | NOT e | EFun (_, e) | Raise e -> find_first_hole e np
  | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2)
  | OR (e1, e2) | AND (e1, e2) | LESS (e1, e2) | LARGER (e1, e2) | EQUAL (e1, e2) | NOTEQ (e1, e2)
  | LESSEQ (e1, e2) | LARGEREQ (e1, e2) | AT (e1, e2) | DOUBLECOLON (e1, e2) | STRCON (e1, e2)
  | EApp (e1, e2) | ELet (_, _, _, _, e1, e2) -> find_first_hole_list [e1; e2] np
  | EBlock (_, ds, e2) -> 
    let es = List.map (fun (f, is_rec, args, typ, e) -> e) ds in
    find_first_hole_list (es@[e2]) np
  | EMatch (e, bs) ->
    let es = e :: (List.map (fun (p, e) -> e) bs) in
    find_first_hole_list es np
  | IF (e1, e2, e3) -> find_first_hole_list [e1; e2; e3] np
  | Hole n -> if BatSet.mem n np then BatSet.empty else BatSet.singleton (l, exp)
  | _ -> BatSet.empty

and find_first_hole_list : lexp list -> hole BatSet.t -> lexp BatSet.t
= fun es np ->
  match es with
  | [] -> BatSet.empty 
  | hd::tl -> 
    let set = find_first_hole hd np in
    if (BatSet.is_empty set) then find_first_hole_list tl np else set

let get_next_states : components -> Workset.work -> lexp -> Workset.work BatSet.t
= fun comp (e, h_t, v_t, subst, init, np) hole ->
  let n = extract_holenum hole in
  let hole_typ = BatMap.find n h_t in
  let hole_env = BatMap.find n v_t in
  (* When current hole is tuple type *)
  let holes = 
  	match hole_typ with 
  	| TTuple ts -> List.map (fun _ -> gen_labeled_hole ()) ts
  	| _ -> []
  in
  let init = List.fold_left (fun acc hole -> BatSet.add (extract_holenum hole) acc) init holes in
  (* Component Extraction *)
  let comp =
    match hole_typ with
    | TTuple ts -> BatSet.add (gen_label(), ETuple (List.map (fun hole -> hole) holes)) comp
    | _ -> comp 
  in
  let comp = Comp.update_var_components hole_env comp in
 	let comp = if BatSet.mem n init then Comp.update_ctor_components hole_env comp else comp in
 	let comp = if BatSet.mem n init then Comp.update_op_components comp else comp in
  let comp = BatSet.map Comp.update_components comp in
  (* Transition *)
  let next_states = BatSet.fold (fun comp set -> 
  	let new_state = 
      try 
        type_directed (n, hole_typ, hole_env) (comp, h_t, v_t, subst, init, np)
      with _ -> None
    in
    match new_state with
    |Some state -> BatSet.add state set
    |None -> set
  ) comp BatSet.empty in
  (* Replace *)
  BatSet.map (fun (e', h_t, v_t, subst, init, np)-> (replace_exp e (n, e'), BatMap.remove n h_t, BatMap.remove n v_t, subst, init, np)) next_states 

let next : components -> Workset.work -> Workset.work BatSet.t
= fun comp (e, h_t, v_t, subst, init, np) ->
  let holes = find_first_hole e np in
  BatSet.fold (fun hole set ->
    BatSet.union (get_next_states comp (e, h_t, v_t, subst, init, np) hole) set
  ) holes BatSet.empty

let is_closed : lexp -> hole BatSet.t -> bool
= fun e np -> BatSet.is_empty (find_first_hole e np)

let rec work : Workset.t -> components -> lexp BatSet.t
= fun workset comp ->
  match Workset.choose workset with
  | None -> BatSet.empty
  | Some ((e, h_t, v_t, subst, init, np), remain) ->
    if is_closed e np then 
    	BatSet.add e (work remain comp)
    else
      let nextstates = next comp (e, h_t, v_t, subst, init, np) in
      let new_workset = BatSet.fold Workset.add nextstates remain in
      work new_workset comp

(*****************************)
(* Applying repair templates *)
(*****************************)
let rec subst_exp : lexp -> repair_template -> lexp
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

let rec subst_decl : decl -> repair_template -> decl
= fun decl temp ->
	match decl with
  | DLet (f, is_rec, args, typ, e) -> DLet (f, is_rec, args, typ, subst_exp e temp)
  | DBlock (is_rec, bindings) ->
    let bindings = List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, args, typ, subst_exp e temp)) bindings in
    DBlock (is_rec, bindings) 
  | _ -> decl

let rec subst_pgm : prog -> repair_template -> prog
= fun pgm temp -> List.map (fun decl -> subst_decl decl temp) pgm

(******************************************)
(* Complete varaibles in repair templates *)
(******************************************)
let rec find_app : lexp -> hole BatSet.t
= fun (l, exp) ->
	match exp with
	| EList es | ECtor (_, es) | ETuple es -> List.fold_left (fun acc e -> BatSet.union acc (find_app e)) BatSet.empty es
  | MINUS e | NOT e | EFun (_, e) | Raise e -> find_app e
  | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2)
  | OR (e1, e2) | AND (e1, e2) | LESS (e1, e2) | LARGER (e1, e2) | EQUAL (e1, e2) | NOTEQ (e1, e2)
  | LESSEQ (e1, e2) | LARGEREQ (e1, e2) | AT (e1, e2) | DOUBLECOLON (e1, e2) | STRCON (e1, e2)
  | ELet (_, _, _, _, e1, e2) -> BatSet.union (find_app e1) (find_app e2)
  | EApp (e1, e2) ->
  	begin match (snd e1, snd e2) with 
  	| Hole n1, Hole n2 -> BatSet.add n2 (BatSet.singleton n1)
  	| _ -> BatSet.union (find_app e1) (find_app e2)
  	end
  | EBlock (_, ds, e2) -> 
    let es = e2 :: (List.map (fun (f, is_rec, args, typ, e) -> e) ds) in
    List.fold_left (fun acc e -> BatSet.union acc (find_app e)) BatSet.empty es
  | EMatch (e, bs) ->
    let es = e :: (List.map (fun (p, e) -> e) bs) in
    List.fold_left (fun acc e -> BatSet.union acc (find_app e)) BatSet.empty es
  | IF (e1, e2, e3) -> List.fold_left (fun acc e -> BatSet.union acc (find_app e)) BatSet.empty [e1; e2; e3]
  | _ -> BatSet.empty

let complete_var : prog -> repair_template BatSet.t -> repair_template BatSet.t
= fun pgm temps ->
	let comp = BatSet.empty in (* using only var components *)
	BatSet.fold (fun (l, exp) acc ->
		try 
			(* Complete template *)
			let initial_workset = BatSet.fold (fun (l, exp) workset -> 
				let pgm' = subst_pgm pgm (l, exp) in
				let (_, h_t, v_t, subst) = Type.run pgm' in
				let v_t = BatMap.map (fun tenv -> 
					BatMap.filterv (fun typ ->
						match typ with
						| TArr _ -> false
						| _ -> true
					) tenv
				) v_t 
				in
				(* add initial state *)
				Workset.add (exp, h_t, v_t, subst, BatSet.empty, find_app exp) workset
			) (BatSet.singleton (l, exp)) Workset.empty in
			let result = BatSet.map (fun exp -> (l, exp)) (work initial_workset comp) in
			BatSet.union result acc
		with _ -> (* Unavailable repair templates (e.g., type error) *) acc
	) temps BatSet.empty

let complete_func : prog -> repair_template BatSet.t -> repair_template BatSet.t
= fun pgm temps -> 
	let comp = Comp.get_const_comp pgm in (* Contant components *)
	BatSet.fold (fun (l, exp) acc ->
		(* Complete template *)
		let initial_workset = BatSet.fold (fun (l, exp) workset -> 
			let pgm' = List.map (fun decl -> subst_decl decl (l, exp)) pgm in
			let (_, h_t, v_t, subst) = Type.run pgm' in
			let init_holes = BatSet.map (fun hole -> extract_holenum hole) (get_all_holes exp) in
			Workset.add (exp, h_t, v_t, subst, init_holes, BatSet.empty) workset
		) (BatSet.singleton (l, exp)) Workset.empty in
		let result = BatSet.map (fun exp -> (l, exp)) (work initial_workset comp) in
		BatSet.union result acc
	) temps BatSet.empty

let complete_temp : prog -> repair_template BatSet.t -> repair_template BatSet.t
= fun pgm temps ->
	(* 
		TODO
		Matching 잘못하면 타입에러남.
	*)
	let temps = complete_var pgm temps in
	complete_func pgm temps 