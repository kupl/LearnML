open Lang
open Util
open Symbol_lang2

(*
 ******************************************************
 	Code for Synthesizing a Test-case
 ******************************************************
*)

module Comp = struct
  (* generate a fresh const component *)
  let const_num = ref 0
  let fresh_const () = (const_num := !const_num +1; !const_num)

  (* Constant Components *)
  let const_comp : components -> components
  = fun comp ->
  	let rec range a b =
  		if a > b then []
  		else a::(range (a+1) b)
  	in
  	let int_comp = List.fold_left (fun comp n -> BatSet.add (0, Const n) comp) comp (range 0 3) in
  	let str_comp = List.fold_left (fun comp n -> BatSet.add (0, String (Char.escaped (Char.chr n))) comp) int_comp (range 97 99) in
  	str_comp

	(* Syntax Components *)
  let all_components : unit -> components
	= fun () ->
		(*
      A DSL having below syntax components  
      except ctor, var (variable comp), tuple, list append, condition let binding, match, exception, hole 
    *)
		BatSet.empty 
		|> BatSet.add (0, SStr 0)
		|> BatSet.add (0, SInt 0)
		|> BatSet.add (0, TRUE)
		|> BatSet.add (0, FALSE)
		|> BatSet.add (0, EList [])
		|> BatSet.add (0, DOUBLECOLON (dummy_hole (), dummy_hole ()))
		|> BatSet.add (0, ETuple [])
		|> BatSet.add (0, EFun (ArgUnder (fresh_tvar ()), dummy_hole ()))
		|> BatSet.add (0, STRCON (dummy_hole (), dummy_hole ()))
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

	(* Reduce comp set by removing operators if the type of given input is not function *)
	let reduce_comp : components -> components
	= fun comp -> 
		let remove_op_comp : components -> components
		= fun comp ->
			comp
			|> BatSet.remove (0, STRCON (dummy_hole (), dummy_hole ()))
			|> BatSet.remove (0, MINUS (dummy_hole ()))
			|> BatSet.remove (0, ADD (dummy_hole (), dummy_hole ()))
			|> BatSet.remove (0, MUL (dummy_hole (), dummy_hole ()))
			|> BatSet.remove (0, SUB (dummy_hole (), dummy_hole ()))
			|> BatSet.remove (0, DIV (dummy_hole (), dummy_hole ()))
			|> BatSet.remove (0, MOD (dummy_hole (), dummy_hole ()))
			|> BatSet.remove (0, NOT (dummy_hole ()))
			|> BatSet.remove (0, OR (dummy_hole (), dummy_hole ()))
			|> BatSet.remove (0, AND (dummy_hole (), dummy_hole ()))
			|> BatSet.remove (0, LESS (dummy_hole (), dummy_hole ()))
			|> BatSet.remove (0, LESSEQ (dummy_hole (), dummy_hole ()))
			|> BatSet.remove (0, EQUAL (dummy_hole (), dummy_hole ()))
			|> BatSet.remove (0, NOTEQ (dummy_hole (), dummy_hole ()))
		in
		let is_fun : components -> bool
		= fun comp -> 	
			(* If a variable x is in components set, it is function *)
			BatSet.exists (fun (l, e) ->
				match e with
				| EVar _ -> true
				| _ -> false
			) comp
		in
		if is_fun comp then comp else remove_op_comp comp

	(* Update Components with fresh hole *)
	let rec update_components : lexp -> lexp
	= fun (l, exp)->
	  match exp with
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
    | EList es -> (gen_label (), EList (List.map update_components es))
    | ECtor (x, es) -> (gen_label (), ECtor (x, List.map update_components es))
    | ETuple es -> (gen_label (), ETuple (List.map update_components es))
	  | MINUS e -> (gen_label (), MINUS (update_components e))
	  | NOT e -> (gen_label (), NOT (update_components e))
	  | EFun (arg, e) -> (gen_label (), EFun (arg, update_components e))
	  | Raise e -> (gen_label (), Raise (update_components e))
	  | ELet (f, is_rec, args, typ, e1, e2) -> (gen_label (), ELet (f, is_rec, args, typ, update_components e1, update_components e2))
		| EBlock (is_rec, ds, e2) -> 
			let ds = List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, args, typ, update_components e)) ds in
			(gen_label (), EBlock (is_rec, ds, update_components e2))
		| EMatch (e, bs) ->
			let bs = List.map (fun (p, e) -> (p, update_components e)) bs in
			(gen_label (), EMatch (update_components e, bs))
		| IF (e1, e2, e3) -> (gen_label (), IF (update_components e1, update_components e2, update_components e3))
		| Hole _ -> gen_labeled_hole ()
    | SInt _ -> (gen_label (), SInt (fresh_const ()))
    | SStr _ -> (gen_label (), SStr (fresh_const ()))
	  | _ -> (gen_label (), exp)

	(* Extract Constant Components in submissions *)
	let rec get_const_comp_exp : components -> lexp -> components
	= fun comp (l, exp) ->
		match exp with 
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
		| Const n -> BatSet.add (l, exp) comp
		| String str -> BatSet.add (l, exp) comp
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
end

module Cost = struct
	(* size of exp *)
  let rec size_cost : lexp -> int
  = fun (_, exp) ->
  	match exp with
	  | EList es -> List.fold_left (fun acc e -> size_cost e + acc) 10 es
	  | ECtor (_, es) -> List.fold_left (fun acc e -> size_cost e + acc) 20 es
	  | ETuple es -> List.fold_left (fun acc e -> size_cost e + acc) 5 es
	  | MINUS e | NOT e -> 15 + size_cost e
	  | EFun (_, e) -> 30 + size_cost e
	  | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2)
	  | OR (e1, e2) | AND (e1, e2) | LESS (e1, e2) | LARGER (e1, e2) | EQUAL (e1, e2) | NOTEQ (e1, e2)
	  | LESSEQ (e1, e2) | LARGEREQ (e1, e2) | STRCON (e1, e2) | AT (e1, e2) | DOUBLECOLON (e1, e2) -> 15 + size_cost e1 + size_cost e2
	  | Hole _ -> 23
    | EVar _  -> 10
    | SStr _ | SInt _ | Const _ | TRUE | FALSE | String _  -> 15
    | _ -> raise (Failure "Invalid Input Syntax")

	(* cost function for generating a test case *)
	let cost_input : input -> int
	= fun input -> List.fold_left (fun cost e -> cost + size_cost e) 0 input
end

module Workset = struct
  type work = input * Type.HoleType.t * Type.VariableType.t

  module OrderedType = struct
    type t = work
    let compare (input1, _, _) (input2, _, _) =
	    let (c1, c2) = (Cost.cost_input input1, Cost.cost_input input2) in
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
  		let input = List.map Normalize.normalize_exp input in
    	if explored input (heap, sset) then (heap, sset)
    	else
      	(Heap.add (input, h_t, v_t) heap, BatSet.add (Print.input_to_string (input)) sset)
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

(************ 
	Generetor 
*************)
let start_time = ref 0.0
let iter = ref 0

let solving_time = ref 0.0

let count = ref 0
let num_of_crash = ref 0

let var_num = ref 0

(* type *)
type state = lexp * Type.HoleType.t * Type.VariableType.t

(* helper functions *)
let fresh_var () = (var_num := !var_num +1; "x_" ^ string_of_int !var_num)

let rec fresh_arg : typ -> arg
= fun typ ->
	match typ with
	| TTuple ts -> ArgTuple (List.map (fun t -> fresh_arg t) ts)
	| _ -> ArgOne (fresh_var (), typ)

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

let rec update_polymorphic : (typ * typ) -> (Type.HoleType.t * Type.VariableType.t * Type.TEnv.t) -> (Type.HoleType.t * Type.VariableType.t * Type.TEnv.t)
= fun (t1, t2) (h_t, v_t, tenv) ->
	let h_t = BatMap.map (fun typ -> update_type (t1, t2) typ) h_t in
	let v_t = BatMap.map (fun tenv -> BatMap.map (fun typ -> update_type (t1, t2) typ) tenv) v_t in
	let tenv = BatMap.map (fun typ -> update_type (t1, t2) typ) tenv in
	(h_t, v_t, tenv)

(* One-step Transition *)
let rec update_state : (int * typ) list -> Type.TEnv.t -> state -> state
= fun ts tenv (e, h_t, v_t) ->
	List.fold_left (fun (e, h_t, v_t) (hole, typ) ->
		let h_t = BatMap.add hole typ h_t in
		let v_t = BatMap.add hole tenv v_t in
		(e, h_t, v_t)
	) (e, h_t, v_t) ts

(* Type-directed Search *)
let type_directed : (int * typ * Type.TEnv.t) -> state -> state option
= fun (hole, typ, tenv) (lexp, h_t, v_t) ->
	(* Transition from hole to exp *)
	match snd lexp with
	(* Const *)
	| SStr _ | String _ -> 		
		begin match typ with
		| TString -> Some (lexp, h_t, v_t)
		| TVar _ ->
			let (h_t, v_t, tenv) = update_polymorphic (typ, TString) (h_t, v_t, tenv) in
			Some (lexp, h_t, v_t)
		| _ -> None
		end
  | SInt _ | Const _ ->
    begin match typ with
		| TInt -> Some (lexp, h_t, v_t) 
		| TVar _ ->
			let (h_t, v_t, tenv) = update_polymorphic (typ, TInt) (h_t, v_t, tenv) in
			Some (lexp, h_t, v_t)
    | _ -> None
    end
	| TRUE | FALSE ->
		begin match typ with
		| TBool -> Some (lexp, h_t, v_t)
		| TVar _ ->
			let (h_t, v_t, tenv) = update_polymorphic (typ, TBool) (h_t, v_t, tenv) in
			Some (lexp, h_t, v_t)
		| _ -> None
		end
	| EList [] ->
		begin match typ with
		| TList t -> Some (lexp, h_t, v_t)
		| TVar _ ->
			let (h_t, v_t, tenv) = update_polymorphic (typ, TList (fresh_tvar ())) (h_t, v_t, tenv) in
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
			let (h_t, v_t, tenv) = update_polymorphic (typ, TInt) (h_t, v_t, tenv) in
			Some (update_state [n1, TInt] tenv (lexp, h_t, v_t))
		| _ -> None
		end
	| ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2) -> 
		let (n1, n2) = (extract_holenum e1, extract_holenum e2) in 
		begin match typ with
		| TInt -> Some (update_state [(n1, TInt); (n2, TInt)] tenv (lexp, h_t, v_t))
		| TVar _ ->
			let (h_t, v_t, tenv) = update_polymorphic (typ, TInt) (h_t, v_t, tenv) in
			Some (update_state [(n1, TInt); (n2, TInt)] tenv (lexp, h_t, v_t))
		| _ -> None
		end
	(* bop *)
	| NOT e1 ->
		let n1 = extract_holenum e1 in
		begin match typ with
		| TBool -> Some (update_state [n1, TBool] tenv (lexp, h_t, v_t))
		| TVar _ ->
			let (h_t, v_t, tenv) = update_polymorphic (typ, TBool) (h_t, v_t, tenv) in
			Some (update_state [n1, TBool] tenv (lexp, h_t, v_t))
		| _ -> None
		end
	| OR (e1, e2) | AND (e1, e2) ->
		let (n1, n2) = (extract_holenum e1, extract_holenum e2) in 
		begin match typ with
		| TBool -> Some (update_state [(n1, TBool); (n2, TBool)] tenv (lexp, h_t, v_t))
		| TVar _ ->
			let (h_t, v_t, tenv) = update_polymorphic (typ, TBool) (h_t, v_t, tenv) in
			Some (update_state [(n1, TBool); (n2, TBool)] tenv (lexp, h_t, v_t))
		| _ -> None
		end
	| LESS (e1, e2) | LESSEQ (e1, e2) ->
		let (n1, n2) = (extract_holenum e1, extract_holenum e2) in 
		begin match typ with
		| TBool -> Some (update_state [(n1, TInt); (n2, TInt)] tenv (lexp, h_t, v_t))
		| TVar _ ->
			let (h_t, v_t, tenv) = update_polymorphic (typ, TBool) (h_t, v_t, tenv) in
			Some (update_state [(n1, TInt); (n2, TInt)] tenv (lexp, h_t, v_t))
		| _ -> None
		end
	| EQUAL (e1, e2) | NOTEQ (e1, e2) -> 
		let (n1, n2) = (extract_holenum e1, extract_holenum e2) in 
		let tv = fresh_tvar () in
		begin match typ with
		| TBool -> Some (update_state [(n1, tv); (n2, tv)] tenv (lexp, h_t, v_t))
		| TVar _ ->
			let (h_t, v_t, tenv) = update_polymorphic (typ, TBool) (h_t, v_t, tenv) in
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
			let (h_t, v_t, tenv) = update_polymorphic (typ, TList tv) (h_t, v_t, tenv) in
			Some (update_state [(n1, TList tv); (n2, TList tv)] tenv (lexp, h_t, v_t))
		| _ -> None
		end
	| DOUBLECOLON (e1, e2) ->
		let (n1, n2) = (extract_holenum e1, extract_holenum e2) in 
		begin match typ with
		| TList t -> Some (update_state [(n1, t); (n2, TList t)] tenv (lexp, h_t, v_t))
		| TVar _ ->
			let tv = fresh_tvar () in
			let (h_t, v_t, tenv) = update_polymorphic (typ, TList tv) (h_t, v_t, tenv) in
			Some (update_state [(n1, tv); (n2, TList tv)] tenv (lexp, h_t, v_t))
		| _ -> None
		end
	(* string *)
	| STRCON (e1, e2) ->
		let (n1, n2) = (extract_holenum e1, extract_holenum e2) in 
		begin match typ with
		| TString -> Some (update_state [(n1, TString); (n2, TString)] tenv (lexp, h_t, v_t))
		| TVar _ ->
			let (h_t, v_t, tenv) = update_polymorphic (typ, TString) (h_t, v_t, tenv) in
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
		begin try
			let subst = Type.unify Type.Subst.empty (typ, var_typ) in
			let (h_t, v_t, tenv) = List.fold_left (fun (h_t, v_t, tenv) (id, typ) ->
				let var_typ = TVar id in
				update_polymorphic (var_typ, typ) (h_t, v_t, tenv)
			) (h_t, v_t, tenv) subst 
			in
			Some (lexp, h_t, v_t)
		with  _ -> None
		end
	(* Ctor *)
	| ECtor (x, es) ->
		let ctor_typ = BatMap.find x tenv in
		let (tname, ts) = get_ctor_type ctor_typ in
		let holes_typ = List.map2 (fun e t -> (extract_holenum e, t)) es ts in
		begin match typ with
		| TBase _ when typ = tname -> Some (update_state holes_typ tenv (lexp, h_t, v_t))
		| TVar _ ->
			let (h_t, v_t, tenv) = update_polymorphic (typ, tname) (h_t, v_t, tenv) in
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

let get_nextstates : components -> Workset.work -> lexp -> Workset.work BatSet.t
= fun comp (input, h_t, v_t) hole ->
	let hole = extract_holenum hole in
	let hole_typ = BatMap.find hole h_t in
	let tenv = BatMap.find hole v_t in
	let components = 
		BatSet.union comp (Comp.get_var_components tenv)
		|> Comp.reduce_comp
		|> BatSet.map Comp.update_components
	in
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

let next : components -> Workset.work -> Workset.work BatSet.t
= fun comp (input, h_t, v_t) ->
	let exp_holes = find_exp_holes input in
	let next_states = BatSet.fold (fun hole set -> 
		BatSet.union (get_nextstates comp (input, h_t, v_t) hole) set
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
	let (tenv, _, _, _) = Type.run pgm in
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
	(input, h_t, v_t)

(* Const symbol *)
let rec get_const_symbols : lexp -> (string * int) BatSet.t
= fun (l, e) ->
	match e with
  | EList es | ECtor (_, es) | ETuple es -> get_const_symbols_list es
  | MINUS e | NOT e | EFun (_, e) | Raise e -> get_const_symbols e
  | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2)
  | OR (e1, e2) | AND (e1, e2) | LESS (e1, e2) | LARGER (e1, e2) | EQUAL (e1, e2) | NOTEQ (e1, e2)
  | LESSEQ (e1, e2) | LARGEREQ (e1, e2) | AT (e1, e2) | DOUBLECOLON (e1, e2) | STRCON (e1, e2)
  | EApp (e1, e2) | ELet (_, _, _, _, e1, e2) -> get_const_symbols_list [e1; e2]
	| EBlock (_, ds, e2) -> 
		let es = List.map (fun (f, is_rec, args, typ, e) -> e) ds in
		get_const_symbols_list (es@[e2])
	| EMatch (e, bs) ->
		let es = e :: (List.map (fun (p, e) -> e) bs) in
		get_const_symbols_list es
	| IF (e1, e2, e3) -> get_const_symbols_list [e1; e2; e3]
  | SStr n -> BatSet.singleton ("S", n)
  | SInt n -> BatSet.singleton ("A", n)
  | _ -> BatSet.empty

and get_const_symbols_list : lexp list -> (string * int) BatSet.t
= fun es ->
	match es with
	| [] -> BatSet.empty
	| hd::tl ->
		let symbols = get_const_symbols hd in
		BatSet.union symbols (get_const_symbols_list tl)

let rec find_const_symbols : input -> (string * int) BatSet.t
= fun input -> get_const_symbols_list input

let rec get_const_symbols : Z3.Model.model -> (string * int) BatSet.t -> (int * exp) list
= fun model symbols ->
	let decls = Z3.Model.get_const_decls model in
  (* Parse Interpreted Model to get mapping of constant symbol *)
  let symbol_map = List.fold_left (fun acc decl -> 
  	let name = Z3.FuncDecl.get_name decl in
  	begin match Z3.Model.get_const_interp model decl with
    | Some value -> 
    	(*
    	print_endline ("DECL : " ^ Z3.FuncDecl.to_string decl);
    	print_endline ("NAME : " ^ Z3.Symbol.to_string name);
    	print_endline ("VALUE : " ^ Z3.Expr.to_string value);
    	*)
    	if String.contains (Z3.Symbol.to_string name) 'A' then
  			let value_string = Z3.Expr.to_string value in
  			let value_string =
  				if Str.string_match (Str.regexp "(- ") value_string 0 
	        then Str.replace_first (Str.regexp "(- ") "-" (Str.replace_first (Str.regexp ")") "" value_string) (* Remove parenthesis *)
	        else value_string 
	      in
	      let id = int_of_string (Str.replace_first (Str.regexp "A") "" (Z3.Symbol.to_string name)) in
  			let value = Const (int_of_string value_string) in
  			(id, value)::acc
  		else if String.contains (Z3.Symbol.to_string name) 'S' then
  			let value_string = Z3.Expr.to_string value in
  			let value_string = (* Remove double quotation *)
  				if Str.string_match (Str.regexp "\"!") value_string 0
  				then "x_" ^ (Str.replace_first (Str.regexp "\"!") "" (Str.replace_first (Str.regexp "!\"") "" value_string)) (* n'th string *)
  				else Str.replace_first (Str.regexp "\"") "" (Str.replace_first (Str.regexp "\"") "" value_string) (* Use constant string *)
  			in
	      let id = int_of_string (Str.replace_first (Str.regexp "S") "" (Z3.Symbol.to_string name)) in
  			let value = String (value_string) in
  			(id, value)::acc
  		else acc
    | None -> acc
    end
  ) [] decls
  in
  (* If some symbols don't have value => Randomly instantiating (Const 1, Str "x") *)
  let symbol_map = BatSet.fold (fun (typ, symbol) map -> 
  	if List.exists (fun (n, v) -> n = symbol) map then map 
  	else if typ = "A" then (symbol, Const 1)::map
  	else (symbol, String "x")::map
  ) symbols symbol_map
	in
	symbol_map

let rec replace_const_symbol : (int * exp) -> lexp -> lexp
= fun (hole, value) (l, e) ->
	match e with 
	| EList es -> (l, EList (List.map (replace_const_symbol (hole, value)) es))
	| ECtor (x, es) -> (l, ECtor (x, List.map (replace_const_symbol (hole, value)) es))
	| ETuple es -> (l, ETuple (List.map (replace_const_symbol (hole, value)) es))
  | MINUS e -> (l, MINUS (replace_const_symbol (hole, value) e))
  | NOT e -> (l, NOT (replace_const_symbol (hole, value) e))
  | EFun (arg, e) -> (l, EFun (arg, (replace_const_symbol (hole, value) e)))
  | Raise e -> (l, Raise (replace_const_symbol (hole, value) e))
  | ADD (e1, e2) -> (l, ADD (replace_const_symbol (hole, value) e1, replace_const_symbol (hole, value) e2))
  | SUB (e1, e2) -> (l, SUB (replace_const_symbol (hole, value) e1, replace_const_symbol (hole, value) e2))
  | MUL (e1, e2) -> (l, MUL (replace_const_symbol (hole, value) e1, replace_const_symbol (hole, value) e2))
  | DIV (e1, e2) -> (l, DIV (replace_const_symbol (hole, value) e1, replace_const_symbol (hole, value) e2))
  | MOD (e1, e2) -> (l, MOD (replace_const_symbol (hole, value) e1, replace_const_symbol (hole, value) e2))
  | OR (e1, e2) -> (l, OR (replace_const_symbol (hole, value) e1, replace_const_symbol (hole, value) e2))
  | AND (e1, e2) -> (l, AND (replace_const_symbol (hole, value) e1, replace_const_symbol (hole, value) e2))
  | LESS (e1, e2) -> (l, LESS (replace_const_symbol (hole, value) e1, replace_const_symbol (hole, value) e2))
  | LARGER (e1, e2) -> (l, LARGER (replace_const_symbol (hole, value) e1, replace_const_symbol (hole, value) e2))
  | EQUAL (e1, e2) -> (l, EQUAL (replace_const_symbol (hole, value) e1, replace_const_symbol (hole, value) e2))
  | NOTEQ (e1, e2) -> (l, NOTEQ (replace_const_symbol (hole, value) e1, replace_const_symbol (hole, value) e2))
  | LESSEQ (e1, e2) -> (l, LESSEQ (replace_const_symbol (hole, value) e1, replace_const_symbol (hole, value) e2))
  | LARGEREQ (e1, e2) -> (l, LARGEREQ (replace_const_symbol (hole, value) e1, replace_const_symbol (hole, value) e2))
  | AT (e1, e2) -> (l, AT (replace_const_symbol (hole, value) e1, replace_const_symbol (hole, value) e2))
  | DOUBLECOLON (e1, e2) -> (l, DOUBLECOLON (replace_const_symbol (hole, value) e1, replace_const_symbol (hole, value) e2))
  | STRCON (e1, e2) -> (l, STRCON (replace_const_symbol (hole, value) e1, replace_const_symbol (hole, value) e2))
  | EApp (e1, e2) -> (l, EApp (replace_const_symbol (hole, value) e1, replace_const_symbol (hole, value) e2))
  | ELet (f, is_rec, args, typ, e1, e2) -> (l, ELet (f, is_rec, args, typ, replace_const_symbol (hole, value) e1, replace_const_symbol (hole, value) e2))
	| EBlock (is_rec, ds, e2) -> 
		let ds = List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, args, typ, replace_const_symbol (hole, value) e)) ds in
		(l, EBlock (is_rec, ds, replace_const_symbol (hole, value) e2))
	| EMatch (e, bs) ->
		let (ps, es) = List.split bs in
		(l, EMatch (replace_const_symbol (hole, value) e, List.combine ps (List.map (replace_const_symbol (hole, value)) es)))
	| IF (e1, e2, e3) -> (l, IF (replace_const_symbol (hole, value) e1, replace_const_symbol (hole, value) e2, replace_const_symbol (hole, value) e3))
  | SStr n | SInt n -> if (n = hole) then (l, value) else (l, e)
  | _ -> (l, e)

(* Main Synthesis Algorithm *)
let rec return_counter_example : prog -> prog -> input -> example option
= fun pgm cpgm input ->
	try
		let v1 = Eval.get_output cpgm input in
		try
			let v2 = Eval.get_output pgm input in
			if not (Eval.value_equality v1 v2) then Some (input, v1) else None
		with e -> Some (input, v1)
	with _ -> None

(*let log = ref (open_out "overhead.txt")*)

let rec work : Workset.t -> components -> prog -> prog -> example option
= fun workset comp pgm cpgm ->
	iter := !iter +1;
	if (Unix.gettimeofday()) -. (!start_time) > 60.0 then None
	(*
  else if (!iter mod 1000 = 0)
	  then
		  begin
			  print_endline ((Workset.workset_info workset) ^ (" Total elapsed : " ^ (string_of_float (Unix.gettimeofday() -. !start_time))));
			  work workset comp pgm cpgm
		  end
	*)
	else
	match Workset.choose workset with
	| None -> None
	| Some ((input, h_t, v_t), remain) ->
		(*let input = List.map (Normalize.normalize_exp) input in*)
		if is_closed input then
			(*let _ = print_endline (Print.input_to_string input) in*)
			(*let _ = Printf.fprintf (!log) "%s\n" (Print.input_to_string input) in*)
			let _ = count := !count + 1 in
	  	let symbols = find_const_symbols input in
	  	if BatSet.is_empty symbols then
	  		(* If input has no symbolic value => checking *)
		  	let ex = return_counter_example pgm cpgm input in
				if ex = None then work remain comp pgm cpgm else ex
			else 
				(* If input has symbolic value => solving *)
				begin match Sym_exec.run pgm cpgm input with
			 	| Some model -> 
			 		let symbol_map = get_const_symbols model symbols in
			 		let input = List.fold_left (fun input (n, v) -> List.map (fun e -> replace_const_symbol (n, v) e) input) input symbol_map in
			  	let ex = return_counter_example pgm cpgm input in
			  	if ex = None then work remain comp pgm cpgm else ex
			 	| None -> 
			 		work remain comp pgm cpgm
				end
		else 
			let nextstates = next comp (input, h_t, v_t) in
			let new_workset = BatSet.fold Workset.add nextstates remain in
			work new_workset comp pgm cpgm

let gen_counter_example : prog -> prog -> example option
= fun pgm cpgm ->
	start_time := Unix.gettimeofday();
	let sketch = get_sketch cpgm in
	let initial_workset = Workset.add sketch Workset.empty in
	let comp = Comp.all_components () in
	(*let comp = Comp.const_comp comp in*)
	let result = work initial_workset comp pgm cpgm in
	result
