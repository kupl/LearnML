open Lang
open Util
open Repair_template

(******************************)
(* Synthesize repair template *)
(******************************)

(*
  state = exp * h_t * v_t * subst
*)
type state = lexp * Type2.LocType.t * Type2.LocEnv.t * Type2.Subst.t
and hole = int

module Workset = struct
  (* Workset *)
  type work = state

  module OrderedType = struct
    type t = work
    let compare (e1, _, _, _) (e2, _, _, _) =
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
  = fun (e, l_t, l_env, subst) (heap, sset) ->
    try
      if explored e (heap, sset) then (heap, sset)
      else (Heap.add (e, l_t, l_env, subst) heap, BatSet.add (Print.exp_to_string e) sset)
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

  let init : work -> t
  = fun work -> add work empty
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
  | Hole n -> if (l = hole) then e' else (l, e)
  | _ -> (l, e)

let extract_holenum : lexp -> hole
= fun (l, e) -> 
  match e with
  | Hole n -> l
  | _ -> raise (Failure "Error during obtain hole number")

(* Update polymorphic type variable t1 in Hole_Type, Var_Type, Subst to t2 *)
let rec update_polymorphic : (typ * typ) -> (Type2.LocType.t * Type2.LocEnv.t * Type2.Subst.t) -> (Type2.LocType.t * Type2.LocEnv.t * Type2.Subst.t)
= fun (t1, t2) (l_t, l_env, subst) ->
  let subst' = Type2.unify subst (Type2.Subst.apply t1 subst, Type.Subst.apply t2 subst) in
  let l_t' = Type2.LocType.update subst' l_t in
  let l_env' = Type2.LocEnv.update subst' l_env in
  (l_t', l_env', subst')

(* Type-directed Transition *)
let rec type_directed : (hole * typ * Type2.TEnv.t) -> state -> state option
= fun (hole, hole_typ, hole_env) (lexp, l_t, l_env, subst) ->
  match snd lexp with
  (* var comp *)
  | EVar x ->
    let var_typ = BatMap.find x hole_env in
    let (l_t, l_env, subst) = update_polymorphic (hole_typ, var_typ) (l_t, l_env, subst) in
    Some (lexp, l_t, l_env, subst)
  | _ -> None

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
  | Hole n ->  BatSet.singleton (l, exp)
  | _ -> BatSet.empty

and find_first_hole_list : lexp list-> lexp BatSet.t
= fun es ->
  match es with
  | [] -> BatSet.empty 
  | hd::tl -> 
    let set = find_first_hole hd in
    if (BatSet.is_empty set) then find_first_hole_list tl else set

let get_next_states : Workset.work -> lexp -> Workset.work BatSet.t
= fun (e, l_t, l_env, subst) hole ->
  let n = extract_holenum hole in
  let hole_typ = BatMap.find n l_t in
  let hole_env = BatMap.find n l_env in
  (* Bound variables *)
  let comp = BatMap.foldi (fun var t set ->
    match t with
    | TCtor _ -> set
    | _ -> BatSet.add (gen_label (), EVar var) set
  ) hole_env BatSet.empty in
  (* Transition *)
  let next_states = BatSet.fold (fun comp set -> 
    let new_state = 
      try 
        type_directed (n, hole_typ, hole_env) (comp, l_t, l_env, subst)
      with _ -> None
    in
    match new_state with
    |Some state -> BatSet.add state set
    |None -> set
  ) comp BatSet.empty in
  (* Replace *)
  BatSet.map (fun (e', l_t, l_env, subst)-> (replace_exp e (n, e'), BatMap.remove n l_t, BatMap.remove n l_env, subst)) next_states 

let next : Workset.work -> Workset.work BatSet.t
= fun (e, l_t, l_env, subst) ->
  let holes = find_first_hole e in
  BatSet.fold (fun hole set ->
    BatSet.union (get_next_states (e, l_t, l_env, subst) hole) set
  ) holes BatSet.empty

let is_closed : lexp -> bool
= fun e -> BatSet.is_empty (find_first_hole e)

let rec work : Workset.t -> lexp BatSet.t
= fun workset ->
  match Workset.choose workset with
  | None -> BatSet.empty
  | Some ((e, l_t, l_env, subst), remain) ->
    if is_closed e then 
      BatSet.add e (work remain)
    else
      let nextstates = next (e, l_t, l_env, subst) in
      let new_workset = BatSet.fold Workset.add nextstates remain in
      work new_workset

let rec complete_template : Type2.LocType.t -> Type2.LocEnv.t -> Type2.Subst.t -> exp_template -> exp_templates
= fun l_t l_env subst e_temp ->
  match e_temp with
  | ModifyExp (l, e) -> BatSet.map (fun e -> ModifyExp (l, e)) (work (Workset.init (e, l_t, l_env, subst)))
  | InsertBranch (l, (p, e)) -> BatSet.map (fun e -> InsertBranch (l, (p, e))) (work (Workset.init (e, l_t, l_env, subst)))
  | _ -> BatSet.singleton e_temp