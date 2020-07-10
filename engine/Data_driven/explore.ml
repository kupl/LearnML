open Lang
open Util
open Repair_template

(* Explore -> gen partial program *)
let rec gen_partial_exp : label -> lexp -> lexp
= fun label (l, exp) ->
  if l = label then (l, gen_hole ())
  else 
    let exp = 
      match exp with
      | NOT e -> NOT (gen_partial_exp label e)
      | MINUS e -> MINUS (gen_partial_exp label e)
      | EFun (arg, e) -> EFun (arg, gen_partial_exp label e)
      | Raise e -> Raise (gen_partial_exp label e)
      | ADD (e1, e2) -> ADD (gen_partial_exp label e1, gen_partial_exp label e2)
      | SUB (e1, e2) -> SUB (gen_partial_exp label e1, gen_partial_exp label e2)
      | MUL (e1, e2) -> MUL (gen_partial_exp label e1, gen_partial_exp label e2)
      | DIV (e1, e2) -> DIV (gen_partial_exp label e1, gen_partial_exp label e2)
      | MOD (e1, e2) -> MOD (gen_partial_exp label e1, gen_partial_exp label e2)
      | OR (e1, e2) -> OR (gen_partial_exp label e1, gen_partial_exp label e2)
      | AND (e1, e2) -> AND (gen_partial_exp label e1, gen_partial_exp label e2)
      | LESS (e1, e2) -> LESS (gen_partial_exp label e1, gen_partial_exp label e2)
      | LARGER (e1, e2) -> LARGER (gen_partial_exp label e1, gen_partial_exp label e2)
      | EQUAL (e1, e2) -> EQUAL (gen_partial_exp label e1, gen_partial_exp label e2)
      | NOTEQ (e1, e2) -> NOTEQ (gen_partial_exp label e1, gen_partial_exp label e2)
      | LESSEQ (e1, e2) -> LESSEQ (gen_partial_exp label e1, gen_partial_exp label e2)
      | LARGEREQ (e1, e2) -> LARGEREQ (gen_partial_exp label e1, gen_partial_exp label e2)
      | AT (e1, e2) -> AT (gen_partial_exp label e1, gen_partial_exp label e2)
      | DOUBLECOLON (e1, e2) -> DOUBLECOLON (gen_partial_exp label e1, gen_partial_exp label e2)
      | STRCON (e1, e2) -> STRCON (gen_partial_exp label e1, gen_partial_exp label e2)
      | EApp (e1, e2) -> EApp (gen_partial_exp label e1, gen_partial_exp label e2)
      | ELet (f, is_rec, args, t, e1, e2) -> ELet (f, is_rec, args, t, gen_partial_exp label e1, gen_partial_exp label e2)
      | IF (e1, e2, e3) -> IF (gen_partial_exp label e1, gen_partial_exp label e2, gen_partial_exp label e3)
      | ECtor (x, es) -> ECtor (x, List.map (gen_partial_exp label) es)
      | EList es -> EList (List.map (gen_partial_exp label) es)
      | ETuple es -> ETuple (List.map (gen_partial_exp label) es)
      | EBlock (is_rec, bindings, e) ->
        let bindings = List.map (fun (f, is_rec, args, t, e) -> (f, is_rec, args, t, gen_partial_exp label e)) bindings in
        EBlock (is_rec, bindings, gen_partial_exp label e)
      | EMatch (e, bs) -> EMatch (gen_partial_exp label e, List.map (fun (p, e) -> (p,gen_partial_exp label e)) bs)
      | _ -> exp
      in
    (l, exp) 

let gen_partial_decl : label -> decl -> decl
= fun label decl ->
  match decl with
  | DLet (f, is_rec, args, t, e) -> DLet (f, is_rec, args, t, gen_partial_exp label e)
  | DBlock (is_rec, bindings) ->
    let bindings = List.map (fun (f, is_rec, args, t, e) -> (f, is_rec, args, t, gen_partial_exp label e)) bindings in
    DBlock (is_rec, bindings) 
  | _ -> decl

let gen_partial_pgm : label -> prog -> prog
= fun label pgm -> List.map (fun decl -> gen_partial_decl label decl) pgm

(*
  state = exp * h_t * v_t * subst * possible functions
*)
type state = lexp * Type.HoleType.t * Type.VariableType.t * Type.Subst.t * id BatSet.t
type hole = int

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
  = fun (e, h_t, v_t, subst, funcs) (heap, sset) ->
    try
      if explored e (heap, sset) then (heap, sset)
      else (Heap.add (e, h_t, v_t, subst, funcs) heap, BatSet.add (Print.exp_to_string e) sset)
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
  | Hole n -> if (n = hole) then e' else (l, e)
  | _ -> (l, e)

let extract_holenum : lexp -> hole
= fun e -> 
  match snd e with
  | Hole n -> n
  | _ -> raise (Failure "Error during obtain hole number")

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

(* Update polymorphic type variable t1 in Hole_Type, Var_Type, Subst to t2 *)
let rec update_polymorphic : (typ * typ) -> (Type.HoleType.t * Type.VariableType.t * Type.Subst.t) -> (Type.HoleType.t * Type.VariableType.t * Type.Subst.t)
= fun (t1, t2) (h_t, v_t, subst) ->
  let subst' = Type.unify subst (Type.Subst.apply t1 subst, Type.Subst.apply t2 subst) in
  let h_t' = Type.HoleType.update subst' h_t in
  let v_t' = Type.VariableType.update subst' v_t in
  (h_t', v_t', subst')

(* Type-directed Transition *)
let rec type_directed : (hole * typ * Type.TEnv.t) -> state -> state option
= fun (hole, hole_typ, hole_env) (lexp, h_t, v_t, subst, funcs) ->
  match snd lexp with
  (* Function application *)
  | EApp (e1, e2) -> 
    let (n1, n2) = (extract_holenum e1, extract_holenum e2) in
    let tv = fresh_tvar () in
    let h_t = 
      BatMap.add n1 (TArr (tv, hole_typ)) h_t 
      |> BatMap.add n2 tv
    in
    let v_t = 
      BatMap.add n1 hole_env v_t
      |> BatMap.add n2 hole_env
    in
    if BatMap.exists (fun x typ -> BatSet.mem x funcs && Type.check_typs typ (TArr (tv, hole_typ))) hole_env then
      Some (lexp, h_t, v_t, subst, funcs)
    else None
  (* Constructor *)
  | ECtor (x, es) ->
    let ctor_typ = BatMap.find x hole_env in
    begin match ctor_typ with 
    | TCtor (tname, ts) ->
      let holes = List.map (fun e -> extract_holenum e) es in
      let h_t = List.fold_left2 (fun h_t hole typ -> BatMap.add hole typ h_t) h_t holes ts in
      let v_t = List.fold_left (fun v_t hole -> BatMap.add hole hole_env v_t) v_t holes in
      let (h_t, v_t, subst) = update_polymorphic (hole_typ, tname) (h_t, v_t, subst) in
      Some (lexp, h_t, v_t, subst, funcs)
    | _ -> None
    end 
  (* var comp *)
  | EVar x ->
    let var_typ = BatMap.find x hole_env in
    let (h_t, v_t, subst) = update_polymorphic (hole_typ, var_typ) (h_t, v_t, subst) in
    (* when a function whose name is x is used, do not use it at next transition *)
    Some (lexp, h_t, v_t, subst, BatSet.remove x funcs)
  (* Undefined components *)
  | _ -> None

let get_next_states : Workset.work -> lexp -> Workset.work BatSet.t
= fun (e, h_t, v_t, subst, funcs) hole ->
  let n = extract_holenum hole in
  let hole_typ = BatMap.find n h_t in
  let hole_env = BatMap.find n v_t in
  (* Bound variables & User_defined constructors *)
  let comp = BatMap.foldi (fun var t set ->
    match t with
    | TCtor (name, ts) ->
      begin match ts with
      | [] -> BatSet.add (gen_label (), ECtor (var, [])) set
      (*
      | hd::tl -> BatSet.add (gen_label (), ECtor (var, [gen_labeled_hole ()])) set
      *)
      | _ -> set
      end
    | _ -> BatSet.add (0, EVar var) set
  ) hole_env BatSet.empty in
  (* Function application *)
  let comp = BatSet.add (gen_label (), EApp (gen_labeled_hole (), gen_labeled_hole ())) comp in
  (* Transition *)
  let next_states = BatSet.fold (fun comp set -> 
    let new_state = 
      try 
        type_directed (n, hole_typ, hole_env) (comp, h_t, v_t, subst, funcs)
      with _ -> None
    in
    match new_state with
    |Some state -> BatSet.add state set
    |None -> set
  ) comp BatSet.empty in
  (* Replace *)
  BatSet.map (fun (e', h_t, v_t, subst, funcs)-> (replace_exp e (n, e'), BatMap.remove n h_t, BatMap.remove n v_t, subst, funcs)) next_states 

let next : Workset.work -> Workset.work BatSet.t
= fun (e, h_t, v_t, subst, funcs) ->
  let holes = find_first_hole e in
  BatSet.fold (fun hole set ->
    BatSet.union (get_next_states (e, h_t, v_t, subst, funcs) hole) set
  ) holes BatSet.empty

let is_closed : lexp -> bool
= fun e -> BatSet.is_empty (find_first_hole e)

let rec work : Workset.t -> lexp BatSet.t
= fun workset ->
  match Workset.choose workset with
  | None -> BatSet.empty
  | Some ((e, h_t, v_t, subst, funcs), remain) ->
    if is_closed e then 
      BatSet.add e (work remain)
    else
      let nextstates = next (e, h_t, v_t, subst, funcs) in
      let new_workset = BatSet.fold Workset.add nextstates remain in
      work new_workset

let rec explore : label BatSet.t -> prog -> repair_template BatSet.t 
= fun labels pgm ->
  BatSet.fold (fun l acc->
    let pgm' = gen_partial_pgm l pgm in
    let (_, h_t, v_t, subst) = Type.run pgm' in
    let hole = (gen_label (), Hole !hole_count) in
    let funcs = BatMap.foldi (fun x typ acc -> 
      match typ with
      | TArr _ -> BatSet.add x acc
      | _ -> acc
    ) (BatMap.find !hole_count v_t) BatSet.empty in
    let workset = Workset.init (hole, h_t, v_t, subst, funcs) in
    let temps = BatSet.map (fun e -> (ModifyExp (l, e), BatMap.empty)) (work workset) in
    BatSet.union acc temps
  ) labels BatSet.empty