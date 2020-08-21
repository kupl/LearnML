open Lang
open Util
open Printf

(*
 ******************************************************
   Code for Synthesizing the "Hole"
 ******************************************************
*)
module Workset = struct
  (* Workset *)
  type work = int * prog * Type.HoleType.t * Type.VariableType.t * Type.Subst.t * examples

  module OrderedType = struct
    type t = work
    let compare (rank1, p1, _, _, _, _) (rank2, p2, _, _ ,_, _) =
    let (c1, c2) = (rank1 + (cost p1), rank2 + (cost p2)) in
      if c1=c2 then 0 else
      if c1>c2 then 1
      else -1
  end

  module Heap = BatHeap.Make (OrderedType)

  (* type of workset : heap * (string set) *)
  type t = Heap.t * string BatSet.t
  let empty = (Heap.empty, BatSet.empty)

  let explored : prog -> t -> bool
  = fun pgm (_, sset) -> BatSet.mem (Print.program_to_string pgm) sset

  let add : work -> t -> t
  = fun (n, pgm, h_t, v_t, subst, ex) (heap, sset) ->
    try
      let pgm = Normalize.normalize pgm in
      if explored pgm (heap, sset) then (heap, sset)
      else (Heap.add (n, pgm, h_t, v_t, subst, ex) heap, BatSet.add (Print.program_to_string pgm) sset)
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
  (* Manipulate components *)

  (* Update bound variables, user-defined constructor *)
  let update_var_components : Type.TEnv.t -> components -> components
  = fun tenv comp ->
    BatMap.foldi (fun var t comp ->
      match t with
      | TCtor (name, ts) ->
        begin match ts with
        | [] -> BatSet.add (gen_label (), ECtor (var, [])) comp
        | hd::tl -> BatSet.add (gen_label (), ECtor (var, [gen_labeled_hole ()])) comp
        end
      | _ -> BatSet.add (gen_label (), EVar var) comp
    ) tenv comp

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
    | _ -> (gen_label (), exp)

  (* Variable component reduction *)
  let except_alias_vars : MustAlias.Sem.equivSet -> components -> components
  = fun alias_info comp ->
    BatSet.fold (fun (p1, p2) comp ->
      match p2 with
      | PVar x -> BatSet.filter (fun (_, exp) -> exp <> EVar x) comp 
      | _ -> comp
  ) alias_info comp
end

type state = lexp * Type.HoleType.t * Type.VariableType.t * Type.Subst.t
type hole = int

let var_num = ref 0
let fresh_var () = (var_num := !var_num +1; "#x_" ^ string_of_int !var_num)

let func_num = ref 0
let fresh_func ()= (func_num := !func_num +1; BindOne ("#f_" ^ string_of_int !func_num))

let rec fresh_arg : typ -> arg
= fun typ ->
  match typ with
  | TTuple ts -> ArgTuple (List.map (fun t -> fresh_arg t) ts)
  | _ -> ArgOne (fresh_var (), typ)

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

(******************************************************)
(* Transition relation rules for type-directed search *)
(******************************************************)
(* Update polymorphic type variable t1 in Hole_Type, Var_Type, Subst to t2 *)
let rec update_polymorphic : (typ * typ) -> (Type.HoleType.t * Type.VariableType.t * Type.Subst.t) -> (Type.HoleType.t * Type.VariableType.t * Type.Subst.t)
= fun (t1, t2) (h_t, v_t, subst) ->
  let subst' = Type.unify subst (Type.Subst.apply t1 subst, Type.Subst.apply t2 subst) in
  let h_t' = Type.HoleType.update subst' h_t in
  let v_t' = Type.VariableType.update subst' v_t in
  (h_t', v_t', subst')

(* One-step Transition *)
let rec update_state : (hole * typ) list -> Type.TEnv.t -> state -> state
= fun ts hole_env (e, h_t, v_t, subst) ->
  List.fold_left (fun (e, h_t, v_t, subst) (hole, typ) ->
    let h_t = Type.HoleType.extend hole typ h_t in
    let v_t = Type.VariableType.extend hole hole_env v_t in
    (e, h_t, v_t, subst)
  ) (e, h_t, v_t, subst) ts

(* Type-directed Transition *)
let rec type_directed : (hole * typ * Type.TEnv.t) -> state -> state option
= fun (hole, hole_typ, hole_env) (lexp, h_t, v_t, subst) ->
  match snd lexp with
  (* const *)
  | Const _ -> 
    begin match hole_typ with
    | TInt -> Some (lexp, h_t, v_t, subst) 
    | TVar _ -> 
      let (h_t, v_t, subst) = update_polymorphic (hole_typ, TInt) (h_t, v_t, subst) in
      Some (lexp, h_t, v_t, subst)
    | _ -> None
    end
  | TRUE | FALSE -> 
    begin match hole_typ with
    | TBool -> Some (lexp, h_t, v_t, subst)
    | TVar _ ->
      let (h_t, v_t, subst) = update_polymorphic (hole_typ, TBool) (h_t, v_t, subst) in
      Some (lexp, h_t, v_t, subst)
    | _ -> None
    end
  | String _ ->
    begin match hole_typ with
    | TString -> Some (lexp, h_t, v_t, subst) 
    | TVar _ ->
      let (h_t, v_t, subst) = update_polymorphic (hole_typ, TString) (h_t, v_t, subst) in
      Some (lexp, h_t, v_t, subst)
    | _ -> None
    end
  (* var comp *)
  | EVar x ->
    let var_typ = BatMap.find x hole_env in
    let (h_t, v_t, subst) = update_polymorphic (hole_typ, var_typ) (h_t, v_t, subst) in
    Some (lexp, h_t, v_t, subst)
  (* aop *)
  | MINUS e -> 
    let n = extract_holenum e in 
    begin match hole_typ with
    |TInt -> Some (update_state [(n, TInt)] hole_env (lexp, h_t, v_t, subst))
    |TVar _ -> 
      let (h_t, v_t, subst) = update_polymorphic (hole_typ, TInt) (h_t, v_t, subst) in
      Some (update_state [(n, TInt)] hole_env (lexp, h_t, v_t, subst))
    |_ -> None
    end
  | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2) -> 
    let (n1, n2) = (extract_holenum e1, extract_holenum e2) in 
    begin match hole_typ with
    | TInt -> Some (update_state [(n1, TInt); (n2, TInt)] hole_env (lexp, h_t, v_t, subst))
    | TVar _ ->
      let (h_t, v_t, subst) = update_polymorphic (hole_typ, TInt) (h_t, v_t, subst) in
      Some (update_state [(n1, TInt); (n2, TInt)] hole_env (lexp, h_t, v_t, subst))
    | _ -> None
    end
  (* bop *)
  | NOT e -> 
    let n = extract_holenum e in 
    begin match hole_typ with
    |TBool -> Some (update_state [(n, TBool)] hole_env (lexp, h_t, v_t, subst))
    |TVar _ -> 
      let (h_t, v_t, subst) = update_polymorphic (hole_typ, TBool) (h_t, v_t, subst) in
      Some (update_state [(n, TBool)] hole_env (lexp, h_t, v_t, subst))
    |_ -> None
    end
  | OR (e1, e2) | AND (e1, e2) -> 
    let (n1, n2) = (extract_holenum e1, extract_holenum e2) in 
    begin match hole_typ with
    | TBool -> Some (update_state [(n1, TBool); (n2, TBool)] hole_env (lexp, h_t, v_t, subst))
    | TVar _ ->
      let (h_t, v_t, subst) = update_polymorphic (hole_typ, TBool) (h_t, v_t, subst) in
      Some (update_state [(n1, TBool); (n2, TBool)] hole_env (lexp, h_t, v_t, subst))
    | _ -> None
    end
  | EQUAL (e1, e2) | NOTEQ (e1, e2) -> 
    let (n1, n2) = (extract_holenum e1, extract_holenum e2) in 
    let tv = fresh_tvar () in
    begin match hole_typ with
    | TBool -> Some (update_state [(n1, tv); (n2, tv)] hole_env (lexp, h_t, v_t, subst))
    | TVar _ ->
      let (h_t, v_t, subst) = update_polymorphic (hole_typ, TBool) (h_t, v_t, subst) in
      Some (update_state [(n1, tv); (n2, tv)] hole_env (lexp, h_t, v_t, subst))
    | _ -> None
    end
  (* abop *)
  | LESS (e1, e2) | LARGER (e1, e2) | LESSEQ (e1, e2) | LARGEREQ (e1, e2) -> 
    let (n1, n2) = (extract_holenum e1, extract_holenum e2) in 
    begin match hole_typ with
    | TBool -> Some (update_state [(n1, TInt); (n2, TInt)] hole_env (lexp, h_t, v_t, subst))
    | TVar _ -> 
      let (h_t, v_t, subst) = update_polymorphic (hole_typ, TBool) (h_t, v_t, subst) in
      Some (update_state [(n1, TInt); (n2, TInt)] hole_env (lexp, h_t, v_t, subst))
    | _ -> None
    end
  (* string *)
  | STRCON (e1, e2) ->
    let (n1, n2) = (extract_holenum e1, extract_holenum e2) in 
    begin match hole_typ with
    | TString -> Some (update_state [(n1, TString); (n2, TString)] hole_env (lexp, h_t, v_t, subst))
    | TVar _ ->
      let (h_t, v_t, subst) = update_polymorphic (hole_typ, TString) (h_t, v_t, subst) in
      Some (update_state [(n1, TString); (n2, TString)] hole_env (lexp, h_t, v_t, subst))
    | _ -> None
    end
  (* list *)
  | AT (e1, e2) -> 
    let (n1, n2) = (extract_holenum e1, extract_holenum e2) in 
    begin match hole_typ with
    | TList t -> Some (update_state [(n1, TList t); (n2, TList t)] hole_env (lexp, h_t, v_t, subst))
    | TVar _ ->
      let tv = fresh_tvar () in
      let (h_t, v_t, subst) = update_polymorphic (hole_typ, TList tv) (h_t, v_t, subst) in
      Some (update_state [(n1, TList tv); (n2, TList tv)] hole_env (lexp, h_t, v_t, subst))
    | _ -> None
    end
  | DOUBLECOLON (e1,e2) -> 
    let (n1, n2) = (extract_holenum e1, extract_holenum e2) in 
    begin match hole_typ with
    | TList t -> Some (update_state [(n1, t); (n2, TList t)] hole_env (lexp, h_t, v_t, subst))
    | TVar _ ->
      let tv = fresh_tvar () in
      let (h_t, v_t, subst) = update_polymorphic (hole_typ, TList tv) (h_t, v_t, subst) in
      Some (update_state [(n1, tv); (n2, TList tv)] hole_env (lexp, h_t, v_t, subst))
    | _ -> None
    end
  | EList es ->
    let ns = List.map (fun e -> extract_holenum e) es in
    begin match hole_typ with
    | TList t -> Some (update_state (List.map (fun n -> (n, t)) ns) hole_env (lexp, h_t, v_t, subst))
    | TVar _ -> 
      let tv = fresh_tvar() in
      let (h_t, v_t, subst) = update_polymorphic (hole_typ, TList tv) (h_t, v_t, subst) in
      Some (update_state (List.map (fun n -> (n, tv)) ns) hole_env (lexp, h_t, v_t, subst))
    | _ -> None
    end
  (* tuple *)
  | ETuple es ->
    let ns = List.map (fun e -> extract_holenum e) es in
    begin match hole_typ with
    | TTuple ts when (List.length ns = List.length ts) -> Some (update_state (List.map2 (fun n typ -> (n, typ)) ns ts) hole_env (lexp, h_t, v_t, subst))
    | TVar _ -> 
      let ts = List.map (fun _ -> fresh_tvar ()) ns in
      let (h_t, v_t, subst) = update_polymorphic (hole_typ, TTuple ts) (h_t, v_t, subst) in
      Some (update_state (List.map2 (fun n typ -> (n, typ)) ns ts) hole_env (lexp, h_t, v_t, subst))
    | _ -> None
    end
  (* ctor *)
  | ECtor (x, es) ->
    let ctor_typ = BatMap.find x hole_env in
    let (tname, ts) = get_ctor_type ctor_typ in
    let holes_typ = List.map2 (fun e typ -> (extract_holenum e, typ)) es ts in
    begin match hole_typ with
    | TBase _ when hole_typ = tname -> Some (update_state holes_typ hole_env (lexp, h_t, v_t, subst))
    | TVar _ -> 
      let (h_t, v_t, subst) = update_polymorphic (hole_typ, tname) (h_t, v_t, subst) in
      Some (update_state holes_typ hole_env (lexp, h_t, v_t, subst))
    | _ -> None
    end
  | IF (e1, e2, e3) -> 
    let (n1, n2, n3) = (extract_holenum e1, extract_holenum e2, extract_holenum e3) in
    Some (update_state [(n1, TBool); (n2, hole_typ); (n3, hole_typ)] hole_env (lexp, h_t, v_t, subst))
  | ELet (f, is_rec, args, typ, e1, e2) ->
    let lexp = (fst lexp, ELet (fresh_func (), is_rec, args, typ, e1, e2)) in
    let (n1, n2) = (extract_holenum e1, extract_holenum e2) in
    let func_typ = Type.type_of_fun args typ in
    let hole_env1 = if is_rec then Type.let_binding (Type.bind_args hole_env args) f func_typ else (Type.bind_args hole_env args) in
    let hole_env2 = Type.let_binding hole_env f func_typ in
    let (lexp, h_t, v_t, subst) = update_state [n1, typ] hole_env1 (lexp, h_t, v_t, subst) in
    Some (update_state [n2, hole_typ] hole_env2 (lexp, h_t, v_t, subst))
  | EFun (arg,  e) ->
    let n = extract_holenum e in
    begin match hole_typ with
    | TArr (t1, t2) ->
      let arg = fresh_arg t1 in
      let hole_env = Type.bind_arg hole_env arg in
      Some (update_state [(n, t2)] hole_env ((fst lexp, EFun (arg, e)), h_t, v_t, subst))
    | TVar _ ->
      let (t1, t2) = (fresh_tvar (), fresh_tvar ()) in
      let arg = fresh_arg t1 in
      let hole_env = Type.bind_arg hole_env arg in
      let (h_t, v_t, subst) = update_polymorphic (hole_typ, TArr (t1, t2)) (h_t, v_t, subst) in
      Some (update_state [(n, t2)] hole_env ((fst lexp, EFun (arg, e)), h_t, v_t, subst))
    | _ -> None
    end
  | EMatch(e, bs) ->
    let (ps, es) = List.split bs in
    (* find type of branch condition *)
    let pat_typ = fresh_tvar () in
    let (tenvs, eqns) = List.fold_left (fun (tenvs, eqns) pat ->
      let (tenv, pat_eqn) = Type.gen_pat_equations (hole_env, eqns) pat pat_typ in
      (tenv::tenvs, pat_eqn@eqns)
    ) ([], []) ps 
    in
    let subst = List.fold_left (fun subst (t1, t2) -> Type.unify subst ((Type.Subst.apply t1 subst), Type.Subst.apply t2 subst)) subst eqns in
    let pat_typ = Type.Subst.apply pat_typ subst in
    (* compute type env of each branch *)
    let tenvs = List.fold_left (fun tenvs tenv ->
      let tenv = BatMap.map (fun typ -> Type.Subst.apply typ subst) tenv in
      tenv::tenvs
    ) [] tenvs 
    in 
    (* Update hole_info of barnch condition *)
    let n = extract_holenum e in
    let (lexp, h_t, v_t, subst) = update_state [(n, pat_typ)] hole_env (lexp, h_t, v_t, subst) in
    (* bound each variable type info to hole in body *)
    let (lexp, h_t, v_t, subst) = List.fold_left2 (fun (lexp, h_t, v_t, subst) e tenv ->
      let n = (extract_holenum e) in
      update_state [(n, hole_typ)] hole_env (lexp, h_t, v_t, subst)
    ) (lexp, h_t, v_t, subst) es tenvs in
    Some (lexp, h_t, v_t, subst)
  | EApp (e1, e2) -> 
    let (n1, n2) = (extract_holenum e1, extract_holenum e2) in
    let tv = fresh_tvar() in
    Some (update_state [(n1, TArr (tv, hole_typ)); (n2, tv)] hole_env (lexp, h_t, v_t, subst))
  | _ -> raise (Failure (Print.exp_to_string lexp ^ " is included in Components set"))
  

(************************************)
(* Get a hole that appears at first *)
(************************************)
let rec find_exp_holes : lexp -> lexp BatSet.t
= fun (l, exp) ->
  match exp with
  | EList es | ECtor (_, es) | ETuple es -> find_exp_holes_list es
  | MINUS e | NOT e | EFun (_, e) | Raise e -> find_exp_holes e
  | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2)
  | OR (e1, e2) | AND (e1, e2) | LESS (e1, e2) | LARGER (e1, e2) | EQUAL (e1, e2) | NOTEQ (e1, e2)
  | LESSEQ (e1, e2) | LARGEREQ (e1, e2) | AT (e1, e2) | DOUBLECOLON (e1, e2) | STRCON (e1, e2)
  | EApp (e1, e2) | ELet (_, _, _, _, e1, e2) -> find_exp_holes_list [e1; e2]
  | EBlock (_, ds, e2) -> 
    let es = List.map (fun (f, is_rec, args, typ, e) -> e) ds in
    find_exp_holes_list (es@[e2])
  | EMatch (e, bs) ->
    let es = e :: (List.map (fun (p, e) -> e) bs) in
    find_exp_holes_list es
  | IF (e1, e2, e3) -> find_exp_holes_list [e1; e2; e3]
  | Hole n -> BatSet.singleton (l, exp)
  | _ -> BatSet.empty

and find_exp_holes_list : lexp list -> lexp BatSet.t
= fun es ->
  match es with
  | [] -> BatSet.empty 
  | hd::tl -> 
    let set = find_exp_holes hd in
    if (BatSet.is_empty set) then find_exp_holes_list tl else set

let find_decl_holes : decl -> lexp BatSet.t 
= fun decl ->
  match decl with
  | DLet (f, is_rec, args, typ, exp) -> find_exp_holes exp
  | DBlock (is_rec, bindings) -> 
    let es = List.map (fun (f, is_rec, args, typ, exp) -> exp) bindings in
    find_exp_holes_list es 
  | _ -> BatSet.empty

let rec find_pgm_holes : prog -> lexp BatSet.t
= fun decls -> 
  match decls with
  | [] -> BatSet.empty 
  | hd::tl -> 
    let set = find_decl_holes hd in
    if (BatSet.is_empty set) then find_pgm_holes tl else set

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

let rec replace_pgm : prog -> (hole * lexp) -> prog
= fun decls (hole, e') -> 
  List.map (fun decl ->
    match decl with
    | DLet (f, is_rec, args, typ, exp) -> DLet (f, is_rec, args, typ, replace_exp exp (hole, e'))
    | DBlock (is_rec, bindings) -> DBlock (is_rec, List.map (fun (f, is_rec, args, typ, exp) -> (f, is_rec, args, typ, (replace_exp exp (hole, e')))) bindings)
    | _ -> decl 
  ) decls

let get_next_states : components -> Workset.work -> lexp -> Workset.work BatSet.t
= fun comp (rank, pgm, h_t, v_t, subst, ex) hole ->
  let n = extract_holenum hole in
  let hole_typ = BatMap.find n h_t in
  let hole_env = BatMap.find n v_t in
  (* Component Extraction *)
  let comp =
    begin match hole_typ with
    | TTuple ts -> BatSet.add (gen_label(), ETuple (List.map (fun _ -> gen_labeled_hole()) ts)) comp
    | _ -> comp
    end 
    |> Comp.update_var_components hole_env
    |> BatSet.map Comp.update_components
  in
  (* Alias Analysis & Component Reduction *)
  let alias_info = MustAlias.Sem.run pgm in
  let comp = Comp.except_alias_vars (MustAlias.Sem.get_aliasSet alias_info n) comp in
  (* Transition *)
  let next_states = BatSet.fold (fun comp set -> 
    let new_state = 
      try 
        type_directed (n, hole_typ, hole_env) (comp, h_t, v_t, subst)
      with _ -> None
    in
    match new_state with
    |Some state -> BatSet.add state set
    |None -> set
  ) comp BatSet.empty in
  (* Replace *)
  BatSet.map (fun (e, h_t, v_t, subst)->
    (rank, replace_pgm pgm (n, e), BatMap.remove n h_t, BatMap.remove n v_t, subst, ex)
  ) next_states 

let next : components -> Workset.work -> Workset.work BatSet.t
= fun comp (rank, pgm, h_t, v_t, subst, ex) ->
  let holes = find_pgm_holes pgm in
  BatSet.fold (fun hole set ->
    BatSet.union (get_next_states comp (rank, pgm, h_t, v_t, subst, ex) hole) set
  ) holes BatSet.empty

(* Main Synthesis Algorithm *)
let start_time = ref 0.0
let iter = ref 0
let count = ref 0
let debug = ref (open_out "debug.txt")

let rec get_output : prog -> input -> value
= fun pgm input ->
  try
    let res_var = "__res__" in
    let pgm = pgm@(External.grading_prog) in
    let pgm' = pgm @ [(DLet (BindOne res_var,false,[],fresh_tvar(),(appify (gen_label(), EVar !Options.opt_entry_func) input)))] in
    let env = Eval.run pgm' in
    lookup_env res_var env
  with e -> failwith (Printexc.to_string e)

let rec is_solution : prog -> examples -> bool
= fun pgm examples ->
  List.for_all (fun (input, output) ->
    try
      Eval.value_equality (get_output pgm input) output
    with
    |TimeoutError -> (*fprintf (!debug) "%s\n" (Print.program_to_string prog);*) false
    |StackOverflow _ -> Eval.infinite_count:=(!Eval.infinite_count)+1; false
    |Stack_overflow -> Eval.infinite_count:=(!Eval.infinite_count)+1; false
    |e -> 
      (*let msg = Printexc.to_string e in
      fprintf (!debug) "%s\n" (Print.program_to_string prog);
      fprintf (!debug) "%s\n" (msg);*)
      false
  ) examples

let is_closed : prog -> bool
= fun pgm -> BatSet.is_empty (find_pgm_holes pgm)

let rec work2 : prog -> Workset.t -> components -> prog option
= fun cpgm workset comp ->
  iter := !iter +1;
  if (Sys.time()) -. (!start_time) > 600.0 then None
  (*
  else if (!iter mod 100 = 0)
    then
      begin
        print_string("Iter : " ^ (string_of_int !iter) ^ " ");
        print_endline((Workset.workset_info workset) ^ (" Total elapsed : " ^ (string_of_float (Sys.time() -. !start_time))));
        work workset comp examples
      end
  *)
  else match Workset.choose workset with
  | None -> None
  | Some ((rank, pgm, h_t, v_t, subst, examples), remain) ->
    if (Infinite.Static.run pgm) then work2 cpgm remain comp
    else
      let _ = fprintf (!debug) "%s\n" ("----------------\n" ^ Print.program_to_string pgm) in
      if is_closed pgm then
        let _ = count := !count +1 in
        if is_solution pgm examples then 
          let new_test = TestGenerator.gen_counter_example pgm cpgm in
          match new_test with
          | None -> Some pgm
          | Some ex ->
            let examples = ex::examples in
            let new_candidates = BatSet.map(
              fun (rank, pgm)->   
                try 
                  let (_, h_t, v_t) = Type.run pgm in
                  (rank, pgm, h_t, v_t, subst, examples)
                with Type.TypeError -> print_endline ("Here"); Print.print_pgm pgm; raise Type.TypeError
            ) (Localize.localization pgm examples) in
            let _ = 
              Print.print_header "Generated examples"; 
              Print.print_examples (List.rev examples);
              Print.print_header "Repair candidate"; Print.print_pgm pgm
            in
            let remain = BatSet.fold Workset.add new_candidates remain in
            work2 cpgm remain comp
        else
          work2 cpgm remain comp
      else if Smt_pruning.smt_pruning pgm examples then
        let nextstates = next comp (rank, pgm, h_t, v_t, subst, examples) in
        let new_workset = BatSet.fold Workset.add nextstates remain in
        work2 cpgm new_workset comp
      else work2 cpgm remain comp

let run : prog -> Workset.work BatSet.t -> components -> prog option
= fun cpgm pgm_set comp ->
  start_time := Unix.gettimeofday();
  let workset = BatSet.fold (fun t set-> Workset.add t set) pgm_set Workset.empty in
  work2 cpgm workset comp 