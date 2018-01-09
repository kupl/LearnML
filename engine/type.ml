open Lang
open Util
open Print

type typ_eqn = (typ * typ) list
type hole_env = (int, id) BatMap.t
type hole_table = (int, typ) BatMap.t 

exception TypeError

let start_time = ref 0.0

let hole_map = ref BatMap.empty
let hole_tbl = ref BatMap.empty

(* type environment : var -> type *)
module TEnv = struct
  type t = (id, typ) BatMap.t
  let empty = BatMap.empty
  let extend (x,t) tenv = BatMap.add x t tenv
  let find tenv x = BatMap.find x tenv
  let rec print tenv = 
    BatMap.iter (fun id typ -> 
      print_endline(id^"|->"^(type_to_string typ)^"\n")) tenv
end

type at_hole_env = (int, TEnv.t) BatMap.t

let at_hole_table = ref BatMap.empty
let at_hole_ttbl = ref BatMap.empty

(* substitution *)
module Subst = struct
  type t = (id * typ) list
  let empty = []
  let find x subst = List.assoc x subst

  (* walk through the type, replacing each type variable by its binding in the substitution *)
  let rec apply : typ -> t -> typ
  = fun typ subst ->
    match typ with
    | TList t -> TList (apply t subst)
    | TTuple l ->  TTuple (apply_to_list l subst)
    | TCtor (t1, l) -> TCtor (apply t1 subst, apply_to_list l subst)
    | TArr (t1,t2) -> TArr (apply t1 subst, apply t2 subst)
    | TVar x -> 
      begin 
        try find x subst
        with _ -> typ
      end
    |_ -> typ

  and apply_to_list : typ list -> t -> typ list
  = fun l subst -> list_map (fun t -> apply t subst) l
  (* add a binding (tv,ty) to the subsutition and propagate the information *)
  let extend tv ty subst = 
    (tv,ty) :: (List.map (fun (x,t) -> (x, apply t [(tv,ty)])) subst)

  let print : t -> unit
  = fun subst -> 
      List.iter (fun (x,ty) -> print_endline (x ^ " |-> " ^ type_to_string ty)) subst
end

(* generate a fresh type variable *)
let tvar_num = ref 0
let fresh_tvar () = (tvar_num := !tvar_num + 1; (TVar ("#" ^ string_of_int !tvar_num)))

(* Support functions *)
let rec bind_arg : TEnv.t -> arg -> TEnv.t
= fun tenv arg ->
  match arg with
  | ArgOne (x, t) -> TEnv.extend (x, t) tenv
  | ArgTuple xs -> List.fold_left bind_arg tenv xs

let rec bind_args : TEnv.t -> arg list -> TEnv.t
= fun tenv args -> List.fold_left bind_arg tenv args

let rec type_of_arg : arg -> typ
= fun arg ->
  match arg with
  | ArgOne (x, t) -> t
  | ArgTuple xs -> TTuple (List.map type_of_arg xs)

let rec type_of_fun : arg list -> typ -> typ
= fun args typ ->
  match args with
  | [] -> typ
  | hd::tl -> TArr (type_of_arg hd, type_of_fun tl typ)

(* construct type eqn of patterns then bind type variablese *)
let rec gen_pat_equations : (TEnv.t * typ_eqn) -> pat -> typ -> (TEnv.t * typ_eqn)
= fun (tenv, eqn) pat ty ->
  match pat with
  | PInt n -> (tenv, (ty, TInt)::eqn)
  | PBool b -> (tenv, (ty, TBool)::eqn)
  | PVar x -> 
    let t = fresh_tvar () in
    (TEnv.extend (x, t) tenv, (ty, t)::eqn)
  | PList ps ->
    let t = fresh_tvar () in
    gen_pat_list_equations (tenv, (ty, TList t)::eqn) ps t
  | PTuple ps ->
    let (env, eqn, ts) = 
      List.fold_left (
        fun (env, eqn, ts) pat ->
          let t = fresh_tvar () in
          let (env, eqn) = gen_pat_equations (env, eqn) pat t in
          (env, eqn, ts@[t])
      ) (tenv, eqn, []) ps
    in
    (env, (ty, TTuple ts)::eqn)
  | PCtor (x, ps) ->
    let tctor = TEnv.find tenv x in
    begin match tctor with
    | TCtor (t_base, ts) -> List.fold_left2 (fun (env, eqn) pat typ -> gen_pat_equations (env, eqn) pat typ) (tenv, (ty, t_base)::eqn) ps ts
    | _ -> raise TypeError
    end
  | PCons ps ->
    let t = fresh_tvar () in
    gen_pat_cons_equations (tenv, [ty, TList t]) ps t
  | PUnder -> (tenv, (ty, fresh_tvar ())::eqn)
  | Pats ps -> gen_pat_list_equations (tenv, eqn) ps ty

and gen_pat_list_equations : (TEnv.t * typ_eqn) -> pat list -> typ -> (TEnv.t * typ_eqn)
= fun (tenv, eqn) ps typ -> 
  List.fold_left (fun (tenv, eqn) pat -> gen_pat_equations (tenv, eqn) pat typ) (tenv, eqn) ps 

and gen_pat_cons_equations : (TEnv.t * typ_eqn) -> pat list -> typ -> (TEnv.t * typ_eqn)
= fun (tenv, eqn) ps typ ->
  match ps with
  | [] -> raise (Failure "Pattern cons does not have args")
  | [p] -> gen_pat_equations (tenv, eqn) p (TList typ)
  | hd::tl ->
    let (env, eqn) = gen_pat_equations (tenv, eqn) hd typ in
    gen_pat_cons_equations (env, eqn) tl typ

(* Construct type equations of expressions *)
let rec gen_equations : TEnv.t -> exp -> typ -> typ_eqn
= fun tenv exp ty ->
  match exp with
  | Const n -> [(ty, TInt)]
  | TRUE | FALSE -> [(ty, TBool)]
  | String str -> [(ty, TString)]
  | EVar x -> [(ty, TEnv.find tenv x)]
  | EList es -> 
    let t = fresh_tvar () in
    (ty, TList t)::(List.fold_left (fun eqn exp -> eqn@(gen_equations tenv exp t)) [] es)
  | ETuple es ->
    let (eqn, ts) = List.fold_left (
      fun (eqn, ts) exp -> 
        let t = fresh_tvar () in
        ((gen_equations tenv exp t), ts@[t])
    ) ([], []) es 
    in
    (ty, TTuple ts)::eqn
  | ECtor (x, es) -> 
    let tctor = TEnv.find tenv x in
    begin match tctor with
    | TCtor (t_base, ts) -> List.fold_left2 (fun eqn exp typ -> gen_equations tenv exp typ) [ty, t_base] es ts
    | _ -> raise TypeError
    end
  | MINUS e -> (ty, TInt)::(gen_equations tenv e TInt)
  | NOT e -> (ty, TBool)::(gen_equations tenv e TBool)
  | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2) -> (ty, TInt)::((gen_equations tenv e1 TInt)@(gen_equations tenv e2 TInt))
  | OR (e1, e2) | AND (e1, e2) -> (ty, TBool)::((gen_equations tenv e1 TBool)@(gen_equations tenv e2 TBool))
  | LESS (e1, e2) | LARGER (e1, e2) | LESSEQ (e1, e2) | LARGEREQ (e1, e2) -> (ty, TBool)::((gen_equations tenv e1 TInt)@(gen_equations tenv e2 TInt))
  | EQUAL (e1, e2) | NOTEQ (e1, e2) -> 
    let t = fresh_tvar () in
    (ty, TBool)::((gen_equations tenv e1 t)@(gen_equations tenv e2 t))
  | AT (e1, e2) ->
    let t = TList (fresh_tvar ()) in
    (ty, t)::((gen_equations tenv e1 t)@(gen_equations tenv e2 t))
  | DOUBLECOLON (e1, e2) ->
    let t = fresh_tvar () in
    (ty, TList t)::((gen_equations tenv e1 t)@(gen_equations tenv e2 (TList t)))
  | IF (e1, e2, e3) -> (gen_equations tenv e1 TBool)@(gen_equations tenv e2 ty)@(gen_equations tenv e3 ty)
  | ELet (f, is_rec, args, typ, e1, e2) ->
    begin match args with
    | [] ->
      let t =fresh_tvar () in
      (typ, t)::((gen_equations tenv e1 t)@(gen_equations (TEnv.extend (f, t) tenv) e2 ty))
    | _ -> 
      let t = fresh_tvar () in
      let (func_typ, args_env) = (type_of_fun args t, bind_args tenv args) in
      (typ, func_typ)::
      (gen_equations (if is_rec then TEnv.extend (f, func_typ) args_env else args_env) e1 t)@
      (gen_equations (TEnv.extend (f, func_typ) tenv) e2 ty)
    end
  | EFun (arg, e) ->
    let t1 = type_of_arg arg in
    let t2 = fresh_tvar () in
    (ty, TArr (t1, t2))::(gen_equations (bind_arg tenv arg) e t2)
  | EApp (e1, e2) ->
    let t = fresh_tvar () in
    (gen_equations tenv e1 (TArr (t, ty)))@(gen_equations tenv e2 t)
  | EMatch (e, bs) ->
    let (ps, es) = List.split bs in
    let typ_pat = fresh_tvar () in
    let typ_exp = fresh_tvar () in
    let branch_eqn = List.fold_left2 (
      fun eqn pat exp -> 
        let (env, eqn) = gen_pat_equations (tenv, eqn) pat typ_pat in
        eqn@(gen_equations env exp typ_exp)
      ) [] ps es in
    (ty, typ_exp)::((gen_equations tenv e typ_pat)@branch_eqn)
  | Hole n ->
    let t = fresh_tvar () in
    let _ =
      (* Update Info for type-directed synthesis *)
      hole_map := BatMap.add n t (!hole_map);
      at_hole_table := BatMap.add n tenv (!at_hole_table);
    in
    [(ty, t)]

(* Unification *)
let rec extract_tvar : id -> typ -> bool
= fun x t ->
  match t with
  |TList t -> extract_tvar x t
  |TTuple l 
  |TCtor (_, l) -> extract_tvar2 x l
  |TArr (t1, t2) -> (extract_tvar x t1) || (extract_tvar x t2)
  |TVar y -> (x=y)
  |_ -> false

and extract_tvar2 : id -> typ list -> bool
= fun x lst -> List.exists (fun t -> extract_tvar x t) lst

let rec unify : typ -> typ -> Subst.t -> Subst.t
= fun typ1 typ2 subst ->
  if(typ1=typ2) then subst
  else match (typ1, typ2) with
  |(TList t1, TList t2) -> unify t1 t2 subst
  |(TTuple tl1, TTuple tl2) -> unify_list tl1 tl2 subst
  |(TCtor (id1, tl1), TCtor (id2, tl2)) ->
    if id1 <> id2 then raise TypeError
    else unify_list tl1 tl2 subst
  |(TArr(t1, t2), TArr(t1',t2')) ->
    let subst' = unify t1 t1' subst in
    let t3 = Subst.apply t2 subst' in
    let t4 = Subst.apply t2' subst' in
      unify t3 t4 subst'
  |(TVar x, typ) -> 
    (*if x occurs in typ, raise type error*)
    begin
      match typ with 
      |TVar y -> 
        if x = y then subst else Subst.extend x typ subst
      |TList t1 ->
        if extract_tvar x t1 then raise TypeError
        else Subst.extend x typ subst
      |TTuple tl -> 
        if extract_tvar2 x tl then raise TypeError
        else Subst.extend x typ subst
      |TCtor (id, tl) ->
        if extract_tvar2 x tl then raise TypeError
        else Subst.extend x typ subst
      |TArr (t1, t2) ->
        if ((extract_tvar x t1) || (extract_tvar x t2)) then raise TypeError
        else Subst.extend x typ subst
      |_ -> Subst.extend x typ subst
    end
  |(typ, TVar x) -> unify (TVar x) typ subst
  |(_, _) -> raise TypeError

and unify_list : typ list -> typ list -> Subst.t -> Subst.t
= fun l1 l2 subst ->
  begin match (l1, l2) with
  | ([], []) -> subst
  | ((hd1::tl1),(hd2::tl2)) -> unify_list tl1 tl2 (unify hd1 hd2 subst)
  | _ -> raise TypeError
  end

let rec unify_all : typ_eqn -> Subst.t -> Subst.t
= fun eqns subst ->
  match eqns with
  |[] -> subst
  |(typ1, typ2)::tl ->
    let subst' = unify (Subst.apply typ1 subst) (Subst.apply typ2 subst) subst in
    unify_all tl subst'
  
let solve : typ_eqn -> Subst.t
= fun eqns -> unify_all eqns Subst.empty

let rec fill_hole_tbl subst holes= 
  if(BatMap.is_empty holes) then ()
  else
    let ((n,id),remain) = BatMap.pop holes in
    let ty = Subst.apply id subst in
    try 
      let _ = BatMap.find n !hole_tbl in
      fill_hole_tbl subst remain
    with _ ->
      let _ = hole_tbl := BatMap.add n ty !hole_tbl in
      fill_hole_tbl subst remain

let rec complete_tenv subst env_table =
  if(BatMap.is_empty env_table) then ()
  else
    let ((n,env),remain) = BatMap.pop env_table in
    let rec change_env env result_env=
      if(BatMap.is_empty env) then result_env
      else 
        let ((id,t),remain) = BatMap.pop env in
        let result_env = BatMap.add id (Subst.apply t subst) result_env in
        change_env remain result_env
    in
    let result_env = change_env env BatMap.empty in
    let _ = at_hole_ttbl:= BatMap.add n result_env (!at_hole_ttbl) in
    complete_tenv subst remain

let typeof : exp -> TEnv.t -> typ -> typ
= fun exp tenv typ->
  let new_tv = fresh_tvar () in
  let eqns = gen_equations tenv exp new_tv in
  let eqns = (new_tv,typ)::eqns in
  let subst = solve eqns in
  let ty = Subst.apply new_tv subst in
  let _ = complete_tenv subst !at_hole_table in
  let _ = fill_hole_tbl subst !hole_map in
    ty

let rec ctors_to_env : ctor list -> TEnv.t -> typ -> TEnv.t
= fun ctors tenv tbase -> list_fold(fun (id,typs) env -> TEnv.extend (id,TCtor(tbase,typs)) env) ctors tenv

let type_decl : decl -> TEnv.t -> TEnv.t
= fun decl tenv -> 
  at_hole_table:=BatMap.empty;
  match decl with
  | DData (id, ctors) -> 
    let tbase = TBase id in
    ctors_to_env ctors tenv tbase
  | DLet (x,is_rec,args,typ,exp) -> 
    begin match args with
    | [] -> (* variable binding *)
      let ty = typeof exp (TEnv.extend (x, typ) tenv) typ in
      TEnv.extend (x, ty) tenv
    | _ ->  (* function binding *)
      let e = ELet(x, is_rec, args, typ, exp, EVar(x)) in
      let ty = typeof e (TEnv.extend (x, typ) tenv) typ in
      TEnv.extend (x, ty) tenv
    end

let run : prog -> prog
= fun decls -> 
  let _ = hole_map:=BatMap.empty in
  let _ = hole_tbl:=BatMap.empty in
  let _ = at_hole_ttbl:=BatMap.empty in
  let _ = start_time:=Sys.time() in
  let _ = (list_fold type_decl decls TEnv.empty) in
  decls
