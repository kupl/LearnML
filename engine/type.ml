open Lang
open Util
open Print

type typ_eqn = (typ * typ) list

exception TypeError

let start_time = ref 0.0


(* type environment : var -> type *)
module TEnv = struct
  type t = (id, typ) BatMap.t
  let empty = BatMap.empty
  let extend : id * typ -> t ->t 
  = fun (x,t) tenv -> BatMap.add x t tenv
  let find tenv x = BatMap.find x tenv
  let rec print tenv = 
    BatMap.iter (fun id typ -> 
      print_endline(id^"|->"^(type_to_string typ))) tenv
end


(*let at_hole_table = ref BatMap.empty
let at_hole_ttbl = ref BatMap.empty
*)
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

module HoleType = struct
  type t = (int, typ) BatMap.t
  
  let empty = BatMap.empty 
  let extend : int -> typ -> t -> t
  = fun hole typ env -> BatMap.add hole typ env
  
  let update : Subst.t -> t -> t
  = fun subst env -> BatMap.map (fun typ -> Subst.apply typ subst) env 

  let print : t -> unit
  = fun env -> BatMap.iter (fun num typ -> print_endline(string_of_int num ^ " -> " ^ type_to_string typ)) env
end

module VariableType = struct
  type t = (int,TEnv.t) BatMap.t
  let empty = BatMap.empty
  let extend : int -> TEnv.t -> t -> t
  = fun hole env varenv -> BatMap.add hole env varenv

  let update : Subst.t -> t -> t
  = fun subst varenv -> BatMap.map (fun tenv -> 
      BatMap.map (fun typ -> Subst.apply typ subst) tenv
    ) varenv

  let print : t -> unit
  = fun varenv -> BatMap.iter (fun num env ->print_endline(string_of_int num);TEnv.print env ) varenv
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
  | PUnit -> (tenv, (ty, TUnit)::eqn)
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
let rec gen_equations : HoleType.t -> VariableType.t -> TEnv.t -> exp -> typ -> typ_eqn * HoleType.t * VariableType.t
= fun hole_typ var_typ tenv exp ty ->
  match exp with
  | EUnit -> ([ty, TUnit], hole_typ, var_typ)
  | Const n -> ([(ty, TInt)],hole_typ,var_typ)
  | TRUE | FALSE -> ([(ty, TBool)],hole_typ,var_typ)
  | String str -> ([(ty, TString)],hole_typ,var_typ)
  | EVar x -> ([(ty, TEnv.find tenv x)],hole_typ,var_typ)
  | EList es -> 
    let t = fresh_tvar () in
    list_fold (fun exp (eqns,h_t,v_t)->
      let (e,h,v) = gen_equations h_t v_t tenv exp t in
      (eqns@e,h,v)
    ) es ([ty,TList t],hole_typ,var_typ) 
  | ETuple es ->
    let (eqns,ts,hole_typ,var_typ) = list_fold (fun exp (eqn,ts,h_t,v_t)->
      let t = fresh_tvar() in
      let (e,h,v) = gen_equations h_t v_t tenv exp t in
      (eqn@e,ts@[t],h,v)
    ) es ([],[],hole_typ,var_typ) in
    ((ty,TTuple ts)::eqns,hole_typ,var_typ)
  | ECtor (x, es) -> 
    let tctor = TEnv.find tenv x in
    begin match tctor with
    | TCtor (t_base, ts) -> list_fold(fun (exp,t) (eqns,h_t,v_t)->
        let (e,h,v) = gen_equations h_t v_t tenv exp t in
        (eqns@e,h,v)
    ) (list_combine es ts) ([ty,t_base],hole_typ,var_typ)
    | _ -> raise TypeError
    end
  | MINUS e -> 
    let (eqns,hole_typ,var_typ) = gen_equations hole_typ var_typ tenv e TInt in 
    ((ty,TInt)::eqns,hole_typ,var_typ)
  | NOT e -> 
    let (eqns,hole_typ,var_typ) = gen_equations hole_typ var_typ tenv e TBool in 
    ((ty,TBool)::eqns,hole_typ,var_typ)
  | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2) -> 
    let (eqns,hole_typ,var_typ) = gen_equations hole_typ var_typ tenv e1 TInt in
    let (eqns',hole_typ,var_typ) = gen_equations hole_typ var_typ tenv e2 TInt in
    ((ty,TInt)::(eqns@eqns'),hole_typ,var_typ)
  | OR (e1, e2) | AND (e1, e2) -> 
    let (eqns,hole_typ,var_typ) = gen_equations hole_typ var_typ tenv e1 TBool in
    let (eqns',hole_typ,var_typ) = gen_equations hole_typ var_typ tenv e2 TBool in
    ((ty,TBool)::(eqns@eqns'),hole_typ,var_typ) 
  | LESS (e1, e2) | LARGER (e1, e2) | LESSEQ (e1, e2) | LARGEREQ (e1, e2) -> 
    let (eqns,hole_typ,var_typ) = gen_equations hole_typ var_typ tenv e1 TInt in
    let (eqns',hole_typ,var_typ) = gen_equations hole_typ var_typ tenv e2 TInt in
    ((ty,TBool)::(eqns@eqns'),hole_typ,var_typ) 
  | EQUAL (e1, e2) | NOTEQ (e1, e2) -> 
    let t = fresh_tvar () in
    let (eqns,hole_typ,var_typ) = gen_equations hole_typ var_typ tenv e1 t in
    let (eqns',hole_typ,var_typ) = gen_equations hole_typ var_typ tenv e2 t in
    ((ty,TBool)::(eqns@eqns'),hole_typ,var_typ) 
  | AT (e1, e2) ->
    let t = TList (fresh_tvar ()) in
    let (eqns,hole_typ,var_typ) = gen_equations hole_typ var_typ tenv e1 t in
    let (eqns',hole_typ,var_typ) = gen_equations hole_typ var_typ tenv e2 t in
    ((ty,t)::(eqns@eqns'),hole_typ,var_typ) 
  | DOUBLECOLON (e1, e2) ->
    let t = fresh_tvar () in
    let (eqns,hole_typ,var_typ) = gen_equations hole_typ var_typ tenv e1 t in
    let (eqns',hole_typ,var_typ) = gen_equations hole_typ var_typ tenv e2 (TList t) in
    ((ty,TList t)::(eqns@eqns'),hole_typ,var_typ) 
  | IF (e1, e2, e3) -> 
    let (eqns,hole_typ,var_typ) = gen_equations hole_typ var_typ tenv e1 TBool in
    let (eqns',hole_typ,var_typ) = gen_equations hole_typ var_typ tenv e2 ty in
    let (eqns'',hole_typ,var_typ) = gen_equations hole_typ var_typ tenv e3 ty in
    (eqns@eqns'@eqns'',hole_typ,var_typ)
  | ELet (f, is_rec, args, typ, e1, e2) ->
    begin match args with
    | [] ->
      let tenv = if(is_rec) then TEnv.extend (f,typ) tenv else tenv in
      let tenv' = TEnv.extend (f,typ) tenv in
      let (eqns,hole_typ,var_typ) = gen_equations hole_typ var_typ tenv e1 typ in
      let (eqns',hole_typ,var_typ) = gen_equations hole_typ var_typ tenv' e2 ty in
      ((eqns@eqns'),hole_typ,var_typ)
    | _ -> 
      let (func_typ, args_env) = (type_of_fun args typ, bind_args tenv args) in
      let tenv = if is_rec then TEnv.extend (f,func_typ) args_env else args_env in
      let tenv' = TEnv.extend (f,func_typ) tenv in
      let (eqns,hole_typ,var_typ) = gen_equations hole_typ var_typ tenv e1 typ in
      let (eqns',hole_typ,var_typ) = gen_equations hole_typ var_typ tenv' e2 ty in
      ((eqns@eqns'),hole_typ,var_typ)
    end
  | EFun (arg, e) ->
    let t1 = type_of_arg arg in
    let t2 = fresh_tvar () in
    let tenv = bind_arg tenv arg in
    let (eqns,hole_typ,var_typ) = gen_equations hole_typ var_typ tenv e t2 in
    ((ty,TArr (t1,t2))::eqns,hole_typ,var_typ)
  | EApp (e1, e2) ->
    let t = fresh_tvar () in
    let (eqns,hole_typ,var_typ) = gen_equations hole_typ var_typ tenv e1 (TArr (t,ty)) in
    let (eqns',hole_typ,var_typ) = gen_equations hole_typ var_typ tenv e2 t in
    (eqns@eqns',hole_typ,var_typ)
  | EMatch (e, bs) ->
    let typ_pat = fresh_tvar () in
    let typ_exp = fresh_tvar () in
    let (eqns,hole_typ,var_typ) = gen_equations hole_typ var_typ tenv e typ_pat in
    list_fold (fun (p,e) (eqns,h_t,v_t) ->
      let (env,pat_eqn) = gen_pat_equations (tenv,eqns) p typ_pat in
      let (exp_eqn,h_t,v_t) = gen_equations h_t v_t env e typ_exp in
      (eqns@pat_eqn@exp_eqn,h_t,v_t)
    ) bs ((ty,typ_exp)::eqns,hole_typ,var_typ)
  | Hole n ->
    let t = fresh_tvar () in
    let hole_typ = HoleType.extend n t hole_typ in
    let var_typ = VariableType.extend n tenv var_typ in
    ([(ty,t)],hole_typ,var_typ)
  | Raise e ->
    gen_equations hole_typ var_typ tenv e TExn

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

let typeof : exp -> TEnv.t * HoleType.t * VariableType.t -> typ * HoleType.t * VariableType.t
= fun exp (tenv,hole_typ,variable_typ)->
  let new_tv = fresh_tvar () in
  let (eqns,hole_typ,variable_typ) = gen_equations hole_typ variable_typ tenv exp new_tv in
  let subst = solve eqns in
  let ty = Subst.apply new_tv subst in
  let hole_typ = HoleType.update subst hole_typ in
  let variable_typ = VariableType.update subst variable_typ in
  (ty,hole_typ,variable_typ)

let rec ctors_to_env : ctor list -> TEnv.t -> typ -> TEnv.t
= fun ctors tenv tbase -> list_fold(fun (id,typs) env -> TEnv.extend (id,TCtor(tbase,typs)) env) ctors tenv

let type_decl : decl -> (TEnv.t * HoleType.t * VariableType.t) -> (TEnv.t * HoleType.t * VariableType.t)
= fun decl (tenv,hole_typ,variable_typ) -> 
  match decl with
  | DExcept(id,typ) ->  (TEnv.extend (id,TCtor (TExn,typ)) tenv,hole_typ,variable_typ)
  | DData (id, ctors) -> 
    let tbase = TBase id in
    (ctors_to_env ctors tenv tbase,hole_typ,variable_typ)
  | DLet (x,is_rec,args,typ,exp) -> 
    begin match args with
    | [] -> (* variable binding *)
      let (ty,hole_typ,variable_typ) = typeof exp ((TEnv.extend (x, typ) tenv),hole_typ,variable_typ) in
      (TEnv.extend (x,ty) tenv,hole_typ,variable_typ)
    | _ ->  (* function binding *)
      let e = ELet(x, is_rec, args, typ, exp, EVar(x)) in
      let (ty,hole_typ,variable_typ) = typeof e ((TEnv.extend (x, typ) tenv),hole_typ,variable_typ) in
      ((TEnv.extend (x, ty) tenv),hole_typ,variable_typ)
    end

let run : prog -> HoleType.t * VariableType.t
= fun decls -> 
  let _ = start_time:=Sys.time() in
  let (_,hole_typ,variable_typ) = (list_fold type_decl decls (TEnv.empty,HoleType.empty,VariableType.empty)) in
  (hole_typ,variable_typ)
