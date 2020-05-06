type program = exp
and exp = 
  | CONST of int
  | VAR of var
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | ISZERO of exp
  | READ
  | IF of exp * exp * exp
  | LET of var * exp * exp
  | LETREC of var * var * exp * exp
  | PROC of var * exp
  | CALL of exp * exp
and var = string

exception TypeError

type typ = TyInt | TyBool | TyFun of typ * typ | TyVar of tyvar
and tyvar = string
type typ_eqn = (typ * typ) list

(* type environment : var -> type *)
type tenv = var -> typ
let tenv_empty = fun _ -> raise (Failure "Type Env is empty")
let tenv_extend (x,t) tenv = fun y -> if x = y then t else (tenv y)
let tenv_find tenv x = tenv x

(* substitution *)
type subst = (tyvar * typ) list
let subst_empty = []
let subst_find x subst = List.assoc x subst

(* walk through the type, replacing each type variable by its binding in the substitution *)
let rec subst_apply : typ -> subst -> typ
=fun typ subst ->
  match typ with
  | TyInt -> TyInt
  | TyBool -> TyBool 
  | TyFun (t1,t2) -> TyFun (subst_apply t1 subst, subst_apply t2 subst)
  | TyVar x -> if List.exists (fun (y, typ) -> x = y) subst then subst_find x subst else typ

(* add a binding (tv,ty) to the subsutition and propagate the information *)
let subst_extend tv ty subst = 
  (tv,ty) :: (List.map (fun (x,t) -> (x, subst_apply t [(tv,ty)])) subst)

let tyvar_num = ref 0

let fresh_tyvar _ = 
	let _ = tyvar_num := !tyvar_num + 1 in
	TyVar ("t" ^ string_of_int !tyvar_num)


let rec gen_equations : tenv -> exp -> typ -> typ_eqn 
=fun tenv e ty ->
  match e with
  | CONST n -> [(ty, TyInt)]
  | VAR x -> [(tenv_find tenv x, ty)]
  | ADD (e1, e2)
  | SUB (e1, e2)
  | MUL (e1, e2)
  | DIV (e1, e2) -> [(ty, TyInt)] @ (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
  | ISZERO e -> [(ty, TyBool)] @ (gen_equations tenv e TyInt)
  | READ -> [(ty, TyInt)]
  | IF (e1, e2, e3) -> (gen_equations tenv e1 TyBool) @ (gen_equations tenv e2 ty) @ (gen_equations tenv e3 ty)
  | LET (x, e1, e2) ->
    let new_tv = fresh_tyvar() in
    (gen_equations tenv e1 new_tv) @ (gen_equations (tenv_extend (x, new_tv) tenv) e2 ty)
  | LETREC (f, x, e1, e2) ->
    let new_tvf = fresh_tyvar() in
    let new_tv1 = fresh_tyvar() in
    let new_tv2 = fresh_tyvar() in
    let tenv' = tenv_extend (f, TyFun (new_tv1, new_tv2)) tenv in
    [(new_tvf, TyFun (new_tv1, new_tv2))] @ (gen_equations (tenv_extend (x, new_tv1) tenv') e1 new_tv2) @ (gen_equations tenv' e2 ty)
  | PROC (x, e) ->
    let new_tv1 = fresh_tyvar() in
    let new_tv2 = fresh_tyvar() in
    [(ty, TyFun (new_tv1, new_tv2))] @ (gen_equations (tenv_extend (x, new_tv1) tenv) e new_tv2)
  | CALL (e1, e2) ->
    let new_tv = fresh_tyvar() in
    (gen_equations tenv e1 (TyFun (new_tv, ty))) @ (gen_equations tenv e2 new_tv)
    
let rec unify : typ -> typ -> subst -> subst
=fun ty1 ty2 subst ->
  match ty1, ty2 with
  | TyInt, TyInt -> subst
  | TyBool, TyBool -> subst
  | TyVar tv, TyInt
  | TyVar tv, TyBool -> subst_extend tv ty2 subst
  | TyVar tv1, TyVar tv2 ->
      if tv1 = tv2 then subst
      else subst_extend tv1 ty2 subst
  | TyVar tv1, TyFun (ty1', ty2') ->
      (match ty1', ty2' with
      | TyVar tv2, TyVar tv3 ->
          if tv1 = tv2 || tv1 = tv3 then raise TypeError
          else subst_extend tv1 ty2 subst
      | TyVar tv2, _
      | _, TyVar tv2 ->
          if tv1 = tv2 then raise TypeError
          else subst_extend tv1 ty2 subst
      | _, _ -> subst_extend tv1 ty2 subst)
  | TyInt, TyVar _ -> unify ty2 ty1 subst
  | TyBool, TyVar _ -> unify ty2 ty1 subst
  | TyFun _, TyVar _ -> unify ty2 ty1 subst
  | TyFun (tv1, tv2), TyFun (tv1', tv2') ->
      let subst'= unify tv1 tv1' subst in
      let subst'' = unify (subst_apply tv2 subst') (subst_apply tv2' subst') subst' in
      subst''
  | _ -> raise TypeError

let rec unifyall : typ_eqn -> subst -> subst
=fun eqns subst ->
  match eqns with
  | [] -> subst
  | (ty1, ty2)::hd ->
    let subst' = unify (subst_apply ty1 subst) (subst_apply ty2 subst) subst in
    unifyall hd subst'    

let solve : typ_eqn -> subst
=fun eqns -> unifyall eqns subst_empty

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty