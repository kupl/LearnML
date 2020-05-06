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
  | VAR x -> [(ty, tenv_find tenv x)]
  | ADD (e1, e2) -> (ty, TyInt)::(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
  | SUB (e1, e2) -> (ty, TyInt)::(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
  | MUL (e1, e2) -> (ty, TyInt)::(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
  | DIV (e1, e2) -> (ty, TyInt)::(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
  | ISZERO e -> (ty, TyBool)::(gen_equations tenv e TyInt)
  | READ -> [(ty, fresh_tyvar ())]
  | IF (e1, e2, e3) -> (gen_equations tenv e1 TyBool)@(gen_equations tenv e2 ty)@(gen_equations tenv e3 ty)
  | LET (x, e1, e2) ->
    let ty1 = fresh_tyvar () in
      (gen_equations tenv e1 ty1)@(gen_equations (tenv_extend (x, ty1) tenv) e2 ty)
  | LETREC (f, x, e1, e2) ->
    let ty1 = fresh_tyvar () in
    let ty2 = fresh_tyvar () in
      (gen_equations (tenv_extend (f, TyFun (ty1, ty2)) (tenv_extend (x, ty1) tenv)) e1 ty2)@(gen_equations (tenv_extend (f,  TyFun (ty1, ty2)) tenv) e2 ty)
  | PROC (x, e) ->
    let ty1 = fresh_tyvar () in
    let ty2 = fresh_tyvar () in
    (ty, TyFun (ty1, ty2))::(gen_equations (tenv_extend (x, ty1) tenv) e ty2)
  | CALL (e1, e2) ->
    let ty1 = fresh_tyvar () in
      (gen_equations tenv e1 (TyFun (ty1, ty)))@(gen_equations tenv e2 ty1)

let solve : typ_eqn -> subst
=fun eqns ->
  let rec occurs_in = fun a ty ->
    match ty with
    | TyVar b -> if a = b then true else false
    | TyFun (ty1, ty2) -> (occurs_in a ty1) || (occurs_in a ty2)
    | _ -> false
  in
  let rec unify = fun typ1 typ2 subst ->
    match typ1, typ2 with
    | (TyInt, TyInt) -> subst
    | (TyBool, TyBool) -> subst
    | (TyVar a, ty) -> (
      match ty with
      | TyInt -> subst_extend a ty subst
      | TyBool -> subst_extend a ty subst
      | TyVar b -> if a = b then subst else subst_extend a (TyVar b) subst
      | TyFun (ty1, ty2) -> if (occurs_in a ty) then raise TypeError else subst_extend a ty subst
      )
    | (ty, TyVar a) -> unify (TyVar a) ty subst
    | (TyFun (t1, t2), TyFun (t1', t2')) ->
      let subst' = unify t1 t1' subst in
      let subst'' = unify (subst_apply t2 subst') (subst_apply t2' subst') subst' in subst''
    | (_, _) -> raise TypeError
  in
  let rec unifyall = fun curr_eqn curr_subst ->
    match curr_eqn with
    | [] -> curr_subst
    | (ty1, ty2)::u ->
      let s = unify (subst_apply ty1 curr_subst) (subst_apply ty2 curr_subst) curr_subst
      in unifyall u s
  in unifyall eqns subst_empty

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty