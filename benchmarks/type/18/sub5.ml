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
  | CONST n -> [ ty, TyInt ]
  | VAR v -> [ ty, tenv_find tenv v ]
  | ADD (x1, x2) -> (ty, TyInt) :: (gen_equations tenv x1 TyInt) @ (gen_equations tenv x2 TyInt)
  | SUB (x1, x2) -> (ty, TyInt) :: (gen_equations tenv x1 TyInt) @ (gen_equations tenv x2 TyInt)
  | MUL (x1, x2) -> (ty, TyInt) :: (gen_equations tenv x1 TyInt) @ (gen_equations tenv x2 TyInt)
  | DIV (x1, x2) -> (ty, TyInt) :: (gen_equations tenv x1 TyInt) @ (gen_equations tenv x2 TyInt)
  | ISZERO x -> (ty, TyBool) :: (gen_equations tenv x TyInt)
  | READ -> [ ty, TyInt ]
  | IF (x1, x2, x3) ->
    let ntv = fresh_tyvar () in
    (ty, ntv) :: (gen_equations tenv x1 TyBool) @ (gen_equations tenv x2 ntv) @ (gen_equations tenv x3 ntv)
  | LET (v, x1, x2) ->
    let ntv = fresh_tyvar () in
    let nev = tenv_extend (v, ntv) tenv in
    (gen_equations tenv x1 ntv) @ (gen_equations nev x2 ty)
  | LETREC (v1, v2, x1, x2) ->
    let arg = fresh_tyvar () in
    let ret = fresh_tyvar () in
    let bdev = tenv_extend (v1, TyFun (arg, ret)) tenv in
    let fnev = tenv_extend (v2, arg) bdev in
    (gen_equations fnev x1 ret) @ (gen_equations bdev x2 ty)
  | PROC (v, x) ->
    let arg = fresh_tyvar () in
    let ret = fresh_tyvar () in
    let nev = tenv_extend (v, arg) tenv in
    (ty, TyFun (arg, ret)) :: gen_equations nev x ret
  | CALL (x1, x2) ->
    let arg = fresh_tyvar () in
    (gen_equations tenv x1 (TyFun (arg, ty))) @ (gen_equations tenv x2 arg)

let solve : typ_eqn -> subst
=fun eqns ->
  let rec solve' eqns subst =
    let rec is_rectype tv a =
      match a with
      | TyFun (TyVar tw, _) -> tv = tw 
      | TyFun (_, TyVar tw) -> tv = tw 
      | TyFun (t1, t2) -> is_rectype tv t1 || is_rectype tv t2
      | _ -> false in
    let extend tv a es =
      if is_rectype tv a then raise TypeError
      else solve' es (subst_extend tv a subst) in
    match eqns with
    | (lhs, rhs) :: es ->
      (match subst_apply lhs subst, subst_apply rhs subst with
      | TyVar ltv, TyVar rtv -> if ltv = rtv then solve' es subst else raise TypeError
      | TyVar ltv, rhs' -> extend ltv rhs' es
      | lhs', TyVar rtv -> extend rtv lhs' es
      | TyInt, TyInt -> solve' es subst
      | TyBool, TyBool -> solve' es subst
      | TyFun (a1, r1), TyFun (a2, r2) -> solve' ((a1, a2) :: (r1, r2) :: es) subst
      | _ -> raise TypeError)
    | [] -> subst in
  solve' eqns subst_empty

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty