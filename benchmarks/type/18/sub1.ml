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
    | CONST (n) -> [(ty, TyInt)]
    | VAR (v) -> [(ty, tenv_find tenv v)]
    | ADD (e1, e2) -> (ty, TyInt)::((gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt))
    | SUB (e1, e2) -> (ty, TyInt)::((gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt))
    | MUL (e1, e2) -> (ty, TyInt)::((gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt))
    | DIV (e1, e2) -> (ty, TyInt)::((gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt))
    | ISZERO (e) -> (ty, TyBool)::(gen_equations tenv e TyInt)
    | READ -> raise TypeError
    | IF (e1, e2, e3) -> (gen_equations tenv e1 TyBool)@(gen_equations tenv e2 ty)@(gen_equations tenv e3 ty)
    | LET (x, e1, e2) -> 
      let alpha = fresh_tyvar () in
      (gen_equations tenv e1 alpha)@(gen_equations (tenv_extend (x, alpha) tenv) e2 ty)
    | LETREC (f, x, e1, e2) -> (* I have to implement *)
      (let t2 = fresh_tyvar () in
      let t1 = fresh_tyvar () in
      let tenv2 = tenv_extend (f, TyFun(t2, t1)) tenv in
      let tenv1 = tenv_extend (x, t2) tenv2 in
      (gen_equations tenv1 e1 t1)@(gen_equations tenv2 e2 ty))
    | PROC (x, e) ->
      (let t1 = fresh_tyvar () in
      let t2 = fresh_tyvar () in
      (ty, (TyFun (t1, t2)))::(gen_equations (tenv_extend (x, t1) tenv) e t2))
    | CALL (e1, e2) -> 
      (let t1 = fresh_tyvar () in
      (gen_equations tenv e1 (TyFun (t1, ty)))@(gen_equations tenv e2 t1))
    
(* HW1: TODO *)
let rec check_same_var v ty =
  match ty with
    | TyVar x -> v=x
    | TyFun (ty1, ty2) -> (check_same_var v ty1)||(check_same_var v ty2)
    | _ -> false

let solve : typ_eqn -> subst
=fun eqns ->
  let rec unify ty1 ty2 subst = 
    match ty1,ty2 with
      | (TyInt, TyInt) -> subst
      | (TyBool, TyBool) -> subst
      | (TyVar (x), ty) -> 
        if check_same_var x ty then raise TypeError else subst_extend x ty subst
      | (ty, TyVar (x)) -> unify (TyVar (x)) ty subst
      | (TyFun (t1, t2), TyFun (t1', t2')) -> 
        (let s1 = unify t1 t1' subst in
        let s2 = unify (subst_apply t2 s1) (subst_apply t2' s1) s1 in
        s2)
      | (_, _) -> raise TypeError
  in
  let rec unifyall tyeqn subst =
    match tyeqn with
      | [] -> subst
      | (ty1, ty2)::tl -> 
        (let s1 = unify (subst_apply ty1 subst) (subst_apply ty2 subst) subst in
        unifyall tl s1)
  in
  unifyall eqns subst_empty

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty