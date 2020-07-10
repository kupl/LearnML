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
=fun tenv e ty -> match e with
  | CONST n -> [(ty, TyInt)]
  | VAR x -> [(tenv x, ty)]
  | ADD (e1, e2) -> (gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)@[(ty, TyInt)]
  | SUB (e1, e2) -> (gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)@[(ty, TyInt)]
  | MUL (e1, e2) -> (gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)@[(ty, TyInt)]
  | DIV (e1, e2) -> (gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)@[(ty, TyInt)]
  | ISZERO e1 -> (gen_equations tenv e1 TyInt)@[(ty, TyBool)]
  | READ -> []
  | IF (e1, e2, e3) -> (gen_equations tenv e1 TyBool)@(gen_equations tenv e2 ty)@(gen_equations tenv e3 ty)
  | LET (x, e1, e2) -> let t1 = fresh_tyvar () in
    (gen_equations tenv e1 t1)@(gen_equations (tenv_extend (x, t1) tenv) e2 ty)
  | LETREC (f, x, e1, e2) -> let t1 = fresh_tyvar () in let t2 = fresh_tyvar () in let te1 = tenv_extend (f, TyFun(t2, t1)) tenv in
    (gen_equations (tenv_extend (x, t2) te1) e1 t1)@(gen_equations te1 e2 ty)
  | PROC (x, e1) -> let t1 = fresh_tyvar () in let t2 = fresh_tyvar () in
    (gen_equations (tenv_extend (x, t1) tenv) e1 t2)@[(ty, TyFun(t1, t2))]
  | CALL (e1, e2) -> let t1 = fresh_tyvar () in
    (gen_equations tenv e1 (TyFun(t1, ty)))@(gen_equations tenv e2 t1)

let solve : typ_eqn -> subst
=fun eqns -> 
    let rec unify t1 t2 subst = match (t1, t2) with
        | (TyInt, TyInt) -> subst
        | (TyBool, TyBool) -> subst
        | (TyVar x, TyVar y) -> if x = y then subst else subst_extend x t2 subst
        | (TyVar x, TyFun(x1, x2)) -> (let rec typein tv ty = match ty with
            | TyInt -> false
            | TyBool -> false
            | TyFun (ty1, ty2) -> (typein tv ty1) || (typein tv ty2)
            | TyVar y -> (tv = y)
            in if (typein x x1) || (typein x x2) then raise TypeError else subst_extend x t2 subst)
        | (TyVar x, _) -> subst_extend x t2 subst
        | (_, TyVar x) -> unify t2 t1 subst
        | (TyFun(x1, y1), TyFun(x2, y2)) -> let s1 = unify x1 x2 subst in unify (subst_apply y1 s1) (subst_apply y2 s1) s1
        | (_, _) -> raise TypeError in
    let rec unifyall tyeqn subst = match tyeqn with
        | [] -> subst
        | (t1, t2)::tl -> let s1 = unify (subst_apply t1 subst) (subst_apply t2 subst) subst in unifyall tl s1 in
    unifyall eqns subst_empty

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty