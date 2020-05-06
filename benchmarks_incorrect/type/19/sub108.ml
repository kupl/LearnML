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
=fun tenv e ty -> (************************************* TODO *************************************)
  match e with
    CONST n -> [(ty, TyInt)]
  | VAR x -> [(ty, (tenv_find tenv x))]
  | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2)
    -> [(ty, TyInt)]@(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
  | ISZERO e -> [(ty, TyBool)]@(gen_equations tenv e TyInt)
  | IF (e, e1, e2) -> (gen_equations tenv e TyBool)@(gen_equations tenv e1 ty)@(gen_equations tenv e2 ty)
  | LET (x, e1, e2) -> let a = fresh_tyvar() in (gen_equations tenv e1 a)@(gen_equations (tenv_extend (x, a) tenv) e2 ty)
  | LETREC (f, x, e1, e2) 
    -> let t1 = fresh_tyvar() in let t2 = fresh_tyvar() in let tenv' = tenv_extend (f, (TyFun (t2, t1))) tenv in
        (gen_equations (tenv_extend (x, t2) tenv') e1 t1)@(gen_equations tenv' e2 ty)
  | PROC (x, e) -> let a1 = fresh_tyvar() in let a2 = fresh_tyvar() in
                     [(ty, (TyFun (a1, a2)))]@(gen_equations (tenv_extend (x, a1) tenv) e a2)
  | CALL (e1, e2) -> let a = fresh_tyvar() in (gen_equations tenv e1 (TyFun (a, ty)))@(gen_equations tenv e2 a)
  | _ -> raise TypeError

let rec unify : typ -> typ -> subst -> subst
=fun t1 t2 s ->
  match (t1, t2) with
    (TyInt, TyInt) | (TyBool, TyBool) -> s
  | (TyVar x1, TyVar x2) -> s
  | (TyVar x, TyFun (t1', t2')) -> 
    (match (t1', t2') with
      (TyVar x1, TyVar x2) -> if (x = x1 || x = x2) then raise TypeError else subst_extend x t2 s
    | (TyVar x1, y) | (y, TyVar x1) -> if (x = x1) then raise TypeError else subst_extend x t2 s
    | _ -> subst_extend x t2 s)
  | (TyVar x, t) -> subst_extend x t s
  | (TyFun (t1', t2'), TyVar x) -> unify t2 t1 s
  | (TyInt, TyVar x) | (TyBool, TyVar x) -> unify t2 t1 s
  | (TyFun (t11, t12), TyFun (t21, t22)) -> let s' = unify t11 t12 s in unify (subst_apply t21 s') (subst_apply t22 s') s'
  | _ -> raise TypeError

let rec unifyall : typ_eqn -> subst -> subst
=fun eqns s ->
  match eqns with
    [] -> s
  | (t1, t2)::tl -> unifyall tl (unify (subst_apply t1 s) (subst_apply t2 s) s)

let solve : typ_eqn -> subst
=fun eqns -> (************************************* TODO *************************************)
  unifyall eqns []

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty