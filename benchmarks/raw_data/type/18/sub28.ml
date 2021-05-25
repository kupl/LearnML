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
  | CONST n -> [(ty,TyInt)]
  | VAR v -> [(ty,tenv_find tenv v)]
  | ADD (e1, e2) ->
    let t1 = fresh_tyvar () in
    let t2 = fresh_tyvar () in
    [(ty,TyInt);(t1,TyInt);(t2,TyInt)]@(gen_equations tenv e1 t1)@(gen_equations tenv e2 t2)
  | SUB (e1, e2) ->
    let t1 = fresh_tyvar () in
    let t2 = fresh_tyvar () in
    [(ty,TyInt);(t1,TyInt);(t2,TyInt)]@(gen_equations tenv e1 t1)@(gen_equations tenv e2 t2)  
  | MUL (e1, e2) ->
    let t1 = fresh_tyvar () in
    let t2 = fresh_tyvar () in
    [(ty,TyInt);(t1,TyInt);(t2,TyInt)]@(gen_equations tenv e1 t1)@(gen_equations tenv e2 t2)
  | DIV (e1, e2) ->
    let t1 = fresh_tyvar () in
    let t2 = fresh_tyvar () in
    [(ty,TyInt);(t1,TyInt);(t2,TyInt)]@(gen_equations tenv e1 t1)@(gen_equations tenv e2 t2)
  | ISZERO e1 ->
    let t1 = fresh_tyvar () in
    [(ty,TyBool);(t1,TyInt)]@(gen_equations tenv e1 t1)
  | READ -> [(ty, TyInt)]
  | IF (e1, e2, e3) ->
    let t1 = fresh_tyvar () in
    let t2 = fresh_tyvar () in
    let eq1 = gen_equations tenv e1 t1 in
    let eq2 = gen_equations tenv e2 t2 in
    let eq3 = gen_equations tenv e3 t2 in
    [(ty,t2);(t1,TyBool)]@eq1@eq2@eq3
  | LET (v, e1, e2) ->
    let t1 = fresh_tyvar () in
    let tenv' = tenv_extend (v,t1) tenv in
    (gen_equations tenv e1 t1)@(gen_equations tenv' e2 ty)
  | LETREC (f, x, e1, e2) ->
    let te1 = fresh_tyvar () in
    let tx = fresh_tyvar  () in
    let tenvE1 = tenv_extend (f, TyFun (tx,te1)) tenv in
    let tenvX = tenv_extend (x, tx) tenv in
    (gen_equations tenvX e1 te1)@(gen_equations tenvE1 e2 ty)
  | PROC (v, e) ->
    let t1 = fresh_tyvar () in
    let t2 = fresh_tyvar () in
    let tenv' = tenv_extend (v, t1) tenv in
    [(ty, TyFun (t1, t2))]@(gen_equations tenv' e t2)
  | CALL (e1, e2) ->
    let t1 = fresh_tyvar () in
    let t2 = fresh_tyvar () in
    [(t1,TyFun (t2, ty))]@(gen_equations tenv e1 t1)@(gen_equations tenv e2 t2)

let rec occur v t = 
  match t with
  | TyInt -> false
  | TyBool -> false
  | TyFun (t1, t2) -> (occur v t1) || (occur v t2)
  | TyVar w -> (v = w)

let rec unify : (typ * typ) -> subst -> subst
= fun (t1, t2) s ->
  match t1, t2 with
  | TyInt, TyInt -> s
  | TyBool, TyBool -> s
  | TyVar v, _ ->
    if occur v t2
    then raise TypeError
    else subst_extend v t2 s
  | _, TyVar v -> unify (t2, t1) s
  | TyFun (t1, t2), TyFun (t3, t4) ->
    let st1 = subst_apply t1 s in
    let st3 = subst_apply t3 s in
    let s' = unify (st1, st3) s in
    let st2 = subst_apply t2 s' in
    let st4 = subst_apply t4 s' in
    unify (st2, st4) s' 
  | _, _ -> raise TypeError

let rec unifyall : (typ * typ) list -> subst -> subst
= fun lst s ->
  match lst with
  | [] -> s
  | (t1,t2)::tl -> 
    let st1 = subst_apply t1 s in
    let st2 = subst_apply t2 s in
    let s' = unify (st1, st2) s in
    unifyall tl s'

let solve : typ_eqn -> subst
=fun eqns -> unifyall eqns subst_empty

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty