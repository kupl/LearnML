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
    | VAR x -> [(ty,tenv_find tenv x)]
    | ADD(e1,e2) -> [(ty,TyInt)]@gen_equations tenv e1 TyInt@gen_equations tenv e2 TyInt
    | SUB(e1,e2) -> [(ty,TyInt)]@gen_equations tenv e1 TyInt@gen_equations tenv e2 TyInt
    | MUL(e1,e2) -> [(ty,TyInt)]@gen_equations tenv e1 TyInt@gen_equations tenv e2 TyInt
    | DIV(e1,e2) -> [(ty,TyInt)]@gen_equations tenv e1 TyInt@gen_equations tenv e2 TyInt
    | ISZERO e -> [(ty,TyBool)]@gen_equations tenv e TyInt
    | READ -> [(ty,TyInt)]
    | IF(e1,e2,e3) -> gen_equations tenv e1 TyBool@gen_equations tenv e2 ty@gen_equations tenv e3 ty
    | LET(v,e1,e2) -> 
      let f = fresh_tyvar() in 
      gen_equations tenv e1 f@gen_equations (tenv_extend (v,f) tenv) e2 ty
    | LETREC(f,x,e1,e2) ->
      let f1 = fresh_tyvar() in
      let f2 = fresh_tyvar() in
      let tenv' = tenv_extend (f,TyFun(f1,f2)) tenv in
      gen_equations tenv' e2 ty@gen_equations (tenv_extend (x, f1) tenv') e1 f2 (*요고?*)
    | PROC(v,e) -> 
      let f1 = fresh_tyvar() in
      let f2 = fresh_tyvar() in
      [(ty, TyFun(f1,f2))]@gen_equations (tenv_extend (v,f1) tenv) e f2
    | CALL(e1,e2) -> 
      let f = fresh_tyvar() in
      gen_equations tenv e1 (TyFun(f,ty))@gen_equations tenv e2 f

let occurence_check = fun x t ->
  match t with
    |TyFun(TyVar a,TyVar b)-> if x=a || x=b then true else false
    |TyFun(TyVar a,_) -> if x=a then true else false
    |TyFun(_,TyVar a) -> if x=a then true else false
    |_-> false

let rec unify_eqn = fun eqn subst->
  match eqn with
    |(TyInt,TyInt) -> subst
    |(TyBool,TyBool) -> subst
    |(TyVar x,t) -> 
      if TyVar x = t then subst else
        if occurence_check x t then raise (Failure "Occurence Error") else subst_extend x t subst
    |(t,TyVar x) -> unify_eqn (TyVar x,t) subst
    |(TyFun(t1,t2),TyFun(t3,t4)) -> 
      let subst' = unify_eqn (t1,t3) subst in
      let s1 = subst_apply t2 subst' in
      let s2 = subst_apply t4 subst' in unify_eqn (s1, s2) subst'
    |(_,_) -> raise (TypeError)

let rec subsolve = fun eqns subst ->
  match eqns with
    |[]->subst
    |(t1, t2)::tl -> 
      let s1 = subst_apply t1 subst in
      let s2 = subst_apply t2 subst in
      let subst' = unify_eqn (s1,s2) subst in 
      subsolve tl subst'

let solve : typ_eqn -> subst
=fun eqns -> subsolve eqns []

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty