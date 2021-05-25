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



let rec happen : string -> typ -> bool
 = fun a t -> 
  match t with
    | TyVar b -> a = b
    | TyFun(b1,b2) -> (happen a b1) || (happen a b2)
    | _ -> false

let rec unify : typ -> typ -> subst -> subst
 = fun t1 t2 s -> 
  match (t1,t2,s) with
    | (TyInt,TyInt,s) -> s
    | (TyBool,TyBool,s) -> s
    | (TyVar a,t,s) -> 
      if happen a t then raise TypeError
      else subst_extend a t s
    | (t,TyVar a,s) -> unify (TyVar a) t s
    | (TyFun (t1,t2), TyFun (tl_,t2_),s) -> let s_ = unify t1 tl_ s in unify (subst_apply t2 s_) (subst_apply t2_ s_) s_
    | (_,_,_) -> raise TypeError

let rec unifyall : typ_eqn -> subst -> subst
=fun eqns s ->
  match eqns with
    |[] -> s
    |(t1,t2)::t ->
      let s2 = unify (subst_apply t1 s) (subst_apply t2 s) s in
      unifyall t s2

let solve : typ_eqn -> subst
=fun eqns -> unifyall eqns subst_empty

let rec gen_equations : tenv -> exp -> typ -> typ_eqn 
=fun tenv e ty -> 
  match e with
    | CONST a -> [(ty, TyInt)]
    | VAR a -> [(ty, (tenv_find tenv a))]
    | ADD (a,b) -> [(ty, TyInt)]@(gen_equations tenv a TyInt)@(gen_equations tenv b TyInt)
    | SUB (a,b) -> [(ty, TyInt)]@(gen_equations tenv a TyInt)@(gen_equations tenv b TyInt)
    | MUL (a,b) -> [(ty, TyInt)]@(gen_equations tenv a TyInt)@(gen_equations tenv b TyInt)
    | DIV (a,b) -> [(ty, TyInt)]@(gen_equations tenv a TyInt)@(gen_equations tenv b TyInt)
    | ISZERO a -> [(ty, TyBool)]@(gen_equations tenv a TyInt)
    | IF (a,b,c) ->  (gen_equations tenv a TyBool)@(gen_equations tenv b ty)@(gen_equations tenv c ty)
    | LET (a,b,c) -> 
      let new_tv = fresh_tyvar () in
      (gen_equations tenv b new_tv)@(gen_equations (tenv_extend (a,new_tv) tenv) c ty)
    | LETREC (f,x,e1,e2) ->
      let new_tv_1 = fresh_tyvar() in
      let new_tv_2 = fresh_tyvar() in 
      (gen_equations (tenv_extend(f,TyFun(new_tv_2,new_tv_1)) (tenv_extend(x,new_tv_2) tenv)) e1 new_tv_1) @ (gen_equations (tenv_extend(f,TyFun(new_tv_2,new_tv_1)) tenv) e2 ty)
    | PROC (a,b) ->
      let new_tv_1 = fresh_tyvar () in
      let new_tv_2 = fresh_tyvar () in
      [(ty, TyFun (new_tv_1, new_tv_2))]@(gen_equations (tenv_extend (a ,new_tv_1) tenv) b new_tv_2)
    | CALL (a,b) -> 
      let new_tv = fresh_tyvar () in
      (gen_equations tenv a (TyFun (new_tv,ty)))@(gen_equations tenv b new_tv)
    | READ -> raise TypeError

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty