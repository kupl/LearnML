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
= fun tenv e ty ->
  match e with
  | CONST n -> [(ty, TyInt)]
  | VAR x -> [(ty, tenv x)]
  | ADD (e1,e2) -> [(ty, TyInt)] @ (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
  | SUB (e1,e2) -> [(ty, TyInt)] @ (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt) 
  | MUL (e1,e2) -> [(ty, TyInt)] @ (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
  | DIV (e1,e2) -> [(ty, TyInt)] @ (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
  | ISZERO n -> [(ty, TyBool)] @ (gen_equations tenv n TyInt)
  | READ -> [(ty,TyInt)]
  | IF (e1,e2,e3) -> (gen_equations tenv e1 TyBool) @ (gen_equations tenv e2 ty) @ (gen_equations tenv e3 ty)
  | LET (x,e1,e2) -> 
    let ty' = fresh_tyvar() in
    let tenv' = tenv_extend (x, ty') tenv in
    (gen_equations tenv e1 ty') @ (gen_equations tenv' e2 ty)
  | LETREC (f,x,e1,e2) ->
    let ty' = fresh_tyvar() in
    let ty'' = fresh_tyvar() in
    let tenv' = tenv_extend (f, TyFun (ty',ty'')) tenv in
    let tenv'' = tenv_extend (x,ty') tenv' in
    (gen_equations tenv'' e1 ty'') @ (gen_equations tenv' e2 ty)
  | PROC (x,e) -> 
    let ty' = fresh_tyvar() in
    let ty'' = fresh_tyvar() in
    let tenv' = tenv_extend (x, ty') tenv in
    [(ty, TyFun (ty',ty''))] @ (gen_equations tenv' e ty'')
  | CALL (e1,e2) ->
    let ty' = fresh_tyvar() in
    (gen_equations tenv e1 (TyFun (ty', ty))) @ (gen_equations tenv e2 ty')

let solve : typ_eqn -> subst
=fun eqns -> 
  let rec occur_check : string -> typ -> bool
    = fun x t ->
    match t with
    | TyVar y -> x = y
    | TyFun(a,b) -> (occur_check x a) || (occur_check x b)
    | _ -> false  
  in

  let rec unify : typ -> typ -> subst -> subst
  = fun t1 t2 s ->
  match (t1,t2,s) with
  | (TyBool,TyBool,s) -> s
  | (TyInt,TyInt,s) -> s
  | (TyVar x,t,s) -> if occur_check x t then raise(Failure "TypeError")  else subst_extend x t s
  | (t,TyVar x, s) -> unify (TyVar x) t s
  | (TyFun(t1,t2), TyFun(t3,t4),s) -> let s' = unify t1 t3 s in unify (subst_apply t2 s') (subst_apply t4 s') s'
  | (_,_,_) -> raise(Failure "TypeError")
  in

  let rec unification : typ_eqn -> subst -> subst
  = fun eq st -> 
  match eq with
  | [] -> st
  | (t1,t2)::tl -> unification tl (unify (subst_apply t1 st) (subst_apply t2 st) st)
  in
  unification eqns subst_empty 

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty