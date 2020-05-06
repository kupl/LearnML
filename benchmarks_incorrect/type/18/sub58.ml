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
    | CONST i -> [(ty, TyInt)]
    
    | VAR v -> [(ty, tenv_find tenv v)]
    
    | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) -> 
      let l1 = [(ty, TyInt)] in 
      let l2 = gen_equations tenv e1 TyInt in 
      let l3 = gen_equations tenv e2 TyInt in
        l1 @ l2 @ l3
      (*[(ty, TyInt)] @ (gen_equation tenv e1 TyInt) @ (gen_equation tenv e2 TyInt)*)
      
    | ISZERO e -> 
      let l1 = [(ty, TyBool)] in 
      let l2 = gen_equations tenv  e TyInt in 
        l1 @ l2
    
    | READ -> [(ty, TyInt)]
    
    | IF (e1, e2, e3) -> 
      let l3 = gen_equations tenv e3 ty in 
      let l2 = gen_equations tenv e2 ty in 
      let l1 = gen_equations tenv e1 TyBool in 
        l1 @ l2 @ l3
    
    | LET (v, e1, e2) ->  
      (*gen_equation tenv (CALL (PROC (v, e2), e1))*)
      let new_v = fresh_tyvar () in 
      let l2 = gen_equations (tenv_extend (v, new_v) tenv) e2 ty in 
      let l1 = gen_equations tenv e1 new_v in 
        l1 @ l2
    
    | LETREC (v1, v2, e1, e2) -> 
      let new_v1 = fresh_tyvar () in
      let new_v2 = fresh_tyvar () in
      let l1 = gen_equations (tenv_extend (v2, new_v1) (tenv_extend (v1, TyFun(new_v1, new_v2)) tenv)) e1 new_v2 in 
      let l2 = gen_equations (tenv_extend (v1, TyFun (new_v1, new_v2)) tenv) e2 ty in 
        l1 @ l2
      
    
    | PROC (v, e) ->
      let new_v1 = fresh_tyvar () in 
      let new_v2 = fresh_tyvar () in 
      let l1 = [(ty, TyFun (new_v1, new_v2))] in 
      let l2 = gen_equations (tenv_extend (v, new_v1) tenv) e new_v2 in 
        l1 @ l2
    
    | CALL (e1, e2) ->
      let new_v = fresh_tyvar () in 
      let l1 = gen_equations tenv e1 (TyFun (new_v, ty)) in 
      let l2 = gen_equations tenv e2 new_v in 
        l1 @ l2
  
  (* HW1: TODO *)
let rec isoccurrence : typ -> typ -> bool
=fun t1 t2 -> 
  match t2 with
    | TyInt | TyBool -> true
    | TyFun (ty1, ty2) -> (isoccurrence t1 ty1) && (isoccurrence t1 ty2)
    | TyVar v -> t1 = t2
    
    
let rec unify : typ -> typ -> subst -> subst
=fun t1 t2 s ->
  match t1,t2 with
    |(TyInt, TyInt) |(TyBool, TyBool) -> s
    |(TyVar v, t) -> if isoccurrence t1 t2 then subst_extend v t s else raise TypeError(* v 가 이미 있는 경우 처리*)
    |(t, TyVar v) -> unify (TyVar v) t s
    |(TyFun (t1, t2), TyFun (t1', t2')) ->
      let s' = unify t1 t1' s in 
      let t1'' = subst_apply t2 s' in 
      let t2'' = subst_apply t2' s' in 
      unify t1'' t2'' s'
    |_ -> raise TypeError
    
let rec unifyall : typ_eqn -> subst -> subst
=fun eqns s ->
  match eqns with
    (t1, t2)::tl -> 
      let s' = unify (subst_apply t1 s) (subst_apply t2 s) s in 
        unifyall tl s'
    |[] -> s
    
    
let solve : typ_eqn -> subst
=fun eqns -> unifyall eqns subst_empty


let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty