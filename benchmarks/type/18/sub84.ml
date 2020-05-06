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
  |CONST n-> [(ty,TyInt)]
  | VAR x -> [(ty,tenv_find tenv x)]
  | ADD (e1,e2)-> 
    let v1=gen_equations tenv e1 TyInt in 
    let v2=gen_equations tenv e2 TyInt in
    [(ty,TyInt)]@v1@v2
    
  | SUB (e1,e2)->
    let v1=gen_equations tenv e1 TyInt in 
    let v2=gen_equations tenv e2 TyInt in
    [(ty,TyInt)]@v1@v2
  | MUL (e1,e2)->
    let v1=gen_equations tenv e1 TyInt in 
    let v2=gen_equations tenv e2 TyInt in
    [(ty,TyInt)]@v1@v2
  | DIV (e1,e2)->
    let v1=gen_equations tenv e1 TyInt in 
    let v2=gen_equations tenv e2 TyInt in
    [(ty,TyInt)]@v1@v2
  | ISZERO e1->
    let v1=gen_equations tenv e1 TyInt in
    [(ty,TyBool)]@v1
    
  (*| READ *)
  | IF (e1,e2,e3)->
    let v1=gen_equations tenv e1 TyBool in 
    let v2=gen_equations tenv e2 ty in
    let v3=gen_equations tenv e3 ty in
    v1@v2@v3
    
  | LET (x,e1,e2)->
    let new_t1=fresh_tyvar() in
    let v1= gen_equations tenv e1 new_t1 in
    let tenv1=tenv_extend (x,new_t1) tenv in
    let v2= gen_equations tenv1 e2 ty in
    v1@v2
  
  | LETREC (f,x,e1,e2)->
      let new_t1=fresh_tyvar() in
      let new_t2=fresh_tyvar()in
      let tenv1=tenv_extend (x, new_t2) tenv in
      let tenv2=tenv_extend (f,TyFun(new_t2,new_t1)) tenv1 in
      let v1=gen_equations tenv2 e1 new_t1 in
      let v2=gen_equations tenv1 e2 ty in
      v1@v2
      
  | PROC (x,e)->
    let new_t1=fresh_tyvar() in
    let new_t2=fresh_tyvar()in
    let ty1=[(ty,TyFun(new_t1,new_t2))] in
    let tenv1=tenv_extend (x,new_t1) tenv in
    let v1=gen_equations tenv1 e new_t2 in
    ty1@v1
    
  | CALL (e1,e2)->
    let new_t1=fresh_tyvar() in
    let v1= gen_equations tenv e1 (TyFun(new_t1,ty)) in
    let v2=gen_equations tenv e2 new_t1 in
    v1@v2
  |_->raise TypeError  

let rec unify 
=fun ty1 ty2 subst -> 
  match (ty1,ty2) with
| (TyInt,TyInt) -> subst
| (TyBool,TyBool) -> subst
| (TyVar a,TyVar b)-> if a=b then subst else subst_extend a (TyVar b) subst
| (t,TyVar a) -> unify (TyVar a) t subst
| (TyVar a,t) -> if subst_apply (TyVar a) subst=t then raise (Failure"fail") else subst_extend a t subst
| (TyFun (ty1,ty2), TyFun (ty1',ty2')) ->
	let s' = unify ty1 ty1' subst in
	let ty1'' = subst_apply ty2 s' in
	let ty2'' = subst_apply ty2' s' in
	unify ty1'' ty2'' s'
|_->raise TypeError

let rec unifyall 
=fun eqns subst -> match eqns with
| [] -> subst
| (ty1,ty2)::eqn ->
	let s' = unify (subst_apply ty1 subst) (subst_apply ty2 subst) subst in
	unifyall eqn s'

let solve : typ_eqn -> subst
=fun eqns -> unifyall eqns subst_empty

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty