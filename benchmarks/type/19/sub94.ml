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
=fun tenv e ty -> (* TODO *)
  match e with
  | CONST n -> [(ty, TyInt)]
  | VAR x -> [(ty, tenv_find tenv x)]
  | ADD(e1,e2) ->
    let l1 = [(ty, TyInt)] in 
    let l2 = gen_equations tenv e1 TyInt in
    let l3 = gen_equations tenv e2 TyInt in
    l1@l2@l3
  | SUB(e1,e2) ->
    let l1 = [(ty, TyInt)] in 
    let l2 = gen_equations tenv e1 TyInt in
    let l3 = gen_equations tenv e2 TyInt in
    l1@l2@l3
  | MUL(e1,e2) ->
    let l1 = [(ty, TyInt)] in 
    let l2 = gen_equations tenv e1 TyInt in
    let l3 = gen_equations tenv e2 TyInt in
    l1@l2@l3
  | DIV(e1,e2) ->
    let l1 = [(ty, TyInt)] in 
    let l2 = gen_equations tenv e1 TyInt in
    let l3 = gen_equations tenv e2 TyInt in
    l1@l2@l3
  | READ -> gen_equations tenv e ty
  | ISZERO e ->
    let l1 = [(ty, TyBool)] in
    let l2 = gen_equations tenv e TyInt in
    l1@l2
  | IF(e1,e2,e3) ->
    let l1 = gen_equations tenv e1 TyBool in
    let l2 = gen_equations tenv e1 ty in
    let l3 = gen_equations tenv e2 ty in
    l1@l2@l3
  | LET( x,e1,e2) ->
    let t = fresh_tyvar () in
    let l1 = gen_equations tenv e1 t in
    let l2 = gen_equations (tenv_extend (x,t) tenv) e2  ty in
    l1@l2
  | LETREC(x1,x2,e1,e2) ->
    let t = fresh_tyvar () in
    let l1 = gen_equations tenv e1 t in
    let l2 = gen_equations tenv e2 t in
    let l3 = gen_equations (tenv_extend (x1,t) tenv) e1  ty in
    let l4 = gen_equations (tenv_extend (x2,t) tenv) e2  ty in
    l1@l2@l3@l4
  | PROC(x,e) ->
    let t1 = fresh_tyvar () in
    let t2 = fresh_tyvar () in
    let l1 = [(ty , TyFun(t1,t2))] in
    let l2 = gen_equations (tenv_extend (x,t1) tenv) e t2 in 
    l1@l2
  | CALL(e1,e2) ->
    let t = fresh_tyvar () in
    let l1 = gen_equations tenv e1 (TyFun (t,ty)) in
    let l2 = gen_equations tenv e2 t in
    l1@l2

(*This function is Change the type to a function of the same type*)
let rec change : typ -> typ -> subst -> subst
=fun t1 t2 s -> 
match (t1,t2) with
| (TyInt,TyInt) -> s
| (TyBool,TyBool) -> s
| (t,TyVar a) -> change (TyVar a) t s
| (TyVar a,t) -> subst_extend a t s
| (TyFun (t1,t2), TyFun (t1',t2')) ->
        let t1'' = subst_apply t2 (change t1 t1' s) in
        let t2'' = subst_apply t2' ((change t1 t1' s)) in
        change t1'' t2'' (change t1 t1' s)

(*This function is uniform all type*)
let rec changeptype : typ_eqn -> subst -> subst
=fun eqn s -> 
match eqn with
| [] -> s
| (t1,t2)::l ->
        changeptype l (change (subst_apply t1 s) (subst_apply t2 s) s)


let solve : typ_eqn -> subst
=fun eqns -> changeptype eqns subst_empty



let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty