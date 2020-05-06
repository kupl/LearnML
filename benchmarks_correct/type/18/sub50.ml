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
  | VAR x -> let tyx = tenv_find tenv x in [(ty, tyx)]
  | ADD (exp1, exp2) ->
    let e1 = gen_equations tenv exp1 TyInt in
    let e2 = gen_equations tenv exp2 TyInt in
    [(ty, TyInt)]@e1@e2
  | SUB (exp1, exp2) ->
    let e1 = gen_equations tenv exp1 TyInt in
    let e2 = gen_equations tenv exp2 TyInt in
    [(ty, TyInt)]@e1@e2
  | MUL (exp1, exp2) ->
    let e1 = gen_equations tenv exp1 TyInt in
    let e2 = gen_equations tenv exp2 TyInt in
    [(ty, TyInt)]@e1@e2
  | DIV (exp1, exp2) ->
    let e1 = gen_equations tenv exp1 TyInt in
    let e2 = gen_equations tenv exp2 TyInt in
    [(ty, TyInt)]@e1@e2
  | ISZERO exp1 -> 
    let e1 = gen_equations tenv exp1 TyInt in
    [(ty, TyBool)]@e1
  | READ -> [(ty, TyInt)]
  | IF (exp1, exp2, exp3) ->
    let e1 = gen_equations tenv exp1 TyBool in
    let e2 = gen_equations tenv exp2 ty in
    let e3 = gen_equations tenv exp3 ty in
    e1@e2@e3
  | LET (x, exp1, exp2) ->
    let tyx = fresh_tyvar () in
    let e1 = gen_equations tenv exp1 tyx in
    let tenv = tenv_extend (x, tyx) tenv in
    let e2 = gen_equations tenv exp2 ty in
    e1@e2
  | LETREC (f, x, exp1, exp2) ->(*추후 확인*)
    let tyx = fresh_tyvar () in
    let tyy = fresh_tyvar () in
    let tenv1 = tenv_extend (x,tyx) tenv in
    let tenv1 = tenv_extend (f, TyFun(tyx, tyy)) tenv1 in
    let tenv2 = tenv_extend (f, TyFun(tyx, tyy)) tenv in
    let e1 = gen_equations tenv1 exp1 tyy in
    let e2 = gen_equations tenv2 exp2 ty in
    e1@e2
  | PROC (x, exp1) ->
    let tyx = fresh_tyvar () in
    let tyy = fresh_tyvar () in
    let tenv = tenv_extend (x, tyx) tenv in
    let e1 = gen_equations tenv exp1 tyy in
    [(ty, TyFun(tyx,tyy))]@ e1
  | CALL (exp1, exp2) ->
    let tyx = fresh_tyvar () in 
    let e1 = gen_equations tenv exp1 (TyFun(tyx, ty)) in
    let e2 = gen_equations tenv exp2 tyx in
    e1@e2;;
  
  
  
let rec check a ty =
match ty with
| TyVar x -> if a = x then true else false
| TyFun (ty1, ty2) -> (check a ty1) && (check a ty2)
| _ -> false
  
let rec unify ty1 ty2 s = 
    if ty1 = ty2 then s 
    else match ty1, ty2 with
      |TyVar a, ty -> if (check a ty) then raise TypeError else subst_extend a ty s
      |ty, TyVar a -> unify (TyVar a) ty s
      |TyFun(ty1, ty2), TyFun(ty3, ty4) ->
        let s = unify ty1 ty3 s in unify (subst_apply ty2 s) (subst_apply ty4 s) s 
      | _ -> raise TypeError 

let solve : typ_eqn -> subst
=fun eqns -> let rec unifyall 
= fun eqns s ->
  match eqns with
    | [] -> s
    | (ty1,ty2)::tl -> let s = unify (subst_apply ty1 s) (subst_apply ty2 s) s in 
    unifyall tl s in 
    unifyall eqns subst_empty
    
let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty