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
    | CONST n -> [(ty, TyInt)]
    | VAR x -> [(ty, tenv_find tenv x)]
    | ADD (e1,e2) -> [(ty,TyInt)] @ gen_equations tenv e1 TyInt @ gen_equations tenv e2 TyInt
    | SUB (e1,e2) -> [(ty,TyInt)] @ gen_equations tenv e1 TyInt @ gen_equations tenv e2 TyInt
    | MUL (e1,e2) -> [(ty,TyInt)] @ gen_equations tenv e1 TyInt @ gen_equations tenv e2 TyInt
    | DIV (e1,e2) -> [(ty,TyInt)] @ gen_equations tenv e1 TyInt @ gen_equations tenv e2 TyInt
    | ISZERO e1 -> [(ty,TyBool)] @ gen_equations tenv e1 TyInt
    | READ -> [(ty, TyInt)]
    | IF (e1,e2,e3) -> gen_equations tenv e1 TyBool @ gen_equations tenv e2 ty @ gen_equations tenv e3 ty
    | LET (x,e1,e2) -> let ty1 = fresh_tyvar() in gen_equations tenv e1 ty1 @ gen_equations ( tenv_extend(x, ty1) tenv ) e2 ty
    | LETREC (f,x,e1,e2) -> let ty1 = fresh_tyvar() in let ty2 = fresh_tyvar() in gen_equations ( tenv_extend(f, TyFun(ty2, ty1)) (tenv_extend(x, ty2) tenv) ) e1 ty1 
      @ gen_equations ( tenv_extend(f, TyFun(ty2, ty1)) tenv ) e2 ty
    | PROC (x,e1) -> let ty1 = fresh_tyvar() in let ty2 = fresh_tyvar() in [(ty,TyFun(ty1,ty2))] @ gen_equations ( tenv_extend(x,ty1) tenv ) e1 ty2
    | CALL (e1,e2) -> let ty1 = fresh_tyvar() in gen_equations tenv e1 (TyFun(ty1,ty)) @ gen_equations tenv e2 ty1

let rec solve : typ_eqn -> subst
=fun eqns -> 
 sol1 eqns subst_empty
and sol1 : typ_eqn -> subst -> subst
=fun eqns sub ->
  match eqns with
  | [] -> sub
  | (ty1,ty2) :: eqn1 -> let sub1 = sol2 (subst_apply ty1 sub) (subst_apply ty2 sub) sub in sol1 eqn1 sub1
and sol2 : typ -> typ -> subst -> subst
=fun ty1 ty2 sub ->
  match (ty1,ty2) with
  | (TyInt,TyInt) -> sub
  | (TyBool,TyBool) -> sub
  | (TyVar x,ty) ->
  (
    match ty with
    | TyInt -> subst_extend x ty2 sub
    | TyBool -> subst_extend x ty2 sub
    | TyVar x1 -> subst_extend x ty2 sub
    | TyFun (ty3,ty4) ->
      if sol3 (TyVar x) ty3 && sol3 (TyVar x) ty4 then subst_extend x ty2 sub else raise TypeError
  ) 
  | (TyFun (t1,t2), TyFun (t3,t4)) -> let sub1 = sol2 t1 t3 sub in let t5 = subst_apply t2 sub1 in let t6 = subst_apply t4 sub1 in sol2 t5 t6 sub1
  | (ty,TyVar x) -> sol2 (TyVar x) ty sub
  | _ -> raise TypeError
and sol3 : typ -> typ -> bool
=fun ty1 ty2 ->
  match ty2 with
  | TyFun (t1,t2) -> (sol3 ty1 t1) && (sol3 ty1 t2)
  | TyVar x ->
    if (TyVar x) = ty1 then false else true
  | _ -> true

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty