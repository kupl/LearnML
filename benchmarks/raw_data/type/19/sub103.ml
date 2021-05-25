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
  | CONST n -> [(ty,TyInt)]
  | VAR x -> [(tenv_find tenv x,ty)]
  | ADD (e1,e2) ->
    let t1 = gen_equations tenv e1 TyInt in
    let t2 = gen_equations tenv e2 TyInt in
    [(ty,TyInt)] @ t1 @ t2
  | SUB (e1,e2) ->
    let t1 = gen_equations tenv e1 TyInt in
    let t2 = gen_equations tenv e2 TyInt in
    [(ty,TyInt)] @ t1 @ t2
  | MUL (e1,e2) ->
    let t1 = gen_equations tenv e1 TyInt in
    let t2 = gen_equations tenv e2 TyInt in
    [(ty,TyInt)] @ t1 @ t2
  | DIV (e1,e2) ->
    let t1 = gen_equations tenv e1 TyInt in
    let t2 = gen_equations tenv e2 TyInt in
    [(ty,TyInt)] @ t1 @ t2
  | ISZERO e1 ->
    let t1 = gen_equations tenv e1 TyInt in
    [(ty,TyBool)] @ t1
  | READ -> [(ty,ty)]
  | IF (e1,e2,e3) ->
    let t1 = gen_equations tenv e1 TyBool in
    let t2 = gen_equations tenv e2 ty in
    let t3 = gen_equations tenv e3 ty in
    t1 @ t2 @ t3
  | LET (x,e1,e2) ->
    let nty = fresh_tyvar () in
    let t1 = gen_equations tenv e1 nty in
    let t2 = gen_equations (tenv_extend (x,nty) tenv) e2 ty in
    t1 @ t2
  | LETREC (f,x,e1,e2) ->
    let nty1 = fresh_tyvar () in
    let nty2 = fresh_tyvar () in
    let extenv = tenv_extend (f,(TyFun (nty2,nty1))) tenv in
    let t1 = gen_equations (tenv_extend (x,nty2) extenv) e1 nty1 in
    let t2 = gen_equations extenv e2 ty in
    t1 @ t2
  | PROC (x,e1) ->
    let nty1 = fresh_tyvar () in
    let nty2 = fresh_tyvar () in
    let t1 = gen_equations (tenv_extend (x,nty1) tenv) e1 nty2 in
    [(ty,TyFun (nty1,nty2))] @ t1
  | CALL (e1,e2) ->
    let nty = fresh_tyvar () in
    let t1 = gen_equations tenv e1 (TyFun (nty,ty)) in
    let t2 = gen_equations tenv e2 nty in
    t1 @ t2

let rec unify : (typ * typ) -> subst -> subst
= fun ty subst -> match ty with
  | (TyInt,TyInt) -> subst
  | (TyBool,TyBool) -> subst
  | (TyVar a,ty1) ->
    begin match ty1 with
    | TyVar b ->
      if a = b then subst
      else subst_extend a ty1 subst
    | TyFun (ty2,ty3) ->
      if TyVar a = ty2 || TyVar a = ty3 then raise TypeError
      else subst_extend a (TyFun (ty2,ty3)) subst
    | _ -> subst_extend a ty1 subst
    end
  | (ty1,TyVar a) -> unify (TyVar a,ty1) subst
  | (TyFun (ty1,ty2),TyFun (ty11,ty22)) ->
    let s = unify (ty1,ty11) subst in
    let st1 = subst_apply ty2 s in
    let st2 = subst_apply ty22 s in
    let ss = unify (st1,st2) s in
    ss
  | (_,_) -> raise TypeError

let rec unifyall : typ_eqn -> subst -> subst
= fun eqns subst -> match eqns with
  | [] -> subst
  | (ty1,ty2)::tl ->
    let st1 = subst_apply ty1 subst in
    let st2 = subst_apply ty2 subst in
    let s = unify (st1,st2) subst in
    unifyall tl s

let solve : typ_eqn -> subst
= fun eqns -> unifyall eqns subst_empty

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty