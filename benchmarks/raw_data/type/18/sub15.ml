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




(* ----------------------------- my code start ------------------------------ *)



let rec gen_equations : tenv -> exp -> typ -> typ_eqn 
=fun tenv e ty -> match e with
  | CONST _ -> [(ty, TyInt)]
  | VAR x -> [(ty, tenv_find tenv x)]
  | ADD (a1, a2) -> [(ty, TyInt)]@(gen_equations tenv a1 TyInt)@(gen_equations tenv a2 TyInt)
  | SUB (s1, s2) -> [(ty, TyInt)]@(gen_equations tenv s1 TyInt)@(gen_equations tenv s2 TyInt)
  | MUL (m1, m2) -> [(ty, TyInt)]@(gen_equations tenv m1 TyInt)@(gen_equations tenv m2 TyInt)
  | DIV (d1, d2) -> [(ty, TyInt)]@(gen_equations tenv d1 TyInt)@(gen_equations tenv d2 TyInt)
  | ISZERO iz -> [(ty, TyBool)]@(gen_equations tenv iz TyInt)
  | READ -> [(ty, TyInt)]
  | IF (i1, i2, i3) -> (gen_equations tenv i1 TyBool)@(gen_equations tenv i2 ty)@(gen_equations tenv i3 ty)
  | LET (lx, l1, l2) -> let newty = fresh_tyvar () in (gen_equations tenv l1 newty)@(gen_equations (tenv_extend (lx, newty) tenv) l2 ty)
  | LETREC (rx1, rx2, re1, re2) -> let nt1 = fresh_tyvar () in let nt2 = fresh_tyvar () in 
    let tempenv = tenv_extend (rx1, TyFun(nt1, nt2)) tenv in 
    (gen_equations (tenv_extend (rx2, nt1) tempenv) re1 nt2)@(gen_equations tempenv re2 ty)
  | PROC (px, pe) -> let newty1 = fresh_tyvar () in let newty2 = fresh_tyvar () in
    [(ty, TyFun (newty1, newty2))]@(gen_equations (tenv_extend (px, newty1) tenv) pe newty2)
  | CALL (c1, c2) -> let newty = fresh_tyvar () in (gen_equations tenv c1 (TyFun (newty, ty)))@(gen_equations tenv c2 newty)

let rec occurs : typ -> typ -> bool
  = fun str ty -> match ty with
    | TyFun(x, y) -> if (x = str || y = str) then true else (occurs str x || occurs str y)
    | _ -> false

let rec unify : typ -> typ -> subst -> subst
  = fun t1 t2 s -> match (t1, t2, s) with
    | (TyInt, TyInt, s) -> s
    | (TyBool, TyBool, s) -> s
    | (TyVar a, TyVar b, s) -> if a = b then s else subst_extend a (TyVar b) s
    | (TyVar a, b, s) -> if occurs (TyVar a) b then raise TypeError else subst_extend a b s
    | (a, TyVar b, s) -> unify (TyVar b) a s
    | (TyFun(a1,b1), TyFun(a2,b2), s) -> let ss = unify a1 a2 s in let sss = unify (subst_apply b1 ss) (subst_apply b2 ss) ss in sss
    | (_, _, _) -> raise TypeError

let rec unifyall : typ_eqn -> subst -> subst
  = fun eqn s -> match (eqn, s) with
    | ([], s) -> s
    | ((t1, t2)::tl, s) -> let ss = unify (subst_apply t1 s) (subst_apply t2 s) s in unifyall tl ss

let solve : typ_eqn -> subst
=fun eqns -> let _ = tyvar_num := 0 in unifyall eqns subst_empty

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty