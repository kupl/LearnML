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
=fun tenv ex ty ->
match ex with 
| CONST n -> [(ty, TyInt)]
| VAR x -> [(ty, (tenv_find tenv x))]
| ADD (exp1, exp2) -> [(ty, TyInt)]@(gen_equations tenv exp1 TyInt)@(gen_equations tenv exp2 TyInt)
| SUB (exp1, exp2) -> [(ty, TyInt)]@(gen_equations tenv exp1 TyInt)@(gen_equations tenv exp2 TyInt)
| MUL (exp1, exp2) -> [(ty, TyInt)]@(gen_equations tenv exp1 TyInt)@(gen_equations tenv exp2 TyInt)
| DIV (exp1, exp2) -> [(ty, TyInt)]@(gen_equations tenv exp1 TyInt)@(gen_equations tenv exp2 TyInt)
| IF (exp1, exp2, exp3) -> (gen_equations tenv exp1 TyBool)@(gen_equations tenv exp2 ty)@(gen_equations tenv exp3 ty)
| LET (a, exp1, exp2) -> let b = fresh_tyvar () in (gen_equations tenv exp1 b)@(gen_equations (tenv_extend (a, b) tenv) exp2 ty)
| LETREC (f, x, exp1, exp2) -> let a = fresh_tyvar () in let b = fresh_tyvar () in (gen_equations (tenv_extend (f, TyFun(b, a))(tenv_extend (x,b) tenv)) exp1 a)@(gen_equations (tenv_extend (f,TyFun(b,a)) tenv) exp2 ty)
| PROC (x, exp1) -> let a = fresh_tyvar () in let b = fresh_tyvar () in [(ty, TyFun (a, b))]@(gen_equations (tenv_extend (x, a) tenv) exp1 b)
| CALL (exp1, exp2) -> let a = fresh_tyvar () in (gen_equations tenv exp1 (TyFun(a, ty)))@(gen_equations tenv exp2 a)
| ISZERO exp1 -> [(ty, TyBool)]@(gen_equations tenv exp1 TyInt)
| READ -> [(ty, TyInt)]
| _ -> raise TypeError


let rec solve : typ_eqn -> subst
=fun eqns ->
let rec unify : typ -> typ -> subst -> subst
= fun ty1 ty2 subs ->
match (ty1, ty2, subs) with
| (TyInt, TyInt, subs) -> subs
| (TyBool, TyBool, subs) -> subs
| (TyVar a1, TyVar a2, subs) -> if a1 != a2 then subst_extend a1 (TyVar a2) subs else subs
| ((TyVar a), t, subs) -> let rec occ : typ -> typ -> bool 
                        = fun a t ->
                        match t with
                        | TyVar a2 -> a = t
                        | TyFun (b1, b2) -> (occ a b1) || (occ a b2)
                        | _ -> false 
                        in
                      if occ (TyVar a) t then (raise TypeError)
                      else subst_extend a t subs
| (t, (TyVar a), subs) -> unify (TyVar a) t subs
| (TyFun (t1, t2), TyFun (tt1, tt2), subs) -> let ang = unify t1 tt1 subs in unify (subst_apply t2 ang) (subst_apply tt2 ang) ang
| (_ ,_ ,_) -> raise TypeError
in
let rec unifyEverything : typ_eqn -> subst -> subst
= fun typequation su ->
match typequation with
| [] -> su
| (t1,t2)::u -> let ang = unify (subst_apply t1 su) (subst_apply t2 su) su in
                unifyEverything u ang
in
unifyEverything eqns subst_empty

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty