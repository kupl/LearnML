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
	| VAR x	-> 
		let t = tenv_find tenv x in 
			[(t, ty)]
	| ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) -> 
		let ty1 = gen_equations tenv e1 TyInt in
		let ty2 = gen_equations tenv e2 TyInt in
			([(ty, TyInt)]@ty1@ty2)
	| ISZERO e1 -> 
		let ty1 = gen_equations tenv e1 TyInt in
			([ty, TyBool]@ty1)
	| READ -> []
	| IF (e1, e2, e3) -> 
		let ty1 = gen_equations tenv e1 TyBool in
		let ty2 = gen_equations tenv e2 ty in
		let ty3 = gen_equations tenv e3 ty in
			(ty1@ty2@ty3)
	| LET (x, e1, e2) -> 
		let a = fresh_tyvar () in
		let ty1 = gen_equations tenv e1 a in
		let new_tenv = tenv_extend (x, a) tenv in
		let ty2 = gen_equations new_tenv e2 ty in
			(ty1@ty2)
	| LETREC (f, x, e1, e2) ->
		let a = fresh_tyvar () in
		let b = fresh_tyvar () in
		let ty1 = [(ty, TyFun (a, b))] in
		let new_tenv = tenv_extend (x, a) tenv in
		let ty2 = gen_equations new_tenv e1 b in
			(ty1@ty2)
	| PROC (x, e1) -> 
		let a = fresh_tyvar () in
		let b = fresh_tyvar () in
		let ty1 = [(ty, TyFun (a, b))] in
		let new_tenv = tenv_extend (x, a) tenv in
		let ty2 = gen_equations new_tenv e1 b in
			(ty1@ty2)
	| CALL (e1, e2) -> 
		let a = fresh_tyvar () in
		let ty1 = gen_equations tenv e1 (TyFun (a, ty)) in
		let ty2 = gen_equations tenv e2 a in
			(ty1@ty2)

let rec typ_src : typ -> typ -> bool
= fun ta tt ->
	match tt with
	| TyInt | TyBool ->  
		if ta = tt then true else false
	| TyVar t ->
		if ta = tt then true else false
	| TyFun (t1, t2) ->  
		(typ_src ta t1) || (typ_src ta t2)

let rec unify : typ -> typ -> subst -> subst
=fun t1 t2 s -> 
	match (t1,t2) with
	| (TyInt, TyInt) -> s
	| (TyBool, TyBool) -> s
	| (TyVar a,t) ->
		if typ_src (TyVar a) t then raise TypeError
		else subst_extend a t s 
	| (t,TyVar a) -> unify (TyVar a) t s
	| (TyFun (t1,t2), TyFun (t1p,t2p)) ->
		let sp = unify t1 t1p s in
		let t1pp = subst_apply t2 sp in
		let t2pp = subst_apply t2p sp in
		let spp = unify t1pp t2pp sp in
			spp
	| _ -> raise TypeError
 
let rec unifyall : typ_eqn -> subst -> subst
=fun eqn s -> 
	match eqn with
	| (t1,t2)::u ->
		let ty1 = subst_apply t1 s in
		let ty2 = subst_apply t2 s in
        let sp = unify ty1 ty2 s in
			unifyall u sp
	| [] -> s
 
let solve : typ_eqn -> subst
=fun eqn -> unifyall eqn subst_empty

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty