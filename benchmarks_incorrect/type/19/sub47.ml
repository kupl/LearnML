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
		| CONST n -> [(ty, (TyInt))]
		| VAR x -> [(ty, tenv_find tenv x)]
		| ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) ->
			let x1 = gen_equations tenv e1 TyInt in
			let x2 = gen_equations tenv e2 TyInt in
			let x3 = [(ty, TyInt)]
			in x1 @ x2 @ x3
		| ISZERO e1 -> 
			let x1 = [(ty, TyBool)] in
			let x2 = gen_equations tenv e1 TyInt
			in x1 @ x2
		| IF (e1, e2, e3) ->
			let x1 = gen_equations tenv e1 TyBool in
			let x2 = gen_equations tenv e2 ty in
			let x3 = gen_equations tenv e3 ty in
			x1 @ x2 @ x3
		| LET (x, e1, e2) -> 
			let t1 = fresh_tyvar() in
			let x1 = gen_equations tenv e1 t1 in
			let x2 = gen_equations (tenv_extend(x, t1) tenv) e2 ty in
			x1 @ x2
		| LETREC (f, x, e1, e2) -> 
			let t1 = fresh_tyvar() in
			let t2 = fresh_tyvar() in
			let x1 = gen_equations (tenv_extend(x, t2) (tenv_extend(f, TyFun(t2, t1)) tenv)) e1 t1 in
			let x2 = gen_equations (tenv_extend( f, TyFun(t2, t1) ) tenv) e2 ty in
			x1 @ x2
		| PROC (x, e1) -> 
			let t1 = fresh_tyvar() in
			let t2 = fresh_tyvar() in
			let x1 = [ (ty, TyFun (t1, t2)) ] in
			let x2 = gen_equations (tenv_extend (x, t1) tenv) e1 t2 in
			x1 @ x2
		| CALL (e1, e2) -> 
			let t1 = fresh_tyvar() in
			let x1 = gen_equations tenv e1 (TyFun (t1,ty)) in
			let x2 = gen_equations tenv e2 t1 in
			x1 @ x2


let rec unify : typ -> typ -> subst -> subst
=fun t1 t2 s ->
	match (t1, t2) with
		| (TyInt, TyInt) | (TyBool, TyBool) -> s

		| (t, TyVar alpha) -> unify (TyVar alpha) t s

		| (TyVar t1, t2) -> subst_extend t1 t2 s

		| (TyFun (t1,t2), TyFun (t3,t4)) ->
			let s1 = unify t1 t3 s in
			let s2 = unify (subst_apply t2 s1) (subst_apply t4 s1) s1 in
			s2

let rec unify_all : typ_eqn -> subst -> subst
=fun eqn s ->
	match eqn with
		| [] -> s
		| (t1, t2) :: u ->
			let s1 = unify (subst_apply t1 s) (subst_apply t2 s) s in
			unify_all u s1


let solve : typ_eqn -> subst
=fun eqns -> unify_all eqns subst_empty



let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty