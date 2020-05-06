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
	| CONST n ->
		(ty,TyInt)::[]
	| VAR x ->
		(ty,tenv_find tenv x)::[]
	| ADD (e1,e2) ->
		(ty,TyInt)::(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
	| SUB (e1,e2) ->
		(ty,TyInt)::(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
	| DIV (e1,e2) ->
		(ty,TyInt)::(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
	| MUL (e1,e2) ->
		(ty,TyInt)::(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
	| ISZERO e ->
		(ty,TyBool)::(gen_equations tenv e TyInt)
	| IF (e1,e2,e3) ->
		(gen_equations tenv e1 TyBool)@(gen_equations tenv e2 ty)@(gen_equations tenv e3 ty)
	| LET (x,e1,e2) ->
		let nt = fresh_tyvar () in
		let tenvn = tenv_extend (x,nt) tenv in
		(gen_equations tenv e1 nt)@(gen_equations tenvn e2 ty)
	| PROC (x,e) ->
		let t1 = fresh_tyvar () in
		let t2 = fresh_tyvar () in
		let tenvn = tenv_extend (x,t1) tenv in
		(ty,TyFun (t1,t2))::(gen_equations tenvn e t2)
	| LETREC (f,x,e1,e2) ->
		let nt = fresh_tyvar () in
		let tenvn = tenv_extend (f,nt) tenv in
		let tenve = tenv_extend (x,fresh_tyvar ()) tenvn in
		(gen_equations tenve e1 nt)@(gen_equations tenve e2 ty)
	| CALL (e1,e2) ->
		let nt = fresh_tyvar () in
		(gen_equations tenv e1 (TyFun (nt,ty)))@(gen_equations tenv e2 nt)
	| _ -> raise (Failure "Syntax Error")

let solve : typ_eqn -> subst
=fun eqns ->
	let rec occurs a t =
		match t with
		| TyInt -> false
		| TyBool -> false
		| TyFun (f1,f2) -> (occurs a f1)||(occurs a f2)
		| TyVar x -> a = x
	in let rec unify t1 t2 s =
		if t1 = t2 then s else 
		match t1,t2 with
		| TyVar a, _ ->
			if occurs a t2
			then raise TypeError
			else subst_extend a t2 s
		| _, TyVar _ -> unify t2 t1 s
		| TyFun (f1,f2), TyFun (f3,f4) ->
			let s1 = unify f1 f3 s in
			let s2 = unify (subst_apply f2 s1) (subst_apply f4 s1) s1 in
			s2
		| _,_ -> raise TypeError
	in let rec unifyall eqns s =
		match eqns with
		| [] -> s
		| (t1,t2)::tl ->
			let s1 = unify (subst_apply t1 s) (subst_apply t2 s) s
			in unifyall tl s1
	in unifyall eqns subst_empty

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty