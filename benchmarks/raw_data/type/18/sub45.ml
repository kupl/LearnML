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
  | VAR v -> [(ty, (tenv_find tenv v))]
  | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) -> [ty, TyInt]@((gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt))
  | ISZERO e -> [ty, TyBool]@(gen_equations tenv e TyInt)
  | READ -> [(ty, TyInt)]
  | IF (e1, e2 ,e3)-> (gen_equations tenv e1 TyBool)@(gen_equations tenv e2 ty)@(gen_equations tenv e3 ty)
  | LET (x, e1, e2) ->
    let t1 = fresh_tyvar() in
    (gen_equations tenv e1 t1)@(gen_equations (tenv_extend (x, t1) tenv) e2 ty)
  | LETREC (f, x, e1, e2) -> 
    let t1 = fresh_tyvar() in
    let t2 = fresh_tyvar() in
    (gen_equations (tenv_extend (f, TyFun(t2, t1)) (tenv_extend (x, t2) tenv)) e1 t1)@(gen_equations (tenv_extend (f, TyFun(t2,t1)) tenv) e2 ty)
  | PROC (x, e) ->
    let t1 = fresh_tyvar() in
    let t2 = fresh_tyvar() in
    [(ty, TyFun(t1, t2))]@(gen_equations (tenv_extend (x, t1) tenv) e t2)
  | CALL (e1, e2) ->
    let t1 = fresh_tyvar() in
    (gen_equations tenv e1 (TyFun (t1, ty)))@(gen_equations tenv e2 t1)

(*******SUBSTITUTION***********SUBSTITUTION***********SUBSTITUTION***********SUBSTITUTION************)
let rec occurs : string -> typ -> bool
=fun a ty->
	match ty with
| TyVar v -> if a = v then true else false
| TyFun (t1,t2) -> (occurs a t1) || (occurs a t2)
| _ -> false

let rec unify :typ -> typ -> subst -> subst
=fun t1 t2 subst ->
	if t1 == t2 then subst else
		(match t1,t2 with
	| TyVar a,ty -> if (occurs a ty) then raise TypeError else subst_extend a ty subst 
	| ty,TyVar a -> unify (TyVar a) ty subst
	| (TyFun (t1,t2),TyFun (t1',t2')) ->
			let subst' = unify t1 t1' subst in
			let t1'' = subst_apply t2 subst' in
			let t2'' = subst_apply t2' subst' in
				unify t1'' t2'' subst'
	| _,_ -> raise TypeError)

let rec unifyall : typ_eqn -> subst -> subst
=fun eqn subst ->
	match eqn with
| [] -> subst
| ((t1,t2)::u) ->
		let t1' = subst_apply t1 subst in
		let t2' = subst_apply t2 subst in
		let subst' = unify t1' t2' subst in
			unifyall u subst'

let solve : typ_eqn -> subst
=fun eqn ->
	match eqn with
| [] -> []
| _ -> unifyall eqn []

(******************************************************************************)

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty