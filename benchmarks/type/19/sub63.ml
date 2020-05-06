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
  | CONST n -> [(TyInt, ty)]
  | VAR v -> [(tenv v, ty)]
  | ADD (e1, e2) -> if (gen_equations tenv e1 ty = [(TyInt, ty)]) && (gen_equations tenv e2 ty = [(TyInt, ty)]) then [(TyInt, TyInt)] else raise TypeError
  | SUB (e1, e2) -> if (gen_equations tenv e1 ty = [(TyInt, ty)]) && (gen_equations tenv e2 ty = [(TyInt, ty)]) then [(TyInt, TyInt)] else raise TypeError
  | MUL (e1, e2) -> if (gen_equations tenv e1 ty = [(TyInt, ty)]) && (gen_equations tenv e2 ty = [(TyInt, ty)]) then [(TyInt, TyInt)] else raise TypeError
  | DIV (e1, e2) -> if (gen_equations tenv e1 ty = [(TyInt, ty)]) && (gen_equations tenv e2 ty = [(TyInt, ty)]) then [(TyInt, TyInt)] else raise TypeError
  | ISZERO e -> if (gen_equations tenv e ty = [(TyInt, ty)]) then [(TyInt, TyBool)] else raise TypeError
  | IF (e1, e2, e3) -> if (gen_equations tenv e1 ty = [(TyBool, ty)]) && (gen_equations tenv e2 ty = gen_equations tenv e3 ty) then [TyBool, ty] else raise TypeError
  |_ -> raise TypeError

let solve : typ_eqn -> subst
=fun eqns -> subst_empty (* TODO *)

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty