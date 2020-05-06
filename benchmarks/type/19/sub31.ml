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


(* TOOOO DOOOO *)
let rec append eq1 eq2 =
  match eq1 with
  | [] -> eq2
  | hd::tl -> hd::(append tl eq2)
(* TOOOO DOOOO *)
let rec gen_equations : tenv -> exp -> typ -> typ_eqn 
=fun tenv e ty -> 
  match e with
  | CONST n -> (ty, TyInt)::[]
  | VAR x -> (tenv_find tenv x, ty)::[]
  | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) -> 
    let eq1 = (ty, TyInt)::(gen_equations tenv e1 TyInt)
    in let eq2 = (gen_equations tenv e2 TyInt)
    in append eq1 eq2
  | ISZERO (e1) -> (ty, TyBool)::(gen_equations tenv e1 TyInt)
  | IF (e1,e2,e3) -> 
    let eq1 = gen_equations tenv e1 TyBool
    in let eq2 = gen_equations tenv e2 ty
    in let eq3 = gen_equations tenv e3 ty
    in append (append eq1 eq2) eq3
  | LET (x,e1,e2) -> 
    let a = fresh_tyvar () (* typ *)
    in let eq1 = gen_equations tenv e1 a
    in let eq2 = gen_equations (tenv_extend (x,a) tenv) e2 ty
    in append eq1 eq2
  | LETREC (f,x,e1,e2) ->
    let a1 = fresh_tyvar ()
    in let a2 = fresh_tyvar ()
    in let eq1 = gen_equations (tenv_extend (f,TyFun(a2,a1)) (tenv_extend (x,a2) tenv)) e1 a1
    in let eq2 = gen_equations (tenv_extend (f,TyFun(a2,a1)) tenv) e2 ty
    in append eq1 eq2
  | PROC (x,e1) ->
    let a1 = fresh_tyvar ()
    in let a2 = fresh_tyvar ()
    in (ty, TyFun (a1,a2))::(gen_equations (tenv_extend (x,a1) tenv) e1 a2)
  | CALL (e1,e2) ->
    let a = fresh_tyvar ()
    in let eq1 = gen_equations tenv e1 (TyFun (a,ty))
    in let eq2 = gen_equations tenv e2 a
    in append eq1 eq2

let solve : typ_eqn -> subst
=fun eqns -> 
(* subst_empty *)
  let rec unify eqn s = 
    match eqn with
    | (TyInt,TyInt) -> s
    | (TyBool,TyBool) -> s
    | (TyVar x, TyVar y) -> s
    | (TyVar x, TyInt) -> subst_extend x TyInt s
    | (TyVar x, TyBool) -> subst_extend x TyBool s
    | (TyInt, TyVar x) -> subst_extend x TyInt s
    | (TyBool, TyVar x) -> subst_extend x TyBool s
    | (TyVar x, TyFun (y,z)) ->
      let tempS' = subst_extend "temp" (TyFun (y,z)) []
      in if ((subst_apply (TyVar x) tempS')=(TyVar x)) then subst_extend x (TyFun (y,z)) s else raise TypeError
    | (TyFun (y,z), TyVar x) -> unify ((TyVar x), (TyFun (y,z))) s
    | (TyFun (a,b), TyFun (c,d)) -> 
      let s' = unify (a,c) s
      in let s'' = unify ((subst_apply b s'),(subst_apply d s')) s'
      in s''
    | (_,_) -> raise TypeError
  in let rec unifyAll eqn s =
    match eqn with
    | [] -> s
    | (x,y)::tl ->  
      let s' = unify (subst_apply x s, subst_apply y s) s
      in unifyAll tl s'
  in unifyAll eqns []

(* TOOOO DOOOO *)
(* TOOOO DOOOO *)

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty