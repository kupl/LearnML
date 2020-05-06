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
    | CONST i -> [(ty, TyInt)]
    | VAR v -> [(ty, tenv v)]
    | ADD (e1, e2) -> (ty, TyInt) ::(gen_equations tenv e1 TyInt) @(gen_equations tenv e2 TyInt)
    | SUB (e1, e2) -> (ty, TyInt) ::(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
    | MUL (e1, e2) -> (ty, TyInt) ::(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
    | DIV (e1, e2) -> (ty, TyInt) ::(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
    | ISZERO i -> (ty, TyBool)::(gen_equations tenv i TyInt)
    | IF (b, tr, f) -> (gen_equations tenv b TyBool)
    @ (gen_equations tenv tr ty)
    @ (gen_equations tenv f ty)
    | LET (v, e1, e2) -> let tv = fresh_tyvar () in
    (gen_equations tenv e1 tv) (* e1 *)
    @(gen_equations (tenv_extend (v, tv) tenv) e2 ty)
    | PROC (arg, exp) ->
    let t_arg = fresh_tyvar () in
    let t_exp = fresh_tyvar () in
    (ty, TyFun (t_arg, t_exp))
    :: (gen_equations (tenv_extend (arg, t_arg) tenv) exp t_exp)
    | CALL (f, arg) ->
    let t_arg = fresh_tyvar () in
    (gen_equations tenv f (TyFun (t_arg, ty))) @ (gen_equations tenv arg t_arg)
    | LETREC (f, arg, body, exp) ->
    let t_arg = fresh_tyvar () in
    let t_body = fresh_tyvar () in
    let tenv1 = (tenv_extend (f, TyFun (t_body, t_arg)) tenv) in
    let tenv2 = (tenv_extend (arg, t_body) tenv1) in
    (gen_equations tenv2 body t_body)
    @ (gen_equations tenv1 exp ty)
    | READ -> [(ty, TyInt)];;

    (*| _ -> raise TypeError;;*)

let rec occur: tyvar -> typ -> bool
  = fun v t -> match t with
    | TyVar x -> if v = x then true else false
    | TyFun (x, y) -> if (occur v x) || (occur v y) then true else false
    | TyInt -> false
    | TyBool -> false
;;

let rec unify: typ -> typ -> subst -> subst
  = fun t1 t2 s -> match t1, t2 with
    | TyInt, TyInt -> s
    | TyBool, TyBool -> s
    (*| TyVar a, TyVar b -> if a = b then s else raise TypeError*)
    | TyVar a, _ -> if (occur a t2) then raise TypeError else subst_extend a t2 s
    | t, TyVar v -> unify (TyVar v) t s
    | (TyFun (t1, t2), TyFun (t1', t2')) ->
      let s' = unify t1 t1' s in
      let first_arg = subst_apply t2 s' in
      let second_arg = subst_apply t2' s' in
      let s'' = unify first_arg second_arg s' in
      s''
    | _ -> raise TypeError
;;

let rec unifyall: typ_eqn -> subst -> subst
  = fun eqns s -> match eqns with
    | [] -> s
    | (tv, tt)::tl -> let s' = unify (subst_apply tv s) (subst_apply tt s) s in
    unifyall tl s';;

let solve : typ_eqn -> subst
=fun eqns -> unifyall eqns subst_empty

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty