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
  | CONST a -> (ty, TyInt) :: []
  | VAR a -> (tenv_find tenv a, ty) :: []
  | ADD (e1, e2) -> (ty, TyInt) :: ((gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt))
  | SUB (e1, e2) -> (ty, TyInt) :: ((gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt))
  | MUL (e1, e2) -> (ty, TyInt) :: ((gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt))
  | DIV (e1, e2) -> (ty, TyInt) :: ((gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt))
  | ISZERO ex -> (ty, TyBool) :: (gen_equations tenv ex TyInt)
  | READ -> (ty, TyInt) :: []
  | IF (e1, e2, e3) -> (gen_equations tenv e1 TyBool) @ (gen_equations tenv e2 ty) @ (gen_equations tenv e2 ty)
  | LET (v, e1, e2) -> let t1 = fresh_tyvar() in (gen_equations tenv e1 t1) @ (gen_equations (tenv_extend (v, t1) tenv) e2 ty)
  | LETREC (f, v, e1, e2) -> let tx = fresh_tyvar() in let t1 = fresh_tyvar() in 
    (gen_equations((tenv_extend (f, TyFun(tx, t1)) (tenv_extend (v, t1) tenv))) e1 t1) @ (gen_equations (tenv_extend (f, TyFun(tx, t1)) (tenv_extend (v, t1) tenv)) e2 ty)
  | PROC (x, e) -> let t1 = fresh_tyvar() in let t2 = fresh_tyvar() in (ty, TyFun(t1, t2)) :: (gen_equations (tenv_extend (x, t1) tenv) e t2)
  | CALL (e1, e2) -> let t1 = fresh_tyvar() in (gen_equations tenv e1 (TyFun(t1, ty))) @ (gen_equations tenv e2 t1)
 
let solve : typ_eqn -> subst
=fun eqns -> 
  let rec find_tyvar typevar typ =
    match typ with
    | TyInt -> false
    | TyBool -> false
    | TyFun (t1, t2) -> if find_tyvar typevar t1 then true else find_tyvar typevar t2
    | TyVar v -> if typevar = v then true else false
  and subst_find typevar subst =
    match subst, typevar with
    | [], TyVar t -> typevar
    | (tv, tp)::tl, TyVar t ->
      if tv = t
      then tp
      else subst_find typevar tl
    | _, _ -> raise (Failure "error")
  and substitution typevar subst =
    match typevar with
    | TyInt -> TyInt
    | TyBool -> TyBool
    | TyFun (t1, t2) -> TyFun (substitution t1 subst, substitution t2 subst)
    | TyVar a -> subst_find (TyVar a) subst
  and unify t1 t2 subst =
    match t1, t2 with
    | TyInt, TyInt -> subst
    | TyBool, TyBool -> subst
    | TyVar v1, t ->
     (match t with
      | TyInt -> subst_extend v1 t subst
      | TyBool -> subst_extend v1 t subst
      | TyFun (t1, t2) -> 
        if (if find_tyvar v1 t1 then true else find_tyvar v1 t2)
        then raise (Failure "v1 occurs in t")
        else subst_extend v1 (TyFun (t1, t2)) subst
      | TyVar v2 ->
        if v1 = v2
        then subst
        else subst_extend v1 (TyVar v2) subst)
    | t, TyVar v1 -> unify t2 t1 subst
    | TyFun (t1, t2), TyFun (t3, t4) ->
      let subst1 = unify t1 t3 subst in
      let subst2 = unify (substitution t2 subst1) (substitution t4 subst1) subst1 in subst2
    | _, _ -> raise (Failure "fail")
  and unifyall tyeqn subst =
    match tyeqn with
    | [] -> subst
    | (t1, t2)::tl -> let s = unify (substitution t1 subst) (substitution t2 subst) subst in unifyall tl s
  in unifyall eqns []

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty