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


let rec gen_equations : tenv -> exp -> typ -> typ_eqn  = fun tenv e ty ->
  match e with
    | CONST e -> [(ty, TyInt)]
    | VAR e -> [(ty, tenv e)]
    | ADD (e1, e2)
    | SUB (e1, e2)
    | MUL (e1, e2)
    | DIV (e1, e2) -> [(ty, TyInt)] @ (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
    | ISZERO e -> [(ty, TyBool)] @ (gen_equations tenv e TyInt)
    | READ -> [] (* 이건 어떻게 하지? *)
    | IF (e1, e2, e3) -> (gen_equations tenv e1 TyBool) @ (gen_equations tenv e2 ty) @ (gen_equations tenv e3 ty)
    | LET (v1, e1, e2) -> let newTy = fresh_tyvar() in (gen_equations tenv e1 newTy) @ (gen_equations (tenv_extend(v1, newTy) tenv) e2 ty)
    | LETREC (v1, v2, e1, e2) -> 
      let newTy1 = fresh_tyvar() in
      let newTy2 = fresh_tyvar() in
      let newEnv1 = tenv_extend (v1, TyFun(newTy1, newTy2)) tenv in
      let newEnv2 = tenv_extend (v2, newTy1) newEnv1 in
      (gen_equations newEnv2 e1 newTy2) @ (gen_equations newEnv1 e2 ty)
    | PROC (v1, e1) -> 
      let newTy1 = fresh_tyvar() in
      let newTy2 = fresh_tyvar() in
      [(ty, TyFun(newTy1, newTy2))] @ (gen_equations (tenv_extend(v1, newTy1) tenv) e1 newTy2)
    | CALL (e1, e2) -> let newTy = fresh_tyvar() in (gen_equations tenv e1 (TyFun(newTy, ty))) @ (gen_equations tenv e2 newTy)
    | _ -> raise TypeError

let rec check_occur : var -> typ -> bool = fun v t ->
  match t with
    | TyInt
    | TyBool -> false
    | TyVar e -> if e = v then true else false
    | TyFun (e1, e2) -> (check_occur v e1) || (check_occur v e2)

let rec unify : (typ * typ) -> subst -> subst = fun eqn sub ->
        match eqn with
          | (TyInt, TyInt) -> sub
          | (TyBool, TyBool) -> sub
          | (TyVar v, ty) -> 
            (match ty with
              | TyVar e -> if v = e then sub else (subst_extend v ty sub)
              | TyFun (e1, e2) -> if (check_occur v e1) || (check_occur v e2) then raise TypeError else (subst_extend v ty sub)
              | _ -> subst_extend v ty sub)
          | (ty, TyVar v) -> unify (TyVar v, ty) sub
          | (TyFun (e1, e2), TyFun(e3, e4)) ->
            let newSub = unify (e1, e2) sub in
            let v1 = subst_apply e4 newSub in
            let v2 = subst_apply e2 newSub in
            unify (v1, v2) newSub
          | (_, _) -> raise TypeError

let rec unification : typ_eqn -> subst -> subst = fun eqn sub ->
  match eqn with
    | [] -> sub
    | (t1, t2)::tl -> let unified_sub = unify (subst_apply t1 sub, subst_apply t2 sub) sub in unification tl unified_sub

let solve : typ_eqn -> subst = fun eqns -> unification eqns subst_empty

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty