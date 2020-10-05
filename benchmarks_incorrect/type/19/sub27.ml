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
    | READ -> [ty, TyVar "READ"]  (*어쩌지*)
    | CONST i -> [ty, TyInt]
    | VAR v -> [ty, tenv_find tenv v]
    | ADD (e1, e2) -> (ty, TyInt)::(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
    | SUB (e1, e2) -> (ty, TyInt)::(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
    | MUL (e1, e2) -> (ty, TyInt)::(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
    | DIV (e1, e2) -> (ty, TyInt)::(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
    | ISZERO e -> (ty, TyBool)::(gen_equations tenv e TyInt)
    | IF (e1, e2, e3) -> (gen_equations tenv e1 TyBool)@(gen_equations tenv e2 ty)@(gen_equations tenv e3 ty)
    | LET (v, e1, e2) -> let new_tv = fresh_tyvar () in
      (gen_equations tenv e1 new_tv)@(gen_equations (tenv_extend (v, new_tv) tenv) e2 ty)
    | LETREC (f, x, e1, e2) -> let new_tv1 = fresh_tyvar () in let new_tv2 = fresh_tyvar () in
      (gen_equations (tenv_extend (f, TyFun (new_tv1, new_tv2)) (tenv_extend (x, new_tv1) tenv)) e1 new_tv2)
      @(gen_equations (tenv_extend (f, TyFun (new_tv1, new_tv2)) tenv) e2 ty)
    | PROC (v, e) -> let new_tv1 = fresh_tyvar () in let new_tv2 = fresh_tyvar () in
      (ty, TyFun (new_tv1, new_tv2))::(gen_equations (tenv_extend (v, new_tv1) tenv) e new_tv2)
    | CALL (e1, e2) -> let new_tv = fresh_tyvar () in
      (gen_equations tenv e1 (TyFun (new_tv, ty)))@(gen_equations tenv e2 new_tv)

let solve : typ_eqn -> subst
=fun eqns -> 
  let rec hasVar v f =
    match f with
      | TyFun (t1, t2) -> (hasVar v t1) || (hasVar v t2)
      | TyVar v' -> if v=v' then true else false
      | _ -> false in
  let rec solve_withsub eq subst =
    match eq with
      | [] -> subst
      | (TyInt, TyInt)::tl -> solve_withsub tl subst
      | (TyBool, TyBool)::tl -> solve_withsub tl subst
      | (TyInt, TyVar v)::tl -> solve_withsub ((TyVar v, TyInt)::tl) subst
      | (TyBool, TyVar v)::tl -> solve_withsub ((TyVar v, TyBool)::tl) subst
      | (TyFun (t1, t2), TyVar v)::tl -> solve_withsub ((TyVar v, TyFun (t1, t2))::tl) subst
      | (TyFun (t1, t2), TyFun (t3, t4))::tl -> solve_withsub ((t1, t3)::(t2, t4)::tl) subst
      | (TyVar v1, TyVar v2)::tl -> 
        if v1 = v2 then solve_withsub tl subst 
        else let t1 = subst_apply (TyVar v1) subst in let t2 = subst_apply (TyVar v2) subst in
        (match (t1, t2) with
          | (TyVar v1', TyVar v2') -> solve_withsub tl (subst_extend v1' (TyVar v2') subst)
          | _ -> solve_withsub ((t1, t2)::tl) subst)
      | (TyVar v, TyFun (t1, t2))::tl ->
        let tv = subst_apply (TyVar v) subst in let tf = subst_apply (TyFun (t1, t2)) subst in
        (match (tv, tf) with
          | (TyVar v', TyFun (t1', t2')) -> 
            if hasVar v' tf then raise TypeError else solve_withsub tl (subst_extend v' (TyFun (t1', t2')) subst)
          | _ -> solve_withsub ((tv, tf)::tl) subst)
      | (TyVar v, t)::tl -> let ty = subst_apply (TyVar v) subst in
        (match ty with
          | TyVar v' -> solve_withsub tl (subst_extend v' t subst)
          | _ -> solve_withsub ((ty, t)::tl) subst)
      | _ -> raise TypeError in
  solve_withsub eqns []

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty