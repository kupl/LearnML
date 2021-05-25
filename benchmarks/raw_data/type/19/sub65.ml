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
  | CONST k -> (ty,TyInt)::[]
  | VAR k -> (ty,tenv_find tenv k)::[]
  | ADD (e1, e2) -> let (a1,a2) = (fresh_tyvar (), fresh_tyvar ()) in
                    [(ty,TyInt);(a1,TyInt);(a2,TyInt)] @ (gen_equations tenv e1 a1) @ (gen_equations tenv e2 a2)
  | MUL (e1, e2) -> let (a1,a2) = (fresh_tyvar (), fresh_tyvar ()) in
                    [(ty,TyInt);(a1,TyInt);(a2,TyInt)] @ (gen_equations tenv e1 a1) @ (gen_equations tenv e2 a2)
  | SUB (e1, e2) -> let (a1,a2) = (fresh_tyvar (), fresh_tyvar ()) in
                    [(ty,TyInt);(a1,TyInt);(a2,TyInt)] @ (gen_equations tenv e1 a1) @ (gen_equations tenv e2 a2)
  | DIV (e1, e2) -> let (a1,a2) = (fresh_tyvar (), fresh_tyvar ()) in
                    [(ty,TyInt);(a1,TyInt);(a2,TyInt)] @ (gen_equations tenv e1 a1) @ (gen_equations tenv e2 a2)
  | ISZERO e1 -> let a1 = fresh_tyvar () in
                 [(ty,TyBool)] @ (gen_equations tenv e1 a1)
  | IF (e1, e2, e3) -> let (a1,a2,a3)  = (fresh_tyvar (), fresh_tyvar (), fresh_tyvar ()) in
                       [(ty,a2);(a2,a3);(a1,TyBool)] @ (gen_equations tenv e1 a1) @ (gen_equations tenv e2 a2) @ (gen_equations tenv e3 a3)
  | LET (k, e1, e2) -> let (a1,a2,ak) = (fresh_tyvar (), fresh_tyvar (), fresh_tyvar()) in
                       [(ty,a2);(ak,a1)] @ (gen_equations tenv e1 a1) @ (gen_equations (tenv_extend (k,a1) tenv) e2 a2)
  | LETREC (k1, k2, e1, e2) -> let (ak1, ak2, a1, a2) = (fresh_tyvar (), fresh_tyvar (), fresh_tyvar (), fresh_tyvar ()) in
                            [(ty,a2); (ak1,TyFun(ak2,a1))] @ (gen_equations (tenv_extend (k2,ak2) tenv) e1 a1) @ (gen_equations (tenv_extend (k1,ak1) tenv) e2 a2)
  | PROC (k, e1) -> let (ak, a1) = (fresh_tyvar (), fresh_tyvar ()) in
                    [(ty,TyFun(ak,a1))] @ (gen_equations (tenv_extend (k,ak) tenv) e1 a1)
  | CALL (e1, e2) -> let (a1, a2, ar) = (fresh_tyvar (), fresh_tyvar (), fresh_tyvar ()) in
                     [(ty,ar);(a1,TyFun(a2,ar))] @ (gen_equations tenv e1 a1) @ (gen_equations tenv e2 a2)
  | _ -> raise TypeError


let solve : typ_eqn -> subst
=fun eqns -> 
  let rec insolve eqns ret = match eqns with
    | [] -> ret
    | (a1,a2)::t -> let (a1, a2) = (subst_apply a1 ret, subst_apply a2 ret) in
      match (a1,a2) with
        | (TyVar a, TyVar b) -> if a = b then (raise TypeError) else insolve t (subst_extend a (TyVar b) ret)
        | (a, TyVar b) -> insolve ((TyVar b,a)::t) ret
        | (TyFun(a1,a2), TyFun(b1,b2)) -> insolve ((a1,b1)::(a2,b2)::t) ret
        | (TyVar a, b) -> insolve t (subst_extend a b ret)
        | (a,b) -> if a = b then insolve t ret else (raise TypeError)
        | _ -> raise TypeError
  in insolve eqns []

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty