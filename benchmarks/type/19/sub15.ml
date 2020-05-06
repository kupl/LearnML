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
=fun tenv e ty ->  (*여기채우기1 *)
    match e with
  | CONST n -> [(ty, TyInt)]
  | VAR x -> [(ty, tenv_find tenv x)]
  | ADD (e1, e2) -> (ty, TyInt)::(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
  | SUB (e1, e2) -> (ty, TyInt)::(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
  | ISZERO e -> (ty, TyBool)::(gen_equations tenv e TyInt)
  | IF (e, e1, e2) -> let typevariable=fresh_tyvar () in 
                          (gen_equations tenv e1 TyBool)@(gen_equations tenv e2 typevariable)@(gen_equations tenv e2 typevariable)
  | LET (x, e1, e2) -> let typevariable=fresh_tyvar () in
                          (gen_equations tenv e1 typevariable)@(gen_equations (tenv_extend (x,typevariable) tenv) e2 ty)
  | PROC (x, e) -> let typevariable=fresh_tyvar () in
                        let typevariable2=fresh_tyvar () in
                            (ty, TyFun (typevariable, typevariable2))::(gen_equations (tenv_extend (x,typevariable) tenv) e typevariable2)
  | CALL (e1, e2) -> let typevariable=fresh_tyvar () in
                         (gen_equations tenv e1 (TyFun(typevariable, ty)))@(gen_equations tenv e2 typevariable)
  

(*여기까지1*)

let solve : typ_eqn -> subst
=fun eqns ->  (* 여기채우기2 *)
(let rec unifyall : typ_eqn -> subst -> subst
            =fun eqns s -> (match eqns with
            |[] -> s
            |(t1, t2)::tl -> (let s'=(let rec unify_fun : typ -> typ -> subst -> subst
                                      =fun t1 t2 sub -> (match (t1, t2, sub) with
                                                        |(TyInt, TyInt, sub) -> sub
                                                        |(TyBool, TyBool, sub) -> sub
                                                        |(TyVar a, t, sub) -> (match (let rec finding_fun : typ -> typ -> bool
                                                                      =fun ty1 ty2 -> (match ty2 with
                                                                                      |TyFun(ty1', ty2') -> if (ty1'=ty1 || ty2'=ty1) 
                                                                                                            then true 
                                                                                                            else (finding_fun ty1 ty1')||(finding_fun ty1 ty2')
                                                                                      |_ -> false)
                                                                                      in (finding_fun (TyVar a) t)) with 
                                                                      |true -> raise TypeError
                                                                      |false -> (subst_extend a t sub))
                                                        |(t, TyVar a, sub) -> (unify_fun (TyVar a) t sub)
                                                        |(TyFun (t1, t2), TyFun (t1', t2'), sub) -> (let sub' = (unify_fun t1 t1' sub) in
                                                                                                  let t1''= (subst_apply t2 sub') in
                                                                                                  let t2''= (subst_apply t2' sub') in
                                                                                                  (unify_fun t1'' t2'' sub'))
                                                        |_ -> raise TypeError) in (unify_fun (subst_apply t1 s) (subst_apply t2 s) s))
                              in (unifyall tl s')))
            in(unifyall eqns subst_empty))
(*여기까지2*)

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty