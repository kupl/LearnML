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


let rec gen_equations : tenv -> exp -> typ -> typ_eqn (*typ_eqn is a sort of list*)
=fun tenv e ty -> 
  match e with
    | CONST n         ->  [(ty, TyInt)]
    | VAR x           ->  [(ty, tenv_find tenv x)]
    | ADD (e1, e2)    ->  let eqns1 = gen_equations tenv e1 TyInt in
                          let eqns2 = gen_equations tenv e2 TyInt in
                          ([(ty, TyInt)]@eqns1)@eqns2
    | SUB (e1, e2)    ->  let eqns1 = gen_equations tenv e1 TyInt in
                          let eqns2 = gen_equations tenv e2 TyInt in
                          ([(ty, TyInt)]@eqns1)@eqns2
    | MUL (e1, e2)    ->  let eqns1 = gen_equations tenv e1 TyInt in
                          let eqns2 = gen_equations tenv e2 TyInt in
                          ([(ty, TyInt)]@eqns1)@eqns2
    | DIV (e1, e2)    ->  let eqns1 = gen_equations tenv e1 TyInt in
                          let eqns2 = gen_equations tenv e2 TyInt in
                          ([(ty, TyInt)]@eqns1)@eqns2
    | ISZERO e1       ->  let eqns1 = gen_equations tenv e1 TyInt in
                          [(ty, TyBool)]@eqns1
    | IF (e1, e2, e3) ->  let eqns1 = gen_equations tenv e1 TyBool in
                          let eqns2 = gen_equations tenv e2 ty in
                          let eqns3 = gen_equations tenv e3 ty in
                          (eqns1@eqns2)@eqns3
    | LET (x, e1, e2) ->  let alpha1 = fresh_tyvar () in
                          let eqns1 = gen_equations tenv e1 alpha1 in
                          let eqns2 = gen_equations (tenv_extend (x, alpha1) tenv) e2 ty in
                          eqns1@eqns2
    | PROC (x, e1)    ->  let alpha1 = fresh_tyvar () in
                          let alpha2 = fresh_tyvar () in
                          let new_tv = TyFun (alpha1, alpha2) in
                          let eqns1  = gen_equations (tenv_extend (x, alpha1) tenv) e1 alpha2 in
                          [(ty, new_tv)]@eqns1
    | CALL (e1, e2)   ->  let alpha1 = fresh_tyvar () in
                          let new_tv = TyFun (alpha1, ty) in
                          let eqns1  = gen_equations tenv e1 new_tv in
                          let eqns2  = gen_equations tenv e2 alpha1 in
                          eqns1@eqns2
    | READ            ->  [(ty, TyInt)]
    | LETREC (f, x, e1, e2) ->  let alpha1 = fresh_tyvar () in
                                let alpha2 = fresh_tyvar () in
                                let new_tv = TyFun (alpha1, alpha2) in
                                let extended_tenv = (tenv_extend (f, new_tv) tenv) in 
                                let eqns1  = gen_equations (tenv_extend (x, alpha1) extended_tenv) e1 alpha2 in
                                let eqns2  = gen_equations extended_tenv e2 ty in
                                eqns1@eqns2

let rec unify : typ -> typ -> subst -> subst
= fun ty1 ty2 s ->
  match (ty1, ty2) with
    | (TyInt, TyInt) -> s
    | (TyBool, TyBool) -> s
    | (TyVar a, t) ->
      (
        match t with
        | TyFun (t1', t2') ->
          (
            match (t = t1' || t = t2') with
              | true  -> raise TypeError
              | false -> subst_extend a t s
          )
        | _ -> subst_extend a t s
      )  
    | (t, TyVar a) -> unify (TyVar a) t s
    | (TyFun (t1, t2), TyFun (t1', t2')) -> let s' = unify t1 t1' s in
                                            let s2' = unify (subst_apply t2 s') (subst_apply t2' s') s' in s2'
    | _ -> raise TypeError
    
let rec unifyall : typ_eqn -> subst -> subst
= fun eqns s ->
  match eqns with
    | [] -> s
    | (t1, t2)::tl -> let s' = unify (subst_apply t1 s) (subst_apply t2 s) s in
                      unifyall tl s'
  
let solve : typ_eqn -> subst (*typ_eqn은 list...*)
= fun eqns ->
  let s = unifyall eqns subst_empty in s

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty