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
  |CONST n -> [(ty, TyInt)] 
  |VAR x -> [(ty, (tenv x))]
  |ADD (e1,e2) -> [(ty, TyInt)] @ (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
  |SUB (e1,e2) -> [(ty, TyInt)] @ (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
  |MUL (e1,e2) -> [(ty, TyInt)] @ (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
  |DIV (e1,e2) -> [(ty, TyInt)] @ (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
  
  |READ -> [(ty, TyInt)]
  |ISZERO e -> [(ty, TyBool)]@(gen_equations tenv e TyInt)
  |IF (e1,e2,e3) -> (gen_equations tenv e1 TyBool) @ (gen_equations tenv e2 ty) @ (gen_equations tenv e3 ty) 
  |LET (x,e1,e2) -> 
      let alp = fresh_tyvar () in 
      (gen_equations tenv e1 alp) @ (gen_equations (tenv_extend (x,alp) tenv) e2 ty) (*the new alpha by freash_tyvar*)
  |LETREC (f,x,e1,e2) ->
                    let alp_1 = fresh_tyvar () in 
                     let alp_2 = fresh_tyvar () in
                       (gen_equations (tenv_extend (f, TyFun(alp_1, alp_2)) (tenv_extend (x,alp_1) tenv) ) e1 alp_2)
                      @ (gen_equations (tenv_extend (f, TyFun(alp_1, alp_2)) tenv)  e2 ty)
(*                      let (gen equations (tenv_extend (x, alp_1) tenv) e1 alp2) @*)
(*                            (gen_equations (tenv_extend (f, (TyFun (alp_1,alp_2))) (tenv_extend (x, alp_1) tenv)) e2 ty )*)
  |PROC (x, e) ->  let alp_1 = fresh_tyvar () in 
                    let alp_2 = fresh_tyvar () in
                       [(ty, TyFun(alp_1,alp_2) )] @ (gen_equations (tenv_extend (x,alp_1) tenv) e alp_2)
  |CALL (e1,e2) -> let alp = fresh_tyvar () in (gen_equations tenv e1 (TyFun (alp,ty)) ) @ (gen_equations tenv e2 alp)
  
  
let rec solve : typ_eqn -> subst
=fun eqns -> (unifyall eqns [])
  

and unifyall: typ_eqn -> subst -> subst
= fun eqns subs->
  match eqns with
    |[]-> subs
    |(t1,t2)::t-> let subs_pri = (unify (subst_apply t1 subs) (subst_apply t2 subs) subs) in
                    (unifyall t subs_pri)
    
    
and unify: typ -> typ -> subst -> subst
= fun t1 t2 subs -> 
  match (t1,t2) with
  | (TyInt, TyInt) -> subs
  | (TyBool, TyBool) ->subs
  | (TyVar x, TyVar y) -> if t1=t2 then subs else (subst_extend x t2 subs)
  | (TyVar alp, _ ) -> if (occur_check t1 t2) then (raise TypeError) else (subst_extend alp t2 subs) 
  | (_, TyVar y) -> (unify t2 t1 subs)
  | (TyFun (t1,t2) , TyFun (t1_pri, t2_pri) ) 
      -> let sprime = (unify t1 t1_pri subs) in 
          let spriprime = (unify (subst_apply t2 sprime) (subst_apply t2_pri sprime) sprime ) in spriprime
          
  | (_,_) -> (raise TypeError)
  
and occur_check: typ -> typ -> bool
= fun alp ty  ->
  match (alp,ty) with
  |(TyVar x, TyVar y) ->  if x=y then true else false
  |(TyVar x, TyInt) -> false
  |(TyVar x, TyBool) -> false
  |(TyVar x, TyFun (t1,t2)) -> (occur_check alp t1) || (occur_check alp t2)
  |(_,_)->(raise TypeError)


let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty