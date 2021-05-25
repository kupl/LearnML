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
=fun tenv e ty -> (* HW1: TODO *)
  match e with
    | CONST n -> [(ty, TyInt)]
    | VAR x -> [(ty, tenv_find tenv x)]
    | ADD (e1,e2) ->
      let l1 = [(ty, TyInt)] and l2 = gen_equations tenv e1 TyInt and l3 = gen_equations tenv e2 TyInt
      in l1@l2@l3
    | SUB (e1,e2) ->
      let l1 = [(ty, TyInt)] and l2 = gen_equations tenv e1 TyInt and l3 = gen_equations tenv e2 TyInt
      in l1@l2@l3
    | MUL (e1,e2) ->
      let l1 = [(ty, TyInt)] and l2 = gen_equations tenv e1 TyInt and l3 = gen_equations tenv e2 TyInt
      in l1@l2@l3
    | DIV (e1,e2) ->
      let l1 = [(ty, TyInt)] and l2 = gen_equations tenv e1 TyInt and l3 = gen_equations tenv e2 TyInt
      in l1@l2@l3
    | ISZERO e ->
      let l1 = [(ty, TyBool)] and l2 = gen_equations tenv e TyInt
      in l1@l2
    | IF (e1,e2,e3) ->
      let l1 = gen_equations tenv e1 TyBool and l2 = gen_equations tenv e2 ty and l3 = gen_equations tenv e3 ty
      in l1@l2@l3
    | LET (x,e1,e2) ->
      let t = fresh_tyvar ()
      in
      let l1 = gen_equations tenv e1 t and l2 = gen_equations (tenv_extend (x,t) tenv) e2 ty
      in l1@l2
    | LETREC (f,x,e1,e2) -> (*raise TypeError*)
      let t1 = fresh_tyvar() and t2 = fresh_tyvar()
      in 
      let l1 = gen_equations (   tenv_extend(f,TyFun(t2,t1)) (tenv_extend(x,t2) tenv)   ) e1 t1
      and l2 = gen_equations (tenv_extend(f,TyFun(t2,t1)) tenv) e2 ty
      in l1@l2
    | PROC (x,e) ->
      let t1 = fresh_tyvar () and t2 = fresh_tyvar ()
      in
      let l1 = [(ty, TyFun (t1,t2))] and l2 = gen_equations (tenv_extend (x,t1) tenv) e t2
      in l1@l2
    | CALL (e1,e2) ->
      let t = fresh_tyvar ()
      in
      let l1 = gen_equations tenv e1 (TyFun (t,ty)) and l2 = gen_equations tenv e2 t
      in l1@l2
    (*| READ -> [(ty,TyInt)]*)
    (*| READ -> raise (Failure "TypeError")*)
    | _ -> raise TypeError





let rec unify : typ -> typ -> subst -> subst
=fun t1 t2 s ->
  
  let rec occurs : string -> typ -> bool
   = fun a t -> 
    match t with
      | TyVar b -> a = b
      | TyFun(b1,b2) -> (occurs a b1) || (occurs a b2)
      | _ -> false
      
  in
  
  match (t1,t2) with
    | (TyInt,TyInt) -> s
    | (TyBool,TyBool) -> s
    | (TyVar a, t) ->
      if occurs a t then raise TypeError else subst_extend a t s
    | (t,TyVar a) -> unify (TyVar a) t s
    | (TyFun (t1,t2), TyFun (t1',t2')) ->
      let s' = unify t1 t1' s
      in
      let t1'' = subst_apply t2 s' and t2'' = subst_apply t2' s'
      in
      unify (t1'') (t2'') s'
    | _ -> raise TypeError

let rec unifyall : typ_eqn -> subst -> subst
=fun eqn s ->
  match eqn with
    | [] -> s
    | (t1,t2)::u ->
      let s' = unify (subst_apply t1 s) (subst_apply t2 s) s in
      unifyall u s'



let solve : typ_eqn -> subst
=fun eqns -> (*subst_empty*) (* HW1: TODO *)
  unifyall eqns subst_empty
  
let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty