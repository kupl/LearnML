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
=fun tenv e ty -> (* TODO *)
  match e with
    | CONST n -> [(ty, TyInt)]
    | VAR x -> [(ty, tenv_find tenv x)]
    | ADD (n1, n2) -> 
      let e1 = gen_equations tenv n1 TyInt in
      let e2 = gen_equations tenv n2 TyInt in
      [(ty, TyInt)]@e1@e2
    | SUB (n1, n2) ->  
      let e1 = gen_equations tenv n1 TyInt in
      let e2 = gen_equations tenv n2 TyInt in
      [(ty, TyInt)]@e1@e2
    | MUL (n1, n2) ->  
      let e1 = gen_equations tenv n1 TyInt in
      let e2 = gen_equations tenv n2 TyInt in
      [(ty, TyInt)]@e1@e2
    | DIV (n1, n2) ->  
      let e1 = gen_equations tenv n1 TyInt in
      let e2 = gen_equations tenv n2 TyInt in
      [(ty, TyInt)]@e1@e2
    | ISZERO n -> [(ty, TyBool)]@(gen_equations tenv n TyInt)
    | IF (n1, n2, n3) -> 
      let e1 = gen_equations tenv n1 TyBool in
      let e2 = gen_equations tenv n2 ty in
      let e3 = gen_equations tenv n3 ty in e1@e2@e3
    | LET (x, n1, n2) ->
      let e1 = fresh_tyvar () in
      let e2 = gen_equations tenv n1 e1 in
      let e3 = gen_equations (tenv_extend (x, e1) tenv) n2 ty in e2@e3
    (*| LETREC (x1, x2, n1, n2) -> 
      let e1 = gen_equations (tenv_extend (x1, ) tenv)*)
      
    | PROC (x, n1) ->
      let e1 = fresh_tyvar () in
      let e2 = fresh_tyvar () in
      let e3 = gen_equations (tenv_extend (x, e1) tenv) n1 e2 in 
      e3@[(ty, TyFun (e1, e2))]
    | CALL (n1, n2) ->
      let e1 = fresh_tyvar () in
      let e2 = gen_equations tenv n1 (TyFun (e1, ty)) in
      let e3 = gen_equations tenv n2 e1 in e2@e3
    
let rec unify : typ -> typ -> subst -> subst
= fun ty1 ty2 s -> match (ty1, ty2) with
  | (TyInt, TyInt) -> s
  | (TyBool, TyBool) -> s
  | (TyVar a, t) -> 
    (match t with
      | TyVar a -> raise TypeError
      |_ -> subst_extend a t s)
  | (t, TyVar a) -> unify (TyVar a) t s
  | (TyFun (t1, t2), TyFun (t1', t2')) -> 
    let s' = unify t1 t1' s in 
    let s'' = unify (subst_apply t2 s') (subst_apply t2' s') s' in s''
  |_ -> raise TypeError


      
let rec unifyall : typ_eqn -> subst -> subst
= fun eqn s -> match eqn with
  | [] -> s
  | (t1, t2)::u -> 
    let s' = unify (subst_apply t1 s) (subst_apply t2 s) s in
    unifyall u s'
      


let solve : typ_eqn -> subst
=fun eqns -> unifyall eqns subst_empty(* TODO *)
 



let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty