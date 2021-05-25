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
    |CONST e1 -> [(ty,TyInt)]
    |VAR v1 -> let t1 = (tenv_find tenv v1) in [(ty,t1)]
    |ADD (e1,e2) ->
      [(ty,TyInt)]@(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
    |SUB (e1,e2) ->
      [(ty,TyInt)]@(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
    |MUL (e1,e2) ->
      [(ty,TyInt)]@(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
    |DIV (e1,e2) ->
      [(ty,TyInt)]@(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
    |READ -> []
    |ISZERO e1 ->
      [(ty,TyBool)]@(gen_equations tenv e1 TyInt)
    |IF (e1,e2,e3) -> 
      let nty1 = fresh_tyvar() in
      let nty2 = fresh_tyvar() in
      (gen_equations tenv e1 TyBool)@(gen_equations tenv e2 nty1)@(gen_equations tenv e3 nty2)@[(ty,nty1)]@[(ty,nty2)]
    |LET (v1,e1,e2) ->
      let nty1 = fresh_tyvar() in
      let tenv' = (tenv_extend (v1,nty1) tenv) in
      (gen_equations tenv' e2 ty)@(gen_equations tenv e1 nty1)
    |LETREC (v1,v2,e1,e2) ->
      let nty1 = fresh_tyvar() in(*x를 위한*)
      let nty2 = fresh_tyvar() in(*e1을 위한*)
      let tenv1 = (tenv_extend (v2,TyFun(nty1,nty2)) tenv) in
      let tenv2 = (tenv_extend (v1,nty1) tenv1) in
      (gen_equations tenv2 e2 ty)@(gen_equations tenv e1 nty2)
    |PROC (v1,e1)->
      let nty1 = fresh_tyvar() in
      let nty2 = fresh_tyvar() in
      let tenv1 = (tenv_extend (v1,nty1) tenv) in
      [(ty,TyFun(nty1,nty2))]@(gen_equations tenv1 e1 nty2)
    |CALL (e1,e2)->
      let nty1 = fresh_tyvar() in
      let te1 = TyFun(nty1,ty) in
      (gen_equations tenv e1 te1)@(gen_equations tenv e2 nty1)
      
      
      
      
      
let solve : typ_eqn -> subst
=fun eqns -> subst_empty 

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty