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
  | CONST a -> [(ty, TyInt)]
  | VAR x -> [(ty, tenv_find tenv x)](*수정해야할지 확인*)
  | ADD (e1, e2) -> 
    [(ty, TyInt)] @ (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
  | SUB (e1, e2) -> 
    [(ty, TyInt)] @ (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
  | MUL (e1, e2) -> 
    [(ty, TyInt)] @ (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt) 
  | DIV (e1, e2) -> 
    [(ty, TyInt)] @ (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
  | ISZERO e -> 
    [(ty, TyBool)] @ (gen_equations tenv e TyInt) 
  | READ -> []
  | IF (e1, e2, e3) -> 
    (gen_equations tenv e1 TyBool) @ (gen_equations tenv e2 ty) @ (gen_equations tenv e3 ty)
  | LET (x, e1, e2) ->
    let t = fresh_tyvar() in
    (gen_equations tenv e1 t) @ (gen_equations (tenv_extend (x, t) tenv) e2 ty)
  | LETREC (x1, x2, e1, e2) ->
    let t1 = fresh_tyvar() in
    let t2 = fresh_tyvar() in
    (gen_equations (tenv_extend (x2, t2) tenv) e1 t1) @ (gen_equations (tenv_extend (x1, t1) tenv) e2 ty)
  | PROC (x, e) ->
    let t1 = fresh_tyvar() in
    let t2 = fresh_tyvar() in
    [(ty, TyFun(t1, t2))] @ (gen_equations (tenv_extend (x, t1) tenv) e t2)
  | CALL (e1, e2) ->
    let t = fresh_tyvar() in
    (gen_equations tenv e1 (TyFun(t, ty))) @ (gen_equations tenv e2 t)

    
let rec unif : typ -> typ -> subst -> subst
= fun t1 t2 subst ->
  let u1 = subst_apply t1 subst in
  let u2 = subst_apply t2 subst in
  (if (u1 = u2) then subst else
  (match (u1, u2) with
    | (TyVar a, TyFun(b1, b2)) -> if (TyVar a = b1) || (TyVar a= b2) then raise(TypeError) else subst_extend a u2 subst 
    | (TyVar a, b) -> subst_extend a u2 subst
    | (a, TyVar b) -> unif t2 t1 subst
    | (TyFun(a1, a2), TyFun(b1, b2)) -> 
      (let s1 = unif a1 b1 subst in
       let s2 = unif a2 b2 s1 in
       s2)
    | (a, b) -> raise(TypeError)))
  
let rec unifall : typ_eqn -> subst -> subst
= fun e s->
  match e with
    | [] -> s
    | [(a,b)] -> unif a b s
    | (a,b)::tl -> unifall tl (unif a b s)  

let solve : typ_eqn -> subst
=fun eqns -> unifall eqns subst_empty

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty