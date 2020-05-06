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


let rec gen_equations : tenv -> exp -> typ -> typ_eqn  (* HW1: TODO *)
=fun tenv exp ty ->
  match exp with
  | CONST n -> [(ty, TyInt)]
  | READ -> [(ty, TyInt)]
  | VAR x -> [(tenv x, ty)]
  | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) -> 
    (ty, TyInt) :: (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
  | ISZERO e ->
    (ty, TyBool) :: (gen_equations tenv e TyInt)
  | IF (e1, e2, e3) -> 
    (gen_equations tenv e2 ty) @ (gen_equations tenv e3 ty) @ (gen_equations tenv e1 TyBool)
  | LET (x, e1, e2) -> let k = fresh_tyvar() in
    (gen_equations (tenv_extend (x, k) tenv) e2 ty) @ (gen_equations tenv e1 k)
  | LETREC (f, x, e1, e2) -> let tx = fresh_tyvar() in let te = fresh_tyvar() in
    (gen_equations (tenv_extend (f, TyFun(tx, te)) tenv) e2 ty) @ (gen_equations (tenv_extend (f, TyFun(tx, te)) (tenv_extend (x, tx) tenv)) e1 te)
  | PROC (x, e) ->  let tx = fresh_tyvar() in let te = fresh_tyvar() in
    (ty, TyFun(tx, te)) :: (gen_equations (tenv_extend (x, tx) tenv) e te)
  | CALL (e1, e2) ->  let k = fresh_tyvar() in
    (gen_equations tenv e1 (TyFun(k, ty))) @ (gen_equations tenv e2 k)

let solve : typ_eqn -> subst (* HW1: TODO *)
=fun eqns ->
  let rec findSub : typ -> subst -> tyvar -> typ
  = fun t subst notv -> match t with
    | TyFun (t1, t2) -> TyFun(findSub t1 subst notv, findSub t2 subst notv)
    | TyVar v -> ( if v = notv then raise TypeError else
      if List.exists (fun (y, ty) -> v = y) subst then subst_find v subst else TyVar v)
    | TyInt -> TyInt | TyBool -> TyBool in
  let rec sol : typ_eqn -> subst -> subst
  = fun eqns subst ->
    match eqns with
      |[] -> subst
      |hd::tl ->
        match hd with
        |(TyInt, TyInt) |(TyBool, TyBool) -> sol tl subst
        |(TyFun(t1, t3), TyFun(t2, t4)) -> sol ((t1,t2) :: (t3,t4) :: tl) subst
        |(TyVar v, t) |(t, TyVar v) ->( let t = findSub t subst v in
          if List.exists (fun (y, ty) -> v = y) subst then sol ((subst_find v subst, t) :: tl) subst
          else sol tl (subst_extend v t subst))
        |_ -> raise TypeError in
  let rec rev lst = match lst with 
    |[] -> []
    |hd::tl -> (rev tl) @ [hd] in
  rev(sol eqns subst_empty)

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty