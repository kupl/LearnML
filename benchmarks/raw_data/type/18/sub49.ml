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
    | CONST i -> [(ty, TyInt)]
    | VAR v -> [(ty, tenv_find tenv v)]
    | ADD (a, b) -> [(ty, TyInt)]@(gen_equations tenv a TyInt)@(gen_equations tenv b TyInt)
    | SUB (a, b) -> [(ty, TyInt)]@(gen_equations tenv a TyInt)@(gen_equations tenv b TyInt)
    | MUL (a, b) -> [(ty, TyInt)]@(gen_equations tenv a TyInt)@(gen_equations tenv b TyInt)
    | DIV (a, b) -> [(ty, TyInt)]@(gen_equations tenv a TyInt)@(gen_equations tenv b TyInt)
    | ISZERO i -> [(ty, TyBool)]@(gen_equations tenv i TyInt)
    | READ -> [(ty, TyInt)]
    | IF (a, b, c) -> (gen_equations tenv a TyBool)@(gen_equations tenv b ty)@(gen_equations tenv c ty)
    | LET (x, e1 ,e2) ->
      begin
      let t = fresh_tyvar () in
      let new_tenv = tenv_extend (x,t) tenv in
      (gen_equations tenv e1 t)@(gen_equations new_tenv e2 ty)
      end
    | LETREC (f, x, e1, e2) ->
      begin
      let t1 = fresh_tyvar () in
      let t2 = fresh_tyvar () in
      let new_tenv1 = tenv_extend (x, t1) tenv in
      let new_tenv2 = tenv_extend (f, TyFun (t1, t2)) new_tenv1 in
      (gen_equations new_tenv2 e1 t2)@(gen_equations new_tenv2 e2 ty)
      end
    | PROC (x, e)->
      begin
      let t1 = fresh_tyvar () in
      let t2 = fresh_tyvar () in
      let new_tenv = tenv_extend (x, t1) tenv in
      [(ty, TyFun(t1, t2))]@(gen_equations new_tenv e t2) 
      end
    | CALL (e1, e2) ->
      begin
      let t = fresh_tyvar () in
      (gen_equations tenv e1 (TyFun (t, ty))) @ (gen_equations tenv e2 t)
      end
      
let rec occurrence_check : typ -> typ -> bool
= fun tvar t ->
  match (tvar, t) with
    |(TyVar x, TyFun(a, b)) ->
      if (TyVar x = a || TyVar x = b) then true else false
    | _ -> false
let rec unify : typ * typ -> subst -> subst
= fun eqn subst ->
  match eqn with
    |(TyInt, TyInt) -> subst
    |(TyBool, TyBool) -> subst
    |(TyVar x, TyVar y) -> if x = y then subst else subst_extend x (TyVar y) subst
    |(TyVar x, typ) -> if (occurrence_check (TyVar x) typ) then raise (TypeError) else subst_extend x typ subst
    |(typ, TyVar x) -> unify (TyVar x, typ) subst
    |(TyFun (t1, t2), TyFun (t1_, t2_)) ->
      begin
      let s1 = unify (t1, t1_) subst in
      let s2 = unify ((subst_apply t2 s1), (subst_apply t2_ s1)) s1 in s2
      end
    |(_, _) -> raise (TypeError)
  

let rec unify_all : typ_eqn -> subst -> subst
= fun eqns subst ->
  match eqns with
    |[] -> subst
    |(t1, t2)::tl ->
      begin
      let s1 = unify ((subst_apply t1 subst), (subst_apply t2 subst)) subst in
      unify_all tl s1
      end

let solve : typ_eqn -> subst
=fun eqns -> unify_all eqns subst_empty

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty