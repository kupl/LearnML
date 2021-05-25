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
    | CONST n -> [(ty, TyInt)]
    | VAR x -> [(ty, tenv_find tenv x)]
    | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) 
      -> (ty, TyInt) :: (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
    | ISZERO e -> (ty, TyBool) :: (gen_equations tenv e TyInt)
    | READ -> [(ty, TyInt)] (*???어떻게 처리???*)
    | IF (e1, e2, e3) -> (gen_equations tenv e1 TyBool) @ (gen_equations tenv e2 ty) @ (gen_equations tenv e3 ty)
    | LET (x, e1, e2) -> 
    begin
      let alpha = fresh_tyvar () in
      let tenv' = tenv_extend (x, alpha) tenv in
      (gen_equations tenv e1 alpha) @ (gen_equations tenv' e2 ty)
    end
    | LETREC (f, x, e1, e2) ->
    begin
      let alpha1 = fresh_tyvar () in
      let alpha2 = fresh_tyvar () in
      let tenv1 = tenv_extend (f, TyFun(alpha1, alpha2)) tenv in (*f 추가*)
      let tenv2 = tenv_extend (x, alpha1) tenv1 in (*f, x 추가*)
      (gen_equations tenv2 e1 alpha2) @ (gen_equations tenv1 e2 ty)
    end
    | PROC (x, e) -> 
    begin
      let alpha1 = fresh_tyvar () in
      let alpha2 = fresh_tyvar () in
      let tenv' = tenv_extend (x, alpha1) tenv in
      (ty, TyFun(alpha1, alpha2)) :: (gen_equations tenv' e alpha2)
    end
    | CALL (e1, e2) ->
    begin
      let alpha = fresh_tyvar() in
      (gen_equations tenv e1 (TyFun(alpha, ty))) @ (gen_equations tenv e2 alpha)
    end;;
    
let rec occurrence_check : tyvar -> typ -> bool
=fun x ty -> 
  match ty with 
    | TyInt | TyBool -> false
    | TyFun (ty1, ty2) -> (occurrence_check x ty1) || (occurrence_check x ty2)
    | TyVar y -> if x = y then true else false;;
    
let rec unify : typ_eqn -> subst -> subst
=fun eqns subst ->
  match eqns with
    | (left, right)::tl ->
    begin
      (*substitution -> equation apply*)
      let right = subst_apply right subst in
      let left = subst_apply left subst in
      match (left, right) with 
        | (TyInt, TyInt) | (TyBool, TyBool) -> unify tl subst
        | (TyVar x, TyVar y) ->
        begin
          if x = y then (unify tl subst)
          else let subst' = subst_extend x (TyVar y) subst in unify tl subst'
        end
        | (TyVar x, ty) ->
        begin
          if (occurrence_check x ty) then raise TypeError
          else let subst' = subst_extend x ty subst in unify tl subst'
        end
        | (ty, TyVar x) -> unify ((TyVar x, ty)::tl) subst (*switch the sides*)
        | (TyFun (ty1, ty2), TyFun (ty3, ty4)) -> unify ((ty1, ty3)::(ty2, ty4)::tl) subst (*simplify*)
        | (_, _) -> raise TypeError
    end
    | [] -> subst;;
    

let solve : typ_eqn -> subst
=fun eqns -> unify eqns subst_empty;;

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty