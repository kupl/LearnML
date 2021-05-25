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
    | VAR x -> [(tenv_find tenv x, ty)]
    | ADD (exp1 ,exp2) -> [ (ty, TyInt) ] @ gen_equations tenv exp1 TyInt @ gen_equations tenv exp2 TyInt
    | DIV (exp1 ,exp2) -> [ (ty, TyInt) ] @ gen_equations tenv exp1 TyInt @ gen_equations tenv exp2 TyInt
    | SUB (exp1 ,exp2) -> [ (ty, TyInt) ] @ gen_equations tenv exp1 TyInt @ gen_equations tenv exp2 TyInt
    | MUL (exp1 ,exp2) -> [ (ty, TyInt) ] @ gen_equations tenv exp1 TyInt @ gen_equations tenv exp2 TyInt
    | ISZERO exp1 -> [ (ty, TyBool) ] @ gen_equations tenv exp1 TyInt
    | IF (exp1, exp2, exp3) -> gen_equations tenv exp1 TyBool @ gen_equations tenv exp2 ty @ gen_equations tenv exp3 ty
    | LET (var1, exp1, exp2) -> let newtype1 = fresh_tyvar () in gen_equations tenv exp1 newtype1 @ gen_equations ( tenv_extend (var1, newtype1) tenv ) exp2 ty
    | PROC (var1, exp1) -> let newtype1 = fresh_tyvar () in let newtype2 = fresh_tyvar () in [ ( ty, TyFun(newtype1, newtype2) ) ] @ gen_equations ( tenv_extend (var1, newtype1) tenv ) exp1 newtype2
    | CALL (exp1, exp2) -> let newtype1 = fresh_tyvar () in gen_equations tenv exp1 ( TyFun (newtype1, ty) ) @ gen_equations tenv exp2 newtype1
    | READ -> [(ty, TyInt)]
    | LETREC (f1, x1, exp1, exp2) -> let newtype1 = fresh_tyvar () in let newtype2 = fresh_tyvar () in  
                                                      let newtype3 = TyFun (newtype2, newtype1) in
                                                      let et_env1 = tenv_extend (f1, newtype3) tenv in
                                                      let et_env2 = tenv_extend (x1, newtype2) et_env1 in
                                                          gen_equations et_env2 exp1 newtype1 @ gen_equations et_env1 exp2 ty
;;

let rec occurcheck tyvar_x1 typ0 =
  match typ0 with
    | TyVar b1 -> if tyvar_x1 = (TyVar b1) then true else false
    | TyFun (t1, t2) -> ( occurcheck tyvar_x1 t1 ) || ( occurcheck tyvar_x1 t2 ) 
    | TyInt -> false
    | TyBool -> false
;;

let rec unify type1 type2 subst0
= match (type1, type2) with
  | ( TyInt, TyInt ) -> subst0
  | ( TyBool, TyBool ) -> subst0
  | ( TyVar x1, TyVar x2 ) -> if ( TyVar x1 = TyVar x2 ) then subst0 else let extend_subst = subst_extend x1 (TyVar x2) subst0 in extend_subst
  | ( TyVar x1, type0 ) -> if ( occurcheck (TyVar x1) type0 = true ) then raise TypeError else let extend_subst = subst_extend x1 type0 subst0 in extend_subst
  | ( type0, TyVar x1 ) -> unify (TyVar x1) type0 subst0
  | ( TyFun (t1, t2), TyFun (t3, t4) ) -> let subst1 = unify t1 t3 subst0 in let subst2 = unify (subst_apply t2 subst1) (subst_apply t4 subst1) subst1 in subst2
  |_ -> raise TypeError
;; 

let rec unifyall type_equation0 subst0 
= match type_equation0 with
  | [] -> subst0
  | ( (type1, type2) :: (type_equation1) ) -> let subst1 = unify (subst_apply type1 subst0) (subst_apply type2 subst0) subst0 in unifyall type_equation1 subst1
;;

let solve : typ_eqn -> subst
=fun eqns -> unifyall eqns []
;;

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty