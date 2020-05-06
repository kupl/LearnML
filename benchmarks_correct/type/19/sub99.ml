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
    | VAR v -> [(ty, tenv_find tenv v)]
    | ADD (e1, e2) -> (ty, TyInt)::gen_equations tenv e1 TyInt@gen_equations tenv e2 TyInt
    | SUB (e1, e2) -> (ty, TyInt)::gen_equations tenv e1 TyInt@gen_equations tenv e2 TyInt
    | MUL (e1, e2) -> (ty, TyInt)::gen_equations tenv e1 TyInt@gen_equations tenv e2 TyInt
    | DIV (e1, e2) -> (ty, TyInt)::gen_equations tenv e1 TyInt@gen_equations tenv e2 TyInt
    | READ -> [(ty, TyInt)]
    | ISZERO e -> (ty, TyBool)::gen_equations tenv e TyInt
    | IF (e1, e2, e3) -> gen_equations tenv e1 TyBool@gen_equations tenv e2 ty@gen_equations tenv e3 ty
    | LET (v, e1, e2) -> let new_ty = fresh_tyvar () in
                         let extended_tenv = tenv_extend (v,new_ty) tenv in
                         gen_equations tenv e1 new_ty@gen_equations extended_tenv e2 ty
    | LETREC (f, v, e1, e2) -> let input_ty = fresh_tyvar () in
                               let output_ty = fresh_tyvar () in
                               let exfun_tenv = tenv_extend (f,TyFun (input_ty, output_ty)) tenv in
                               let infun_tenv = tenv_extend (v,input_ty) exfun_tenv in
                               gen_equations infun_tenv e1 output_ty@gen_equations exfun_tenv e2 ty
    | PROC (v, e) -> let input_ty = fresh_tyvar () in
                     let output_ty = fresh_tyvar () in
                     let extended_tenv = tenv_extend (v,input_ty) tenv in
                     (ty, TyFun (input_ty,output_ty))::gen_equations extended_tenv e output_ty
    | CALL (e1, e2) -> let input_ty = fresh_tyvar () in
                       gen_equations tenv e1 (TyFun (input_ty, ty))@gen_equations tenv e2 input_ty

let solve : typ_eqn -> subst
=fun eqns -> 
    let rec unify_all eqns subst =
        match eqns with
        | [] -> subst
        | (t1, t2)::tl -> let sub_t1 = subst_apply t1 subst in
                          let sub_t2 = subst_apply t2 subst in
                          let new_subst = unify sub_t1 sub_t2 subst in
                          unify_all tl new_subst
    and unify typ1 typ2 subst = 
        match (typ1, typ2) with
        | (TyInt, TyInt) -> subst
        | (TyBool, TyBool) -> subst
        | (TyVar v1, TyVar v2) -> if v1 = v2
                                  then subst
                                  else subst_extend v1 (TyVar v2) subst
        | (TyVar v, TyFun (t1, t2)) -> 
            (match (t1, t2) with
             | (TyVar v1, _) -> if v = v1
                                then raise TypeError
                                else subst_extend v (TyFun (t1, t2)) subst
             | (_, TyVar v2) -> if v = v2
                                then raise TypeError
                                else subst_extend v (TyFun (t1, t2)) subst
             | (_, _) -> subst_extend v (TyFun (t1, t2)) subst)
        | (TyVar v, t) -> subst_extend v t subst
        | (t, TyVar v) -> unify (TyVar v) t subst
        | (TyFun (t1, t2), TyFun (t3, t4)) -> let subst1 = unify t1 t3 subst in
                                              let sub_t2 = subst_apply t2 subst1 in
                                              let sub_t4 = subst_apply t4 subst1 in 
                                              unify sub_t2 sub_t4 subst1
        | (_,_) -> raise TypeError
    in unify_all eqns subst_empty 

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty