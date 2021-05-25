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
  | CONST n -> (ty, TyInt)::[]
  | VAR x -> (ty, tenv_find tenv x)::[]
  | ADD (e1, e2) -> (ty, TyInt)::((gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt))
  | SUB (e1, e2) -> (ty, TyInt)::((gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt))
  | MUL (e1, e2) -> (ty, TyInt)::((gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt))
  | DIV (e1, e2) -> (ty, TyInt)::((gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt))
  | ISZERO exp -> (ty, TyBool)::(gen_equations tenv exp TyInt)
  | READ -> (ty, TyInt)::[]
  | IF  (cond, e1, e2) -> (gen_equations tenv cond TyBool) @ (gen_equations tenv e1 ty) @ (gen_equations tenv e2 ty) 
  | LET  (x, e1, e2) -> let new_tyvar = (fresh_tyvar ()) in
                          (gen_equations tenv e1 new_tyvar) @ (gen_equations (tenv_extend (x, new_tyvar) tenv) e2 ty) 
  | LETREC  (f, x, e1, e2) -> let new_tyvar1 = (fresh_tyvar ()) in
                              let new_tyvar2 = (fresh_tyvar ()) in
                              (gen_equations (tenv_extend (f, TyFun (new_tyvar1, new_tyvar2)) (tenv_extend (x, new_tyvar1) tenv)) e1 new_tyvar2) @ (gen_equations (tenv_extend (f, TyFun (new_tyvar1, new_tyvar2)) tenv) e2 ty)
  | PROC  (x, exp) ->  let new_tyvar1 = (fresh_tyvar ()) in
                       let new_tyvar2 = (fresh_tyvar ()) in
                          (ty, TyFun (new_tyvar1, new_tyvar2))::(gen_equations (tenv_extend (x, new_tyvar1) tenv) exp new_tyvar2) 
  | CALL (e1, e2) -> let new_tyvar = (fresh_tyvar ()) in
                        (gen_equations tenv e1 (TyFun (new_tyvar, ty))) @ (gen_equations tenv e2 new_tyvar) 

let rec inner = fun eq subst ->
    match eq with
    | (TyInt, TyInt)::tl | (TyBool, TyBool)::tl -> inner tl subst
    | (TyVar x, TyVar y)::tl -> if x=y then inner tl subst (* else inner tl (subst_extend x (subst_apply (TyVar y) subst) subst) *)
                                else let apply_x = subst_apply (TyVar x) subst in
                                     let apply_y = subst_apply (TyVar y) subst in
                                     if apply_x = apply_y then inner tl subst
                                     else (match (apply_x, apply_y) with
                                        | (TyVar ax, TyVar ay) -> inner tl (subst_extend ax apply_y subst)
                                        | _ -> inner ((apply_x, apply_y)::tl) subst
                                       )
                                       
    | (TyVar x, t)::tl -> let xtype = subst_apply (TyVar x) subst in
                          if xtype=(TyVar x) then inner tl (subst_extend x t subst) else inner ((xtype, t)::tl) subst
    | (t, TyVar x)::tl -> inner ((TyVar x, t)::tl) subst
    | (TyFun (l1, l2), TyFun (r1, r2))::tl -> inner ((l1, r1)::(l2, r2)::tl) subst
    | [] -> subst
    | (TyInt, TyBool)::_ | (TyBool, TyInt)::_ | (TyInt, TyFun (_, _))::_ | (TyFun (_, _), TyInt)::_ | (TyBool, TyFun (_, _))::_ | (TyFun (_, _), TyBool)::_ -> raise TypeError

let solve : typ_eqn -> subst
=fun eqns -> 
  inner eqns subst_empty

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty