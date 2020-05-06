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
    |CONST n -> [(ty, TyInt)]
    |VAR x -> [(ty, tenv x)]
    |READ -> [(ty, fresh_tyvar ())]
    |ADD (e1, e2) -> (ty, TyInt)::(gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
    |SUB (e1, e2) -> (ty, TyInt)::(gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
    |MUL (e1, e2) -> (ty, TyInt)::(gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
    |DIV (e1, e2) -> (ty, TyInt)::(gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
    |ISZERO e1 -> (ty, TyBool)::(gen_equations tenv e1 TyInt)
    |IF (e1, e2, e3) ->
      let condition_eqn = (gen_equations tenv e1 TyBool) in
      let e2_eqn = (gen_equations tenv e2 ty) in
      let e3_eqn = (gen_equations tenv e3 ty) in
      condition_eqn @ e2_eqn @ e3_eqn
    |LET (varname, varexp, body) ->
      let var_type = fresh_tyvar () in
      let exp_eqn = gen_equations tenv varexp var_type in
      let extended_tenv = tenv_extend (varname, var_type) tenv in
      let body_eqn = gen_equations extended_tenv body ty in
      exp_eqn @ body_eqn
    |LETREC (fname, varname, fbody, body) ->
      let var_type = fresh_tyvar () in
      let fresult_type = fresh_tyvar () in
      let f_type = TyFun (var_type, fresult_type) in
      let extended_tenv = tenv_extend (varname, var_type) (tenv_extend (fname, f_type) tenv) in
      gen_equations extended_tenv body ty
      
    |PROC (varname, fbody) ->
      let var_type = fresh_tyvar () in
      let result_type = fresh_tyvar () in
      let extended_tenv = tenv_extend (varname, var_type) tenv in
      let body_eqn = gen_equations extended_tenv fbody result_type in
      (ty, TyFun (var_type, result_type))::body_eqn
    |CALL (f, p) -> 
      let p_type = fresh_tyvar () in
      let f_eqn = gen_equations tenv f (TyFun (p_type, ty)) in
      let p_eqn = gen_equations tenv p p_type in
      f_eqn @ p_eqn



let isTyVar x =
  match x with
    TyVar _ -> true
    |_ -> false

let rec occCheck t1 t2 =
        match t2 with
          TyVar x -> if t1 = t2 then true else false
          |TyFun (t3, t4) -> (occCheck t1 t3) || (occCheck t1 t4)
          |TyInt -> false
          |TyBool -> false


let rec change (t1, t2) typ =
  match typ with
    TyInt -> TyInt
    |TyBool -> TyBool
    |TyFun (t3, t4) -> TyFun (change (t1, t2) t3, change (t1, t2) t4)
    |TyVar varname ->
      if (TyVar varname) = t1 then t2 else TyVar varname


let apply_to_eqns : typ_eqn -> (typ * typ) -> typ_eqn
= fun eqns (t1, t2) ->
  let apply_both f (a, b) = (f a, f b) in
  List.map (apply_both (change (t1,t2))) eqns
  

let solve : typ_eqn -> subst
=fun eqns ->
  let rec solve' eq subs =
    match eq with
      |[] -> subs
      |(t1,t2)::es ->
        if t1 = t2 then solve' es subs else
          match t1, t2 with
            |TyBool, TyInt|TyInt, TyBool|TyFun (_,_), TyInt|TyFun (_,_), TyBool|TyInt, TyFun (_,_)|TyBool, TyFun (_,_) -> raise TypeError
            |TyFun (t1, t2), TyFun (t3, t4) ->
              let new1 = (t1, t3) in
              let new2 = (t2, t4) in
              solve' (new1::new2::es) subs
            |TyVar x, TyVar y -> solve' (apply_to_eqns es (t1, t2)) (subst_extend x t2 subs)
            |_, TyVar _ -> solve' ((t2, t1)::es) subs
            |TyVar x, ty -> if occCheck (TyVar x) ty then raise TypeError else solve' (apply_to_eqns es (t1,t2)) (subst_extend x ty subs)
  in solve' eqns []

let rec is_used let_exp =
  match let_exp with
    LET(vname, vexp, body) ->(
      let rec is_in var e =
        match e with
          CONST _ |READ -> false
          |VAR x -> if x = var then true else false
          |ADD (e1, e2) |SUB (e1, e2) |MUL (e1, e2)|DIV (e1, e2) |CALL (e1, e2) -> is_in var e1 || is_in var e2
          |ISZERO e1 -> is_in var e1
          |IF (e1, e2, e3) -> is_in var e1 || is_in var e2 || is_in var e3
          |LET (vn, e1, e2) -> if vn = var then is_in var e1 else is_in var e1 || is_in var e2
          |LETREC (fn, vn, e1 ,e2) -> if vn = var || fn = var then is_in var e1 else is_in var e1 || is_in var e2
          |PROC(vn, fb) -> if vn = var then false else is_in var fb
      in is_in vname body
        )
    |_ -> raise TypeError

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty