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
= let intBinOpGenEq : tenv -> typ -> exp -> exp -> typ_eqn
  = fun tenv ty e1 e2 ->
    let teq1 = gen_equations tenv e1 TyInt in
    let teq2 = gen_equations tenv e2 TyInt in
    (ty, TyInt) :: (teq1 @ teq2) in
  fun tenv e ty -> 
  (match e with
  | CONST n -> [(ty, TyInt)]
  | VAR v -> [(ty, tenv_find tenv v)]
  | ADD (e1, e2) -> intBinOpGenEq tenv ty e1 e2
  | SUB (e1, e2) -> intBinOpGenEq tenv ty e1 e2
  | MUL (e1, e2) -> intBinOpGenEq tenv ty e1 e2
  | DIV (e1, e2) -> intBinOpGenEq tenv ty e1 e2
  | READ -> [(ty, TyInt)]
  | ISZERO e1 -> (ty, TyBool) :: (gen_equations tenv e1 TyInt)
  | IF (e1, e2, e3) ->
      let teq1 = gen_equations tenv e1 TyBool in
      let teq2 = gen_equations tenv e2 ty in
      let teq3 = gen_equations tenv e3 ty in
      (teq1 @ teq2 @ teq3)
  | LET (v, e1, e2) ->
      let nty = fresh_tyvar () in
      let teq1 = gen_equations tenv e1 nty in
      let ntenv = tenv_extend (v, nty) tenv in
      let teq2 = gen_equations ntenv e2 ty in
      (teq1 @ teq2)
  | LETREC (v1, v2, e1, e2) ->
      let nty_fx = fresh_tyvar () in
      let nty_x = fresh_tyvar () in
      let nty_f = TyFun (nty_x, nty_fx) in
      let ntenv = tenv_extend (v2, nty_x) (tenv_extend (v1, nty_f) tenv) in
      let teq1 = gen_equations ntenv e1 nty_fx in
      let teq2 = gen_equations ntenv e2 ty in
      (teq1 @ teq2)
  | PROC (v, e1) ->
      let nty_x = fresh_tyvar () in
      let nty_e = fresh_tyvar () in
      let ntenv = tenv_extend (v, nty_x) tenv in
      let teq1 = gen_equations ntenv e1 nty_e in
      (ty, TyFun(nty_x, nty_e)) :: teq1
  | CALL (e1, e2) ->
      let nty_e2 = fresh_tyvar () in
      let nty_e1 = TyFun(nty_e2, ty) in
      let teq1 = gen_equations tenv e1 nty_e1 in
      let teq2 = gen_equations tenv e2 nty_e2 in
      (teq1 @ teq2)
  )


let rec isOccur : tyvar -> typ -> bool = fun v t ->
  match t with
  | TyVar vv -> v = vv
  | TyFun (tt1, tt2) -> isOccur v tt1 && isOccur v tt2
  | _ -> false

let rec unify : typ -> typ -> subst -> subst
= fun t1 t2 sub ->
  (match t1, t2 with
  | TyInt, TyInt
  | TyBool, TyBool -> sub
  | TyFun (tt1, tt2), TyFun (tt3, tt4) ->
      let s1 : subst = unify tt1 tt3 sub in
      let tt22 = subst_apply tt2 s1 in
      let tt44 = subst_apply tt4 s1 in
      let s2 = unify tt22 tt44 s1 in
      s2
  | TyVar v1, TyFun _ -> if isOccur v1 t2 then raise TypeError else subst_extend v1 t2 sub
  | TyVar v1, _ -> subst_extend v1 t2 sub
  | _, TyVar _ -> unify t2 t1 sub
  | _ -> raise TypeError
  )

let solve : typ_eqn -> subst
= fun eqns ->
  List.fold_left (fun sub ty -> unify (subst_apply (fst ty) sub) (subst_apply (snd ty) sub) sub) subst_empty eqns

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty