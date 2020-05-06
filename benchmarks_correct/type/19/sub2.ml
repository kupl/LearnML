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
    | ADD (e1, e2) -> [(ty, TyInt)] @ (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
    | SUB (e1, e2) -> [(ty, TyInt)] @ (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
    | MUL (e1, e2) -> [(ty, TyInt)] @ (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
    | DIV (e1, e2) -> [(ty, TyInt)] @ (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
	| READ -> [ty, TyInt]
    | ISZERO e1 -> [(ty, TyBool)] @ (gen_equations tenv e1 TyInt)
    | IF (e1, e2, e3) -> (gen_equations tenv e1 TyBool) @ (gen_equations tenv e2 ty) @ (gen_equations tenv e3 ty)
    | LET (v, e1, e2) -> 
      let alpha = fresh_tyvar() in
      (gen_equations tenv e1 alpha) @ (gen_equations (tenv_extend (v, alpha) tenv) e2 ty)
    | LETREC (f, x, e1, e2) ->
      let a1 = fresh_tyvar() in
      let a2 = fresh_tyvar() in
      (gen_equations (tenv_extend (x, a1) (tenv_extend (f, TyFun(a1, a2)) tenv)) e1 a2) @ (gen_equations (tenv_extend (f, TyFun(a1, a2)) tenv) e2 ty)
    | PROC (v, e1) -> 
      let a1 = fresh_tyvar() in
      let a2 = fresh_tyvar() in
      [(ty, TyFun(a1, a2))] @ (gen_equations (tenv_extend (v, a1) tenv) e1 a2)
    | CALL (e1, e2) ->
      let alpha = fresh_tyvar() in
      (gen_equations tenv e1 (TyFun(alpha, ty))) @ (gen_equations tenv e2 alpha)

let rec ocrcheck : tyvar -> typ -> bool
= fun typevar itstype ->
  match itstype with
    | TyInt -> false
    | TyBool -> false
    | TyFun (a1, a2) -> (ocrcheck typevar a1) || (ocrcheck typevar a2)
    | TyVar x -> if x=typevar then true else false
    
let rec unify : (typ * typ) -> subst -> subst
= fun (t1, t2) sbst ->
  match (t1, t2) with
    | (TyInt, TyInt) -> sbst
    | (TyBool, TyBool) -> sbst
    | (TyVar a, TyVar b) -> if (a=b) then sbst
                            else (subst_extend a (TyVar b) sbst)
    | (TyVar a, tp) -> if (ocrcheck a tp) then raise TypeError
                       else (subst_extend a tp sbst)
    | (tp, TyVar a) -> unify (TyVar a, tp) sbst
    | (TyFun (a, b), TyFun(c, d)) -> 
      let sprm = unify (a, c) sbst in
      let sdbp = unify (subst_apply b sprm, subst_apply d sprm) sprm in
      sdbp
    | _ -> raise TypeError
    
let rec unifyall : typ_eqn -> subst -> subst
= fun eqn sbst ->
  match eqn with
    | [] -> sbst
    | hd::tl ->
      (match hd with
        | (t1, t2) -> 
          let sprm = unify (subst_apply t1 sbst, subst_apply t2 sbst) sbst
          in (unifyall tl sprm))

let solve : typ_eqn -> subst
=fun eqns ->
  unifyall eqns subst_empty

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty