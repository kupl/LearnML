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
  | CONST n -> [(ty, TyInt)]
  | VAR x -> [(ty, (tenv_find tenv x))]
  | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) ->
    ([(ty, TyInt)]@(gen_equations tenv e1 TyInt))@(gen_equations tenv e2 TyInt)
  | READ -> [(ty, TyInt)]
  | ISZERO e1 ->
    [(ty, TyBool)]@(gen_equations tenv e1 TyInt)
  | IF (e1, e2, e3) ->
    ((gen_equations tenv e1 TyBool)@(gen_equations tenv e2 ty))@(gen_equations tenv e3 ty)
  | LET (x, e1, e2) ->
    let t = fresh_tyvar () in
      (gen_equations tenv e1 t)@(gen_equations (tenv_extend (x, t) tenv) e2 ty)
  | LETREC (f, x, e1, e2) ->
    let t = fresh_tyvar () in
    let tf = TyFun (t, ty) in
      (gen_equations tenv e1 t)@(gen_equations (tenv_extend (f, tf) (tenv_extend (x, t) tenv)) e2 ty)
  | PROC (x, e1) ->
    let t1 = fresh_tyvar () in
    let t2 = fresh_tyvar () in
    let tf = TyFun (t1, t2) in
      [(ty, tf)]@(gen_equations (tenv_extend (x, t1) tenv) e1 t2)
  | CALL (e1, e2) ->
    let t = fresh_tyvar () in
    let tf = TyFun (t, ty) in
    (gen_equations tenv e1 tf)@(gen_equations tenv e2 t);;

let rec chk_occur : typ -> typ -> bool
= fun ty1 ty2 ->
  if ty1 = ty2 then true
  else match ty2 with
    | TyFun (a, b) -> (chk_occur ty1 a) || (chk_occur ty1 b)
    | _ -> false

let rec solve_rec : typ_eqn -> subst -> subst
= fun eqns subst -> match eqns with
  | ((t1, t2)::tl) ->
    let tt1 = subst_apply t1 subst in
    let tt2 = subst_apply t2 subst in
    if tt1 = tt2 then (solve_rec tl subst)
    else (match (tt1, tt2) with
      | (TyFun (a1, b1), TyFun(a2, b2)) ->
        (solve_rec ((a1, a2)::((b1, b2)::tl)) subst)
      | (TyVar x, _) ->
        if (chk_occur tt1 tt2) then raise TypeError
        else let ttb = (subst_apply tt2 subst) in
          (solve_rec tl (subst_extend x ttb subst))
      | (_, TyVar x) ->
        if (chk_occur tt2 tt1) then raise TypeError
        else let ttb = (subst_apply tt1 subst) in
          (solve_rec tl (subst_extend x ttb subst))
      | (_, _) -> raise TypeError)
  | [] -> subst;;

let solve : typ_eqn -> subst
=fun eqns -> solve_rec eqns subst_empty;;

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty