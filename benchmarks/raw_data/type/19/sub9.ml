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
  | VAR x -> [ty, (tenv_find tenv x)]
  | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) ->
    [(ty, TyInt)] @ (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
  | ISZERO e1 ->
    [(ty, TyBool)] @ (gen_equations tenv e1 TyInt)
  | READ -> [(ty, TyInt)]
  | IF (e1, e2, e3) ->
    (gen_equations tenv e1 TyBool) @ (gen_equations tenv e2 ty) @ (gen_equations tenv e3 ty)
  | LET (x, e1, e2) ->
    let new_tv1 = (fresh_tyvar ()) in
    let tenv1 = (tenv_extend (x, new_tv1) tenv) in
    (gen_equations tenv e1 new_tv1) @ (gen_equations tenv1 e2 ty)
  | LETREC (f, x, e1, e2) ->
    let new_tv1 = (fresh_tyvar ()) in
    let new_tv2 = (fresh_tyvar ()) in
    let tenv1 = (tenv_extend (f, TyFun (new_tv2, new_tv1)) tenv) in
    let tenv2 = (tenv_extend (x, new_tv2) tenv1) in
    (gen_equations tenv2 e1 new_tv1) @ (gen_equations tenv1 e2 ty)
  | PROC (x, e1) ->
    let new_tv1 = (fresh_tyvar ()) in
    let new_tv2 = (fresh_tyvar ()) in
    let tenv1 = (tenv_extend (x, new_tv1) tenv) in
    [(ty, TyFun (new_tv1, new_tv2))] @ (gen_equations tenv1 e1 new_tv2)
  | CALL (e1, e2) ->
    let new_tv1 = (fresh_tyvar ()) in
    (gen_equations tenv e1 (TyFun (new_tv1, ty))) @ (gen_equations tenv e2 new_tv1)

let solve : typ_eqn -> subst
=fun eqns ->
  let rec occurcheck v ty =
    match ty with
    | TyFun (t1, t2) ->
      if t1 = v || t2 = v then true
      else (occurcheck v t1) || (occurcheck v t2)
    | _ -> false
  in
  let rec unif teq sublist =
    match teq with
    | hd::tl ->
      let (t1, t2) = hd in
      let newt1 = (subst_apply t1 sublist) in
      let newt2 = (subst_apply t2 sublist) in
      (match newt1 with
      | TyVar x ->
        if (occurcheck newt1 newt2) then raise (TypeError)
        else let newsub = (subst_extend x newt2 sublist) in
        (unif tl newsub)
      | _ -> 
        (match newt2 with
        | TyVar y ->
          if (occurcheck newt2 newt1) then raise (TypeError)
          else let newsub = (subst_extend y newt1 sublist) in
          (unif tl newsub)
        | _ -> 
          (match (newt1, newt2) with
          | (TyInt, TyInt) | (TyBool, TyBool) -> (unif tl sublist)
          | (TyFun (l1, l2), TyFun (r1, r2)) ->
            let nteq = (l1, r1)::(l2, r2)::tl in
            (unif nteq sublist)
          | _ -> raise (TypeError)
          )
        )
      )
    | _ -> sublist
  in (unif eqns [])

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty