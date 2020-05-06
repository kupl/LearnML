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
  | PROC (var, exp) -> 
      let typ1 = fresh_tyvar () in
      let typ2 = fresh_tyvar () in
      let n1 = [(ty, TyFun (typ1,typ2))] in
      let n2 = gen_equations (tenv_extend (var, typ1) tenv) exp typ2 in
        n1@n2
  | LETREC (var1, var2, exp1, exp2) -> 
      let typ1 = fresh_tyvar () in
      let typ2 = fresh_tyvar () in
      raise TypeError
  | READ -> [(ty, TyInt)]
  | CONST n -> [(ty, TyInt)]
  | VAR var -> [(ty, tenv_find tenv var)]
  | CALL (exp1, exp2) ->
      let typ = fresh_tyvar () in
      let n2 = gen_equations tenv exp2 (typ) in
      let n1 = gen_equations tenv exp1 (TyFun (typ, ty)) in
        n1@n2
  | ADD (exp1, exp2) ->
      let n3 = gen_equations tenv exp2 TyInt in
      let n2 = gen_equations tenv exp1 TyInt in
      let n1 = (ty, TyInt) in
        n1::n2@n3
  | SUB (exp1, exp2) ->
      let n3 = gen_equations tenv exp2 TyInt in
      let n2 = gen_equations tenv exp1 TyInt in
      let n1 = (ty, TyInt) in
        n1::n2@n3
  | MUL (exp1, exp2) ->
      let n3 = gen_equations tenv exp2 TyInt in
      let n2 = gen_equations tenv exp1 TyInt in
      let n1 = (ty, TyInt) in
        n1::n2@n3
  | DIV (exp1, exp2) ->
      let n3 = gen_equations tenv exp2 TyInt in
      let n2 = gen_equations tenv exp1 TyInt in
      let n1 = (ty, TyInt) in
        n1::n2@n3
  | ISZERO exp ->
      let n2 = gen_equations tenv exp TyInt in
      let n1 = [(ty, TyBool)] in
        n1@n2
  | IF (exp1, exp2, exp3) ->
      let n3 = gen_equations tenv exp3 ty in
      let n2 = gen_equations tenv exp2 ty in
      let n1 = gen_equations tenv exp1 TyBool in
        n1@n2@n3
  | LET (var, exp1, exp2) ->
      let typ = fresh_tyvar () in
      let n2 = gen_equations (tenv_extend (var, typ) tenv) exp2 ty in
      let n1 = gen_equations tenv exp1 typ in
        n1@n2
;;

(* TyInt | TyBool | TyFun of typ * typ | TyVar of tyvar *)
(* 
(TyInt, TyInt)
(TyBool, TyBool)
(TyBool, TyInt) - Error
(TyInt, TyBool) - Error
(TyFun(t1, t2), TyFun(t1', t2')) - (t1, t1'), (t2, t2')
 *)
let rec check_occurence : string -> typ -> bool
= fun var typ1 ->
  match typ1 with
  | (TyInt | TyBool) -> true 
  | TyFun (typ1, typ2) -> 
    let a = check_occurence var typ1 in
    let b = check_occurence var typ2 in
    true
  | TyVar var2 ->
    if (var = var2) then
      raise TypeError
    else true
  ;;

let rec unify : (typ * typ) -> subst -> subst
= fun typ_pair subs ->
  match typ_pair with
  | (typ, TyVar var) -> 
    let test = check_occurence var typ in
      if test = false then raise TypeError
      else subst_extend var typ subs
  | (TyVar var, typ) -> 
      subst_extend var typ subs
  | (TyFun (typ1,typ2), TyFun (typ1',typ2')) ->
    let subs' = unify (typ1, typ1') subs in
    let typ1'' = subst_apply typ2 subs' in
    let typ2'' = subst_apply typ2' subs' in
      unify (typ1'', typ2'') subs'
  | (TyInt, TyInt) -> subs
  | (TyBool, TyBool) -> subs
  | (_, _) -> raise TypeError
;;

let rec unifyall : typ_eqn -> subst -> subst
= fun eqns subs ->
  match eqns with
  | [] -> subs
  | (t1, t2)::tl -> 
    let app1 = (subst_apply t1 subs) in
    let app2 = (subst_apply t2 subs) in
    let subs' = unify(app1, app2) subs in
      unifyall tl subs'
;;

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