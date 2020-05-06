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
  | VAR x -> [(ty, tenv_find tenv x)]
  | ADD (e1, e2) -> [(ty, TyInt)] @ (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
  | SUB (e1, e2) -> [(ty, TyInt)] @ (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
  | MUL (e1, e2) -> [(ty, TyInt)] @ (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
  | DIV (e1, e2) -> [(ty, TyInt)] @ (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
  | ISZERO e -> [(ty, TyBool)] @ (gen_equations tenv e TyInt)
  | READ -> [(ty, TyInt)]
  | IF (e1, e2, e3) -> (gen_equations tenv e1 TyBool) @ (gen_equations tenv e2 ty) @ (gen_equations tenv e3 ty)
  | LET (x, e1, e2) -> let a = fresh_tyvar () in (gen_equations tenv e1 a) @ (gen_equations (tenv_extend (x, a) tenv) e2 ty)
  | LETREC (f, x, e1, e2) -> let a1 = fresh_tyvar () in let a2 = fresh_tyvar () in 
    (gen_equations (tenv_extend (f, TyFun (a1, a2)) tenv) e2 ty) @ (gen_equations (tenv_extend (f, TyFun (a1, a2)) (tenv_extend (x, a1) tenv)) e1 a2)
  | PROC (x, e) -> let a1 = fresh_tyvar () in let a2 = fresh_tyvar () in [(ty, (TyFun (a1, a2)))] @ (gen_equations (tenv_extend (x, a1) tenv) e a2)
  | CALL (e1, e2) -> let a = fresh_tyvar () in (gen_equations tenv e1 (TyFun (a, ty))) @ (gen_equations tenv e2 a) 
  
let rec contra_check = fun typ1 typ2 -> match typ1, typ2 with
  |TyVar x, _ -> false
  |_, TyVar y -> false
  |TyFun (ty1, ty2), TyFun (t1, t2) -> if (contra_check ty1 t1) then true else (contra_check ty2 t2)
  |_, _ -> if (typ1 = typ2) then false else true
  
let simp_check = fun typ1 typ2 -> match typ1, typ2 with
  |TyFun (x1, x2), TyFun (y1, y2) -> true
  |_ -> false
  
let simp_eqns = fun typ1 typ2 -> match typ1, typ2 with
  |TyFun (x1, x2), TyFun (y1, y2) -> [(x1, y1); (x2, y2)]
  |_ -> [];;
  
let notvar_check = fun typ -> match typ with
  |TyVar x -> false
  |_ -> true
  
let rec overlap_check = fun typ1 typ2 -> match typ1 with 
  |TyVar x -> (match typ2 with
    |TyFun (t1, t2) -> if (overlap_check typ1 t1) then true else (overlap_check typ1 t2)
    |_ -> if (typ1 = typ2) then true else false)
   |TyFun (ty1, ty2) -> (match typ2 with
    |TyFun (t1, t2) -> if (overlap_check ty1 t1) then true else
      if (overlap_check ty1 t2) then true else
      if (overlap_check ty2 t1) then true else (overlap_check ty2 t2)
    |_ -> if (overlap_check ty1 typ2) then true else (overlap_check ty2 typ2))
  |_ -> false  
  
let rec update_subst = fun subst result -> match subst with
  |[] -> result
  |(x, typ) :: tl -> update_subst tl ((x, (subst_apply typ (tl @ result))) :: result) 

let solve : typ_eqn -> subst
=fun eqns -> let rec f eq subs = (match eq with
  |[]-> subs
  |(t1, t2) :: tl -> let typ1 = (subst_apply t1 subs) in let typ2 = (subst_apply t2 subs) in if (typ1 = typ2) then f tl subs else
    if (contra_check typ1 typ2) then raise TypeError else
    if (simp_check typ1 typ2) then f ((simp_eqns typ1 typ2) @ tl) subs else 
    if (notvar_check typ1) then f ([(typ2, typ1)] @ tl) subs else
    if (overlap_check typ1 typ2) then raise TypeError else
    (match typ1 with 
      |TyVar x ->f tl (update_subst (subst_extend x typ2 subs) [])
      |_ -> raise TypeError)) in f eqns subst_empty
  
let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty