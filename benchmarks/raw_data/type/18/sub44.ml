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
  | CONST n -> [(ty, TyInt)] @ []
  | VAR x ->  [(ty, tenv_find tenv x)] @[]
  | ADD (e1,e2) -> [(ty, TyInt)] @ (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
  | SUB (e1,e2) -> [(ty, TyInt)] @ (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
  | MUL (e1,e2) -> [(ty, TyInt)] @ (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
  | DIV (e1,e2) -> [(ty, TyInt)] @ (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
  | ISZERO e -> [(ty,TyBool)] @ (gen_equations tenv e TyInt)
  | READ -> [(ty, TyInt)] @ []
  | IF (e1,e2,e3) -> (gen_equations tenv e1 TyBool) @ (gen_equations tenv e2 ty) @ (gen_equations tenv e3 ty)
  | LET (x,e1,e2) -> 
    begin
    let ty1 = fresh_tyvar() in
    (gen_equations tenv e1 ty1) @ (gen_equations (tenv_extend (x,ty1) tenv) e2 ty)
    end
  | LETREC (f,x,e1,e2) ->
    begin
    let ty1 = fresh_tyvar() in
    (gen_equations (tenv_extend (x,ty1) tenv) e1 ty1) @ (gen_equations (tenv_extend (x,ty1) tenv) e2 ty)
    end
  | PROC (x,e) -> 
    begin
    let ty1 = fresh_tyvar() in
    let ty2 = fresh_tyvar() in
    [(ty, TyFun(ty1,ty2))] @ (gen_equations (tenv_extend (x,ty1) tenv) e ty2)
    end
  | CALL (e1,e2) -> 
    begin
    let ty1 = fresh_tyvar() in
    (gen_equations tenv e1 (TyFun(ty1,ty))) @ (gen_equations tenv e2 ty1)
    end;;

let rec check_occur = fun tyvar ty ->
  match ty with
    |TyInt -> false
    |TyBool -> false
    |TyFun (t1, t2) -> check_occur tyvar t1 || check_occur tyvar t2
    |TyVar x -> tyvar = x

let rec unify = fun eqn subst ->
  match eqn with
    |(TyInt, TyInt) -> subst
    |(TyBool, TyBool) -> subst
    |(TyVar t1, TyVar t2) -> if t1=t2 then subst else subst_extend t1 (TyVar t2) subst
    |(TyVar t, ty) -> if (check_occur t ty) then raise TypeError else (subst_extend t ty subst)
    |(ty, TyVar t) -> unify(TyVar t, ty) subst
    |(TyFun(t1,t2), TyFun(t3,t4)) -> 
      begin
      let subst1 = unify(t1,t3) subst in
      let subst2 = unify(subst_apply t2 subst1, subst_apply t4 subst1) subst1 in
      subst2
      end
    |_ -> raise TypeError

let rec unifyall = fun eqn subst ->
  match eqn with
    |[] -> subst
    |(t1, t2)::u -> let subst1 = unify ((subst_apply t1 subst), (subst_apply t2 subst)) subst in (unifyall u subst1)
  
let solve : typ_eqn -> subst
=fun eqns -> unifyall eqns subst_empty
   
let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty