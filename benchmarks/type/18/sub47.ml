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


(* HW1: TODO *)

let rec gen_equations : tenv -> exp -> typ -> typ_eqn 
=fun tenv e ty -> match e with 
  | CONST n -> [(ty, TyInt)]
  | VAR x -> [(ty, tenv_find tenv x)](*need to revisit*)
  | ADD (e1, e2) -> (ty, TyInt)::(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
  | SUB (e1, e2) -> (ty, TyInt)::(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
  | MUL (e1, e2) -> (ty, TyInt)::(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
  | DIV (e1, e2) -> (ty, TyInt)::(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
  | ISZERO e -> (ty,TyBool)::(gen_equations tenv e TyInt)
  | READ -> [](*pass*)
  | IF (e1,e2,e3) -> (gen_equations tenv e1 TyBool)@(gen_equations tenv e2 ty)@(gen_equations tenv e3 ty)
  | LET (x,e1,e2) -> let a = fresh_tyvar() in (gen_equations tenv e1 a)@(gen_equations (tenv_extend (x,a) tenv) e2 ty)(*Usage of type variable*)
  | LETREC  (f, x, e1,e2) -> let a = fresh_tyvar() in let b = fresh_tyvar() in (gen_equations (tenv_extend (x,a) (tenv_extend (f,TyFun(a,b)) tenv)) e2 ty)@(gen_equations (tenv_extend (f,TyFun(a,b))(tenv_extend (x,a) tenv)) e1 b)
  | PROC (x, e) -> let a = fresh_tyvar() in let b = fresh_tyvar() in (ty,TyFun(a,b))::(gen_equations (tenv_extend (x,a) tenv) e b)
  | CALL (e1, e2) -> let a = fresh_tyvar() in (gen_equations tenv e1 (TyFun(a,ty)) )@(gen_equations tenv e2 a)

let rec check_occur typ var =(*check for one type*)
    match typ with
    | TyInt -> false
    | TyBool -> false
    | TyFun (t1,t2) -> (check_occur t1 var) || (check_occur t2 var)
    | TyVar x -> if x=var then true else false
    
let rec replace2 typ v t =(*change one type's Var v to t*)
    match typ with
    | TyInt -> TyInt
    | TyBool -> TyBool
    | TyFun (t1,t2) -> TyFun((replace2 t1 v t),(replace2 t2 v t))
    | TyVar x -> if x = v then t else TyVar x
    
let rec replace (v, t) subs1 subs2= match subs1 with
  |(tyvar, ty)::tl -> replace (v,t) tl (subst_extend tyvar (replace2 ty v t) subs2)
  |[]-> subs2

let rec unify eqn subs = match eqn with
  |(TyInt, TyInt) -> subs
  |(TyBool, TyBool) -> subs
  |(TyVar a, TyVar b) -> if a=b then subs else subst_extend a (TyVar b) (replace (a, TyVar b) subs [])
  |(TyVar a, ty) -> if (check_occur ty a) then raise TypeError else subst_extend a ty (replace (a, ty) subs [])
  |(TyFun(t1,t2),TyFun(t3,t4)) -> let s1 = (unify (t1,t3) subs) in let s2 = (unify ((subst_apply t2 s1),(subst_apply t4 s1)) s1) in s2
  |(ty, TyVar a) -> (unify ((TyVar a), ty) subs)
  |(_,_)->raise TypeError
let solve : typ_eqn -> subst
=fun eqns -> let rec unifyall equations substitution =
  (match equations with
    |(t1,t2)::tl -> let s1 = (unify((subst_apply t1 substitution),(subst_apply t2 substitution)) substitution) in (unifyall tl s1)
    |[] -> substitution) 
    in unifyall eqns subst_empty

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty