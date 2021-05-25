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
  | CONST num -> [ty,TyInt]
  | VAR var -> [(ty, (tenv_find tenv var))]
  | ADD (e1, e2) -> [(ty, TyInt)]@(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
  | SUB (e1, e2) -> [(ty, TyInt)]@(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
  | MUL (e1, e2) -> [(ty, TyInt)]@(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
  | DIV (e1, e2) -> [(ty, TyInt)]@(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
  | READ -> [ty,TyInt]
  | ISZERO(e) -> [(ty, TyBool)]@(gen_equations tenv e TyInt)
  | IF(e1, e2, e3) -> 
    let q1 = gen_equations tenv e1 TyBool in
    let tv = fresh_tyvar() in
    let q2 = gen_equations tenv e2 tv in
    let q3 = gen_equations tenv e3 tv in
    [(ty,tv)]@(q1)@(q2)@(q3)
  | LET(var, e1, e2) -> 
    let tv = fresh_tyvar() in
    let q1 = gen_equations tenv e1 tv in
    let q2 = gen_equations (tenv_extend (var, tv) tenv) e2 ty in
    q1@q2
  | LETREC (var1, var2, e1, e2) -> 
    let tv1 = fresh_tyvar() in
    let tv2 = fresh_tyvar() in
    let ex = tenv_extend (var1, tv1) tenv in
    let q1 = gen_equations (tenv_extend (var2, TyFun (tv1, tv2)) ex) e1 tv2 in
    let q2 = gen_equations (tenv_extend (var2, TyFun (tv1, tv2)) tenv) e2 tv2 in
    q1@q2  
  | PROC(var, e1) -> 
    let tv1 = fresh_tyvar() in
    let tv2 = fresh_tyvar() in
    let q1 = gen_equations (tenv_extend (var,tv1) tenv) e1 tv2 in
    [(ty, TyFun(tv1,tv2))]@q1
  | CALL(e1, e2) -> 
    let tv1 = fresh_tyvar() in
    let q1 = gen_equations tenv e2 tv1 in
    let tv2 = TyFun(tv1, ty) in
    let q2 = gen_equations tenv e1 tv2 in
    q1@q2
let rec unify : typ_eqn -> subst -> subst
= fun eqn  sub -> match eqn with
| [] -> sub
| (tv,ty)::tl -> 
  let tv1 = subst_apply tv sub in
  let ty1 = subst_apply ty sub in
   (match tv1,ty1 with
    | TyInt,TyInt -> unify tl sub
    | TyBool,TyBool -> unify tl sub
    | TyVar x , TyVar y -> 
      if x = y then raise (TypeError)
      else unify tl (subst_extend x (TyVar y)  sub)
    | TyVar x, TyFun(y1,y2) -> 
      if ((TyVar x = y1)||(TyVar x = y2)) then raise (TypeError)
      else unify tl (subst_extend x (TyFun (y1,y2)) sub)
    | TyVar x, _ -> unify tl (subst_extend x ty1 sub)   
    | _, TyVar x -> unify tl (subst_extend x tv1 sub)   
    | (TyFun(x1,x2),TyFun(x1',x2')) -> 
      let newsub = unify ((x1 ,x1')::tl) sub in
      let x3 = subst_apply x2 newsub in
      let x4 = subst_apply x2' newsub in
      unify ((x3, x4)::tl) newsub
    |_,_ -> raise (TypeError)
    )
let solve : typ_eqn -> subst
=fun eqns -> unify eqns subst_empty
let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty