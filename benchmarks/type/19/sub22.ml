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
    | CONST n ->[(ty,TyInt)]
    | VAR x ->[(ty,tenv_find tenv x)]
    | ADD (e1,e2) ->(ty,TyInt)::(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
    | SUB (e1,e2) ->(ty,TyInt)::(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
    | MUL (e1,e2) ->(ty,TyInt)::(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
    | DIV (e1,e2) ->(ty,TyInt)::(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
    | ISZERO e ->(ty, TyBool)::(gen_equations tenv e TyInt)
    | READ -> [(ty,TyInt)]
    | IF (e1,e2,e3) ->(gen_equations tenv e1 TyBool)@(gen_equations tenv e2 ty)@(gen_equations tenv e3 ty)
    | LET (x,e1,e2) ->let a = fresh_tyvar() in
        let newenv= tenv_extend (x,a) tenv in 
        (gen_equations tenv e1 a)@(gen_equations newenv e2 ty)
    | LETREC (x,y,e1,e2) ->let a1 = fresh_tyvar() in
        let a2 = fresh_tyvar() in
        let newenv1 = tenv_extend (x, TyFun (a1, a2)) tenv in
        let newenv2 = tenv_extend (y, a1) newenv1 in
        (gen_equations newenv2 e1 a2) @ (gen_equations newenv1 e2 ty)
    | PROC (x,e) ->let a = fresh_tyvar() in
        let b = fresh_tyvar() in
        let y = TyFun(a,b) in
        let newenv= tenv_extend (x,a) tenv in
        (ty,y)::(gen_equations newenv e b)
    | CALL (e1,e2) ->let a = fresh_tyvar() in
        let b = TyFun(a,ty) in
        (gen_equations tenv e1 b)@(gen_equations tenv e2 a)



 let rec unify : typ -> typ -> subst -> subst
= fun t1 t2 subst ->
match (t1,t2) with
 |(TyInt,TyInt) -> subst
 |(TyBool,TyBool) -> subst
 |(TyVar x1,TyVar x2) -> subst
 |(alfa,TyVar a) -> unify (TyVar a) alfa subst
      |(TyVar a, alfa) ->
        if t1 = t2 then raise TypeError  else subst_extend a alfa subst
 |(TyFun (tf1,tf2),TyFun(tf1',tf2')) ->
    let subst' = unify tf1 tf1' subst in
    let subst'' = unify (subst_apply tf2 subst') (subst_apply tf2' subst') subst' in
    subst''
 |(_,_) -> raise TypeError



let rec unifyall : typ_eqn -> subst -> subst
= fun eqns subst ->
match eqns with
|[] -> subst
|(t1,t2)::u -> let t1' = subst_apply t1 subst in let t2' = subst_apply t2 subst in 
let subst' = unify t1' t2' subst in unifyall u subst'
        
let solve : typ_eqn -> subst
=fun eqns -> unifyall eqns subst_empty

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty