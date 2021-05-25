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
  | VAR x -> [(ty, (tenv_find tenv x))]     (*이건 확인할 것. 불안함.*)
  | ADD (e1, e2) -> [(ty, TyInt)]@(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
  | SUB (e1, e2) -> [(ty, TyInt)]@(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
  | MUL (e1, e2) -> [(ty, TyInt)]@(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
  | DIV (e1, e2) -> [(ty, TyInt)]@(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
  | ISZERO e -> [(ty, TyBool)]@(gen_equations tenv e TyInt)
  | IF (e1, e2, e3) -> (gen_equations tenv e1 TyBool)@(gen_equations tenv e2 ty)@(gen_equations tenv e3 ty)
  | LET (x, e1, e2) -> let a = fresh_tyvar() in
                        [(ty, a)]@(gen_equations tenv e1 a)@(gen_equations (tenv_extend (x, a) tenv) e2 ty)
  | LETREC (f, x, e1, e2) -> let a1 = fresh_tyvar() in
                              let a2 = fresh_tyvar() in
                                [(TyVar x, a2); (TyVar f, TyFun (a2, a1))]@(gen_equations (tenv_extend (f, TyFun (a2, a1)) (tenv_extend (x, a2) tenv)) e1 a1)@(gen_equations (tenv_extend (f, TyFun (a2, a1)) tenv) e2 ty)
  | PROC (f, e) -> let a1 = fresh_tyvar() in
                    let a2 = fresh_tyvar() in
                      [(ty, TyFun (a1, a2))]@(gen_equations (tenv_extend (f, a1) tenv) e a2)
  | CALL (e1, e2) -> let a = fresh_tyvar() in
                      (gen_equations tenv e1 (TyFun (a, ty)))@(gen_equations tenv e2 a)
  | READ -> []

let rec solve : typ_eqn -> subst
=fun eqns -> unifyall eqns subst_empty

and unifyall : typ_eqn -> subst -> subst 
=fun eqns s ->
  match eqns with
  | [] -> s
  | (t1, t2)::t -> let s' = unify (subst_apply t1 s) (subst_apply t2 s) s
                    in unifyall t s'

and unify : typ -> typ -> subst -> subst
=fun t1 t2 s -> 
  match (t1, t2, s) with
  | (TyInt, TyInt, s) -> s 
  | (TyBool, TyBool, s) -> s 
  | (TyVar a, TyVar b, s) -> if (a = b) then s else subst_extend a (TyVar b) s
  | (TyVar a, t, s) -> if (include_check t a) then raise TypeError
                        else subst_extend a t s
  | (t, TyVar a, s) -> unify (TyVar a) t s
  | (TyFun (t1, t2), TyFun (t3, t4), s) -> let s1 = unify t1 t3 s in
                                            let s2 = unify (subst_apply t2 s1) (subst_apply t4 s1) s1 in
                                              s2
  | (_, _, _) -> raise TypeError

and include_check : typ -> tyvar -> bool
=fun t a ->
  match t with
  | TyInt -> false
  | TyBool -> false
  | TyVar x -> if (a = x) then true else false
  | TyFun (t1, t2) -> (include_check t1 a)||(include_check t2 a)

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty