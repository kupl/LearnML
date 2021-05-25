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


let rec unify_all : typ_eqn -> subst -> subst
= fun eqns s -> (match eqns with
                 | [] -> s
                 | (t1, t2)::tl -> unify_all tl (unify (subst_apply t1 s) (subst_apply t2 s)s)
                )
                 and unify : typ -> typ -> subst -> subst
                     = fun t1 t2 s -> (match (t1,t2) with
                                       | (TyInt, TyInt) -> s
                                       | (TyBool, TyBool) -> s
                                       | (TyVar v, t) -> if occurs v t then raise (Failure "TypeError")
                                                         else subst_extend v t s
                                       | (t, TyVar v) -> unify (TyVar v) t s
                                       | (TyFun (t1,t2), TyFun(t1', t2')) -> let s' = unify t1 t1' s in
                                                                             unify (subst_apply t2 s') (subst_apply t2' s') s'
                                       | (_,_) -> raise (Failure "TypeError")
                                        )


                 and occurs : string -> typ -> bool
                     = fun v t  -> (match t with
                                    | TyVar b -> v = b
                                    | TyFun(b1, b2) -> (occurs v b1) || (occurs v b2)
                                    | _ -> false                                            
                                    )

let rec gen_equations : tenv -> exp -> typ -> typ_eqn 
=fun tenv e ty -> match e with
  | CONST n -> [(ty, TyInt)]
  | VAR v -> [(ty, (tenv_find tenv v))]
  | ADD (e1, e2) -> let a = gen_equations tenv e1 TyInt in
                    let b = gen_equations tenv e2 TyInt in
                    [(ty, TyInt)] @ a @ b
  | SUB (e1, e2) -> let a = gen_equations tenv e1 TyInt in
                    let b = gen_equations tenv e2 TyInt in
                    [(ty, TyInt)] @ a @ b
  | MUL (e1, e2) -> let a = gen_equations tenv e1 TyInt in
                    let b = gen_equations tenv e2 TyInt in
                    [(ty, TyInt)] @ a @ b
  | DIV (e1, e2) -> let a = gen_equations tenv e1 TyInt in
                    let b = gen_equations tenv e2 TyInt in
                    [(ty, TyInt)] @ a @ b

  | ISZERO e -> [(ty, TyBool)] @ (gen_equations tenv e TyInt)
  | READ -> [(ty, TyInt)]
  | IF (e1, e2, e3) -> let a = gen_equations tenv e1 TyBool in
                       let b = gen_equations tenv e2 ty in
                       let c = gen_equations tenv e3 ty in
                       a @ b @ c
  | LET (x, e1, e2) -> let ty' = fresh_tyvar() in
                       let a = gen_equations tenv e1 ty' in
                       let b = gen_equations (tenv_extend (x, ty') tenv) e2 ty in
                       a @ b
                       
  | LETREC (f, x, e1, e2) -> let ty' = fresh_tyvar () in
                             let ty'' = fresh_tyvar () in
                             let a = gen_equations (tenv_extend (x, ty') (tenv_extend (f, TyFun (ty', ty'')) tenv)) e1 ty'' in
                             let b = gen_equations (tenv_extend (f, TyFun (ty', ty''))tenv) e2 ty in
                             a @ b
  | PROC (x, e) -> let ty' = fresh_tyvar () in
                   let ty'' =fresh_tyvar () in
                    (ty, TyFun (ty', ty''))::(gen_equations (tenv_extend (x,ty') tenv) e ty'')
  | CALL (e1, e2) -> let ty' = fresh_tyvar () in
                     let a = gen_equations tenv e1 (TyFun (ty', ty)) in
                     let b = gen_equations tenv e2 ty' in
                     a @ b
 
let solve : typ_eqn -> subst
=fun eqns ->unify_all eqns []


let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty