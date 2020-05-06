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
    | CONST n -> [ty, TyInt]
    | VAR x -> [(tenv_find tenv x), ty]
    | ADD (e1, e2)
    | SUB (e1, e2)
    | MUL (e1, e2)
    | DIV (e1, e2) -> 
        [ty, TyInt]@
        (gen_equations tenv e1 TyInt)@
        (gen_equations tenv e2 TyInt)
    | ISZERO e -> 
        [ty, TyBool]@
        (gen_equations tenv e TyInt)
    | READ -> []
    | IF (e1, e2, e3) -> 
        (gen_equations tenv e1 TyBool)@
        (gen_equations tenv e2 ty)@
        (gen_equations tenv e3 ty)
    | LET (x, e1, e2) -> 
        let a = fresh_tyvar () in 
        (gen_equations tenv e1 a)@
        (gen_equations (tenv_extend (x, a) tenv) e2 ty)
    | LETREC (f, x, e1, e2) -> 
        let a = fresh_tyvar () in
        let b = fresh_tyvar () in
        (gen_equations (tenv_extend (f, TyFun (b, a)) (tenv_extend (x, b) tenv)) e1 a)@
        (gen_equations (tenv_extend (f, TyFun (b, a)) tenv) e2 ty) 
    | PROC (x, e) -> 
        let a = fresh_tyvar () in
        let b = fresh_tyvar () in 
        [ty, TyFun (a, b)]@
        (gen_equations (tenv_extend (x, a) tenv) e b)
    | CALL (e1, e2) -> 
        let a = fresh_tyvar () in
        (gen_equations tenv e1 (TyFun (a, ty)))@
        (gen_equations tenv e2 a)
    
    
let solve : typ_eqn -> subst
=fun eqns ->
  let rec unifyall = fun eqn subst ->
    match eqn with
      | [] -> subst
      | (tx, ty)::tl ->
          let app_tx = subst_apply tx subst in
          let app_ty = subst_apply ty subst in
          let rec unify = fun t1 t2 s ->
            if t1 = t2 then s
            else 
              (match (t1, t2) with
                | (TyVar tv, t2) ->
                    begin match t2 with
                      | TyFun (t', t'') -> 
                        if t' = TyVar tv || t'' = TyVar tv then raise TypeError else 
                        let extend_Sub = subst_extend tv t2 s
                        in extend_Sub
                      | _ -> let extend_Sub = subst_extend tv t2 s
                             in extend_Sub
                    end
                | (t1, TyVar tv) ->
                    unify (TyVar tv) t1 s
                | (TyFun (a, b), TyFun (c, d)) ->
                    let s' = unify a c s in
                    let s'' = unify (subst_apply b s') (subst_apply d s') s'
                    in s''
                | (_, _) -> 
                    raise TypeError)
          in unifyall tl (unify app_tx app_ty subst)
  in unifyall eqns subst_empty

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty