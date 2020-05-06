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
  | VAR x -> [(tenv_find tenv x, ty)] 
  
  | ADD (e1, e2) -> gen_aop_equations tenv e1 e2 ty
  | SUB (e1, e2) -> gen_aop_equations tenv e1 e2 ty
  | MUL (e1, e2) -> gen_aop_equations tenv e1 e2 ty
  | DIV (e1, e2) -> gen_aop_equations tenv e1 e2 ty
  
  | ISZERO e -> let eqn1 = [(ty, TyBool)] in 
                let eqn2 = gen_equations tenv e TyInt in
                eqn1 @ eqn2
  | READ -> []
  | IF (e1, e2, e3) -> let eqn1 = gen_equations tenv e1 TyBool in
                       let eqn2 = gen_equations tenv e2 ty in
                       let eqn3 = gen_equations tenv e3 ty in
                       eqn1 @ eqn2 @ eqn3
  
  | LET (x, e1, e2) -> let new_tv = fresh_tyvar () in
                       let eqn1 = gen_equations tenv e1 new_tv in
                       let tenv' = tenv_extend (x, new_tv) tenv in
                       let eqn2 = gen_equations tenv' e2 ty in
                       eqn1 @ eqn2
  
  | LETREC (f, x, e1, e2) -> let new_tv1 = fresh_tyvar () in 
                             let new_tv2 = fresh_tyvar () in
                             let tenv1 = tenv_extend (f, TyFun (new_tv2, new_tv1)) tenv in
                             let tenv2 = tenv_extend (x, new_tv2) tenv1 in
                             let eqn1 = gen_equations tenv2 e1 new_tv1 in
                             let eqn2 = gen_equations tenv1 e2 ty in
                             eqn1 @ eqn2
  
  | PROC (x, e) -> let new_tv1 = fresh_tyvar () in 
                   let new_tv2 = fresh_tyvar () in
                   let eqn1 = [(ty, TyFun (new_tv1, new_tv2))] in
                   let tenv' = tenv_extend (x, new_tv1) tenv in
                   let eqn2 = gen_equations tenv' e new_tv2 in
                   eqn1 @ eqn2
  | CALL (e1, e2) -> let new_tv = fresh_tyvar () in
                     let eqn1 = gen_equations tenv e1 (TyFun(new_tv ,ty)) in
                     let eqn2 = gen_equations tenv e2 new_tv in
                     eqn1 @ eqn2

and gen_aop_equations = fun tenv e1 e2 ty -> 
  let eqn1 = [(ty, TyInt)] in
  let eqn2 = gen_equations tenv e1 TyInt in
  let eqn3 = gen_equations tenv e2 TyInt in
  eqn1 @ eqn2 @ eqn3


let solve : typ_eqn -> subst
=fun eqns -> 
  let rec unify : typ -> typ -> subst -> subst
  = fun t1 t2 s -> begin match t1, t2 with
    | TyInt, TyInt -> s
    | TyBool, TyBool -> s
    | TyVar a, t -> begin match t with
                    | TyVar b -> if a = b then s else subst_extend a (TyVar b) s
                    | TyInt -> subst_extend a TyInt s
                    | TyBool -> subst_extend a TyBool s
                    | TyFun (t1', t2') -> if (TyVar a) = t1' || (TyVar a) = t2'
                                          then raise TypeError
                                          else subst_extend a (TyFun (t1', t2')) s end
    | t, TyVar a -> unify (TyVar a) t s
    | TyFun (t1, t2), TyFun (t3, t4) -> let s1 = unify t1 t3 s in
                                        let t2' = subst_apply t2 s1 in
                                        let t4' = subst_apply t4 s1 in
                                        let s2 =  unify t2' t4' s1 in
                                        s2
    | _, __ -> raise TypeError
  end  
    
  in let rec unifyall : typ_eqn -> subst -> subst
  = fun eqns s -> begin match eqns with
    | [] -> s
    | (t1, t2)::u -> let t1' = subst_apply t1 s in
                     let t2' = subst_apply t2 s in
                     let s' = unify t1' t2' s in
                     unifyall u s' 
  end
  
  in unifyall eqns subst_empty


let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty