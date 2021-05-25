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
=fun tenv e ty -> (*[]*) (* TODO *)
  match e with
    CONST (n) -> [(ty, TyInt)]
    | VAR (v) -> [(ty, tenv_find tenv v)]
    | ADD (e1, e2) -> let l = [(ty, TyInt)] in
                        let li = gen_equations tenv e1 TyInt in
                        let lis = gen_equations tenv e2 TyInt in
                        l@li@lis
    | SUB (e1, e2) -> let l = [(ty, TyInt)] in
                        let li = gen_equations tenv e1 TyInt in
                        let lis = gen_equations tenv e2 TyInt in
                        l@li@lis
    | MUL (e1, e2) -> let l = [(ty, TyInt)] in
                        let li = gen_equations tenv e1 TyInt in
                        let lis = gen_equations tenv e2 TyInt in
                        l@li@lis
    | DIV (e1, e2) -> let l = [(ty, TyInt)] in
                        let li = gen_equations tenv e1 TyInt in
                        let lis = gen_equations tenv e2 TyInt in
                        l@li@lis
    | ISZERO (e) -> let l = [(ty, TyBool)] in
                      let li = gen_equations tenv e TyInt in
                      l@li
    | READ -> [(ty, TyInt)]
    | IF (e1, e2, e3) -> let l = gen_equations tenv e1 TyBool in
                          let li = gen_equations tenv e2 ty in
                          let lis = gen_equations tenv e3 ty in
                          l@li@lis
    | LET (x, e1, e2) -> let t = fresh_tyvar () in
                          let l = gen_equations tenv e1 t in
                          let li = gen_equations (tenv_extend (x, t) tenv) e2 ty in
                          l@li
    | LETREC (f, x, e1, e2) -> (*let t1 = fresh_tyvar () in*)
                                let t2 = fresh_tyvar () in
                                let t3 = fresh_tyvar () in
(*                                let l = [(t1, TyFun (t2, t3))] in*)
                                let tenv1 = tenv_extend (x, t2) tenv in
                                let tenv2 = tenv_extend (f, TyFun (t2, t3)) tenv1 in
                                let li = gen_equations tenv2 e1 t3 in
                                let lis = gen_equations tenv2 e2 ty in
                                li@lis (*l@*)
    | PROC (x, e) -> let t1 = fresh_tyvar () in
                      let t2 = fresh_tyvar () in
                      let l = [(ty, TyFun (t1, t2))] in
                      let li = gen_equations (tenv_extend (x, t1) tenv) e t2 in
                      l@li
    | CALL (e1, e2) -> let t = fresh_tyvar () in
                        let l = gen_equations tenv e1 (TyFun(t, ty)) in
                        let li = gen_equations tenv e2 t in
                        l@li
    

let rec solve : typ_eqn -> subst
=fun eqns -> (*subst_empty*) (* TODO *)
  unifyall eqns []
  
and unifyall te ss = match te with
                      [] -> ss
                      | (h1, h2)::t -> let s = unify (subst_apply h1 ss) (subst_apply h2 ss) ss in
                                        unifyall t s
                                        
and unify t1 t2 ss = match t1, t2 with
                      TyInt, TyInt -> ss
                      | TyBool, TyBool -> ss
                      | TyVar (v1), TyVar (v2) -> let s = subst_extend v1 t2 ss in
                                                    subst_extend v2 t1 s
                      | TyVar (v1), t3 -> ( match t3 with
                                            TyFun (a, b) -> if occurencecheck t1 a || occurencecheck t1 b then raise TypeError
                                                                                                          else subst_extend v1 t3 ss
                                            | _ -> subst_extend v1 t3 ss )
                      | t3, TyVar (v1) -> unify (TyVar (v1)) t3 ss
                      | TyFun(a, b), TyFun (c, d) -> let s = unify a c ss in
                                                      unify (subst_apply b s) (subst_apply d s) s
                      | _, _ -> raise TypeError
                                                      
                      
and occurencecheck a b = match b with
                          TyFun (c, d) -> (occurencecheck a c) && (occurencecheck a d)
                          | _ -> a = b

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty