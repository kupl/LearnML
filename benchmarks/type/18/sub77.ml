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
  =fun tenv e ty ->  (* HW1: TODO *)
    match e with
    | CONST(n) -> (ty, TyInt)::[]
    | VAR(v) -> (ty, (tenv_find tenv v) )::[]
    | ADD(e1, e2) -> (ty, TyInt)::((gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt))
    | SUB(e1, e2) -> (ty, TyInt)::((gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt))
    | MUL(e1, e2) -> (ty, TyInt)::((gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt))
    | DIV(e1, e2) -> (ty, TyInt)::((gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt))
    | ISZERO(e) -> (ty, TyBool)::(gen_equations tenv e TyInt)
    | READ -> (ty, TyInt)::[]
    | IF(e1, e2, e3) -> (gen_equations tenv e1 TyBool)@(gen_equations tenv e2 ty)@(gen_equations tenv e3 ty)
    | LET(x, e1, e2) -> let al=fresh_tyvar () in (gen_equations tenv e1 al)@(gen_equations (tenv_extend (x,al) tenv) e2 ty)
    | PROC(x, e) -> let al=fresh_tyvar() in let be=fresh_tyvar() in (ty, TyFun(al,be))::(gen_equations (tenv_extend (x,al) tenv) e be)
    | CALL(e1, e2) -> let al=fresh_tyvar() in (gen_equations tenv e1 (TyFun(al, ty)))@(gen_equations tenv e2 al)
    | LETREC(f,x,e1,e2) -> let a1=fresh_tyvar() in let a2=fresh_tyvar() in (gen_equations (tenv_extend (x, a1) tenv) e1 a2)@(gen_equations (tenv_extend (f, TyFun(a1,a2)) tenv) e2 ty);;


let solve : typ_eqn -> subst
  =fun eqns -> (* HW1: TODO *)
    let rec occur = fun a tp ->
      match tp with
      | TyVar(v) -> a=v
      | TyFun(t1, t2) -> (occur a t1)||(occur a t2)
      | _ -> false
    in
    let rec unify : typ -> typ -> subst -> subst
      =fun tt1 tt2 subst ->
        match (subst_apply tt1 subst, subst_apply tt2 subst) with
        | TyInt, TyInt -> subst
        | TyBool, TyBool -> subst
        | TyFun(t1,t2), TyFun(t3,t4) -> let subst_p=(unify t1 t3 subst) in let subst_pp=(unify (subst_apply t2 subst_p) (subst_apply t4 subst_p) subst_p) in subst_pp
        | TyVar(a), tp ->
          (match tp with
           |TyFun(t1,t2) -> if (occur a tp) then (raise TypeError) else (subst_extend a tp subst)
           |TyVar(b) -> if a=b then subst else (subst_extend a tp subst)
           |_ -> (subst_extend a tp subst)
          )
        | tp, TyVar(a)-> (unify (TyVar(a)) tp subst)
        | _,_ -> raise TypeError
    in
    let rec unifyall: typ_eqn -> subst -> subst
      =fun eqns subst ->
        match eqns with
        | [] -> subst
        | (t1, t2)::tl -> let subst_p = (unify (subst_apply t1 subst) (subst_apply t2 subst) subst) in unifyall tl subst_p
    in 
    unifyall eqns subst_empty;;

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty