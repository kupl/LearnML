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
 | VAR x -> [(ty,tenv_find tenv x)]
 | ADD (e1,e2) -> [(ty,TyInt)]@(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
 | SUB (e1,e2) -> [(ty,TyInt)]@(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
 | MUL (e1,e2) -> [(ty,TyInt)]@(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
 | DIV (e1,e2) -> [(ty,TyInt)]@(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
 | ISZERO e1 -> [(ty,TyBool)]@(gen_equations tenv e1 TyInt)
 | READ -> let a = fresh_tyvar() in [(ty, a)] (* 일단 fresh variable을 입해주는걸로 했음*)
 | IF (e1,e2,e3) -> (gen_equations tenv e1 TyBool)@(gen_equations tenv e2 ty)@(gen_equations tenv e3 ty)
 | LET (x,e1,e2) -> 
    let t1 =fresh_tyvar() in (gen_equations tenv e1 t1)@(gen_equations (tenv_extend (x,t1) tenv) e2 ty)
 | LETREC (f,x,e1,e2) -> 
    let t1 = fresh_tyvar() in 
    let t2 = fresh_tyvar() in
    let new_env =  (tenv_extend (f,TyFun(t2,t1)) tenv) in
    (gen_equations (tenv_extend (x,t2) new_env) e1 t1) @  (gen_equations new_env e2 ty)
 | PROC (x,e) ->
    let t1 = fresh_tyvar() in
    let t2 = fresh_tyvar() in
    [(ty,TyFun(t1,t2))]@(gen_equations (tenv_extend (x,t1) tenv) e t2)
 | CALL (e1,e2) -> let t1 = fresh_tyvar() in
    (gen_equations tenv e1 (TyFun(t1,ty))) @ (gen_equations tenv e2 t1) 

(* solve에서 equation에 만약 variable이 좌항에 있다면 우항으로 바꿔주는것도 해야하나? ㅇㅇ*)
let rec solve : typ_eqn -> subst
=fun eqns -> 
  unifyall eqns subst_empty
          
and unifyall : typ_eqn -> subst -> subst
= fun eqns s ->
  match eqns with
  | [] -> s
  | hd::tl -> 
    let t1,t2 = hd in 
    let t_1 = subst_apply t1 s in
    let t_2 = subst_apply t2 s in
    let s_temp = unify t_1 t_2 s in
    unifyall tl s_temp

and unify : typ -> typ -> subst -> subst
= fun t1 t2 s ->
  match t1,t2 with
  | TyInt, TyInt -> s
  | TyBool, TyBool -> s
  | TyVar x, TyVar y -> 
    if x=y then s else subst_extend x (TyVar y) s (* 이 else일때 둘이 안바꿔줘도 되나??*)
  | _, TyVar x -> unify t2 t1 s (* Not TyVar, TyVar인 경우 *)
  | TyVar x, y ->
    (match y with
     | TyInt -> subst_extend x TyInt s (* TyVar, TyInt *)
     | TyBool -> subst_extend x TyBool s (* TyVar, TyBool *)
     | TyFun (t1,t2) -> 
       let b = occur_check (TyVar x) (TyFun (t1,t2)) in
       if b=false then subst_extend x y s else raise TypeError
     | TyVar z -> raise TypeError
     )
  | l,r ->
    (match l,r with
     | TyFun(l1,l2), TyFun(r1,r2) -> 
       let s_temp = unify l1 r1  s in
       let s_temp_2 = unify (subst_apply l2 s_temp) (subst_apply r2 s_temp) s_temp in
       s_temp_2
     | _ -> raise TypeError)

and occur_check : typ -> typ -> bool (* true -> occur! *)
= fun target check_type ->
  match check_type with
  | TyFun(t1,t2) -> 
    (match t1, t2 with
     | TyFun (t1_1,t1_2), TyFun (t2_1,t2_2) -> (occur_check target t1)||(occur_check target t2)
     | TyFun (t1_1,t1_2), _ -> (occur_check target t1)||(t2=target)
     | _, TyFun(t2_1,t2_2) -> (t1=target)||(occur_check target t2)
     | _,_ -> (t1=target)||(t2=target)
    )
    | _ -> raise (Failure "occur_check_error!")



let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty