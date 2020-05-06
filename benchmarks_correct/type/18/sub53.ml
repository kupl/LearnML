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
    | CONST n -> [(ty,TyInt)]
    | VAR v -> [(ty, tenv_find tenv v)] 
    | ADD (e1,e2) -> (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt) @ [(ty, TyInt)]
    | SUB (e1,e2) -> (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt) @ [(ty, TyInt)]
    | MUL (e1,e2) -> (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt) @ [(ty, TyInt)]
    | DIV (e1,e2) -> (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt) @ [(ty, TyInt)]
    | ISZERO e -> (gen_equations tenv e TyInt) @ [(ty, TyBool)]
    | READ -> [(ty, TyInt)]
    | IF (e1,e2,e3) -> let t = fresh_tyvar() in 
                       (gen_equations tenv e1 TyBool) @ (gen_equations tenv e2 t) @ 
                       (gen_equations tenv e3 t ) @ [(ty, t)]
    | LET (x,e1,e2) -> let t1 = fresh_tyvar() in  
                       let t2 = fresh_tyvar() in
                       (gen_equations tenv e1 t1) @ (gen_equations (tenv_extend (x,t1) tenv) e2 t2) @ [(ty, t2)]
    | LETREC (f,x,e1,e2) -> let t1 = fresh_tyvar() in  
                            let t2 = fresh_tyvar() in
                            let t = fresh_tyvar() in

                            let eq1 = [(ty,t)] in
                            let tenv = tenv_extend (f, TyFun(t2,t1)) tenv in
                            let eq2 = gen_equations tenv e2 t in
                            let tenv = tenv_extend (x,t2) tenv in
                            let eq3 = gen_equations tenv e1 t1 in
                            eq1 @ eq2 @ eq3
                          

    | PROC (x,body) -> let t1 = fresh_tyvar() in  
                    let t2 = fresh_tyvar() in
                    (gen_equations (tenv_extend (x,t1) tenv) body t2) @ [(ty, TyFun(t1,t2))]
    | CALL (e1,e2) -> let t1 = fresh_tyvar() in  
                      let t2 = fresh_tyvar() in
                      (gen_equations tenv e1 (TyFun(t1,t2))) @ (gen_equations tenv e2 t1) @[(ty,t2)]


let rec tvinright: var -> typ -> bool
= fun a t -> 
  match t with 
    |TyInt -> false
    |TyBool -> false
    |TyFun(t1,t2) -> tvinright a t1 || tvinright a t2
    |TyVar tt -> a = tt

let rec unify : typ -> typ -> subst -> subst
= fun ty1 ty2 subst ->
    match ty1, ty2 with
      | TyInt, TyInt -> subst
      | TyBool, TyBool -> subst
      | TyVar a1, TyVar a2 -> if a1 = a2 then subst else subst_extend a1 (TyVar a2) subst
      | TyVar a, ty -> if tvinright a ty then raise TypeError else subst_extend a ty subst
      | ty, TyVar a -> unify (TyVar a) ty subst
      | TyFun(t1,t2), TyFun(t1', t2') -> let subst' = unify t1 t1' subst in
                                     let subst'' = unify (subst_apply t2 subst') (subst_apply t2' subst') subst' in
                                     subst''
      |_-> raise TypeError


let rec unifyall: typ_eqn -> subst -> subst 
= fun eqns subst ->
    match eqns with
      | [] -> subst
      | (t1,t2) :: tl -> let subst' = unify (subst_apply t1 subst) (subst_apply t2 subst) subst in
                         unifyall tl subst'



let solve : typ_eqn -> subst
=fun eqns -> unifyall eqns (subst_empty)                 

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty