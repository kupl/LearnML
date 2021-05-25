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
    | VAR x -> [(ty, (tenv x))]
    | ADD (e1,e2) -> [(ty,TyInt)]@(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
    | SUB (e1,e2) -> [(ty,TyInt)]@(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
    | MUL (e1,e2) -> [(ty,TyInt)]@(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
    | DIV (e1,e2) -> [(ty,TyInt)]@(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
    | ISZERO ne -> [(ty, TyBool)]@(gen_equations tenv ne TyInt)
    | READ -> [(ty, TyInt)]
    | IF (e1,e2,e3) -> (gen_equations tenv e1 TyBool)@(gen_equations tenv e2 ty)@(gen_equations tenv e3 ty)
    | LET (x, e1, e2) -> (let alp = fresh_tyvar() 
                          in (gen_equations tenv e1 alp)@(gen_equations (tenv_extend (x,alp) tenv) e2 ty))
    | LETREC (f,x,e1,e2) -> (let alp1 = fresh_tyvar() 
                              in let alp2 = fresh_tyvar() 
                              in (gen_equations (tenv_extend (f, TyFun(alp2, alp1)) (tenv_extend (x,alp2) tenv)) e1 alp1)@(gen_equations (tenv_extend (f, TyFun(alp2, alp1)) tenv) e2 ty))
    | CALL (e1,e2) -> (let alp = fresh_tyvar() 
                        in (gen_equations tenv e1 (TyFun(alp,ty)))@(gen_equations tenv e2 alp))
    | PROC (x,e) -> (let alp1 = fresh_tyvar() and alp2 = fresh_tyvar() 
                      in [(ty, TyFun(alp1, alp2))]@(gen_equations (tenv_extend (x,alp1) tenv) e alp2))

let solve : typ_eqn -> subst
=fun eqns -> 
  let rec occurcheck : typ -> typ -> bool
  = fun t1 t2 ->
    match t2 with
      | TyFun (a,b) -> if(a=t1 || b=t1) then true else (occurcheck t1 a) || (occurcheck t1 b)
      | _ -> false
  in let rec unify : typ * typ -> subst -> subst
  = fun (t1,t2) s ->
    match (t1,t2) with
      | (TyInt, TyInt) -> s
      | (TyBool, TyBool) -> s
      | (TyInt, TyBool) -> raise TypeError
      | (TyBool, TyInt) -> raise TypeError
      | (TyVar a, TyVar b) -> if a=b then s else (if (occurcheck (TyVar a) (TyVar b)) then raise TypeError else (subst_extend a (TyVar b) s))
      | (TyVar alp, t) -> if (occurcheck (TyVar alp) t) then raise TypeError else (subst_extend alp t s)
      | (t, TyVar alp) -> unify ((TyVar alp), t) s
      | (TyFun(t1, t2), TyFun(t1p, t2p)) -> (let sp = (unify (t1, t1p) s) in (unify ((subst_apply t2 sp),(subst_apply t2p sp)) sp))
      | _ -> raise TypeError
  in let rec unifyall : typ_eqn -> subst -> subst
  = fun eqn s ->
    match eqn with
      | [] -> s
      | (t1,t2)::u -> (let sp = (unify ((subst_apply t1 s),(subst_apply t2 s)) s)
                        in (unifyall u sp))
  in unifyall eqns subst_empty

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty