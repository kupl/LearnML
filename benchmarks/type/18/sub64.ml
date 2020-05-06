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
  | VAR v -> [(ty, (tenv_find tenv v))]
  | ADD (exp1, exp2) -> let eqn1 = gen_equations tenv exp1 TyInt in
  let eqn2 = gen_equations tenv exp2 TyInt in
  (((ty, TyInt)::eqn1)@eqn2)
  | SUB (exp1, exp2) -> let eqn1 = gen_equations tenv exp1 TyInt in
  let eqn2 = gen_equations tenv exp2 TyInt in
  (((ty, TyInt)::eqn1)@eqn2)
  | MUL (exp1, exp2) -> let eqn1 = gen_equations tenv exp1 TyInt in
  let eqn2 = gen_equations tenv exp2 TyInt in
  (((ty, TyInt)::eqn1)@eqn2)
  | DIV (exp1, exp2) -> let eqn1 = gen_equations tenv exp1 TyInt in
  let eqn2 = gen_equations tenv exp2 TyInt in
  (((ty, TyInt)::eqn1)@eqn2)
  | ISZERO n -> let eqn = gen_equations tenv n TyInt in
  ((ty, TyBool)::eqn)
  | READ -> []
  | IF (exp1, exp2, exp3) -> let eqn1 = gen_equations tenv exp1 TyBool in
  let eqn2 = gen_equations tenv exp2 ty in
  let eqn3 = gen_equations tenv exp3 ty in
  ((eqn1@eqn2)@eqn3)
  | LET (x, exp1, exp2) -> let alpha = fresh_tyvar () in
  let eqn1 = gen_equations tenv exp1 alpha in
  let eqn2 = gen_equations (tenv_extend (x, alpha) tenv) exp2 ty in
  (eqn1@eqn2)
  | LETREC (f, x, exp1, exp2) -> let alpha = fresh_tyvar () in
  let betha = fresh_tyvar () in
  let eqn1 = gen_equations tenv (VAR x) alpha in
  let tenv1 = tenv_extend (x, alpha) tenv in
  let eqn2 = gen_equations tenv1 exp1 betha in
  let tenv2 = tenv_extend (f, (TyFun (alpha, betha))) tenv1 in
  let eqn3 = gen_equations tenv2 exp2 ty in
  ((eqn1@eqn2)@eqn3)
  | PROC (x, exp1) -> let alpha = fresh_tyvar () in
  let betha = fresh_tyvar () in
  let eqn = gen_equations (tenv_extend (x, alpha) tenv) exp1 betha in
  ((ty, TyFun(alpha, betha))::eqn)
  | CALL (exp1, exp2) -> let alpha = fresh_tyvar () in
  let eqn1 = gen_equations tenv exp1 (TyFun(alpha, ty)) in
  let eqn2 = gen_equations tenv exp2 alpha in
  (eqn1@eqn2)

let solve : typ_eqn -> subst
=fun eqns -> let rec unify : typ -> typ -> subst -> subst
=fun typ1 typ2 ss -> match (typ1, typ2) with
  | (TyInt, TyInt) -> ss
  | (TyBool, TyBool) -> ss
  | (TyVar t1, t2) -> if (t2=TyVar t1) then ss else
    let rec occur : tyvar -> typ -> bool = fun tv e -> begin match e with
      | TyVar t -> (t=tv)
      | TyFun (h, t) -> if ((occur tv h)=true || (occur tv t)=true) then true else false
      | _ -> false end
      in if ((occur t1 t2)=true) then raise TypeError else subst_extend t1 t2 ss
  | (t1, TyVar t2) -> unify typ2 typ1 ss
  | (TyFun (t1, t2), TyFun (t1p, t2p)) -> let sp = unify t1 t1p ss in
  let ssp = unify (subst_apply t2 sp) (subst_apply t2p sp) sp in ssp
  | _ -> raise TypeError
  in let rec unifyall : typ_eqn -> subst -> subst
  =fun typeq s -> begin match typeq with
    | [] -> s
    | hd::u -> begin match hd with
      | (t1, t2) -> let sp = unify (subst_apply t1 s) (subst_apply t2 s) s in unifyall u sp
      | _ -> raise TypeError end end
      in unifyall eqns subst_empty

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty