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
  | VAR x -> let xty = tenv_find tenv x in [(ty, xty)]
  | ADD (e1, e2) ->  let eqn = [(ty, TyInt)] and
    eqn1 = gen_equations tenv e1 TyInt and
    eqn2 = gen_equations tenv e2 TyInt in
    eqn @ eqn1 @ eqn2
  | SUB (e1, e2) -> let eqn = [(ty, TyInt)] and
    eqn1 = gen_equations tenv e1 TyInt and
    eqn2 = gen_equations tenv e2 TyInt in
    eqn @ eqn1 @ eqn2
  | MUL (e1, e2) -> let eqn = [(ty, TyInt)] and
    eqn1 = gen_equations tenv e1 TyInt and
    eqn2 = gen_equations tenv e2 TyInt in
    eqn @ eqn1 @ eqn2
  | DIV (e1, e2) -> let eqn = [(ty, TyInt)] and
    eqn1 = gen_equations tenv e1 TyInt and
    eqn2 = gen_equations tenv e2 TyInt in
    eqn @ eqn1 @ eqn2
  | ISZERO e -> let eqn = [(ty, TyBool)] and eqn1 = gen_equations tenv e TyInt in
    eqn @ eqn1
  | READ -> [(ty, TyInt)]
  | IF (e1, e2, e3) -> let eqn = gen_equations tenv e1 TyBool and
    eqn1 = gen_equations tenv e2 ty and eqn2 = gen_equations tenv e3 ty in
    eqn @ eqn1 @ eqn2
  | LET (x, e1, e2) -> let new_tyvar = fresh_tyvar() in
    let xty = tenv_extend (x, new_tyvar) tenv in
    let eqn1 = gen_equations tenv e1 new_tyvar in
    let eqn2 = gen_equations xty e2 ty in
    eqn1 @ eqn2
  | LETREC (f, x, e1, e2) -> let new_tyvar1 = fresh_tyvar() and new_tyvar2 = fresh_tyvar() and
    new_tyvar3 = fresh_tyvar() in
    let ft = tenv_extend (f, TyFun(new_tyvar2, new_tyvar1)) tenv in
    let xt = tenv_extend (x, new_tyvar2) ft in
    let eqn = [(TyVar f, TyFun(new_tyvar2, new_tyvar1));(TyVar x, new_tyvar2);(ty, new_tyvar3)] in
    let eqn1 = gen_equations xt e1 new_tyvar1 in
    let eqn2 = gen_equations ft e2 new_tyvar3 in
    eqn @ eqn1 @ eqn2
  | PROC (x, e) -> let new_tyvar1 = fresh_tyvar() in
    let new_tyvar2 = fresh_tyvar() in
    let xt = tenv_extend (x, new_tyvar1) tenv in
    let eqn = [(ty, TyFun(new_tyvar1, new_tyvar2))] in
    let eqn1 = gen_equations xt e new_tyvar2 in
    eqn @ eqn1
  | CALL (e1, e2) -> let new_tyvar = fresh_tyvar() in
    let eqn1 = gen_equations tenv e1 (TyFun(new_tyvar, ty)) in
    let eqn2 = gen_equations tenv e2 new_tyvar in
    eqn1 @ eqn2
    
let rec check : string -> typ -> bool
= fun a ty -> match ty with
  | TyVar x -> if a = x then false else true
  | TyFun (ty1, ty2) -> (check a ty1) && (check a ty2)
  | _ -> true
  
and set_tyvar : typ -> typ -> subst -> subst
=fun t1 t2 subst -> if t1 = t2 then subst else
  match t1, t2 with
    | TyVar a, ty -> if (check a ty) then subst_extend a ty subst else raise TypeError
    | ty, TyVar a -> set_tyvar (TyVar a) ty subst
    | TyFun(ty1, ty2), TyFun(ty3, ty4) -> let s = set_tyvar ty1 ty3 subst in 
      let s1 = subst_apply ty2 s and s2 = subst_apply ty4 s in set_tyvar s1 s2 s 
    | _ -> raise TypeError 

and solve_list : typ_eqn -> subst -> subst
= fun eqns t-> match eqns with
  | [] -> t
  | (t1,t2)::tl -> let s1 = subst_apply t1 t and s2 = subst_apply t2 t in
    let s = set_tyvar s1 s2 t in solve_list tl s

let solve : typ_eqn -> subst
=fun eqns -> match eqns with
  | [] -> subst_empty
  | _ -> solve_list eqns []

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty