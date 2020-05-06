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
    |CONST n -> [(ty, TyInt)]
    |VAR x -> [(ty, tenv_find tenv x)]
    |ADD (e1, e2) ->
      [(ty, TyInt)]@(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
    |SUB (e1, e2) ->
      [(ty, TyInt)]@(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
    |MUL (e1, e2) ->
      [(ty, TyInt)]@(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
    |DIV (e1, e2) ->
      [(ty, TyInt)]@(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
    |ISZERO e -> [(ty, TyBool)]@(gen_equations tenv e TyInt)
    |READ -> []
    |IF (e1, e2, e3) ->
      (gen_equations tenv e1 TyBool)@(gen_equations tenv e2 ty)@(gen_equations tenv e3 ty)
    |LET (x, e1, e2) ->
      let t1 = fresh_tyvar() in
      let tenv1 = tenv_extend (x, t1) tenv in
      (gen_equations tenv e1 t1)@(gen_equations tenv1 e2 ty)
    |LETREC (f, x, e1, e2) ->
      let t1 = fresh_tyvar() in
      let t2 = fresh_tyvar() in
      let tenv1 = tenv_extend (f, TyFun(t2, t1)) tenv in
      let tenv2 = tenv_extend (x,t2) tenv1 in
      (gen_equations tenv2 e1 t1)@(gen_equations tenv1 e2 ty)
    |PROC (x,e) ->
      let t1 = fresh_tyvar() in
      let t2 = fresh_tyvar() in
      let tenv1 = tenv_extend (x, t1) tenv in
      [(ty, TyFun(t1,t2))]@(gen_equations tenv1 e t2)
    |CALL (e1, e2) ->
      let t1 = fresh_tyvar() in
      (gen_equations tenv e2 t1)@(gen_equations tenv e1 (TyFun(t1,ty)))
    |_ -> raise TypeError


let rec checker : typ -> typ -> subst -> bool
= fun x f s ->
  if x = f then true
  else match f with
      |TyFun (t1, t2) ->
        if (t1 = x || t2 = x) then true
        else
          let a = subst_apply t1 s in
          let b = subst_apply t2 s in
          (checker x a s || checker x b s)
      |_ -> false
      
let rec conq : typ -> typ -> subst -> subst
= fun t1 t2 s ->
  (match (t1, t2) with
    |(TyInt, TyInt) -> s
    |(TyBool, TyBool) -> s
    |(TyVar a, t) ->
      if (checker (TyVar a) t s) then raise TypeError
      else (subst_extend a t s)
    |(t, TyVar a) -> conq (TyVar a) t s
    |(TyFun(e1, e2), TyFun(e3, e4)) ->
      let x = conq e1 e3 s in
      let v1 = subst_apply e2 x in
      let v2 = subst_apply e4 x in
      conq v1 v2 x
    |(_, _) -> raise TypeError)

let rec subsolve : typ_eqn -> subst -> subst
= fun eqns s ->
  match eqns with
    |[] -> s
    |hd::tl ->
      (match hd with
        |(t1, t2) ->
          let x1 = subst_apply t1 s in
          let x2 = subst_apply t2 s in
          let s1 = conq x1 x2 s in
          subsolve tl s1)

let solve : typ_eqn -> subst
=fun eqns -> subst_empty (* TODO *)

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty