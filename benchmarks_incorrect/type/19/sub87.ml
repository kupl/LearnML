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
    | CONST a -> [ty,TyInt]
    | VAR v -> 
      let temp = tenv_find tenv v in
      [ty, temp]
    | ADD (a, b) -> [ty, TyInt]@(gen_equations tenv a TyInt)@(gen_equations tenv b TyInt)
    | SUB (a, b) -> [ty, TyInt]@(gen_equations tenv a TyInt)@(gen_equations tenv b TyInt)
    | MUL (a, b) -> [ty, TyInt]@(gen_equations tenv a TyInt)@(gen_equations tenv b TyInt)
    | DIV (a, b) -> [ty, TyInt]@(gen_equations tenv a TyInt)@(gen_equations tenv b TyInt)
    | ISZERO e -> [ty, TyBool]@(gen_equations tenv e TyInt)
    | READ -> [] 
    | IF (e1, e2, e3) -> (gen_equations tenv e1 TyBool)@(gen_equations tenv e2 ty)@(gen_equations tenv e3 ty)
    | PROC (x, e) -> 
      let typeX = fresh_tyvar() in
      let typeE = fresh_tyvar() in
      let newTEnv = tenv_extend (x,typeX) tenv in
      [ty, TyFun (typeX, typeE)]@(gen_equations newTEnv e typeE)
     | CALL (e1, e2) ->
      let new1 = fresh_tyvar() in
      (gen_equations tenv e1 (TyFun (new1, ty)))@(gen_equations tenv e2 new1)
    | LET (x, e1, e2) ->
      let new1 = fresh_tyvar() in
      let newTEnv = tenv_extend (x, new1) tenv in
      (gen_equations tenv e1 new1)@(gen_equations newTEnv e2 ty)
    | LETREC (x, f, e1, e2) ->
      let t1 = fresh_tyvar() in
      let t2 = fresh_tyvar() in
      let temp = TyFun (t2, t1) in 
      let newTEnv1 = tenv_extend (f, temp) tenv in
      let newTEnv2 = tenv_extend (x, t2) newTEnv1 in
      (gen_equations newTEnv2 e1 t1)@(gen_equations newTEnv1 e2 ty)

let solve : typ_eqn -> subst
=fun eqns ->
  
  let rec unify t1 t2 subs = 
    let temp1 = subst_apply t1 subs in
    let temp2 = subst_apply t2 subs in
    
      match (temp1, temp2) with
        | (TyInt, TyInt) -> subs
        | (TyBool, TyBool) -> subs
        | (TyInt, TyBool) -> raise TypeError
        | (TyBool, TyInt) -> raise TypeError
        | (TyFun (a1, a2), TyFun (b1, b2)) -> 
          if (a1 == b1)&&(a2 == b2) then subs
          else
            let tempS = unify a1 b1 subs in
            unify a2 b2 tempS
        | (TyInt, TyVar x) -> unify t2 t1 subs
        | (TyBool, TyVar x) -> unify t2 t1 subs
        | (TyFun (a,b), TyVar x) -> unify t2 t1 subs
        | (TyVar x, TyFun (a,b)) ->
          if (temp1 == a || temp1 == b) then raise TypeError
          else subst_extend x temp2 subs
        | (TyVar x, TyVar y) -> 
          subst_extend x temp2 subs
        | (TyVar x, TyInt) -> subst_extend x temp2 subs
        | (TyVar x, TyBool) -> subst_extend x temp2 subs
        | (TyFun (a1, a2), _) -> raise TypeError
        | (_, TyFun (a1, a2)) -> raise TypeError
    in
    
  let rec unifyAll eq subs =
    match eq with
      | (t1, t2)::tl ->
        let subs' = unify t1 t2 subs in
        unifyAll tl subs'
      | [] -> subs
    in
  
  let s = subst_empty in
  unifyAll eqns s
  

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty