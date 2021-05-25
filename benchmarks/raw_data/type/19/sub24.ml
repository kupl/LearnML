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
  | CONST x -> [(ty, TyInt)]
  | VAR v -> [(ty,(tenv_find tenv v))]
  | ADD (a,b) -> [(ty,TyInt)]@(gen_equations tenv a TyInt)@(gen_equations tenv b TyInt)
  | SUB (a,b) -> [(ty,TyInt)]@(gen_equations tenv a TyInt)@(gen_equations tenv b TyInt)
  | MUL (a,b) -> [(ty,TyInt)]@(gen_equations tenv a TyInt)@(gen_equations tenv b TyInt)
  | DIV (a,b) -> [(ty,TyInt)]@(gen_equations tenv a TyInt)@(gen_equations tenv b TyInt)
  | READ -> [(ty, TyInt)]
  | ISZERO e -> [(ty,TyBool)]@(gen_equations tenv e TyInt)
  | IF (e1,e2,e3) -> (gen_equations tenv e1 TyBool)@(gen_equations tenv e2 ty)@(gen_equations tenv e3 ty)
  | LET (x,e1,e2) -> 
    let ty_1 = fresh_tyvar () in
    (gen_equations tenv e1 ty_1)@(gen_equations (tenv_extend (x,ty_1) tenv) e2 ty)
  | LETREC (f,x,e1,e2) ->
    let tx  = fresh_tyvar ()    in (** type of x   **)
     let tfx = fresh_tyvar ()    in (** type of f x **)
     let tf  = TyFun (tx, tfx) in (** type of f   **)
     let tenvf  = tenv_extend (f, tf) tenv  in (** f in env **) 
     let tenvxf = tenv_extend (x, tx) tenvf in (** x and f in env **) 
     let eq1 = gen_equations tenvxf e1 tfx in (** type e1 = type (f x) **)
     let eq2 = gen_equations tenvf  e2 ty  in (** type e2 = typ **)
     eq1 @ eq2
  | PROC (x,e) -> 
    let ty_1 = fresh_tyvar () in (*TyVar ("t"^x)*)
    let ty_2 = fresh_tyvar () in
    [(ty,TyFun (ty_1,ty_2))]@(gen_equations (tenv_extend (x,ty_1) tenv) e ty_2)
  | CALL (e1,e2) ->
    let ty_1 = fresh_tyvar () in
    (gen_equations tenv e1 (TyFun (ty_1,ty)))@(gen_equations tenv e2 ty_1)


let solve : typ_eqn -> subst
=fun eqns -> 
  List.fold_left (
    let rec f : subst -> (typ*typ) -> subst = fun a b -> let (ty_1,ty_2) = b in
    let k1 = (subst_apply ty_1 a) in
    let k2 = (subst_apply ty_2 a) in
    match k1 with
    | TyVar k -> subst_extend k k2 a
    | TyFun (k1_1,k1_2) -> 
      (
      match k2 with
      | TyFun (k2_1,k2_2) ->
        let k1_1 = (subst_apply k1_1 a) in
        let k1_2 = (subst_apply k1_2 a) in
        let k2_1 = (subst_apply k2_1 a) in
        let k2_2 = (subst_apply k2_2 a) in
        f (f a (k1_1,k2_1)) (k1_2,k2_2)
      | _ -> raise TypeError
      )
    | _ -> 
      match k2 with
      | TyVar k -> subst_extend k k1 a
      | _ -> if k1=k2 then a else raise TypeError
    in f
  ) subst_empty eqns
  
  

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty