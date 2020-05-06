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
    | VAR v -> [(ty, (tenv_find tenv v))]
    | ADD (a,b) -> (ty, TyInt)::(gen_equations tenv a TyInt)@(gen_equations tenv b TyInt)
    | SUB (a,b) -> (ty, TyInt)::(gen_equations tenv a TyInt)@(gen_equations tenv b TyInt)
    | MUL (a,b) -> (ty, TyInt)::(gen_equations tenv a TyInt)@(gen_equations tenv b TyInt)
    | DIV (a,b) -> (ty, TyInt)::(gen_equations tenv a TyInt)@(gen_equations tenv b TyInt)
    | ISZERO b -> [(ty, TyBool)]@(gen_equations tenv b TyInt)
    | READ -> [(ty, TyInt)](*질문*)
    | IF (e1,e2,e3) -> (gen_equations tenv e1 TyBool)@(gen_equations tenv e2 ty)@(gen_equations tenv e3 ty)
    | LET (v,e1,e2) -> 
      let tyv = fresh_tyvar () in
      let tenv2 = tenv_extend (v,tyv) tenv in
      (gen_equations tenv e1 tyv)@(gen_equations tenv2 e2 ty)
    | LETREC (f,x,e1,e2) -> 
      let tyx = fresh_tyvar () in
      let tye1 = fresh_tyvar () in
      let tyf = TyFun(tyx,tye1) in
      let tenvf = tenv_extend (f,tyf) tenv in
      let tenvx = tenv_extend (x,tyx) tenvf in
      (gen_equations tenvx e1 tye1)@(gen_equations tenvf e2 ty)
      (*질문
      of var * var * exp * exp*)
    | PROC (x,e) ->
      let tyx = fresh_tyvar () in
      let ty' = fresh_tyvar () in
      let tenv2 = tenv_extend (x,tyx) tenv in
        (ty,TyFun(tyx,ty'))::(gen_equations tenv2 e ty')
    | CALL (e1,e2) ->
      let tye2 = fresh_tyvar () in
      (gen_equations tenv e1 (TyFun(tye2,ty)))@(gen_equations tenv e2 tye2)
    
(*        type typ = TyInt | TyBool | TyFun of typ * typ | TyVar of tyvar
        and tyvar = string
        type typ_eqn = (typ * typ) list*)

let solve : typ_eqn -> subst
=fun eqns -> 
  let rec unify : typ -> typ -> subst -> subst
  = fun t1 t2 subst ->
    match (t1,t2) with
      |TyInt, TyInt -> subst
      |TyBool, TyBool -> subst
      |TyVar a, TyVar b -> if(a=b) then subst else subst_extend a t2 subst
      |TyVar a,_ ->
        let rec occur_check : typ -> typ -> bool
          = fun a b ->
            (match b with
              |TyFun(t1,t2) -> (occur_check a t1) || (occur_check a t2)
              |_ -> if (a=b) then true else false) in
        if(occur_check t1 t2) then raise TypeError else subst_extend a t2 subst
      |_,TyVar a -> unify t2 t1 subst
      |TyFun(a1,b1), TyFun(a2,b2) ->
        let s = unify a1 a2 subst in
        let s' = unify (subst_apply b1 s) (subst_apply b2 s) s in
        s'
      |_ -> raise TypeError in
  let rec unify_all : typ_eqn -> subst -> subst
  = fun eqns subst ->
    match eqns with
      |[] -> subst
      |(t1,t2)::tl ->
        let s = unify (subst_apply t1 subst) (subst_apply t2 subst) subst in
        unify_all tl s in
  unify_all eqns subst_empty

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty