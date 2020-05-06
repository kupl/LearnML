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
    | CONST num -> [(ty,TyInt)]
    | VAR var ->[(tenv_find tenv var,ty)]
    | ADD (e1,e2) -> 
      let n1 = [(ty,TyInt)] in
      let n2 = gen_equations tenv e1 TyInt in
      let n3 = gen_equations tenv e2 TyInt in
      n1 @ n2 @ n3
    | SUB (e1,e2) -> 
      let n1 = [(ty,TyInt)] in
      let n2 = gen_equations tenv e1 TyInt in
      let n3 = gen_equations tenv e2 TyInt in
      n1 @ n2 @ n3
    | MUL (e1,e2) ->
      let n1 = [(ty,TyInt)] in
      let n2 = gen_equations tenv e1 TyInt in
      let n3 = gen_equations tenv e2 TyInt in
      n1 @ n2 @ n3
    | DIV (e1,e2) ->
      let n1 = [(ty,TyInt)] in
      let n2 = gen_equations tenv e1 TyInt in
      let n3 = gen_equations tenv e2 TyInt in
      n1 @ n2 @ n3
    
    | ISZERO e1 ->
      let n = [(ty,TyBool)] in
      let n2 = gen_equations tenv e TyInt in
      n @ n2
    | IF (e1,e2,e3) ->
      let n = gen_equations  tenv e1 TyBool in
      let n2 = gen_equations tenv e2 ty in
      let n3 = gen_equations tenv e3 ty in
      n@n2@n3
    | LET (var,e1,e2) ->
      let alp = fresh_tyvar () in
      let n = gen_equations tenv e1 alp in
      let n2 = gen_equations ( tenv_extend(var,alp) tenv ) e2 ty in
      n@n2
    | LETREC (f, x, e1, e2) ->
      let ty1  = fresh_tyvar () in 
      let ty2 = fresh_tyvar () in 
      let tf  = TyFun (ty1, ty2) in 
      let tenv1  = tenv_extend (f, tf) tenv in  
      let tenv2 = tenv_extend (x, ty1) tenv1 in 
      let n1 = gen_equations tenv2 e1 ty2 in  
      let n2 = gen_equations tenv1  e2 ty in
         n1 @ n2
    | PROC (v,e) ->
      let alp1 = fresh_tyvar () in
      let alp2 = fresh_tyvar () in
      let n = [(ty,TyFun(alp1,alp2))] in
      let n2=  gen_equations (tenv_extend(v,alp1) tenv ) e alp2  in
      n@n2
    | CALL (e1,e2) ->
      let alp = fresh_tyvar() in
      let n = gen_equations tenv e1 ( TyFun(alp , ty) )in
      let n2 = gen_equations tenv e2 alp in
      n@n2


let rec solve: typ_eqn ->subst
=fun eqns -> 
 finsolve eqns subst_empty
   
  
   
    and rpmsolve p1 p2 sb = 
        match (p1,p2) with 
	   | (TyBool,TyBool)-> sb
       | (TyInt,TyInt) -> sb
       | (TyVar v,e)-> subst_extend v e sb
   
       | (ty, TyVar v) -> rpmsolve (TyVar v) ty sb 
       | (TyFun (e1,e2), TyFun(e3,e4)) -> 
            let newsb = rpmsolve e1 e3 sb in
            let newe3 = subst_apply e2 newsb in
            let newe4 = subst_apply e4 newsb in
            rpmsolve newe3 newe4 newsb
	and finsolve eqn sb =
        match eqn with
       | []-> sb
       | (e,ee) :: t ->
             let newsb =  rpmsolve (subst_apply e sb)  (subst_apply ee sb) sb
                 in finsolve t newsb

  
    
let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty