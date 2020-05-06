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
    | CONST i -> [(ty, TyInt)]
    | VAR v -> 
      let a = tenv_find tenv v in
      [(ty, a)]
    | ADD (e1, e2) ->
      let ty_eqn1 = gen_equations tenv e1 TyInt in
      let ty_eqn2 = gen_equations tenv e2 TyInt in
      ty_eqn1 @ ty_eqn2 @ ( [(ty,TyInt)] )
    | SUB (e1, e2) ->
      let ty_eqn1 = gen_equations tenv e1 TyInt in
      let ty_eqn2 = gen_equations tenv e2 TyInt in
      ty_eqn1 @ ty_eqn2 @ ( [(ty,TyInt)] )
    | MUL (e1, e2) ->
      let ty_eqn1 = gen_equations tenv e1 TyInt in
      let ty_eqn2 = gen_equations tenv e2 TyInt in
      ty_eqn1 @ ty_eqn2 @ ( [(ty,TyInt)] )
    | DIV (e1, e2) ->
      let ty_eqn1 = gen_equations tenv e1 TyInt in
      let ty_eqn2 = gen_equations tenv e2 TyInt in
      ty_eqn1 @ ty_eqn2 @ ( [(ty,TyInt)] )
    | ISZERO e1 ->  
      let ty_eqn1 = gen_equations tenv e1 TyInt in
      ty_eqn1 @ [ty, TyBool]
    | IF (e1, e2, e3) ->
      let newT1 = fresh_tyvar() in
      let ty_eqn1 = gen_equations tenv e1 TyBool in
      let ty_eqn2 = gen_equations tenv e2 newT1 in
      let ty_eqn3 = gen_equations tenv e3 newT1 in
      ty_eqn1 @ ty_eqn2 @ ty_eqn3 @ [(ty, newT1)]
    | READ ->
      [(ty, TyInt)]
    | LET (v1, e1, e2) ->
      let newT1 = fresh_tyvar() in
      let ty_eqn1 = gen_equations tenv e1 newT1 in
      let tenv' = tenv_extend (v1, newT1) tenv in
      let ty_eqn2 = gen_equations tenv' e2 ty in
      ty_eqn1 @ ty_eqn2
    | LETREC (v1, v2, e1, e2) ->
      let newT1 = fresh_tyvar() in
      let tenv2' = tenv_extend (v1, TyFun(newT1, newT1)) tenv in
      let ty_eqn1 = gen_equations tenv2' (PROC (v2,e1)) (TyFun (newT1,newT1)) in
      let ty_eqn2 = gen_equations tenv2' e2 ty in
      ty_eqn1 @ ty_eqn2
    | PROC (v1, e1) ->
      let newT1 = fresh_tyvar() in
      let newT2 = fresh_tyvar() in
      let tenv' = tenv_extend (v1, newT1) tenv in
      let ty_enq1 = gen_equations tenv' e1 newT2 in
      [(ty, TyFun (newT1, newT2))] @ ty_enq1
    | CALL (e1, e2) ->
      let newT1 = fresh_tyvar() in
      let ty_enq1 = gen_equations tenv e1 (TyFun (newT1, ty)) in
      let ty_enq2 = gen_equations tenv e2 newT1 in
      ty_enq1 @ ty_enq2
  

let solve : typ_eqn -> subst
=fun eqns -> 
  let emptysub = [] in
  let simplify longeq lst = 
    match longeq with
      | (TyFun (ty1, ty2), TyFun (ty3, ty4) ) -> (ty1, ty3)::(ty2, ty4)::lst
      | _ -> longeq::lst
  in
  let rec simplifyeqn eqn lst =
    match eqn with
      | [] -> lst
      | hd::tl -> simplifyeqn tl (simplify hd lst)
  in
  let eqns = simplifyeqn eqns [] in
  let rec f eq subst =
    match eq with
      | [] -> subst
      | (ty1, ty2) ::tl ->  
        let ty1 = subst_apply ty1 subst in
        let ty2 = subst_apply ty2 subst in
        let tylst = simplify (ty1, ty2) [] in
        if tylst = [(ty1,ty2)] then 
          if ty1 == ty2 then f tl subst else
            let v1, ty2 = 
            (match ty1 with
              | TyVar v1 -> v1, ty2
              | _ -> 
                (match ty2 with
                  | TyVar v2 -> v2, ty1
                  | _ ->
                    raise (TypeError)
                )
            ) in
          let subst' = subst_extend v1 ty2 subst in
          f tl subst'
        else
          f (tl@tylst) subst
  in
  f eqns emptysub

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty