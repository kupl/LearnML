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
=fun tenv e ty ->  (* TODO *)
match e with 
  | CONST i -> [(ty, TyInt)]
    
  | VAR v -> [(ty, (tenv_find tenv v))]
  | READ -> [(ty, TyVar "readtype")]
  | ADD (e1, e2) -> 
    let typ_eqn1 = gen_equations tenv e1 TyInt in
    let typ_eqn2 = gen_equations tenv e2 TyInt in
    [(ty, TyInt)] @ typ_eqn1 @ typ_eqn2
  | SUB (e1, e2) ->
    let typ_eqn1 = gen_equations tenv e1 TyInt in
    let typ_eqn2 = gen_equations tenv e2 TyInt in
    [(ty, TyInt)] @ typ_eqn1 @ typ_eqn2
  | MUL (e1, e2) ->
    let typ_eqn1 = gen_equations tenv e1 TyInt in
    let typ_eqn2 = gen_equations tenv e2 TyInt in
    [(ty, TyInt)] @ typ_eqn1 @ typ_eqn2
  | DIV (e1, e2) ->
    let typ_eqn1 = gen_equations tenv e1 TyInt in
    let typ_eqn2 = gen_equations tenv e2 TyInt in
    [(ty, TyInt)] @ typ_eqn1 @ typ_eqn2
  | ISZERO e1 -> 
    let typ_eqn1 = gen_equations tenv e1 TyInt in
    [(ty, TyBool)] @ typ_eqn1
  | IF (e1, e2, e3) ->
    let typ_eqn1 = gen_equations tenv e1 TyBool in
    let typ_eqn2 = gen_equations tenv e2 ty in
    let typ_eqn3 = gen_equations tenv e3 ty in
    typ_eqn1 @ typ_eqn2 @ typ_eqn3
  | LET (v1, e1, e2) -> 
    let ty1 = fresh_tyvar () in
    let typ_eqn1 = gen_equations tenv e1 ty1 in
    let new_tenv = (tenv_extend (v1, ty1) tenv) in
    let typ_eqn2 = gen_equations new_tenv e2 ty in
    typ_eqn1 @ typ_eqn2
  | LETREC (v1, v2, e1, e2) -> 
    let ty1 = fresh_tyvar () in
    let ty2 = fresh_tyvar () in 
    let ty3 = fresh_tyvar () in
	let tenv1 = (tenv_extend (v1, ty1) tenv) in
    let tenv2 = (tenv_extend (v2, ty2) tenv1) in	
    let typ_eqn1 = gen_equations tenv2 e1 ty3 in
    let typ_eqn2 = gen_equations tenv1 e2 ty in
    [(ty1, TyFun(ty2, ty3))] @ typ_eqn1 @ typ_eqn2 
  | PROC (v1, e1) ->
    let ty1 = fresh_tyvar () in
    let ty2 = fresh_tyvar () in 
    let tenv1 = (tenv_extend (v1, ty1) tenv) in
    let typ_eqn1 = gen_equations tenv1 e1 ty2 in
    [(ty, TyFun(ty1, ty2))] @ typ_eqn1
  | CALL(e1, e2) ->
    let ty1 = fresh_tyvar () in
    let typ_eqn1 = gen_equations tenv e1 (TyFun(ty1, ty)) in
    let typ_eqn2 = gen_equations tenv e2 ty1 in
    typ_eqn1 @ typ_eqn2

(*for solve*)

let rec findtype = fun typ1 typ2 ->
  match typ1 with 
    | TyFun (t1, t2) -> (findtype t1 typ2) || (findtype t2 typ2)
    | typ -> if typ = typ2 then true else false 
        
let solve : typ_eqn -> subst
=fun eqns -> (* TODO *)
let sub = [] in
let rec recsolve = fun eqns sub ->
  match eqns with
    | [] -> sub 
    | hd::tl ->
      (match hd with
        | (typ1, typ2) -> 
          (match ((subst_apply typ1 sub), (subst_apply typ2 sub)) with
            | (TyInt, TyInt) -> recsolve tl sub
            | (TyBool, TyBool) -> recsolve tl sub
            | (TyVar v1, TyVar v2) -> 
              if (v1 = v2) then recsolve tl sub else recsolve tl (subst_extend (v1) (TyVar v2) sub)
            | (TyInt, TyBool) -> raise TypeError
            | (TyBool, TyInt) -> raise TypeError
            | (TyVar v, typ) ->  
              if (findtype typ (TyVar v)) then raise TypeError else recsolve tl (subst_extend (v) typ sub) 
            | (typ, TyVar v) -> recsolve ((typ2, typ1)::tl) sub
            | (TyFun(t1, t2), TyFun(t3, t4)) -> recsolve ([(t1, t3);(t2, t4)] @ tl) sub
            |_ -> raise (Failure "equation error")
           )
        )
in let sol = recsolve eqns sub
in sol


let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty