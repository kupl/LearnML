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
    | CONST _ -> [(ty, TyInt)]
    | VAR x -> [(ty, tenv_find tenv x)]
    | ADD (a, b) -> [(ty, TyInt)] @ (gen_equations tenv a TyInt) @ (gen_equations tenv b TyInt)
    | SUB (a, b) -> [(ty, TyInt)] @ (gen_equations tenv a TyInt) @ (gen_equations tenv b TyInt)
    | MUL (a, b) -> [(ty, TyInt)] @ (gen_equations tenv a TyInt) @ (gen_equations tenv b TyInt)
    | DIV (a, b) -> [(ty, TyInt)] @ (gen_equations tenv a TyInt) @ (gen_equations tenv b TyInt)
    | ISZERO n -> [(ty, TyBool)] @ (gen_equations tenv n TyInt)
    | READ -> []
    | IF (e1, e2, e3) -> (gen_equations tenv e1 TyBool) @ (gen_equations tenv e2 ty) @ (gen_equations tenv e3 ty)
    | LET (v, e1, e2) -> (let new_tv = fresh_tyvar () in
                            (gen_equations tenv e1 new_tv) @ (gen_equations (tenv_extend (v, new_tv) tenv) e2 ty))
    | LETREC (v1, v2, e1, e2) -> (let new_tv1 = fresh_tyvar () in
                                  let new_tv2 = fresh_tyvar () in
                                    let new_tenv = tenv_extend (v1, TyFun (new_tv1, new_tv2)) tenv in
                                      (gen_equations (tenv_extend (v2, new_tv1) new_tenv) e1 new_tv2) @ (gen_equations new_tenv e2 ty))
    | PROC (x, e1) -> (let new_tv1 = fresh_tyvar () in
                        let new_tv2 = fresh_tyvar () in
                          [(ty, TyFun (new_tv1, new_tv2))] @ (gen_equations (tenv_extend (x, new_tv1) tenv) e1 new_tv2))
    | CALL (e1, e2) -> let new_tv = fresh_tyvar () in
                        (gen_equations tenv e1 (TyFun (new_tv, ty))) @ (gen_equations tenv e2 new_tv)
                        
let rec occur_check
= fun tv ty ->
  match ty with
    | TyVar _ -> tv = ty
    | TyFun (tf1, tf2) -> occur_check tv tf1 || occur_check tv tf2
    | _ -> false

let solve : typ_eqn -> subst
=fun eqns ->
  let rec solve_loop
  = fun eq subst ->
      match eq with
        | [] -> subst
        | (ty1, ty2)::tl -> (let tya1 = subst_apply ty1 subst in
                              let tya2 = subst_apply ty2 subst in
                                match tya1, tya2 with
                                  | TyInt, TyInt -> solve_loop tl subst
                                  | TyInt, TyVar _ -> solve_loop ((ty2, ty1)::tl) subst
                                  | TyBool, TyBool -> solve_loop tl subst
                                  | TyBool, TyVar _ -> solve_loop ((ty2, ty1)::tl) subst
                                  | TyVar tv1, TyVar tv2 -> if tv1 = tv2 then solve_loop tl subst else solve_loop tl (subst_extend tv1 tya2 subst)
                                  | TyVar tv, TyFun _ -> if occur_check tya1 tya2 then raise TypeError else solve_loop tl (subst_extend tv tya2 subst)
                                  | TyVar tv, _ -> solve_loop tl (subst_extend tv tya2 subst)
                                  | TyFun _, TyVar _ -> solve_loop ((ty2, ty1)::tl) subst
                                  | TyFun (tf1, tf2), TyFun (tf3, tf4) -> (let subst1 = solve_loop [(tf1, tf3)] subst in
                                                                            solve_loop ((tf2, tf4)::tl) subst1)
                                  | _, _ -> raise TypeError)
  in solve_loop eqns subst_empty

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty