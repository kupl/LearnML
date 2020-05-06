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
    | VAR x -> let real = tenv_find tenv x in [(ty, real)]
    | ADD (e1, e2) -> aoc_equation tenv (e1,e2) ty
    | SUB (e1, e2) -> aoc_equation tenv (e1,e2) ty
    | MUL (e1, e2) -> aoc_equation tenv (e1,e2) ty
    | DIV (e1, e2) -> aoc_equation tenv (e1,e2) ty
    | ISZERO e -> let body_eq = gen_equations tenv e TyInt
                  in (ty, TyBool)::body_eq
    | IF (condition, body_true, body_false) -> let condition_eq = gen_equations tenv condition TyBool
                                               and true_eq = gen_equations tenv body_true ty
                                               and false_eq = gen_equations tenv body_false ty
                                               in let body_eq = true_eq@false_eq
                                                  in condition_eq@body_eq
                                                  
    | LET (x, e1, e2) -> let t1 = fresh_tyvar ()
                         in let x_eq = gen_equations tenv e1 t1
                            in let tenv = tenv_extend (x, t1) tenv
                               in let body_eq = gen_equations tenv e2 ty
                                  in x_eq @ body_eq
    (*LETREC을 다시 작성해보자*)
    | LETREC (f, x, e1 ,e2) -> let t1 = fresh_tyvar ()
                               and t2 = fresh_tyvar ()
                               and ft = fresh_tyvar ()
                               in let main_type = TyFun (t1, t2)
                                  in let f_eq = [(ft, main_type)]
                                     in let tenv = tenv_extend (f, main_type) tenv
                                        in let x_eq = gen_equations tenv e2 t1
                                           in let tenv = tenv_extend (x, t1) tenv
                                              in let body_eq = gen_equations tenv e1 t2
                                                 in let total_eq = [(ty, t2)]
                                                    in let partial_eq1 = total_eq @ f_eq
                                                       in let partial_eq2 = partial_eq1 @ x_eq
                                                          in partial_eq2 @ body_eq
    | PROC (x,e) -> let t1 = fresh_tyvar ()
                    and t2 = fresh_tyvar ()
                    in let main_eq = [(ty,(TyFun (t1,t2)))]
                       in let tenv = tenv_extend (x, t1) tenv
                          in let body_eq = gen_equations tenv e t2
                             in main_eq @ body_eq
                    
    | CALL (e1, e2) -> let x = fresh_tyvar ()
                       in let type_1 = TyFun (x, ty)
                          in let e1_eq = gen_equations tenv e1 type_1
                             in let e2_eq = gen_equations tenv e2 x
                                in e1_eq @ e2_eq
                       
    | READ -> [(ty, TyInt)]

and aoc_equation tenv (e1,e2) ty = 
  let e1_eq = gen_equations tenv e1 TyInt
  in let e2_eq = gen_equations tenv e2 TyInt
     in let sub_eq = e1_eq @ e2_eq
        in (ty, TyInt)::sub_eq;;
        
(*---------------------------------------------------------------------------------------------------*)

let rec replace_all prv_subst subst cnt length = 
  match prv_subst with
    | [] -> if cnt = length then subst else replace_all subst subst_empty (cnt+1) length
  
    | (tv, ty)::tl ->  let new_subst = replace_inner tv ty tl subst_empty
                       in replace_all new_subst (subst_extend tv ty subst) cnt length

and replace_inner main_tv main_ty prv_subst subst = 
  match prv_subst with
    | [] -> subst
    | (tv, ty)::tl -> let new_ty = replace_single main_tv main_ty ty
                     in let new_subst = subst_extend tv new_ty subst
                        in replace_inner main_tv main_ty tl new_subst

and replace_single main_tv main_ty typ = 
  match typ with
    | TyInt | TyBool -> typ
    | TyVar x -> if x = main_tv then main_ty else typ
    | TyFun(t1, t2) -> let new_t1 = replace_single main_tv main_ty t1
                       and new_t2 = replace_single main_tv main_ty t2
                       in TyFun (new_t1, new_t2)

and is_occurence tv ty =
  match ty with
    | TyInt -> false
    | TyBool -> false
    | TyVar x -> if tv = x then true else false
    | TyFun (t1, t2) -> (is_occurence tv t1) || (is_occurence tv t2)
    
(*-----------------------------------------------------------------------*)

and unify typ1 typ2 subst = 
  match typ1, typ2 with
    | TyInt, TyInt -> subst
    | TyBool, TyBool -> subst
    | TyVar x, TyVar y -> if x = y then subst
                          else let new_subst = subst_extend x typ2 subst
                               in replace_all new_subst subst_empty 0 (List.length new_subst)
    
    | TyVar x, _ -> if (is_occurence x typ2) = true then raise TypeError
                    else let new_subst = subst_extend x typ2 subst
                         in replace_all new_subst subst_empty 0 (List.length new_subst)
    | _, TyVar x -> unify typ2 typ1 subst
    
    | TyFun(t1_1, t2_1), TyFun(t1_2, t2_2) -> let subst1 = unify t1_1 t1_2 subst
                                              in let subst2 = unify (subst_apply t2_1 subst1) (subst_apply t2_2 subst1) subst1
                                                 in subst2
    | _, _ -> raise TypeError

(*-----------------------------------------------------------------------*)

and unifyall equation subst = 
  match equation with
    | [] -> subst
    | (tv, ty)::tl -> let new_subst = unify (subst_apply tv subst) (subst_apply ty subst) subst
                      in unifyall tl new_subst

(*--------------------------------------------------------------------*)

let solve : typ_eqn -> subst = 
fun eqns -> unifyall eqns subst_empty
  
(*--------------------------------------------------------------------*)

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty