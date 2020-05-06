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
exception OccurrenceCheckError

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



(* Generate type equations (V-algorithm). *)
let rec gen_equations : tenv -> exp -> typ -> typ_eqn 
= fun tenv e ty ->
  match e with
  | CONST n -> [(ty, TyInt)]
  | VAR x -> [(ty, tenv_find tenv x)]
  | ADD (e1, e2) -> let eq1 = [(ty, TyInt)] in
                      let eq2 = gen_equations tenv e1 TyInt in
                        let eq3 = gen_equations tenv e2 TyInt in
                          eq1 @ eq2 @ eq3
  | SUB (e1, e2) -> let eq1 = [(ty, TyInt)] in
                      let eq2 = gen_equations tenv e1 TyInt in
                        let eq3 = gen_equations tenv e2 TyInt in
                          eq1 @ eq2 @ eq3
  | DIV (e1, e2) -> let eq1 = [(ty, TyInt)] in
                      let eq2 = gen_equations tenv e1 TyInt in
                        let eq3 = gen_equations tenv e2 TyInt in
                          eq1 @ eq2 @ eq3
  | MUL (e1, e2) -> let eq1 = [(ty, TyInt)] in
                      let eq2 = gen_equations tenv e1 TyInt in
                        let eq3 = gen_equations tenv e2 TyInt in
                          eq1 @ eq2 @ eq3
  | ISZERO e -> let eq1 = [(ty, TyBool)] in
                  let eq2 = gen_equations tenv e TyInt in
                    eq1 @ eq2
  | READ -> [(ty, TyInt)]
  | IF (e1, e2, e3) -> let eq1 = gen_equations tenv e1 TyBool in
                         let eq2 = gen_equations tenv e2 ty in
                           let eq3 = gen_equations tenv e3 ty in
                             eq1 @ eq2 @ eq3
  | LET (x, e1, e2) -> let t1 = fresh_tyvar () in (* x binding *)
                         let eq1 = gen_equations tenv e1 t1 in
                           let eq2 = gen_equations (tenv_extend (x, t1) tenv) e2 ty in
                             eq1 @ eq2
  | LETREC (f, x, e1, e2) -> let t1 = fresh_tyvar () in (* x binding *)
                               let t2 = fresh_tyvar () in (* function binding *)
                                 let t3 = TyFun (t1, t2) in (* f type *)
                                   let tenv' = tenv_extend (x, t1) tenv in (* tenv_f = [f |-> t3]tenv *)
                                     let tenv'' = tenv_extend (f, t3) tenv' in (* tenv_xf = [x |-> t1]tenv_f = [f |-> t3, x |-> t1]tenv *)
                                       let eq1 = gen_equations tenv'' e1 t2 in
                                         let eq2 = gen_equations tenv'' e2 ty in
                                           eq1 @ eq2
  | PROC (x, e) -> let t1 = fresh_tyvar () in
                     let t2 = fresh_tyvar () in
                       let eq1 = [(ty, TyFun (t1, t2))] in
                         let eq2 = gen_equations (tenv_extend (x, t1) tenv) e t2 in
                           eq1 @ eq2
  | CALL (e1, e2) -> let t1 = fresh_tyvar () in
                       let eq1 = gen_equations tenv e1 (TyFun (t1, ty)) in
                         let eq2 = gen_equations tenv e2 t1 in
                           eq1 @ eq2
;;


(* Deconstruct equation to use for occurrence check. Basically gets ride of TyFun. *)
let rec deconstruct_eq : typ list -> typ list -> typ list
= fun ty l ->
  match ty with
  | [] -> l
  | hd :: tl -> (match hd with
                | TyInt | TyBool -> deconstruct_eq tl l
                | TyVar _ -> deconstruct_eq tl (hd :: l)
                | TyFun (a, b) -> (match (a, b) with
                                  | (TyVar _, TyVar _) -> deconstruct_eq tl (a :: b :: l)
                                  | (TyFun _, TyVar _) -> deconstruct_eq (a :: tl) (b :: l)
                                  | (TyVar _, TyFun _) -> deconstruct_eq (b :: tl) (a :: l)
                                  | _ -> deconstruct_eq tl l
                                   )
                 )
;;


(* Check if variable on left-hand side occurs on right-hand side.  *)
let rec occurrence_check : typ -> typ -> bool
= fun t1 t2 ->
  let new_t2 = deconstruct_eq [t2] [] in
    List.mem t1 new_t2
;;


(* Unification algorithm. *)
let rec unify : typ * typ * subst -> subst
= fun (typ1, typ2, subst) ->
  match (typ1, typ2) with
  | (TyInt, TyInt) -> subst
  | (TyBool, TyBool) -> subst
  | (TyVar x, TyVar y) -> subst_extend x typ2 subst
  | (TyVar a, t) -> if ((occurrence_check typ1 t) = true) then raise OccurrenceCheckError else (subst_extend a typ2 subst)
  | (t, TyVar a) -> unify (typ2, t, subst)
  | (TyFun (a, b), TyFun (a', b')) -> let subst' = unify (a, a', subst) in 
                                                    let (b', b'') = ((subst_apply b subst'), (subst_apply b' subst')) in
                                                      unify (b', b'', subst')
  | _ -> raise TypeError
;;


(* Apply unification to all. *)
let rec unifyall : typ_eqn * subst -> subst
= fun (eqn, subst) ->
  match eqn with
  | [] -> subst
  | (t1, t2) :: u -> let t1' = subst_apply t1 subst and t2' = subst_apply t2 subst in
                       let subst' = unify (t1', t2', subst) in
                              unifyall (u, subst')
;;


(* Solve equations. *)
let solve : typ_eqn -> subst
= fun eqns ->
  unifyall (eqns, subst_empty)
;;

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty