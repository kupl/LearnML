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


(*let tenv_func : typ -> tenv
= fun t ->*) 

let rec gen_equations : tenv -> exp -> typ -> typ_eqn 
=fun tenv e ty -> match e with
                   | CONST (n) -> [(ty, TyInt)]
                   | VAR (v) -> let nty = tenv_find tenv v
                                in [(ty, nty)]
                   | ADD (e1, e2) -> (ty, TyInt) :: ((gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt))
                   | SUB (e1, e2) -> (ty, TyInt) :: ((gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt))
                   | MUL (e1, e2) -> (ty, TyInt) :: ((gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt))
                   | DIV (e1, e2) -> (ty, TyInt) :: ((gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt))
                   | ISZERO (e) -> (ty, TyBool) :: (gen_equations tenv e TyInt)
                   | IF (b, e1, e2) -> ((gen_equations tenv b TyBool) @ (gen_equations tenv e1 ty)) @ (gen_equations tenv e2 ty)
                   | LET (v, e1, e2) -> let a = fresh_tyvar()
                                        in (gen_equations tenv e1 a) @ (gen_equations (tenv_extend(v, a) tenv) e2 ty)
                   | LETREC (v1, v2, e1, e2) -> let t1 = fresh_tyvar()
                                                in let t2 = fresh_tyvar()
                                                   in let n_tenv = tenv_extend (v2, t2) tenv
                                                      in let new_tenv = tenv_extend (v1, TyFun(t1, t2)) n_tenv
                                                             in let new_tenv2 = tenv_extend(v1, TyFun(t2, t1)) tenv
                                                                in (gen_equations new_tenv e1 t1) @ (gen_equations new_tenv2 e2 ty)
                   | PROC (x, e) -> let a1 = fresh_tyvar()
                                    in let a2 = fresh_tyvar()
                                       in (ty, TyFun(a1, a2)) :: (gen_equations (tenv_extend (x, a1) tenv) e a2)
                   | CALL (e1, e2) -> let a = fresh_tyvar()
                                      in (gen_equations tenv e1 (TyFun(a, ty))) @ (gen_equations tenv e2 a)
                   (*| READ -> *)
                   | _ -> raise(TypeError);;

  
    
(*let substitution = [];;

let rec apply_subst : typ_eqn -> subst -> typ_eqn
= fun eqns subst -> match eqns with
                        | [] -> []
                        | (typ1, typ2)::tl -> ((subst_apply typ1 subst), (subst_apply typ2 subst)) :: apply_subst tl subst
                        | _ -> raise(TypeError);;*)


(*********************************************************)
let rec delete_true_false_and_simplify : typ_eqn -> typ_eqn
= fun eqns -> match eqns with
                | [] -> []
                | hd::tl -> begin match hd with
                              | (TyInt, TyInt) -> delete_true_false_and_simplify tl
                              | (TyBool, TyBool) -> delete_true_false_and_simplify tl
                              | (TyInt, TyBool) -> raise(TypeError)
                              | (TyBool, TyInt) -> raise(TypeError)
                              | (TyFun (a1, a2), TyFun (b1, b2)) -> (delete_true_false_and_simplify [(a1, b1) ; (a2, b2)]) @ (delete_true_false_and_simplify tl)
                              | (TyInt, TyVar (v)) -> (TyVar (v), TyInt) :: delete_true_false_and_simplify tl
                              | (TyBool, TyVar (v)) -> (TyVar (v), TyBool) :: delete_true_false_and_simplify tl
                              | (TyFun (a, b), TyVar (v)) -> (delete_true_false_and_simplify [(TyVar (v), TyFun (a, b))]) @ delete_true_false_and_simplify tl
                              | (TyVar (v), TyFun (TyVar (a), TyVar (b))) -> if v = a || v = b then raise(TypeError)
                                                                             else hd :: (delete_true_false_and_simplify tl)
                              | (TyVar (v), TyFun (TyVar (a), b)) -> if v = a then raise(TypeError)
                                                                     else hd :: (delete_true_false_and_simplify tl)
                                      
                              | (TyVar (v), TyFun (a, TyVar (b))) -> if v = b then raise(TypeError)
                                                                     else hd :: (delete_true_false_and_simplify tl)
                              | _ -> hd :: delete_true_false_and_simplify tl
                            end
                | _ -> raise(TypeError);;
                

let rec subst_to_eqns : subst -> typ_eqn -> typ_eqn
= fun subst eqns -> match eqns with
                      | (typ1, typ2) :: tl -> let t1 = subst_apply typ1 subst
                                              in let t2 = subst_apply typ2 subst
                                                 in (t1, t2) :: (subst_to_eqns subst tl)
                      | [] -> eqns;;
                      
let rec eqn_to_sub : (typ * typ) -> typ -> typ
= fun (s, ty) sub -> match sub with
                             | TyInt -> sub
                             | TyBool -> sub
                             | TyVar(a) -> if s = sub then ty else sub
                             | TyFun(a, b) -> TyFun((eqn_to_sub (s, ty) a), (eqn_to_sub (s, ty) b));;
                                      
                      
let rec apply_eqn_to_subst : (typ * typ) -> subst -> subst
= fun eqn subst -> match subst with
                    | (name, typ)::tl -> let new_typ = eqn_to_sub eqn typ
                                         in subst_extend name new_typ (apply_eqn_to_subst eqn tl)
                    | [] -> [];;
                    
let move_eqn_to_subst : (typ * typ) -> subst -> subst
= fun eqn subst -> let new_subst = apply_eqn_to_subst eqn subst
                   in match eqn with
                        | (TyVar(s), t) -> let tyvar = s
                                           in let typ = t
                                              in subst_extend tyvar typ new_subst
                        | _ -> raise(TypeError);;

let rec before_solve : typ_eqn -> subst -> subst 
= fun eqns subst -> match eqns with
                     | hd::tl -> let new_subst = move_eqn_to_subst hd subst 
                                 in before_solve tl new_subst
                     | [] -> subst;;

let switch = ref 1;;
let subst = ref [];;

let rec solve : typ_eqn -> subst
= fun eqns -> if (!switch mod 2 = 1) then (
                let _ = (switch := !switch + 1) in
                let _ = (subst := []) in
                (let n_eqns = subst_to_eqns !subst eqns 
                  in let new_eqns = delete_true_false_and_simplify n_eqns
                     in match new_eqns with
                          | hd::tl -> let _ = (subst := move_eqn_to_subst hd !subst) in solve tl
                                  | [] -> !subst))
              else (let n_eqns = subst_to_eqns !subst eqns 
                    in let new_eqns = delete_true_false_and_simplify n_eqns
                        in match new_eqns with
                            | hd::tl -> let _ = (subst := move_eqn_to_subst hd !subst) in solve tl
                            | [] -> let _ = switch := !switch + 1 in !subst);;

(**********************************************************)

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty