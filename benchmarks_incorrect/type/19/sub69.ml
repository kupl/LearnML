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
    | CONST(n) -> [(ty, TyInt)]
    | VAR(v) -> let t = (tenv_find tenv v) in 
                [(t, ty)]
    | ADD(e1, e2) -> [(ty, TyInt)] @ (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
    | SUB(e1, e2) -> [(ty, TyInt)] @ (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
    | MUL(e1, e2) -> [(ty, TyInt)] @ (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
    | DIV(e1, e2) -> [(ty, TyInt)] @ (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
    | ISZERO(e) -> [(ty, TyBool)] @ (gen_equations tenv e TyInt)
    | READ -> 
      let new_tv = fresh_tyvar () in
      [(ty, new_tv)]
    | IF(e1, e2, e3) -> (gen_equations tenv e1 TyBool) @ (gen_equations tenv e2 ty) @ (gen_equations tenv e3 ty)
    | LET(v, e1, e2) -> 
      let new_tv = fresh_tyvar () in
      let new_tenv = (tenv_extend (v, new_tv) tenv) in 
      (gen_equations tenv e1 new_tv) @ (gen_equations new_tenv e2 ty)
    | LETREC(v1, v2, e1, e2) ->
      let new_tv1 =  fresh_tyvar () in
      let new_tv2 =  fresh_tyvar () in
      let new_tv3 =  TyFun(new_tv1, new_tv2) in
      let new_tenv1 = (tenv_extend (v2, new_tv1) tenv) in
      let new_tenv2 = (tenv_extend (v1, new_tv3) new_tenv1) in
      (gen_equations new_tenv2 e1 new_tv2) @ (gen_equations new_tenv2 e2 ty)
    | PROC(v, e) ->
      let new_tv1 =  fresh_tyvar () in
      let new_tv2 =  fresh_tyvar () in
      let new_tv3 =  TyFun(new_tv1, new_tv2) in
      let new_tenv = (tenv_extend (v, new_tv1) tenv) in
      [(ty, new_tv3)] @ (gen_equations new_tenv e new_tv2)
    | CALL(e1, e2) ->
      let new_tv1 = fresh_tyvar () in
      let new_tv2 = TyFun(new_tv1, ty) in
      (gen_equations tenv e1 new_tv2) @ (gen_equations tenv e2 new_tv1)

let solve : typ_eqn -> subst
=fun eqns ->
  (let rec solve1 
  = fun eqns1 subst1 ->
     (match eqns1 with
	 | h :: t ->
	    (match h with
	        | (t1, t2) -> 
		   (match t1 with
		       | TyVar(x1) -> 
			  let st1 = (subst_apply t1 subst1) in
			  let st2 = (subst_apply t2 subst1) in
			  (match st1, st2 with
			      | TyVar(x2), _ ->
			         let subst2 = (subst_extend x2 st2 subst1) in
				 (solve1 t subst2)
			      | _,  TyVar(y2) ->
			         let subst2 = (subst_extend y2 st1 subst1) in
				 (solve1 t subst2)
			      | TyFun(tx1, tx2), TyFun(ty1, ty2) ->
			         let neqns = ([(tx1, ty1)] @ [(tx2, ty2)] @ t) in
				 (solve1 neqns subst1)
			      | TyInt, TyBool -> raise TypeError
			      | TyBool, TyInt -> raise TypeError
			      | TyFun(tf1, tf2), TyInt -> raise TypeError
			      | TyFun(tf1, tf2), TyBool -> raise TypeError
			      | TyInt, TyFun(tf1, tf2) -> raise TypeError
			      | TyBool, TyFun(tf1, tf2) -> raise TypeError
			      | _, _ ->
			        (solve1 t subst1))
		       | TyInt -> 
		          (match t2 with
			      | TyInt -> (solve1 t subst1)
			      | TyVar(y1) -> 
			          let st2 = (subst_apply t2 subst1) in
				  (match st2 with
				      | TyInt -> (solve1 t subst1)
				      | TyVar(y2) -> 
				         let subst2 = (subst_extend y2 TyInt subst1) in
					 (solve1 t subst2)
				      | _ -> raise TypeError))
		       | TyBool ->
		          (match t2 with
			      | TyBool -> (solve1 t subst1)
			      | TyVar(y1) -> 
			          let st2 = (subst_apply t2 subst1) in
				  (match st2 with
				      | TyBool -> (solve1 t subst1)
				      | TyVar(y2) -> 
				         let subst2 = (subst_extend y2 TyBool subst1) in
					 (solve1 t subst2)
				      | _ -> raise TypeError))
		       | TyFun(tx1, tx2) ->
		          let st1 = (subst_apply t1 subst1) in
			  let stx1 = (subst_apply tx1 subst1) in
			  let stx2 = (subst_apply tx2 subst1) in
		          let st2 = (subst_apply t2 subst1) in 
			  (match st2 with
			      | TyVar(ty) -> 
			         let subst2 = (subst_extend ty st1 subst1) in
				 (solve1 t subst2)
			      | TyFun(ty1, ty2) -> 
			         let neqns = ([(stx1, ty1)] @ [(stx2, ty2)] @ t) in
				 (solve1 neqns subst1)
			      | _ -> raise TypeError)
			)
		)
	| [] -> subst1) in 
(solve1 eqns subst_empty));;

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty