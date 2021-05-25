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
    | VAR x -> [(tenv_find tenv x, ty)]
    | ADD (e1, e2) -> 
	  let et0 = [(ty, TyInt)] in
      let et1 = gen_equations tenv e1 (TyInt) in
      let et2 = (gen_equations tenv e2 (TyInt)) in
	  et2@(et1@et0)
    | SUB (e1, e2) -> 
      let et0 = [(ty, TyInt)] in
      let et1 = gen_equations tenv e1 (TyInt) in
      let et2 = (gen_equations tenv e2 (TyInt)) in
	  et1@(et2@et0)
    | MUL (e1, e2) -> 
      let et0 = [(ty, TyInt)] in
      let et1 = gen_equations tenv e1 (TyInt) in
      let et2 = (gen_equations tenv e2 (TyInt)) in
	  et2@(et1@et0)
    | DIV (e1, e2) -> 
      let et0 = [(ty, TyInt)] in
      let et1 = gen_equations tenv e1 (TyInt) in
      let et2 = (gen_equations tenv e2 (TyInt)) in
	  et2@(et1@et0)
    | READ -> [(ty, TyInt)]
    | ISZERO e ->
      let et0 = [(ty, TyBool)] in
	  let et = gen_equations tenv e (TyInt) in
	  (match et with
	    | (tt, ty1)::tl -> et@et0
	  )
    | IF (e1, e2, e3) ->
      let et1 = gen_equations tenv e1 (TyBool) in
	  let et2 = gen_equations tenv e2 ty in
	  let et3 = gen_equations tenv e3 ty in
      (match et2, et3 with
	    | (tt1, tyy)::tl1, (tt2, tyt)::tl2 -> 
		  if tyy = tyt then et3@(et2@et1) else raise TypeError)
    | LET (v, e1, e2) -> 
	  let et1 = gen_equations tenv e1 (fresh_tyvar ()) in
	  (match et1 with
	   | (tt1, typ1)::tl -> (gen_equations (tenv_extend (v, typ1) tenv) e2 ty)@et1
	   )
    | LETREC (v1, v2, e1, e2) -> 
	  let t1 = fresh_tyvar () in
	  let t2 = fresh_tyvar () in
	  let et1 = gen_equations (tenv_extend (v2, TyVar v2) (tenv_extend (v1, TyFun (t2, t1)) tenv)) e1 t1 in
	  (gen_equations (tenv_extend (v1, TyFun (t2, t1)) tenv) e2 (fresh_tyvar ()))@et1
    | PROC (v, e) -> 
	  let t1 = TyVar v in
	  let t2 = fresh_tyvar () in
	  let et0 = [(ty, TyFun (t1, t2))] in
	  et0@(gen_equations (tenv_extend (v, t1) tenv) e t2)
    | CALL (e1, e2) -> 
	  let a = fresh_tyvar () in
	  let et1 = gen_equations tenv e1 (TyFun (a, ty)) in
	  (gen_equations tenv e2 a)@et1
	  (* TODO *)

let rec solve : typ_eqn -> subst
= fun eqns -> unify eqns subst_empty 
  
and unify 
= fun eqns subst ->
  match eqns with
   | [] -> subst
   | hd::tl ->
   (match hd with
	 | (TyInt, TyInt) -> unify tl subst
     | (TyBool, TyBool) -> unify tl subst
     | (TyVar t1, TyVar t2) -> if t1 = t2 then unify tl subst else unify tl (subst_extend t1 (TyVar t2) subst) 
     | (TyVar t1, TyInt) -> unify tl (subst_extend t1 TyInt subst)
     | (TyVar t1, TyBool) -> unify tl (subst_extend t1 TyBool subst)
     | (TyVar t1, TyFun (tyf1, tyf2)) -> if find (TyFun (tyf1, tyf2)) (TyVar t1) = true then raise TypeError else unify tl (subst_extend t1 (TyFun (tyf1, tyf2)) subst)
     | (TyInt, TyVar t1) -> unify ((TyVar t1, TyInt)::tl) subst
	 | (TyBool, TyVar t1) -> unify ((TyVar t1, TyBool)::tl) subst
	 | (TyFun (t1, t2), TyFun (tf1, tf2)) -> 
	    let sp = unify ((t1, tf1)::tl) subst in
		let spp = unify ((t2, tf2)::tl) sp in
        spp
     | _ -> raise TypeError)		
	 
and find
= fun eqns a ->
   match eqns with
    | TyInt -> false
	| TyBool -> false
	| TyVar t -> if a = TyVar t then true else false
	| TyFun (t1, t2) ->
	  let tf1 = find t1 a in
	  let tf2 = find t1 a in
	  (match tf1, tf2 with
	    | false, false -> false
		| _ -> true)
		
(* TODO *)


let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty