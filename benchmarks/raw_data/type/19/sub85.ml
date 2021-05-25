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
    let rec helpgen htenv he hty hres = match he with
      | CONST n -> (hty, TyInt)::hres
      | VAR x -> (hty, tenv_find htenv x)::hres
      | ADD (e1,e2) -> helpgen htenv e2 TyInt (helpgen htenv e1 TyInt ((hty, TyInt)::hres))
      | SUB (e1,e2) -> helpgen htenv e2 TyInt (helpgen htenv e1 TyInt ((hty, TyInt)::hres))
      | MUL (e1,e2) -> helpgen htenv e2 TyInt (helpgen htenv e1 TyInt ((hty, TyInt)::hres))
      | DIV (e1,e2) -> helpgen htenv e2 TyInt (helpgen htenv e1 TyInt ((hty, TyInt)::hres))
      | ISZERO e -> helpgen htenv e TyInt ((hty, TyBool)::hres)
      | READ -> hres
      | IF (e1,e2,e3) -> helpgen htenv e3 hty (helpgen htenv e2 hty (helpgen htenv e1 TyBool hres))
      | LET (x,e1,e2) -> let nv = fresh_tyvar () in helpgen (tenv_extend (x,nv) htenv) e2 hty (helpgen htenv e1 nv hres) 
      | LETREC (f,x,e1,e2) -> let nv1 = fresh_tyvar () in let nv2 = fresh_tyvar () in helpgen (tenv_extend (f,TyFun (nv1,nv2)) htenv) e2 hty (helpgen (tenv_extend (x,nv1) (tenv_extend (f,TyFun (nv1,nv2)) htenv)) e1 nv1 hres)
      | PROC (x,e) -> let nv1 = fresh_tyvar () in let nv2 = fresh_tyvar () in helpgen (tenv_extend (x,nv1) htenv) e nv2 ((hty, TyFun (nv1,nv2))::hres)
      | CALL (e1,e2) -> let nv = fresh_tyvar () in helpgen htenv e2 nv (helpgen htenv e1 (TyFun (nv,hty)) hres)

    in helpgen tenv e ty [];;

(*print_typ_eqns (gen_equations tenv_empty (PROC ("f", PROC ("x", SUB (CALL (VAR "f", CONST 3), CALL (VAR "f", VAR "x"))))) (fresh_tyvar ()));;*)

let solve : typ_eqn -> subst
  =fun eqns -> 
    let rec unify tpl = match tpl with
      | (TyInt,TyInt,sbst) -> sbst
      | (TyBool,TyBool,sbst) -> sbst
      | (TyVar x, TyVar y, sbst) -> if x=y then sbst else subst_extend x (TyVar y) sbst
      | (sth,TyVar x, sbst) -> unify (TyVar x,sth,sbst)
      | (TyVar t1,t2,sbst) -> if (TyVar t1)=t2 then sbst else (match t2 with | TyFun (f1,f2) -> if (TyVar t1)=f1 || (TyVar t1) = f2 then raise TypeError else subst_extend t1 t2 sbst |_ -> subst_extend t1 t2 sbst)
      | (TyFun (x1,x2), TyFun (y1,y2),sbst) -> let s1 = unify (x1,y1,sbst) in let s2 = unify ((subst_apply x2 s1),(subst_apply y2 s1),s1) in s2
      | (_,_,_) -> raise TypeError
    in let rec unifyall l s = match l with
        | [] -> s
        | hd::tl -> match hd with (t1,t2) -> let s1 = unify ((subst_apply t1 s),(subst_apply t2 s),s) in unifyall tl s1 
    in unifyall eqns subst_empty

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty