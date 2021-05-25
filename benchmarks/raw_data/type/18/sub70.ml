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

let rec string_of_type ty = 
  match ty with
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyFun (t1,t2) -> "(" ^ string_of_type t1 ^ " -> " ^ string_of_type t2 ^ ")"
  | TyVar x -> x

let rec gen_equations : tenv -> exp -> typ -> typ_eqn 
=fun tenv e ty -> match e with
  | CONST n -> [(ty, TyInt)]
  | VAR x -> [(ty, tenv x)]
  | ADD (e1, e2) -> (ty, TyInt)::(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
  | SUB (e1, e2) -> (ty, TyInt)::(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
  | MUL (e1, e2) -> (ty, TyInt)::(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
  | DIV (e1, e2) -> (ty, TyInt)::(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
  | ISZERO n -> (ty, TyBool)::(gen_equations tenv n TyInt)
  | READ -> [(ty, TyInt)]
  | IF (x, e1, e2) -> (gen_equations tenv x TyBool)@(gen_equations tenv e1 ty)@(gen_equations tenv e2 ty)
  | LET (x, e1, e2) -> let t = fresh_tyvar() in (gen_equations tenv e1 t)@(gen_equations (tenv_extend (x, t) tenv) e2 ty)
  | LETREC (f, x, e1, e2) -> let t = fresh_tyvar() in let t' = fresh_tyvar() in let tenv' = tenv_extend (x, t') tenv in let tenv'' = tenv_extend (f, t) tenv'
    in (gen_equations tenv e1 t)@(gen_equations tenv'' e2 ty)
  | PROC (x, e) -> let t = fresh_tyvar() in let t' = fresh_tyvar() 
    in (ty, TyFun(t, t'))::(gen_equations (tenv_extend (x, t) tenv) e t')
  | CALL (e1, e2) -> let t = fresh_tyvar() in (gen_equations tenv e1 (TyFun(t, ty)))@(gen_equations tenv e2 t);;



let rec funct_eq lst = match lst with
  | [] -> []
  | (t1, t2)::tl -> match t1 with
    | TyInt -> (match t2 with 
      | TyInt -> (t1, t2)::(funct_eq tl)
      | TyVar x -> (t1, t2)::(List.map (fun (x', t) -> if TyVar x = x' then (TyInt, t) else (x', t)) tl)
      | _ -> raise TypeError)
    | TyBool -> (match t2 with 
      | TyBool -> (t1, t2)::(funct_eq tl)
      | TyVar x -> (t1, t2)::(List.map (fun (x', t) -> if TyVar x = x' then (TyBool, t) else (x', t)) tl)
      | _ -> raise TypeError)
    | TyFun(a, b) -> (match t2 with 
      | TyFun(c, d) -> [(a, c)]@[(b, d)]@(funct_eq tl)
      | TyVar x -> (t1, t2)::(funct_eq tl)
      | _ -> raise TypeError)
    | TyVar x -> (t1, t2)::(funct_eq tl);;
    
let rec funct_su lst = match lst with
  | [] -> []
  | (x, t)::tl -> match subst_find x tl with
    | TyFun (a, b) -> (match t with 
      | TyFun (c, d) -> (string_of_type a, c)::(string_of_type b, d)::lst
      | _ -> lst)
    | _ -> lst;;

let rec equal_eq lst1 lst2 lst3 = match lst1 with
  | [] -> lst3
  | (t1, t2)::tl -> match tl with
    | [] -> equal_eq lst2 [] lst3
    | (t3, t4)::tl' -> if t1 = t3 then equal_eq ((t1, t2)::tl') ((t3, t4)::lst2) (lst3@[(t2, t4)]) 
      else if t1 = t4 then equal_eq ((t1, t2)::tl') ((t3, t4)::lst2) (lst3@[(t2, t3)]) 
      else if t2 = t3 then equal_eq ((t1, t2)::tl') ((t3, t4)::lst2) (lst3@[(t1, t4)]) 
      else if t2 = t4 then equal_eq ((t1, t2)::tl') ((t3, t4)::lst2) (lst3@[(t1, t3)]) 
      else equal_eq ((t1, t2)::tl') ((t3, t4)::lst2) lst3;;
    
    (*
    
let rec equal_eq lst1 lst2 lst3 = match lst1 with
  | [] -> lst3
  | (t1, t2)::tl -> match tl with
    | [] -> equal_eq lst2 [] lst3@[(t1, t2)]
    | (t3, t4)::tl' -> if t1 = t3 then equal_eq ((t1, t2)::tl') (lst2@[(t3, t4)]) (lst3@[(t2, t4)]) 
      else if t1 = t4 then equal_eq ((t1, t2)::tl') (lst2@[(t3, t4)]) (lst3@[(t2, t3)]) 
      else if t2 = t3 then equal_eq ((t1, t2)::tl') (lst2@[(t3, t4)]) (lst3@[(t1, t4)]) 
      else if t2 = t4 then equal_eq ((t1, t2)::tl') (lst2@[(t3, t4)]) (lst3@[(t1, t3)]) 
      else equal_eq ((t1, t2)::tl') (lst2@[(t3, t4)]) lst3;;
    *)
let rec apply2 t sub = match t with
  | TyInt -> TyInt
  | TyBool -> TyBool
  | TyFun(t1, t2) -> TyFun(apply2 t1 sub, apply2 t2 sub)
  | TyVar x -> match sub with 
    | [] -> t
    | (t1', t2')::tl -> if t1' = t2' then apply2 t tl else if t = t1' then t2' else apply2 t tl

let rec extend1 t1 t2 sub = 
  (t1, t2)::(List.map (fun (x, t) -> (x, apply2 t [(t1, t2)])) sub)
    
let rec extend2 lst sub = 
  match lst with 
    | [] -> sub
    | (t1, t2)::tl ->  let sub' = (extend1 (t1) (apply2 t2 sub) sub)
      in extend2 tl (extend1 (t2) (apply2 t1 sub') sub')
      
    
let rec sub_extend lst sub = match lst with
  | [] -> sub
  | (t1, t2)::tl -> let sub' = (subst_extend (string_of_type t1) (subst_apply t2 sub) sub)
    in sub_extend tl (subst_extend (string_of_type t2) (subst_apply t1 sub') sub')


let solve : typ_eqn -> subst
=fun eqns -> sub_extend (equal_eq (funct_eq (extend2 eqns [])) [] (funct_eq (extend2 eqns []))) [];;

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty