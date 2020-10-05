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
=fun tenv e ty -> match e with
ADD (e1, e2) | SUB (e1,e2) | MUL (e1,e2) | DIV(e1,e2) -> (ty, TyInt) :: (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
|CONST n -> [(ty, TyInt)]
|VAR x -> [(ty, tenv_find tenv x)]
|ISZERO e -> (ty, TyBool) :: (gen_equations tenv e TyInt)
|IF (e1,e2,e3) -> (gen_equations tenv e1 TyBool) @ (gen_equations tenv e2 ty) @ (gen_equations tenv e3 ty)
|READ -> []
|LET (x,e1,e2) -> let a = fresh_tyvar () in (gen_equations tenv e1 a) @ (gen_equations (tenv_extend (x, a) tenv) e2 ty)
|PROC (x, e) -> let a1 = fresh_tyvar () in
                 let a2 = fresh_tyvar () in 
				 (ty, TyFun (a1, a2)) :: (gen_equations (tenv_extend (x, a1) tenv) e a2)
|CALL (e1,e2) -> let a = fresh_tyvar () in 
                 (gen_equations tenv e1 (TyFun(a,ty))) @ (gen_equations tenv e2 a)
|LETREC (f,x,e1,e2) -> let a1 = fresh_tyvar ()  in 
                       let a2 = fresh_tyvar () in
                       let f_tenv = tenv_extend ( f, TyFun (a1, a2) ) tenv in
                       (gen_equations (tenv_extend (x, a1) f_tenv ) e1 a2 ) @ (gen_equations f_tenv e2 ty)
 					   

let rec occur : typ -> typ -> bool
= fun t1 t2 -> match t2 with
|TyFun (a,b) -> ((occur t1 a) || (occur t1 b))
| _ -> if t1 = t2 then true else false


			 
let rec unify :typ -> typ -> subst -> subst
= fun t1 t2 subst -> if (t1=t2) then subst else match t1 with
TyInt | TyBool -> (match t2 with 
                   |TyVar x-> (unify t2 t1 subst)
		           |_ -> raise TypeError)
| TyVar	x -> if (occur t1 t2) then raise TypeError else (subst_extend x t2 subst) 
| TyFun (a,b) -> (match t2 with
                 | TyFun (c,d) -> let s1 = (unify a c  subst) in 
                                  let s2 = (unify (subst_apply b s1) (subst_apply d s1) s1) in s2
                 | _ -> (unify t2 t1 subst)	)							  

and unifyall : typ_eqn -> subst -> subst
= fun eqns subst -> match eqns with
| [] -> subst
| (t1,t2)::tl -> let s1 = (unify (subst_apply t1 subst) (subst_apply t2 subst) subst) in (unifyall tl s1)



and solve : typ_eqn -> subst
=fun eqns -> (unifyall eqns subst_empty)




let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty