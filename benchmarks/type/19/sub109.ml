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
=fun tenv e ty ->(* TODO *)					
					(match e with
					
					| CONST n -> ([(ty, TyInt)])
					| VAR x -> let (eqn) =  [((tenv_find tenv x), ty)] in eqn
					| ADD (e1, e2) -> biop tenv e1 e2 ty
					| SUB (e1, e2) -> biop tenv e1 e2 ty
					| MUL (e1, e2) -> biop tenv e1 e2 ty
					| DIV (e1, e2) -> biop tenv e1 e2 ty
					| ISZERO e -> ([(ty, TyBool)] @ (gen_equations tenv e TyInt))
					| IF (e1, e2, e3) -> ((gen_equations tenv e1 TyBool) @ (gen_equations tenv e2 ty) @ (gen_equations tenv e3 ty))
					| LET (x, e1, e2) -> let alpha = fresh_tyvar () in ((gen_equations tenv e1 alpha)@ (gen_equations (tenv_extend (x,alpha) tenv) e2 ty))
					| PROC (x, e0) -> let (a1) = fresh_tyvar() in let (a2) = fresh_tyvar() in ([(ty, (TyFun(a1, a2)))]@[(TyVar x, a1)]@(gen_equations (tenv_extend (x,a1) tenv) e0 a2))
					| CALL (e1, e2) -> let alpha = fresh_tyvar() in ((gen_equations tenv e1 (TyFun(alpha, ty)))@(gen_equations tenv e2 alpha))
					| LETREC (f, x, e1, e2) -> let a1 = fresh_tyvar() in let a2 = fresh_tyvar() in ((gen_equations (tenv_extend (f, TyFun(a2,a1)) (tenv_extend (x, a2) tenv)) e1 a1)@ [((tenv_find tenv x), a2)]@ (gen_equations (tenv_extend(f, TyFun(a2,a1)) tenv) e2 ty)@[((tenv_find tenv f), a1)]) 
					)
					
		and biop tenv e1 e2 ty = ([(ty, TyInt)]@(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt))

let solve : typ_eqn -> subst
=fun eqns -> (* TODO *)
			
			let rec unifyall : typ_eqn -> subst -> subst 
			= fun eqns sub -> 
							let rec unify : (typ * typ) -> subst -> subst
							= fun (t1, t2) s0 -> if (t1 = t2) then s0 else
								(match t1 with
								| TyVar x -> (match t2 with 
												| TyFun (a,b) -> 
												if ((t1=a) || (t1=b)) then raise TypeError
												else subst_extend x t2 s0
												| _ -> subst_extend x t2 s0)
								
								| TyInt | TyBool-> if ((t2=TyInt)||(t2=TyBool))&&(t1!=t2) then raise TypeError
													else unify (t2, t1) s0 
								| TyFun (f1, f2) -> (match t2 with
													| TyFun (f1', f2') -> let s1 = unify(f1, f1') s0 in
																			let s2 = unify( (subst_apply f2 s1), (subst_apply f2' s1)) s1 in s2
													| _ -> raise TypeError)
								| _ -> raise TypeError) in 
					
			
							(match eqns with
							| [] -> sub
							| (t1, t2)::tl -> let sub1 = unify ((subst_apply t1 sub), (subst_apply t2 sub)) sub in unifyall tl sub1)
			in unifyall eqns subst_empty			

			

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty