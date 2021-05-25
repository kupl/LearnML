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
    | CONST n ->  [(ty, TyInt)]
    | VAR x ->  [(ty,tenv_find tenv x)]
    | ADD (e1,e2) -> let lst1 = gen_equations tenv e1 (TyInt) in
                    let lst2 = gen_equations tenv e2 (TyInt) in
                    [(ty,TyInt)]@lst1@lst2
    | MUL (e1,e2) -> let lst1 = gen_equations tenv e1 (TyInt) in
                    let lst2 = gen_equations tenv e2 (TyInt) in
                    [(ty,TyInt)]@lst1@lst2
    | DIV (e1,e2) -> let lst1 = gen_equations tenv e1 (TyInt) in
                    let lst2 = gen_equations tenv e2 (TyInt) in
                    [(ty,TyInt)]@lst1@lst2
    | SUB (e1,e2) -> let lst1 = gen_equations tenv e1 (TyInt) in
                    let lst2 = gen_equations tenv e2 (TyInt) in
                    [(ty,TyInt)]@lst1@lst2
    | ISZERO e1 -> let lst1 = gen_equations tenv e1 (TyInt) in
                    ( match lst1 with
                       |(ty1,TyInt)::tl -> [(ty,TyBool)]@lst1
                       |_-> raise TypeError)
    | IF (e1,e2,e3) ->let lst2 = gen_equations tenv e2 ty in
                      let lst3 = gen_equations tenv e3 ty in
                      let lst1 = gen_equations tenv e1 (TyBool) in
                      lst1@lst3@lst2
   
    | LET (x,e1,e2) ->  let newty1 = fresh_tyvar() in
                        let lst2 = gen_equations (tenv_extend (x,newty1) tenv) e2 ty in
                        let lst1 = gen_equations tenv e1 newty1 in
                         lst1@lst2
    | PROC (x,e1) ->  let newty1 = fresh_tyvar() in
                     let newty2 = fresh_tyvar() in
                     let newtenv = tenv_extend (x, newty1) tenv in
                     let lst1 = gen_equations newtenv e1 newty2 in
                     [(ty,TyFun(newty1,newty2))]@lst1
    | CALL (e1,e2) -> let lst1 = gen_equations tenv e1 ty in
                      let newty1 = fresh_tyvar()in
                      let lst2 = gen_equations tenv e2 newty1 in
                      (match lst1 with
                        |(ty1,t3)::tl -> [(TyFun(newty1,ty),t3)]@lst2
                        |_->raise TypeError)
    |LETREC (f,x,e1,e2) -> let newty1 = fresh_tyvar() in
                            let newty2 = fresh_tyvar() in
                            let newtenv1 = tenv_extend (x, newty2) tenv in
                            let newtenv2 = tenv_extend (f, newty1) newtenv1 in
                            let lst2 = gen_equations newtenv2 e2 ty in
                            let lst1 = gen_equations newtenv2 e1 newty1 in
                            lst1@lst2
	|READ -> [];;
   

let rec solve : typ_eqn -> subst
=fun eqns -> 
match eqns with
|(TyVar tv, ty)::tl ->let st= solve tl in
                      let ftv = if List.exists (fun (y, ty) -> tv = y) st then subst_find tv st else (TyVar tv) in
					( match (ftv,ty) with
                         |(TyVar x, TyBool) -> subst_extend x TyBool st
						 |(TyVar x, TyInt) -> subst_extend x TyInt st
						 |(TyVar x, TyFun(t1,t2))-> let ft1 = subst_apply t1 st in
										            let ft2 = subst_apply t2 st in
										             subst_extend x (TyFun(ft1,ft2)) st
						 |(TyVar x, TyVar t1) -> let ft1 = if List.exists (fun (y, ty) -> t1 = y) st then subst_find t1 st else (TyVar t1) in
						                         subst_extend tv ft1 st
						 |_-> solve ((ftv,ty)::tl)
						)
|(ty,TyVar tv)::tl -> (match ty with
                      |TyVar t1 -> subst_extend tv (TyVar t1) (solve tl)
					  |_-> solve ((TyVar tv,ty)::tl))
|(TyBool, TyBool)::tl -> solve tl
|(TyInt, TyInt)::tl -> solve tl
|(TyFun(t1,t2), TyFun(t3,t4))::tl -> solve ((t1,t3)::(t2,t4)::tl)
|[]-> subst_empty
|_->raise TypeError;;


let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty