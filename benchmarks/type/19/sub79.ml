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
  | CONST n -> [ty,TyInt]
  | VAR x -> [ty,tenv_find tenv x]
  | ADD (e1,e2) -> [ty,TyInt]@ (gen_equations tenv e1 TyInt)@ (gen_equations tenv e2 TyInt)
  | SUB (e1,e2) -> [ty,TyInt]@ (gen_equations tenv e1 TyInt)@ (gen_equations tenv e2 TyInt)
  | MUL (e1,e2) -> [ty,TyInt]@ (gen_equations tenv e1 TyInt)@ (gen_equations tenv e2 TyInt)
  | DIV (e1,e2) -> [ty,TyInt]@ (gen_equations tenv e1 TyInt)@ (gen_equations tenv e2 TyInt)
  | ISZERO e -> [ty, TyBool]@ (gen_equations tenv e TyInt)
  | READ -> [ty, TyInt]
  | IF (e1,e2,e3)-> (gen_equations tenv e1 TyBool)@ (gen_equations tenv e2 ty)@ (gen_equations tenv e3 ty)
  | LET (x,e1,e2)-> let a = fresh_tyvar() in
                     (gen_equations tenv e1 a)@ (gen_equations (tenv_extend (x,a) tenv)  e2 ty)
  | LETREC (f,x,e1,e2)-> let a = fresh_tyvar() in
                          let b = fresh_tyvar() in 
                            (gen_equations (tenv_extend (x,a) (tenv_extend (f,TyFun(a,b)) tenv)) e1 b)@(gen_equations (tenv_extend (f,TyFun(a,b)) tenv) e2 ty)
							
  | PROC (x,e)-> let a = fresh_tyvar() in
                  let b = fresh_tyvar() in 
                    [ty,TyFun(a,b)]@ (gen_equations (tenv_extend (x,a) tenv) e b)
  | CALL (e1,e2)-> let a = fresh_tyvar() in
                    (gen_equations tenv e1 (TyFun (a,ty)))@ (gen_equations tenv e2 a)

(* TODO *)

let rec solve : typ_eqn -> subst
=fun eqns ->  (* TODO *)
let rec make_subst: typ_eqn -> subst -> subst
=fun eqns sb ->
 match eqns with
   |[]-> sb
   |(x, y)::tl-> let t1=subst_apply x sb in
                  let t2=subst_apply y sb in
                   (match (t1, t2) with 
                      |(TyInt,TyInt) -> make_subst tl sb
                      |(TyBool,TyBool) -> make_subst tl sb
                      |(TyVar v1,TyVar v2) -> make_subst tl (subst_extend v1 t2 sb) 
                      |(TyFun (tx1,ty1),TyFun (tx2,ty2)) -> make_subst tl (make_subst ([(tx1,tx2)]@[(ty1,ty2)]) sb)
                      |(TyVar v,TyInt) -> make_subst tl (subst_extend v t2 sb)
                      |(TyVar v,TyBool) -> make_subst tl (subst_extend v t2 sb)
                      |(TyVar v,TyFun (tx,ty)) -> (match (tx,ty) with
                                                    |(TyVar a,TyVar b) -> if (a=v||b=v) then raise TypeError else  make_subst tl (subst_extend v t2 sb)
                                                    |(TyVar a,_)-> if a=v then raise TypeError else  make_subst tl (subst_extend v t2 sb)
                                                    |(_,TyVar b)-> if b=v then raise TypeError else  make_subst tl (subst_extend v t2 sb)
                                                    |_->make_subst tl (subst_extend v t2 sb)
                                                   )
                     
                      |(TyInt,TyVar v) -> make_subst tl (subst_extend v t1 sb)
                      |(TyBool,TyVar v) -> make_subst tl (subst_extend v t1 sb)
                      |(TyFun (tx,ty),TyVar v) ->  (match (tx,ty) with
                                                    |(TyVar a,TyVar b) -> if (a=v||b=v) then raise TypeError else  make_subst tl (subst_extend v t1 sb)
                                                    |(TyVar a,_)-> if a=v then raise TypeError else  make_subst tl (subst_extend v t1 sb)
                                                    |(_,TyVar b)-> if b=v then raise TypeError else  make_subst tl (subst_extend v t1 sb)
                                                    |_->make_subst tl (subst_extend v t2 sb)
                                                   )
                      |_ -> raise TypeError
                       )
  in
  let subst = subst_empty in make_subst eqns subst
     
     

     
    
let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty