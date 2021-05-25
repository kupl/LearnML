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
=fun tenv e ty -> (* TODO *)
  match e with 
    | CONST(n) -> [(ty,TyInt)]
    | READ -> [(ty,TyInt)]
    | VAR(v) -> [(tenv_find tenv v),ty](*tenv에서 v에 해당하는 것 찾아서 그걸 박아놔야함*)
    | ADD(e1,e2) -> [(ty,TyInt)]@(gen_equations tenv e1 TyInt )@(gen_equations tenv e2 TyInt )
    | SUB(e1,e2) -> [(ty,TyInt)]@(gen_equations tenv e1 TyInt )@(gen_equations tenv e2 TyInt )
    | MUL(e1,e2) -> [(ty,TyInt)]@(gen_equations tenv e1 TyInt )@(gen_equations tenv e2 TyInt )
    | DIV(e1,e2) -> [(ty,TyInt)]@(gen_equations tenv e1 TyInt )@(gen_equations tenv e2 TyInt )
    | ISZERO(e1) -> [(ty,TyBool)]@(gen_equations tenv e1 TyInt)
    | IF(e1,e2,e3) -> (gen_equations tenv e2 ty)@(gen_equations tenv e3 ty)@(gen_equations tenv e1 TyBool)
    | LET(v,e1,e2) -> let nv = fresh_tyvar () 
                      in (gen_equations tenv e1 nv)@(gen_equations (tenv_extend (v,nv) tenv) e2 ty)
    | LETREC(v1,v2,e1,e2) -> let nv1 = fresh_tyvar ()(*nv1 = *) 
                             in let nv2 = fresh_tyvar ()
                                in let ntenv = tenv_extend (v1,TyFun(nv1,nv2)) tenv
                                   in  (gen_equations (tenv_extend (v2,nv1) ntenv) e1 nv2)@(gen_equations ntenv e2 ty)
    | PROC(v,e1) -> let nv1 = fresh_tyvar ()
                    in let nv2 = fresh_tyvar ()
                       in (ty,TyFun(nv1,nv2))::(gen_equations (tenv_extend (v,nv1) tenv) e1 nv2)
    | CALL(e1,e2) -> let nv1 = fresh_tyvar ()
                     in (gen_equations tenv e1 (TyFun(nv1,ty)) )@(gen_equations tenv e2 nv1)



let solve : typ_eqn -> subst
=fun eqns -> (*subst_empty  TODO *)
  let rec unifyall : typ_eqn -> subst -> subst
  = fun tyeqn subst -> 
    match tyeqn with
      | [] -> subst
      | (lt,rt)::tl ->  let subst' =
                            let rec unify : typ*typ*subst->subst
                            = fun ttsubst -> 
                              match ttsubst with
                                | (TyInt,TyInt,rsubst) -> rsubst
                                | (TyBool,TyBool,rsubst) -> rsubst
                                
                                | (TyVar(v1),TyVar(v2),rsubst) -> if v1 = v2 then rsubst
                                                                  else subst_extend v1 (TyVar(v2)) rsubst
                                                                  
                                | (TyVar(v),t,rsubst) -> let rec vch
                                                         = fun v' t ->
                                                           match t with
                                                             | TyInt -> true
                                                             | TyBool -> true
                                                             | TyFun(t1,t2) -> ((vch v' t1) && (vch v' t2))
                                                             | TyVar(v1) -> v'<>v1
                                                         in if (vch v t) then
                                                             subst_extend v t rsubst else raise TypeError
                                                            
                                | (t,TyVar(v),rsubst) ->  unify (TyVar(v),t,rsubst)
                                | (TyFun(t1,t2),TyFun(t1',t2'),rsubst) -> let s'= (unify (t1,t1',rsubst))
                                                                          in let s''= (unify ( (subst_apply t2 s') , (subst_apply t2' s') , s' ))
                                                                            in s''
                                | (_,_,_) -> raise TypeError
                        in unify ((subst_apply lt subst), (subst_apply rt subst) ,subst)
                      
                      
        in (*let _ = subst_print subst in let _=print_endline "" in*) unifyall tl subst'
  in unifyall eqns [] (*U*)

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty