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
  | CONST n -> [(ty, TyInt)]
  | VAR x   -> [(ty, tenv_find tenv x)]
  | ADD (a, b) -> (ty, TyInt) :: ((gen_equations tenv a TyInt) @ (gen_equations tenv b TyInt))
  | SUB (a, b) -> (ty, TyInt) :: ((gen_equations tenv a TyInt) @ (gen_equations tenv b TyInt))
  | MUL (a, b) -> (ty, TyInt) :: ((gen_equations tenv a TyInt) @ (gen_equations tenv b TyInt))
  | DIV (a, b) -> (ty, TyInt) :: ((gen_equations tenv a TyInt) @ (gen_equations tenv b TyInt))
  | READ -> []
  | ISZERO x -> (ty, TyBool) :: (gen_equations tenv x TyInt)
  | IF (a, b, c) -> (gen_equations tenv a TyBool) @ ((gen_equations tenv b ty) @ (gen_equations tenv c ty))
  | LET (x, e1, e2) -> let a = fresh_tyvar () in
                      (gen_equations tenv e1 a) @ (gen_equations (tenv_extend (x, a) tenv) e2 ty)
  | LETREC(f, x, e1, e2) -> let a = fresh_tyvar () in
                      (gen_equations tenv e1 a) @ (gen_equations (tenv_extend (f, a) tenv) e2 ty)
  | PROC (x, e1) -> let a = fresh_tyvar () in
                    let b = fresh_tyvar () in
                      (ty, TyFun (a, b)) :: (gen_equations (tenv_extend (x, a) tenv) e1 b)
  | CALL (e1, e2) -> let a = fresh_tyvar () in
                      (gen_equations tenv e1 (TyFun (a, ty))) @ (gen_equations tenv e2 a)
  | _ -> raise(TypeError)

let rec unification : subst -> typ_eqn -> subst
=fun subs eqns -> match eqns with
  | [] -> subs
  | hd::tl -> match hd with
    | (TyVar t, TyFun (t1, t2)) -> let a = subst_apply t1 subs in
                                    let b = subst_apply t2 subs in
                                      let c = subst_apply (TyVar t) subs in
                                      if TyVar t = a || TyVar t = b then raise(TypeError) else (match c with
                                        | TyVar t -> unification (subst_extend t (TyFun (a, b)) subs) tl
                                        | TyFun (t3, t4) -> unification subs ((t3, t1) :: ((t4, t2) :: tl))
                                        | TyInt  -> raise(TypeError)
                                        | TyBool -> raise(TypeError))
    | (TyVar t, TyInt)  -> let a = subst_apply (TyVar t) subs in
                            (match a with
                              | TyInt -> unification subs tl
                              | TyBool -> raise(TypeError)
                              | TyVar t1 -> if t1 = t then unification (subst_extend t TyInt subs) tl else unification (subst_extend t1 TyInt subs) tl
                              | TyFun (a, b) -> raise(TypeError))
    | (TyVar t, TyBool) -> let a = subst_apply (TyVar t) subs in
                            (match a with
                              | TyInt -> raise(TypeError)
                              | TyBool -> unification subs tl
                              | TyVar t1 -> if t1 = t then unification (subst_extend t TyBool subs) tl else unification (subst_extend t1 TyBool subs) tl
                              | TyFun (a, b) -> raise(TypeError))
    | (TyVar t1, TyVar t2) -> if t1 = t2 then unification subs tl else let a = subst_apply (TyVar t1) subs in
                                                                          let b = subst_apply (TyVar t2) subs in
                                                                            if a <> (TyVar t1)  && b = (TyVar t2) then unification (subst_extend t2 a subs) tl else
                                                                              unification (subst_extend t1 b subs) tl
    | (TyInt, TyVar t)  -> unification subs ((TyVar t, TyInt)  :: tl)
    | (TyBool, TyVar t) -> unification subs ((TyVar t, TyBool) :: tl)
    | (TyFun (t1, t2), TyVar t) -> unification subs ((TyVar t, TyFun(t1, t2))::tl)
    | (TyFun (t1, t2), TyFun (t3, t4)) -> unification subs ((t1, t3) :: ((t2, t4) :: tl))
    | (TyInt, TyInt)   -> unification subs tl
    | (TyBool, TyBool) -> unification subs tl
    | (TyInt, TyBool)  -> raise(TypeError)
    | (TyBool, TyInt)  -> raise(TypeError)
    | (_, _) -> raise(TypeError)

let solve : typ_eqn -> subst
=fun eqns -> unification subst_empty eqns

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty