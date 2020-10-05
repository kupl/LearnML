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
    | CONST i -> [(ty, TyInt)]
    | VAR x -> [(ty, (tenv_find tenv x))]
    | ADD (e1, e2) -> (ty, TyInt) :: ((gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt))
    | SUB (e1, e2) -> (ty, TyInt) :: ((gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt))
    | MUL (e1, e2) -> (ty, TyInt) :: ((gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt))
    | DIV (e1, e2) -> (ty, TyInt) :: ((gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt))
    | ISZERO e1 -> (ty, TyBool) :: (gen_equations tenv e1 TyInt)
    | IF (e1, e2, e3) -> 
      (gen_equations tenv e1 TyBool) @ (gen_equations tenv e2 ty) @ (gen_equations tenv e3 ty)
    | LET (v1, e1, e2) -> 
      let nty1 = fresh_tyvar () in
      (gen_equations tenv e1 nty1) @ (gen_equations (tenv_extend (v1, nty1) tenv) e2 ty)
    | LETREC (f, v1, e1, e2) ->
      let nty1 = fresh_tyvar () in
      let nty2 = fresh_tyvar () in
      (gen_equations (tenv_extend (f, TyFun (nty1, nty2)) (tenv_extend (v1, nty1) tenv)) e1 nty2) 
      @ (gen_equations (tenv_extend  (f, TyFun (nty1, nty2)) tenv) e2 ty)
    | PROC (v1, e1) ->
      let nty1 = fresh_tyvar () in
      let nty2 = fresh_tyvar () in
      (ty, TyFun (nty1, nty2)) :: (gen_equations (tenv_extend (v1, nty1) tenv) e1 nty2)
    | CALL (e1, e2) ->
      let nty1 = fresh_tyvar () in
      (gen_equations tenv e1 (TyFun (nty1, ty))) @ (gen_equations tenv e2 nty1)
    | READ -> []

let solve : typ_eqn -> subst
=fun eqns -> 
  let rec unify : typ -> typ -> subst -> subst
  = fun typ1 typ2 subst ->
    if typ1 == TyInt && typ2 == TyInt then subst
    else if typ1 == TyBool && typ2 == TyBool then subst
    else 
      match typ1 with
        | TyVar t1 -> 
          (match typ2 with
            | TyVar t2 ->
              if typ1 = typ2 then subst
              else subst_extend t1 typ2 subst
            | _ ->
              (let rec isin : typ -> typ -> bool
              = fun a t ->
                match t with
                  | TyInt -> false
                  | TyBool -> false
                  | TyVar ts -> if a = t then true else false
                  | TyFun (t1,t2) -> isin a t1 || isin a t2 in
                  if (isin typ1 typ2) then raise (TypeError) else subst_extend t1 typ2 subst))
                  
        | TyFun (t1, t2) ->
          (match typ2 with
            | TyFun (t1_d, t2_d) ->
              (let s_d = unify t1 t1_d subst in
              let s_dd = unify (subst_apply t2 s_d) (subst_apply t2_d s_d) s_d in
              s_dd)
            | TyVar t2 ->  unify typ2 typ1 subst
            | _ -> raise (TypeError))
            
        | _ ->
          (match typ2 with
            | TyVar t2 ->  unify typ2 typ1 subst
            | _ -> raise (TypeError)) in
      
  let rec unify_all : typ_eqn -> subst -> subst
  = fun eqn_lst subst ->
    match eqn_lst with
      | [] -> subst
      | hd::tl ->
        match hd with (t1, t2) -> 
          let s_d = unify (subst_apply t1 subst) (subst_apply t2 subst) subst in
          unify_all tl s_d in
  unify_all eqns subst_empty

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty