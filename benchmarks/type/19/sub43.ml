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


let rec change_typ : (tyvar * typ) -> typ -> typ
= fun (x, t) eqn ->
  match eqn with
  | TyVar y -> if x = y then t else eqn
  | TyFun (t2_1, t2_2) -> TyFun (change_typ (x, t) t2_1, change_typ (x, t) t2_2)
  | _ -> eqn

let rec prop_eqns : (tyvar * typ) -> typ_eqn -> typ_eqn
= fun (x, t) eqns ->
  match eqns with
  | [] -> []
  | (hd, t2)::tl -> (hd, change_typ (x, t) t2)::(prop_eqns (x, t) tl)

let rec gen_equations : tenv -> exp -> typ -> typ_eqn 
=fun tenv e ty ->
  match e with
  | CONST n -> [(ty, TyInt)]
  | VAR x -> [(ty, tenv_find tenv x)] 
  | ADD (e1, e2) -> (ty, TyInt)::(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
  | SUB (e1, e2) -> (ty, TyInt)::(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
  | MUL (e1, e2) -> (ty, TyInt)::(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
  | DIV (e1, e2) -> (ty, TyInt)::(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
  | ISZERO e -> (ty, TyBool)::(gen_equations tenv e TyInt)
  | IF (e1, e2, e3) -> (gen_equations tenv e1 TyBool)@(gen_equations tenv e2 ty)@(gen_equations tenv e3 ty)
  | READ -> [(ty, TyInt)]
  | LET (x, e1, e2) -> 
      let var_type = fresh_tyvar () in (gen_equations tenv e1 var_type)@(gen_equations (tenv_extend (x, var_type) tenv) e2 ty)
  | LETREC (f, x, e1, e2) ->
      let var_type = fresh_tyvar () in
      let e1_type = fresh_tyvar() in
        (gen_equations (tenv_extend (f, TyFun (var_type, e1_type)) (tenv_extend (x, var_type) tenv)) e1 e1_type)@(gen_equations (tenv_extend (f, TyFun (var_type, e1_type)) tenv) e2 ty)
  | PROC (x, e) -> 
      let var_type = fresh_tyvar () in
      let func_type = fresh_tyvar () in
        (ty, TyFun (var_type, func_type))::(gen_equations (tenv_extend (x, var_type) tenv) e func_type)
  | CALL (e1, e2) ->
      let proc_type = fresh_tyvar() in
      let input_type = fresh_tyvar() in
        (ty, proc_type)::(gen_equations tenv e1 (TyFun (input_type, proc_type)))@(gen_equations tenv e2 input_type)

let solve : typ_eqn -> subst
=fun eqns ->
  let rec unify : typ_eqn -> subst -> subst
  = fun eqn subset ->
    match eqn with
    | [] -> subset
    | hd::tl ->
      let (t_hd, t_tl) = hd in
      let applying_sub_hd = subst_apply t_hd subset in (* subset에 있는걸로 equation 업데이트 *)
      (*let applying_sub_tl = subst_apply t_tl subset in*)
      if t_hd <> applying_sub_hd (*|| t_tl <> applying_sub_tl*) (* 바뀌었는가? *)
      then unify ((applying_sub_hd, t_tl)::tl) subset (* 바뀌었으면 applying 한걸로 다시 계산 *)
      else (* 안바뀌었으면 unify 계산 *)
      (
        match hd with
        | (TyInt, TyInt) -> unify tl subset (* unify (int, int, S) *)
        | (TyBool, TyBool) -> unify tl subset (* unify (bool, bool, S) *)
        | (TyVar t1, t) ->
          (match t with
          | TyInt | TyBool -> unify (prop_eqns (t1, t) tl) (subst_extend t1 t subset) (* 그냥 subset에 적용시키고 다음꺼 실행 *)
          | TyVar t2 ->  
              if t1 = t2 
              then unify tl subset (* 같으면 그냥 넘기기 *)
              else unify (prop_eqns (t1, t) tl) (subst_extend t1 t subset) (* 안같으면 propagation 후 넘기기 *)
          | TyFun (t2_1, t2_2) ->
            
              let rec check_typ : tyvar -> typ -> bool
              = fun x typ ->
                match typ with
                | TyVar y -> if x = y then true else false
                | TyFun (typ_1, typ_2) -> (check_typ x typ_1) || (check_typ x typ_2)
                | _ -> false
              in 
              
              if not (check_typ t1 t) (* 자기 자신 타입이 있는지 확인 *)
              then unify (prop_eqns (t1, t) tl) (subst_extend t1 t subset) (* t2식에 t1이 없다면 propagation 후 넘기기 *)
              else raise (Failure "Type Error")
          )
        | (TyFun (t1_1, t1_2), TyFun(t2_1, t2_2)) ->
            unify ([(t1_1, t2_1); (t1_2, t2_2)]@tl) subset
        | (TyInt, TyVar x) -> unify ((TyVar x, TyInt)::tl) subset (* 오른쪽만 변수면 자리 바꾸기 *) 
        | (TyBool, TyVar x) -> unify ((TyVar x, TyBool)::tl) subset (* 오른쪽만 변수면 자리 바꾸기 *) 
        | (TyFun(t1, t2), TyVar x) -> unify ((TyVar x, TyFun(t1,t2))::tl) subset (* 오른쪽만 변수면 자리 바꾸기 *) 
        | _ -> raise (Failure "TypeError")
      )
  in unify eqns []
        
    
    
let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty