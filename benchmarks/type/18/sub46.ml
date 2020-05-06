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
    |CONST n -> [(ty, TyInt)]
    |VAR v -> [(ty, tenv_find tenv v)]
    |ADD (e1,e2) -> [(ty, TyInt)] @ (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
    |SUB (e1,e2) -> [(ty, TyInt)] @ (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
    |MUL (e1,e2) -> [(ty, TyInt)] @ (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
    |DIV (e1,e2) -> [(ty, TyInt)] @ (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
    |READ -> []
    |ISZERO e -> [(ty, TyBool)] @ (gen_equations tenv e TyInt)
    |IF (e1,e2,e3) -> (gen_equations tenv e1 TyBool) @ (gen_equations tenv e2 ty) @ (gen_equations tenv e3 ty)
    |LET (v,e1,e2) -> 
      let newalpa = fresh_tyvar() in
      (gen_equations tenv e1 newalpa) @ (gen_equations (tenv_extend (v,newalpa) tenv) e2 ty)
    |LETREC (f,v,e1,e2) ->
      let newalpa1 = fresh_tyvar() in
      let newalpa2 = fresh_tyvar() in
      let nenv = tenv_extend (v,newalpa1) tenv in
      let nnenv = tenv_extend (f,TyFun(newalpa1,newalpa2)) nenv in
      (gen_equations nnenv e1 newalpa2) @ (gen_equations nnenv e2 ty)
    |PROC (v,e) -> 
      let newalpa1 = fresh_tyvar() in
      let newalpa2 = fresh_tyvar() in
      [(ty,TyFun(newalpa1,newalpa2))] @ (gen_equations (tenv_extend (v,newalpa1) tenv) e newalpa2)
    |CALL (e1,e2) -> 
      let newalpa = fresh_tyvar() in
      (gen_equations tenv e1 (TyFun(newalpa,ty))) @ (gen_equations tenv e2 newalpa)
      
let switch x y = 
  match x, y with
    |TyVar v, _ -> (x,y)
    |_, TyVar v -> (y,x)
    |_, _ -> (x,y)
        
let solve : typ_eqn -> subst
=fun eqns -> 
  let rec fs subs eqa = 
    (match eqa with 
      |[] -> subs
      |(x',y')::tl -> 
        let nx = subst_apply x' subs in
        let ny = subst_apply y' subs in
        if nx = ny then fs subs tl 
        else
          let (x,y) = switch nx ny in
          (match x,y with 
            |TyVar xv, _ -> 
              let nsub = subst_extend xv y subs in
              fs nsub tl
            |TyFun(a',b'), TyFun(c',d') ->
              let na = subst_apply a' subs in
              let nb = subst_apply b' subs in
              let nc = subst_apply c' subs in
              let nd = subst_apply d' subs in
              let (a,c) = switch na nc in
              let (b,d) = switch nb nd in
              if a = c && b = d then fs subs tl
              else 
                if a = c then 
                (match b,d with
                  |TyVar bv, _ ->
                    let nsub = subst_extend bv d subs in
                    fs nsub tl
                  |_, _ -> raise TypeError)
              else 
                if b = d then
                (match a,c with
                  |TyVar av, _ -> 
                    let nsub = subst_extend av c subs in
                    fs nsub tl
                  |_, _ -> raise TypeError)
              else
                (match a,c,b,d with
                  |TyVar av, _, TyVar bv, _ ->
                    let nsub = subst_extend av c subs in
                    let nnsub = subst_extend bv d nsub in
                    fs nnsub tl
                  |_, _, _, _ -> raise TypeError)
            |_, _ -> raise TypeError)
      |_ -> raise TypeError)
      
  in fs subst_empty eqns

let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty