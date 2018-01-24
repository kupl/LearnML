open Lang
open Util
open Print

type typ_eqn = (typ * typ) list

exception TypeError

let start_time = ref 0.0

(* Convert user_def typ var -> predefined typ *)
module Converter = struct 
  type t = (id, typ) BatMap.t (* env of user_def_typ -> typ*)
  let empty = BatMap.empty
  
  let extend (x, t) env = BatMap.add x t env
  
  let find env x = BatMap.find x env

  let rec print env = BatMap.iter (fun x typ -> print_endline (x ^ " |-> " ^type_to_string typ)) env

  let rec convert_typ : t -> typ -> typ
  = fun env typ ->
    match typ with
    | TBase x -> (try find env x with _ -> typ)
    | TList t -> TList (convert_typ env t)
    | TTuple ts -> TTuple (List.map (convert_typ env) ts)
    | TCtor (t, ts) -> TCtor (convert_typ env t, List.map (convert_typ env) ts)
    | TArr (t1, t2) -> TArr (convert_typ env t1, convert_typ env t2)
    | _ -> typ

  let rec convert_arg : t -> arg -> arg
  = fun env arg ->
    match arg with
    | ArgUnder t -> ArgUnder (convert_typ env t)
    | ArgOne (x, t) -> ArgOne (x, convert_typ env t)
    | ArgTuple xs -> ArgTuple (List.map (convert_arg env) xs)

  let rec convert_args : t -> arg list -> arg list
  = fun env args -> List.map (convert_arg env) args

  let rec convert_exp : t -> exp -> exp
  = fun env exp ->
    match exp with 
    | EList es -> EList (List.map (convert_exp env) es)
    | ETuple es -> ETuple (List.map (convert_exp env) es)
    | ECtor (x, es) -> ECtor (x, List.map (convert_exp env) es)
    | MINUS e -> MINUS (convert_exp env e)
    | NOT e -> NOT (convert_exp env e)
    | ADD (e1, e2) -> ADD (convert_exp env e1, convert_exp env e2)
    | SUB (e1, e2) -> SUB (convert_exp env e1, convert_exp env e2)
    | MUL (e1, e2) -> MUL (convert_exp env e1, convert_exp env e2)
    | DIV (e1, e2) -> DIV (convert_exp env e1, convert_exp env e2) 
    | MOD (e1, e2) -> MOD (convert_exp env e1, convert_exp env e2)
    | OR (e1, e2) -> OR (convert_exp env e1, convert_exp env e2)
    | AND (e1, e2) -> AND (convert_exp env e1, convert_exp env e2)
    | LESS (e1, e2) -> LESS (convert_exp env e1, convert_exp env e2)
    | LARGER (e1, e2) -> LARGER (convert_exp env e1, convert_exp env e2)
    | LESSEQ (e1, e2) -> LESSEQ (convert_exp env e1, convert_exp env e2)
    | LARGEREQ (e1, e2) -> LARGEREQ (convert_exp env e1, convert_exp env e2)
    | EQUAL (e1, e2) -> EQUAL (convert_exp env e1, convert_exp env e2)
    | NOTEQ (e1, e2) -> NOTEQ (convert_exp env e1, convert_exp env e2)
    | AT (e1, e2) -> AT (convert_exp env e1, convert_exp env e2)
    | DOUBLECOLON (e1, e2) -> DOUBLECOLON (convert_exp env e1, convert_exp env e2)
    | IF (e1, e2, e3) -> IF (convert_exp env e1, convert_exp env e2, convert_exp env e3)
    | ELet (f, is_rec, args, typ, e1, e2) -> ELet (f, is_rec, convert_args env args, convert_typ env typ, convert_exp env e1, convert_exp env e2)
    | EFun (arg, e) -> EFun (convert_arg env arg, convert_exp env e)
    | EApp (e1, e2) -> EApp (convert_exp env e1, convert_exp env e2)
    | EMatch (e, bs) ->
      let (ps, es) = List.split bs in
      EMatch (convert_exp env e, List.combine ps (List.map (convert_exp env) es))
    | Raise e -> Raise (convert_exp env e)
    | _ -> exp

  let rec convert_ctor : t -> ctor -> ctor
  = fun env (x, ts) -> (x, List.map (convert_typ env) ts)

  let rec convert_decl : t -> decl -> (t * decl)
  = fun env decl ->
    match decl with  
    | DExcept(x, ts) -> (env, DExcept (x, List.map (convert_typ env) ts))
    | DEqn (x, typ) -> (extend (x, typ) env, DEqn (x, typ))
    | DData (x, ctors) -> (env, DData (x, List.map (convert_ctor env) ctors))
    | DLet (f, is_rec, args, typ, exp) -> (env, DLet (f, is_rec, convert_args env args, convert_typ env typ, convert_exp env exp))
    | DBlock (is_rec, bindings) ->
      let bindings = List.map (fun (f, is_rec, args, typ, exp) -> (f, is_rec, convert_args env args, convert_typ env typ, convert_exp env exp)) bindings in
      (env, DBlock (is_rec, bindings))
    | TBlock decls -> 
      let env = List.fold_left extend_type_decls env decls in
      let decls = List.map (fun decl -> snd (convert_decl env decl)) decls in
      (env, TBlock decls)

  and extend_type_decls : t -> decl -> t
  = fun env decl ->
    match decl with
    | DEqn (x, typ) -> extend (x,typ) env
    | DData (x, ctors) -> env
    | _ -> raise (Failure "Invalid Type declaration")

  let rec convert : t -> prog -> prog
  = fun env decls -> 
    match decls with      
    | [] -> []
    | decl::decls -> let (t, decl) = convert_decl env decl in decl::(convert t decls)
end

(* type environment : var -> type *)
module TEnv = struct
  type t = (id, typ) BatMap.t
  let empty = BatMap.empty
  let extend : id * typ -> t ->t 
  = fun (x,t) tenv -> BatMap.add x t tenv
  let find tenv x = BatMap.find x tenv
  let rec print tenv = 
    BatMap.iter (fun id typ -> 
      print_endline(id^"|->"^(type_to_string typ))) tenv
end

(* substitution *)
module Subst = struct
  type t = (id * typ) list
  let empty = []
  let find x subst = List.assoc x subst

  (* walk through the type, replacing each type variable by its binding in the substitution *)
  let rec apply : typ -> t -> typ
  = fun typ subst ->
    match typ with
    | TList t -> TList (apply t subst)
    | TTuple l ->  TTuple (apply_to_list l subst)
    | TCtor (t1, l) -> TCtor (apply t1 subst, apply_to_list l subst)
    | TArr (t1,t2) -> TArr (apply t1 subst, apply t2 subst)
    | TVar x -> (try find x subst with _ -> typ)
    |_ -> typ

  and apply_to_list : typ list -> t -> typ list
  = fun l subst -> list_map (fun t -> apply t subst) l
  (* add a binding (tv,ty) to the subsutition and propagate the information *)
  let extend tv ty subst = 
    (tv,ty) :: (List.map (fun (x,t) -> (x, apply t [(tv,ty)])) subst)

  let print : t -> unit
  = fun subst -> 
      List.iter (fun (x,ty) -> print_endline (x ^ " |-> " ^ type_to_string ty)) subst
end

(* hole -> type table *)
module HoleType = struct
  type t = (int, typ) BatMap.t
  
  let empty = BatMap.empty 
  let extend : int -> typ -> t -> t
  = fun hole typ env -> BatMap.add hole typ env
  
  let update : Subst.t -> t -> t
  = fun subst env -> BatMap.map (fun typ -> Subst.apply typ subst) env 

  let print : t -> unit
  = fun env -> BatMap.iter (fun num typ -> print_endline(string_of_int num ^ " -> " ^ type_to_string typ)) env
end

(* hole -> variable type table *)
module VariableType = struct
  type t = (int,TEnv.t) BatMap.t
  let empty = BatMap.empty
  let extend : int -> TEnv.t -> t -> t
  = fun hole env varenv -> BatMap.add hole env varenv

  let update : Subst.t -> t -> t
  = fun subst varenv -> BatMap.map (fun tenv -> 
      BatMap.map (fun typ -> Subst.apply typ subst) tenv
    ) varenv

  let print : t -> unit
  = fun varenv -> BatMap.iter (fun num env ->print_endline(string_of_int num);TEnv.print env ) varenv
end

(* Support functions *)
let rec bind_arg : TEnv.t -> arg -> TEnv.t
= fun tenv arg ->
  match arg with
  | ArgUnder t -> tenv
  | ArgOne (x, t) -> TEnv.extend (x, t) tenv
  | ArgTuple xs -> List.fold_left bind_arg tenv xs

let rec bind_args : TEnv.t -> arg list -> TEnv.t
= fun tenv args -> List.fold_left bind_arg tenv args

let rec type_of_arg : arg -> typ
= fun arg ->
  match arg with
  | ArgUnder t | ArgOne (_, t) -> t
  | ArgTuple xs -> TTuple (List.map type_of_arg xs)

let rec type_of_fun : arg list -> typ -> typ
= fun args typ ->
  match args with
  | [] -> typ
  | hd::tl -> TArr (type_of_arg hd, type_of_fun tl typ)

let rec let_binding : TEnv.t -> let_bind -> typ -> TEnv.t
= fun tenv x typ->
  match (x, typ) with
  | BindOne x, _ -> TEnv.extend (x, typ) tenv
  | BindTuple xs, TTuple ts -> (try List.fold_left2 (let_binding) tenv xs ts with _ -> raise TypeError)
  | BindTuple xs, TVar _ -> List.fold_left (
    fun env x -> 
      let t = fresh_tvar () in
      let_binding env x t
    ) tenv xs
  | _ -> raise TypeError

(* construct type eqn of patterns then bind type variablese *)
let rec gen_pat_equations : (TEnv.t * typ_eqn) -> pat -> typ -> (TEnv.t * typ_eqn)
= fun (tenv, eqn) pat ty ->
  match pat with
  | PUnit -> (tenv, (ty, TUnit)::eqn)
  | PInt n -> (tenv, (ty, TInt)::eqn)
  | PBool b -> (tenv, (ty, TBool)::eqn)
  | PVar x -> 
    let t = fresh_tvar () in
    (TEnv.extend (x, t) tenv, (ty, t)::eqn)
  | PList ps ->
    let t = fresh_tvar () in
    gen_pat_list_equations (tenv, (ty, TList t)::eqn) ps t
  | PTuple ps ->
    let (env, eqn, ts) = 
      List.fold_left (
        fun (env, eqn, ts) pat ->
          let t = fresh_tvar () in
          let (env, eqn) = gen_pat_equations (env, eqn) pat t in
          (env, eqn, ts@[t])
      ) (tenv, eqn, []) ps
    in
    (env, (ty, TTuple ts)::eqn)
  | PCtor (x, ps) ->
    let tctor = TEnv.find tenv x in
    begin match tctor with
    | TCtor (t_base, ts) -> List.fold_left2 (fun (env, eqn) pat typ -> gen_pat_equations (env, eqn) pat typ) (tenv, (ty, t_base)::eqn) ps ts
    | _ -> raise TypeError
    end
  | PCons ps ->
    let t = fresh_tvar () in
    gen_pat_cons_equations (tenv, [ty, TList t]) ps t
  | PUnder -> (tenv, (ty, fresh_tvar ())::eqn)
  | Pats ps -> gen_pat_list_equations (tenv, eqn) ps ty

and gen_pat_list_equations : (TEnv.t * typ_eqn) -> pat list -> typ -> (TEnv.t * typ_eqn)
= fun (tenv, eqn) ps typ -> 
  List.fold_left (fun (tenv, eqn) pat -> gen_pat_equations (tenv, eqn) pat typ) (tenv, eqn) ps 

and gen_pat_cons_equations : (TEnv.t * typ_eqn) -> pat list -> typ -> (TEnv.t * typ_eqn)
= fun (tenv, eqn) ps typ ->
  match ps with
  | [] -> raise (Failure "Pattern cons does not have args")
  | [p] -> gen_pat_equations (tenv, eqn) p (TList typ)
  | hd::tl ->
    let (env, eqn) = gen_pat_equations (tenv, eqn) hd typ in
    gen_pat_cons_equations (env, eqn) tl typ

(* Construct type equations of expressions *)
let rec gen_equations : HoleType.t -> VariableType.t -> TEnv.t -> exp -> typ -> (typ_eqn * HoleType.t * VariableType.t)
= fun hole_typ var_typ tenv exp ty ->
  match exp with
  | EUnit -> ([ty, TUnit], hole_typ, var_typ)
  | Const n -> ([(ty, TInt)],hole_typ,var_typ)
  | TRUE | FALSE -> ([(ty, TBool)],hole_typ,var_typ)
  | String str -> ([(ty, TString)],hole_typ,var_typ)
  | EVar x -> ([(ty, TEnv.find tenv x)],hole_typ,var_typ)
  | EList es -> 
    let t = fresh_tvar () in
    list_fold (fun exp (eqns,h_t,v_t)->
      let (e,h,v) = gen_equations h_t v_t tenv exp t in
      (eqns@e,h,v)
    ) es ([ty,TList t],hole_typ,var_typ) 
  | ETuple es ->
    let (eqns,ts,hole_typ,var_typ) = list_fold (fun exp (eqn,ts,h_t,v_t)->
      let t = fresh_tvar() in
      let (e,h,v) = gen_equations h_t v_t tenv exp t in
      (eqn@e,ts@[t],h,v)
    ) es ([],[],hole_typ,var_typ) in
    ((ty,TTuple ts)::eqns,hole_typ,var_typ)
  | ECtor (x, es) -> 
    let tctor = TEnv.find tenv x in
    begin match tctor with
    | TCtor (t_base, ts) -> list_fold(fun (exp,t) (eqns,h_t,v_t)->
        let (e,h,v) = gen_equations h_t v_t tenv exp t in
        (eqns@e,h,v)
    ) (list_combine es ts) ([ty,t_base],hole_typ,var_typ)
    | _ -> raise TypeError
    end
  | MINUS e -> 
    let (eqns,hole_typ,var_typ) = gen_equations hole_typ var_typ tenv e TInt in 
    ((ty,TInt)::eqns,hole_typ,var_typ)
  | NOT e -> 
    let (eqns,hole_typ,var_typ) = gen_equations hole_typ var_typ tenv e TBool in 
    ((ty,TBool)::eqns,hole_typ,var_typ)
  | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2) -> 
    let (eqns,hole_typ,var_typ) = gen_equations hole_typ var_typ tenv e1 TInt in
    let (eqns',hole_typ,var_typ) = gen_equations hole_typ var_typ tenv e2 TInt in
    ((ty,TInt)::(eqns@eqns'),hole_typ,var_typ)
  | OR (e1, e2) | AND (e1, e2) -> 
    let (eqns,hole_typ,var_typ) = gen_equations hole_typ var_typ tenv e1 TBool in
    let (eqns',hole_typ,var_typ) = gen_equations hole_typ var_typ tenv e2 TBool in
    ((ty,TBool)::(eqns@eqns'),hole_typ,var_typ) 
  | LESS (e1, e2) | LARGER (e1, e2) | LESSEQ (e1, e2) | LARGEREQ (e1, e2) -> 
    let (eqns,hole_typ,var_typ) = gen_equations hole_typ var_typ tenv e1 TInt in
    let (eqns',hole_typ,var_typ) = gen_equations hole_typ var_typ tenv e2 TInt in
    ((ty,TBool)::(eqns@eqns'),hole_typ,var_typ) 
  | EQUAL (e1, e2) | NOTEQ (e1, e2) -> 
    let t = fresh_tvar () in
    let (eqns,hole_typ,var_typ) = gen_equations hole_typ var_typ tenv e1 t in
    let (eqns',hole_typ,var_typ) = gen_equations hole_typ var_typ tenv e2 t in
    ((ty,TBool)::(eqns@eqns'),hole_typ,var_typ) 
  | AT (e1, e2) ->
    let t = TList (fresh_tvar ()) in
    let (eqns,hole_typ,var_typ) = gen_equations hole_typ var_typ tenv e1 t in
    let (eqns',hole_typ,var_typ) = gen_equations hole_typ var_typ tenv e2 t in
    ((ty,t)::(eqns@eqns'),hole_typ,var_typ) 
  | DOUBLECOLON (e1, e2) ->
    let t = fresh_tvar () in
    let (eqns,hole_typ,var_typ) = gen_equations hole_typ var_typ tenv e1 t in
    let (eqns',hole_typ,var_typ) = gen_equations hole_typ var_typ tenv e2 (TList t) in
    ((ty,TList t)::(eqns@eqns'),hole_typ,var_typ) 
  | STRCON (e1,e2) ->
    let (eqns,hole_typ,var_typ) = gen_equations hole_typ var_typ tenv e1 TString in
    let (eqns',hole_typ,var_typ) = gen_equations hole_typ var_typ tenv e2 TString in
    ((ty,TString)::(eqns@eqns'),hole_typ,var_typ)
  | IF (e1, e2, e3) -> 
    let (eqns,hole_typ,var_typ) = gen_equations hole_typ var_typ tenv e1 TBool in
    let (eqns',hole_typ,var_typ) = gen_equations hole_typ var_typ tenv e2 ty in
    let (eqns'',hole_typ,var_typ) = gen_equations hole_typ var_typ tenv e3 ty in
    (eqns@eqns'@eqns'',hole_typ,var_typ)
  | ELet (f, is_rec, args, typ, e1, e2) ->
    let (func_typ, args_env) = (type_of_fun args typ, bind_args tenv args) in
    let tenv1 = if is_rec then let_binding args_env f func_typ else args_env in
    let tenv2 = let_binding tenv f func_typ in
    let (eqns,hole_typ,var_typ) = gen_equations hole_typ var_typ tenv1 e1 typ in
    let (eqns',hole_typ,var_typ) = gen_equations hole_typ var_typ tenv2 e2 ty in
    ((eqns@eqns'),hole_typ,var_typ)
  | EBlock (is_rec, bindings, e2) ->
    (* initialize tenv *)
    let tenv = 
      if is_rec then
        List.fold_left (fun tenv (f, is_rec, args, typ, exp) -> 
          let ty = type_of_fun args typ in
          let_binding tenv f ty
        ) tenv bindings
      else tenv
    in
    (* gen each (eqn, hole_typ, var_typ) of decls *)
    let results = List.map (fun (f, is_rec, args, typ, exp) -> 
      let exp = ELet(f, is_rec, args, typ, exp, let_to_exp f) in
      let ty = type_of_fun args typ in
      gen_equations hole_typ var_typ tenv exp ty
    ) bindings in 
    (* merge all results together *)
    let (eqn, hole_typ, var_typ) = List.fold_left (fun (eqn, hole_typ, var_typ) (eqn', hole_typ', var_typ') -> 
      (eqn@eqn', BatMap.union hole_typ hole_typ', BatMap.union var_typ var_typ')
    ) ([], hole_typ, var_typ) results in 
    (* binding each decl with result *)
    let tenv = List.fold_left (fun tenv (f, is_rec, args, typ, exp) -> 
      let ty = type_of_fun args typ in
      let_binding tenv f ty
    ) tenv bindings in
    (* gen eqn of e2 *)
    let (eqn', hole_typ, var_typ) = gen_equations hole_typ var_typ tenv e2 ty in
    (eqn@eqn', hole_typ, var_typ)
  | EFun (arg, e) ->
    let t1 = type_of_arg arg in
    let t2 = fresh_tvar () in
    let tenv = bind_arg tenv arg in
    let (eqns,hole_typ,var_typ) = gen_equations hole_typ var_typ tenv e t2 in
    ((ty,TArr (t1,t2))::eqns,hole_typ,var_typ)
  | EApp (e1, e2) ->
    let t = fresh_tvar () in
    let (eqns,hole_typ,var_typ) = gen_equations hole_typ var_typ tenv e1 (TArr (t,ty)) in
    let (eqns',hole_typ,var_typ) = gen_equations hole_typ var_typ tenv e2 t in
    (eqns@eqns',hole_typ,var_typ)
  | EMatch (e, bs) ->
    let typ_pat = fresh_tvar () in
    let typ_exp = fresh_tvar () in
    let (eqns, hole_typ, var_typ) = gen_equations hole_typ var_typ tenv e typ_pat in
    (* Inference each branches *)
    let results = 
      List.map (fun (pat, exp) ->
        let (tenv, pat_eqn) = gen_pat_equations (tenv, eqns) pat typ_pat in
        let (exp_eqn, hole_typ, var_typ) = gen_equations hole_typ var_typ tenv exp typ_exp in
        (pat_eqn@exp_eqn, hole_typ, var_typ)
      ) bs
    in
    (* Merge them together *)
    List.fold_left (fun (eqn, hole_typ, var_typ) (eqn', hole_typ', var_typ') -> 
      (eqn@eqn', BatMap.union hole_typ hole_typ', BatMap.union var_typ var_typ')
    ) (eqns, hole_typ, var_typ) results
  | Hole n ->
    let t = fresh_tvar () in
    let hole_typ = HoleType.extend n t hole_typ in
    let var_typ = VariableType.extend n tenv var_typ in
    ([(ty,t)],hole_typ,var_typ)
  | Raise e ->
    gen_equations hole_typ var_typ tenv e TExn

(* Unification *)
let rec extract_tvar : id -> typ -> bool
= fun x t ->
  match t with
  | TList t -> extract_tvar x t
  | TTuple l | TCtor (_, l) -> extract_tvar2 x l
  | TArr (t1, t2) -> (extract_tvar x t1) || (extract_tvar x t2)
  | TVar y -> x = y
  | _ -> false

and extract_tvar2 : id -> typ list -> bool
= fun x lst -> List.exists (fun t -> extract_tvar x t) lst

let rec unify : Subst.t -> (typ * typ) -> Subst.t
= fun subst (t1, t2) ->
  if t1 = t2 then subst else
  match t1, t2 with
  | TList t1, TList t2 -> unify subst (t1, t2)
  | TTuple ts1, TTuple ts2 -> unify_list subst (ts1, ts2)
  | TCtor (x1, ts1), TCtor (x2, ts2) -> if x1 <> x2 then raise TypeError else unify_list subst (ts1, ts2)
  | TArr (t1, t2), TArr (t1', t2') ->
    let subst' = unify subst (t1, t1') in
    let (t2, t2') = (Subst.apply t2 subst', Subst.apply t2' subst') in
    unify subst' (t2, t2')
  | TVar x, t ->
    (*if x occurs in t recursively, raise type error*)
    begin match t with
    | TVar y -> if x = y then subst else Subst.extend x t subst
    | TList ts -> if extract_tvar x ts then raise TypeError else Subst.extend x t subst
    | TTuple ts -> if extract_tvar2 x ts then raise TypeError else Subst.extend x t subst
    | TCtor (id, ts) -> if extract_tvar2 x ts then raise TypeError else Subst.extend x t subst
    | TArr (t1, t2) -> if ((extract_tvar x t1) || (extract_tvar x t2)) then raise TypeError else Subst.extend x t subst
    | _ -> Subst.extend x t subst
    end
  | t, TVar x -> unify subst (TVar x, t)
  | _ -> raise TypeError (* type base *) 

and unify_list : Subst.t -> (typ list * typ list) -> Subst.t
= fun subst (ts1, ts2) -> List.fold_left2 (fun subst t1 t2 -> unify subst (t1, t2)) subst ts1 ts2

(* Generate Substitution, HoleType Table, VarType Table using Type equations *)
let solve : (typ_eqn * HoleType.t * VariableType.t) -> (Subst.t * HoleType.t * VariableType.t)
= fun (eqns, hole_typ, var_typ) ->
  let subst = List.fold_left (fun subst (t1, t2) -> unify subst ((Subst.apply t1 subst), Subst.apply t2 subst)) Subst.empty eqns in
  let hole_typ = HoleType.update subst hole_typ in
  let var_typ = VariableType.update subst var_typ in
  (subst, hole_typ, var_typ)

let rec type_decl : (TEnv.t * HoleType.t * VariableType.t) -> decl -> (TEnv.t * HoleType.t * VariableType.t)
= fun (tenv, hole_typ, var_typ) decl ->
  match decl with
  | DExcept(id, typ) ->  (TEnv.extend (id, TCtor (TExn, typ)) tenv, hole_typ, var_typ)
  | DEqn (x, typ) -> (tenv, hole_typ, var_typ)
  | DData (id, ctors) -> let tbase = TBase id in (ctors_to_env tenv tbase ctors, hole_typ, var_typ)
  | DLet (f, is_rec, args, typ, exp) ->
    let exp =
      begin match f with
      | BindUnder -> exp
      | _ -> ELet(f, is_rec, args, typ, exp, let_to_exp f)
      end
    in
    let ty = type_of_fun args typ in
    let (eqns, hole_typ, var_typ) = gen_equations hole_typ var_typ tenv exp ty in
    let (subst, hole_typ, var_typ) = solve (eqns, hole_typ, var_typ) in
    let ty = Subst.apply ty subst in
    (let_binding tenv f ty, hole_typ, var_typ)
  | DBlock (is_rec, bindings) ->
    (* initialize tenv *)
    let tenv = 
      if is_rec then
        List.fold_left (fun tenv (f, is_rec, args, typ, exp) -> 
          let ty = type_of_fun args typ in
          let_binding tenv f ty
        ) tenv bindings
      else tenv
    in
    (* gen each (eqn, hole_typ, var_typ) of decls *)
    let results = List.map (fun (f, is_rec, args, typ, exp) -> 
      let exp = ELet(f, is_rec, args, typ, exp, let_to_exp f) in
      let ty = type_of_fun args typ in
      gen_equations hole_typ var_typ tenv exp ty
    ) bindings in 
    (* merge all results together then solve it *)
    let (eqn, hole_typ, var_typ) = List.fold_left (fun (eqn, hole_typ, var_typ) (eqn', hole_typ', var_typ') -> 
      (eqn@eqn', BatMap.union hole_typ hole_typ', BatMap.union var_typ var_typ')
    ) ([], hole_typ, var_typ) results in 
    let (subst, hole_typ, var_typ) = solve (eqn, hole_typ, var_typ) in
    (* binding each decl with result *)
    let tenv = List.fold_left (fun tenv (f, is_rec, args, typ, exp) -> 
      let ty = type_of_fun args typ in
      let_binding tenv f (Subst.apply ty subst)
    ) tenv bindings in
    (tenv, hole_typ, var_typ)
  | TBlock decls -> List.fold_left (type_decl) (tenv, hole_typ, var_typ) decls

and ctors_to_env : TEnv.t -> typ -> ctor list -> TEnv.t
= fun tenv tbase ctors -> List.fold_left (fun env (x, typs) -> TEnv.extend (x, TCtor (tbase, typs)) env) tenv ctors

let run : prog -> (TEnv.t * HoleType.t * VariableType.t)
= fun decls -> 
  let _ = start_time:=Sys.time() in
  let decls = decls@(External.grading_prog) in
  let decls = Converter.convert Converter.empty decls in
  let (init_env, _, _) = List.fold_left type_decl (TEnv.empty, HoleType.empty, VariableType.empty) (External.init_prog) in
  let (tenv, hole_typ, var_typ) = List.fold_left type_decl (init_env, HoleType.empty, VariableType.empty) decls in
  (BatMap.diff tenv init_env, hole_typ, BatMap.map (fun tenv -> BatMap.diff tenv init_env) var_typ)
