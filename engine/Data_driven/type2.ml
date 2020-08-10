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
    | TRef t -> TRef (convert_typ env t)
    | _ -> typ

  let rec convert_arg : t -> arg -> arg
  = fun env arg ->
    match arg with
    | ArgUnder t -> ArgUnder (convert_typ env t)
    | ArgOne (x, t) -> ArgOne (x, convert_typ env t)
    | ArgTuple xs -> ArgTuple (List.map (convert_arg env) xs)

  let rec convert_args : t -> arg list -> arg list
  = fun env args -> List.map (convert_arg env) args

  let rec convert_exp : t -> lexp -> lexp
  = fun env (l,exp) ->
    let exp = 
      begin match exp with 
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
      | EBlock (is_rec, bindings, e2) -> EBlock (is_rec, List.map (fun (f, is_rec, args, typ, e) -> f, is_rec, convert_args env args, convert_typ env typ, convert_exp env e) bindings, convert_exp env e2)
      | EFun (arg, e) -> EFun (convert_arg env arg, convert_exp env e)
      | EApp (e1, e2) -> EApp (convert_exp env e1, convert_exp env e2)
      | EMatch (e, bs) ->
        let (ps, es) = List.split bs in
        EMatch (convert_exp env e, List.combine ps (List.map (convert_exp env) es))
      | Raise e -> Raise (convert_exp env e)
      | ERef e -> ERef (convert_exp env e)
      | EDref e -> EDref (convert_exp env e)
      | EAssign (e1, e2) -> EAssign (convert_exp env e1, convert_exp env e2)
      | _ -> exp
      end in
    (l,exp)

  let rec convert_ctor : t -> ctor -> ctor
  = fun env (x, ts) -> (x, List.map (convert_typ env) ts)

  let rec convert_decl : t -> decl -> (t * decl)
  = fun env decl ->
    match decl with  
    | DExcept(x, ts) -> (env, DExcept (x, List.map (convert_typ env) ts))
    | DEqn (x, typ) -> (extend (x, convert_typ env typ) env, DEqn (x, convert_typ env typ))
    | DData (x, ctors) -> (env, DData (x, List.map (convert_ctor env) ctors))
    | DLet (f, is_rec, args, typ, exp) -> (env, DLet (f, is_rec, convert_args env args, convert_typ env typ, convert_exp env exp))
    | DBlock (is_rec, bindings) ->
      let bindings = List.map (fun (f, is_rec, args, typ, exp) -> (f, is_rec, convert_args env args, convert_typ env typ, convert_exp env exp)) bindings in
      (env, DBlock (is_rec, bindings))
    | TBlock decls -> 
      let env = List.fold_left extend_type_decls env decls in
      let env = fix (fun env -> List.fold_left extend_type_decls env decls) env in
      let decls = List.map (fun decl -> snd (convert_decl env decl)) decls in
      (env, TBlock decls)

  and extend_type_decls : t -> decl -> t
  = fun env decl ->
    match decl with
    | DEqn (x, typ) -> extend (x, convert_typ env typ) env
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
  let find tenv x = try BatMap.find x tenv with Not_found -> raise (Failure ("TEnv : " ^ x ^ " is not found!"))
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
    | TRef t -> TRef (apply t subst)
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

(* label -> type table *)
module LocType = struct
  type t = (label, typ) BatMap.t
  
  let empty = BatMap.empty 

  let extend : label -> typ -> t -> t
  = fun l typ t -> BatMap.add l typ t
  
  let update : Subst.t -> t -> t
  = fun subst t -> BatMap.map (fun typ -> Subst.apply typ subst) t

  let print : t -> unit
  = fun t -> BatMap.iter (fun num typ -> print_endline(string_of_int num ^ " -> " ^ type_to_string typ)) t
end

(* label -> variable type table *)
module LocEnv = struct
  type t = (int, TEnv.t) BatMap.t

  let empty = BatMap.empty

  let find : label -> t -> TEnv.t
  = fun l t -> BatMap.find l t 

  let extend : label -> TEnv.t -> t -> t
  = fun l env t -> BatMap.add l env t

  let update : Subst.t -> t -> t
  = fun subst t -> BatMap.map (fun tenv -> 
      BatMap.map (fun typ -> Subst.apply typ subst) tenv
    ) t

  let print : t -> unit
  = fun t -> BatMap.iter (fun num env ->print_endline(string_of_int num);TEnv.print env ) t
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

let rec update_arg_type : arg -> typ -> arg
= fun arg t ->
  match arg with
  | ArgUnder _ -> ArgUnder t
  | ArgOne (x, _) -> ArgOne (x,t)
  | ArgTuple xs -> 
    begin match t with
    | TTuple l -> ArgTuple(List.map2 update_arg_type xs l)
    | _ -> arg
    end

let rec let_binding : TEnv.t -> let_bind -> typ -> TEnv.t
= fun tenv x typ->
  match (x, typ) with
  | BindUnder, _ -> tenv
  | BindOne x, _ -> TEnv.extend (x, typ) tenv
  | BindTuple xs, TTuple ts -> (try List.fold_left2 (let_binding) tenv xs ts with _ -> raise TypeError)
  | BindTuple xs, TVar _ -> List.fold_left (
    fun env x -> 
      let t = fresh_tvar () in
      let_binding env x t
    ) tenv xs
  | _ -> raise TypeError

(* construct type eqn of patterns then bind type variables *)
let rec gen_pat_equations : TEnv.t -> pat -> typ -> (TEnv.t * typ_eqn)
= fun tenv pat ty ->
  match pat with
  | PUnit -> (tenv, [ty, TUnit])
  | PInt n -> (tenv, [ty, TInt])
  | PBool b -> (tenv, [ty, TBool])
  | PVar x -> 
    let t = fresh_tvar () in
    (TEnv.extend (x, t) tenv, [ty, t])
  | PList ps ->
    let t = fresh_tvar () in
    List.fold_left (fun (tenv, eqns) p -> 
      let (tenv, eqn) = gen_pat_equations tenv p t in
      (tenv, eqn@eqns)
    ) (tenv, [ty, TList t]) ps 
  | PTuple ps ->
    let ts = List.map (fun p -> fresh_tvar ()) ps in
    List.fold_left2 (fun (tenv, eqns) p t ->
      let (tenv, eqn) = gen_pat_equations tenv p t in
      (tenv, eqn@eqns)
    ) (tenv, [ty, TTuple ts]) ps ts 
  | PCtor (x, ps) ->
    let c_typ = TEnv.find tenv x in
    begin match c_typ with
    | TCtor (t_name, ts) -> 
      List.fold_left2 (fun (tenv, eqns) p t -> 
        let (tenv, eqn) = gen_pat_equations tenv p t in
        (tenv, eqn@eqns)
      ) (tenv, [ty, t_name]) ps ts
    | _ -> raise TypeError
    end
  | PCons ps -> 
    let t = fresh_tvar () in
    begin match ps with
    | [] -> raise (Failure "Pattern cons does not have args")
    | [p] -> 
      let (tenv, eqns) = gen_pat_equations tenv p (TList t) in
      (tenv, (ty, TList t)::eqns)
    | phd::tl -> 
      let (tenv, eqn1) = gen_pat_equations tenv phd t in
      let (tenv, eqn2) = gen_pat_equations tenv (PCons tl) (TList t) in
      (tenv, (ty, TList t)::(eqn1@eqn2))
    end
  | PUnder -> (tenv, [ty, fresh_tvar ()])
  | Pats ps -> 
    List.fold_left (fun (tenv, eqns) p -> 
      let (tenv, eqn) = gen_pat_equations tenv p ty in
      (tenv, eqn@eqns)
    ) (tenv, []) ps 

(* Construct type equations of expressions *)
let rec gen_equations : LocType.t -> LocEnv.t -> TEnv.t -> lexp -> typ -> (typ_eqn * LocType.t * LocEnv.t)
= fun l_t l_env tenv (l, exp) ty ->
  (* let _ = print_endline (exp_to_string (l, exp)) in *)
  match exp with
  | EUnit -> ([ty, TUnit], LocType.extend l TUnit l_t, LocEnv.extend l tenv l_env)
  | SInt n | Const n -> ([(ty, TInt)], LocType.extend l TInt l_t, LocEnv.extend l tenv l_env)
  | TRUE | FALSE -> ([(ty, TBool)], LocType.extend l TBool l_t, LocEnv.extend l tenv l_env)
  | SStr _ -> ([(ty, TString)], LocType.extend l TString l_t, LocEnv.extend l tenv l_env)
  | String str -> ([(ty, TString)], LocType.extend l TString l_t, LocEnv.extend l tenv l_env)
  | EVar x -> 
    let tyx = TEnv.find tenv x in
    ([(ty, tyx)], LocType.extend l tyx l_t, LocEnv.extend l tenv l_env)
  | EList es -> 
    let t = fresh_tvar () in
    List.fold_left (fun (eqns, l_t, l_env) e ->
      let (eqn, l_t, l_env) = gen_equations l_t l_env tenv e t in
      (eqn@eqns, l_t, l_env)
    ) ([ty, TList t], LocType.extend l (TList t) l_t, LocEnv.extend l tenv l_env) es 
  | ETuple es ->
    let ts = List.map (fun e -> fresh_tvar ()) es in
    List.fold_left2 (fun (eqns, l_t, l_env) e t ->
      let (eqn, l_t, l_env) = gen_equations l_t l_env tenv e t in
      (eqn@eqns, l_t, l_env)
    ) ([ty, TTuple ts], LocType.extend l (TTuple ts) l_t, LocEnv.extend l tenv l_env) es ts
  | ECtor (c, es) -> 
    let c_typ = TEnv.find tenv c in
    begin match c_typ with
    | TCtor (t_name, ts) -> 
      List.fold_left2 (fun (eqns, l_t, l_env) e t ->
        let (eqn, l_t, l_env) = gen_equations l_t l_env tenv e t in
        (eqn@eqns, l_t, l_env)
      ) ([ty, t_name], LocType.extend l t_name l_t, LocEnv.extend l tenv l_env) es ts
    | _ -> raise TypeError
    end
  | MINUS e -> 
    let (eqns, l_t, l_env) = gen_equations l_t l_env tenv e TInt in 
    ((ty, TInt)::eqns, LocType.extend l TInt l_t, LocEnv.extend l tenv l_env)
  | NOT e -> 
    let (eqns, l_t, l_env) = gen_equations l_t l_env tenv e TBool in 
    ((ty, TBool)::eqns, LocType.extend l TBool l_t, LocEnv.extend l tenv l_env)
  | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2) -> 
    let (eqns1, l_t, l_env) = gen_equations l_t l_env tenv e1 TInt in
    let (eqns2, l_t, l_env) = gen_equations l_t l_env tenv e2 TInt in
    ((ty, TInt)::(eqns1@eqns2), LocType.extend l TInt l_t, LocEnv.extend l tenv l_env)
  | OR (e1, e2) | AND (e1, e2) -> 
    let (eqns1, l_t, l_env) = gen_equations l_t l_env tenv e1 TBool in
    let (eqns2, l_t, l_env) = gen_equations l_t l_env tenv e2 TBool in
    ((ty, TBool)::(eqns1@eqns2), LocType.extend l TBool l_t, LocEnv.extend l tenv l_env) 
  | LESS (e1, e2) | LARGER (e1, e2) | LESSEQ (e1, e2) | LARGEREQ (e1, e2) -> 
    let (eqns1, l_t, l_env) = gen_equations l_t l_env tenv e1 TInt in
    let (eqns2, l_t, l_env) = gen_equations l_t l_env tenv e2 TInt in
    ((ty, TBool)::(eqns1@eqns2), LocType.extend l TBool l_t, LocEnv.extend l tenv l_env) 
  | EQUAL (e1, e2) | NOTEQ (e1, e2) -> 
    let t = fresh_tvar () in
    let (eqns1, l_t, l_env) = gen_equations l_t l_env tenv e1 t in
    let (eqns2, l_t, l_env) = gen_equations l_t l_env tenv e2 t in
    ((ty, TBool)::(eqns1@eqns2), LocType.extend l TBool l_t, LocEnv.extend l tenv l_env) 
  | AT (e1, e2) ->
    let t = TList (fresh_tvar ()) in
    let (eqns1, l_t, l_env) = gen_equations l_t l_env tenv e1 t in
    let (eqns2, l_t, l_env) = gen_equations l_t l_env tenv e2 t in
    ((ty, t)::(eqns1@eqns2), LocType.extend l t l_t, LocEnv.extend l tenv l_env) 
  | DOUBLECOLON (e1, e2) ->
    let t = fresh_tvar () in
    let (eqns1, l_t, l_env) = gen_equations l_t l_env tenv e1 t in
    let (eqns2, l_t, l_env) = gen_equations l_t l_env tenv e2 (TList t) in
    ((ty, TList t)::(eqns1@eqns2), LocType.extend l (TList t) l_t, LocEnv.extend l tenv l_env) 
  | STRCON (e1,e2) ->
    let (eqns1, l_t, l_env) = gen_equations l_t l_env tenv e1 TString in
    let (eqns2, l_t, l_env) = gen_equations l_t l_env tenv e2 TString in
    ((ty, TString)::(eqns1@eqns2), LocType.extend l TString l_t, LocEnv.extend l tenv l_env)
  | IF (e1, e2, e3) -> 
    let (eqns1, l_t, l_env) = gen_equations l_t l_env tenv e1 TBool in
    let (eqns2, l_t, l_env) = gen_equations l_t l_env tenv e2 ty in
    let (eqns3, l_t, l_env) = gen_equations l_t l_env tenv e3 ty in
    (eqns1@eqns2@eqns3, LocType.extend l ty l_t, LocEnv.extend l tenv l_env)
  | ELet (f, is_rec, args, typ, e1, e2) ->
    let (func_typ, args_env) = (type_of_fun args typ, bind_args tenv args) in
    let tenv1 = if is_rec then let_binding args_env f func_typ else args_env in
    let tenv2 = let_binding tenv f func_typ in
    let (eqns1, l_t, l_env) = gen_equations l_t l_env tenv1 e1 typ in
    let (eqns2, l_t, l_env) = gen_equations l_t l_env tenv2 e2 ty in
    (eqns1@eqns2, LocType.extend l ty l_t, LocEnv.extend l tenv l_env)
  | EBlock (is_rec, bindings, e2) ->
    (* initialize tenv *)
    let tenv' = List.fold_left (fun tenv (f, is_rec, args, typ, exp) -> 
      let ty_func = type_of_fun args typ in
      let_binding tenv f ty_func
    ) tenv bindings in
    (* gen (eqn, l_t, l_env) of each decl *)
    let (eqns1, l_t, l_env) = List.fold_left (fun (eqns, l_t, l_env) (f, is_rec, args, typ, e) ->
      let tenv = if is_rec then tenv' else tenv in
      let (eqn, l_t, l_env) = gen_equations l_t l_env (bind_args tenv args) e typ in
      (eqn@eqns, l_t, l_env)
    ) ([], l_t, l_env) bindings in
    (* gen eqn of e2 *)
    let (eqns2, l_t, l_env) = gen_equations l_t l_env tenv' e2 ty in
    (eqns1@eqns2, LocType.extend l ty l_t, LocEnv.extend l tenv l_env)
  | EFun (arg, e) ->
    let t1 = type_of_arg arg in
    let t2 = fresh_tvar () in
    let (eqns, l_t, l_env) = gen_equations l_t l_env (bind_arg tenv arg) e t2 in
    ((ty, TArr (t1, t2))::eqns, LocType.extend l (TArr (t1, t2)) l_t, LocEnv.extend l tenv l_env)
  | EApp (e1, e2) ->
    let t = fresh_tvar () in
    let (eqns1, l_t, l_env) = gen_equations l_t l_env tenv e1 (TArr (t, ty)) in
    let (eqns2, l_t, l_env) = gen_equations l_t l_env tenv e2 t in
    (eqns1@eqns2, LocType.extend l ty l_t, LocEnv.extend l tenv l_env)
  | EMatch (e, bs) ->
    let typ_p = fresh_tvar () in
    let typ_e = ty in
    let (eqns, l_t, l_env) = gen_equations l_t l_env tenv e typ_p in
    (* Inference each branches *)
    List.fold_left (fun (eqns, l_t, l_env) (p, e) ->
      let (tenv, pat_eqn) = gen_pat_equations tenv p typ_p in
      let (exp_eqn, l_t, l_env) = gen_equations l_t l_env tenv e typ_e in
      (pat_eqn@exp_eqn@eqns, l_t, l_env)
    ) (eqns, LocType.extend l ty l_t, LocEnv.extend l tenv l_env) bs 
  | Hole n ->
    let t = fresh_tvar () in
    ([ty, t], LocType.extend l t l_t, LocEnv.extend l tenv l_env)
  | Raise e -> 
    let (eqns, l_t, l_env) = gen_equations l_t l_env tenv e TExn in
    (eqns, LocType.extend l ty l_t, LocEnv.extend l tenv l_env)
  | ERef e -> 
    let t = fresh_tvar () in
    let (eqns, l_t, l_env) = gen_equations l_t l_env tenv e t in 
    ((ty, TRef t)::eqns, LocType.extend l (TRef t) l_t, LocEnv.extend l tenv l_env)
  | EDref e -> 
    let t = fresh_tvar () in
    let (eqns, l_t, l_env) = gen_equations l_t l_env tenv e (TRef t) in 
    ((ty, t)::eqns, LocType.extend l t l_t, LocEnv.extend l tenv l_env)
  | EAssign (e1, e2) -> 
    let t = fresh_tvar () in
    let (eqns1, l_t, l_env) = gen_equations l_t l_env tenv e1 (TRef t) in
    let (eqns2, l_t, l_env) = gen_equations l_t l_env tenv e2 t in
    ((ty, TUnit)::(eqns1@eqns2), LocType.extend l TUnit l_t, LocEnv.extend l tenv l_env)
  
let rec print_eqn : typ_eqn -> unit
= fun eqn -> 
  print_endline "----------- Type Equation -----------";
  List.iter (fun (t1, t2) -> print_endline (Print.type_to_string t1 ^ " := " ^ Print.type_to_string t2)) eqn

(* Unification *)
let rec is_exist : id -> typ -> bool
= fun x t ->
  match t with
  | TList t | TRef t -> is_exist x t
  | TTuple l | TCtor (_, l) -> is_exist2 x l
  | TArr (t1, t2) -> (is_exist x t1) || (is_exist x t2)
  | TVar y -> x = y
  | _ -> false

and is_exist2 : id -> typ list -> bool
= fun x lst -> List.exists (fun t -> is_exist x t) lst

let rec unify : Subst.t -> (typ * typ) -> Subst.t
= fun subst (t1, t2) ->
  if t1 = t2 then subst else
  match t1, t2 with
  | TList t1, TList t2 | TRef t1, TRef t2 -> unify subst (t1, t2)
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
    | TList ts | TRef ts -> if is_exist x ts then raise TypeError else Subst.extend x t subst
    | TTuple ts -> if is_exist2 x ts then raise TypeError else Subst.extend x t subst
    | TCtor (id, ts) -> if is_exist2 x ts then raise TypeError else Subst.extend x t subst
    | TArr (t1, t2) -> if ((is_exist x t1) || (is_exist x t2)) then raise TypeError else Subst.extend x t subst
    | _ -> Subst.extend x t subst
    end
  | t, TVar x -> unify subst (TVar x, t)
  | _ -> raise TypeError (* type base *) 

and unify_list : Subst.t -> (typ list * typ list) -> Subst.t
= fun subst (ts1, ts2) -> List.fold_left2 (fun subst t1 t2 -> unify subst (t1, t2)) subst ts1 ts2

let rec check_typs : typ -> typ -> bool
= fun t1 t2 -> 
  try 
    let _ = unify Subst.empty (t1, t2) in
    true
  with _ -> false 

(* Generate Substitution, HoleType Table, VarType Table using Type equations *)
let solve : (typ_eqn * LocType.t * LocEnv.t) -> (Subst.t * LocType.t * LocEnv.t)
= fun (eqns, l_t, l_env) ->
  let subst = List.fold_left (fun subst (t1, t2) -> unify subst ((Subst.apply t1 subst), Subst.apply t2 subst)) Subst.empty eqns in
  let l_t = LocType.update subst l_t in
  let l_env = LocEnv.update subst l_env in
  (subst, l_t, l_env)

let rec type_decl : (TEnv.t * LocType.t * LocEnv.t * Subst.t) -> decl -> (TEnv.t * LocType.t * LocEnv.t * Subst.t)
= fun (tenv, l_t, l_env, subst) decl ->
  match decl with
  | DExcept(id, typ) ->  (TEnv.extend (id, TCtor (TExn, typ)) tenv, l_t, l_env, subst)
  | DEqn (x, typ) -> (tenv, l_t, l_env, subst)
  | DData (id, ctors) -> let tbase = TBase id in (ctors_to_env tenv tbase ctors, l_t, l_env, subst)
  | DLet (f, is_rec, args, typ, exp) ->
    let exp =
      begin match f with
      | BindUnder -> exp
      | _ -> (gen_label(), ELet(f, is_rec, args, typ, exp, let_to_exp f))
      end
    in
    let ty = type_of_fun args typ in
    let (eqns, l_t, l_env) = gen_equations l_t l_env tenv exp ty in
    let (subst', l_t, l_env) = solve (eqns, l_t, l_env) in
    let ty = Subst.apply ty subst' in
    (let_binding tenv f ty, l_t, l_env, subst @ subst')
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
    (* gen each (eqn, l_t, l_env) of decls *)
    let results = List.map (fun (f, is_rec, args, typ, exp) -> 
      let exp = (gen_label(), ELet(f, is_rec, args, typ, exp, let_to_exp f)) in
      let ty = type_of_fun args typ in
      gen_equations l_t l_env tenv exp ty
    ) bindings in 
    (* merge all results together then solve it *)
    let (eqn, l_t, l_env) = List.fold_left (fun (eqn, l_t, l_env) (eqn', l_t', l_env') -> 
      (eqn@eqn', BatMap.union l_t l_t', BatMap.union l_env l_env')
    ) ([], l_t, l_env) results in 
    let (subst', l_t, l_env) = solve (eqn, l_t, l_env) in
    (* binding each decl with result *)
    let tenv = List.fold_left (fun tenv (f, is_rec, args, typ, exp) -> 
      let ty = type_of_fun args typ in
      let_binding tenv f (Subst.apply ty subst')
    ) tenv bindings in
    (tenv, l_t, l_env, subst @ subst')
  | TBlock decls -> List.fold_left (type_decl) (tenv, l_t, l_env, subst) decls

and ctors_to_env : TEnv.t -> typ -> ctor list -> TEnv.t
= fun tenv tbase ctors -> List.fold_left (fun env (x, typs) -> TEnv.extend (x, TCtor (tbase, typs)) env) tenv ctors

let run : prog -> (TEnv.t * LocType.t * LocEnv.t * Subst.t)
= fun decls -> 
  let _ = start_time:=Sys.time() in
  let decls = Converter.convert Converter.empty decls in
  let (init_env, l_t, l_env, subst) = List.fold_left type_decl (TEnv.empty, LocType.empty, LocEnv.empty, Subst.empty) (!library_pgm) in
  let (tenv, l_t, l_env, subst) = List.fold_left type_decl (init_env, l_t, l_env, subst) decls in
  (tenv, l_t, l_env, subst)
