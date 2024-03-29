open Lang
open Util
open CallGraph 

(*******************************************)
(* Translate programs into identical forms *)
(*******************************************)
module Type_annotate = struct
  (* Annotate type to distingush function type *)
  let rec annotate_arg : Type2.Subst.t -> arg -> arg
  = fun subst arg -> 
    match arg with
    | ArgUnder typ -> ArgUnder (Type2.Subst.apply typ subst)
    | ArgOne (x, typ) -> ArgOne (x, Type2.Subst.apply typ subst)
    | ArgTuple args -> ArgTuple (annotate_args subst args) 

  and annotate_args : Type2.Subst.t -> arg list -> arg list
  = fun subst args -> List.map (fun arg -> annotate_arg subst arg) args

  let rec annotate_exp : Type2.Subst.t -> lexp -> lexp
  = fun subst (l, exp) ->
    match exp with
    | EUnit | Const _ | TRUE | FALSE | String _ | EVar _ -> (l, exp)
    | EFun (arg, e) -> (l, EFun (annotate_arg subst arg, annotate_exp subst e))
    | ERef e | EDref e | Raise e | MINUS e | NOT e -> (l, update_unary exp (annotate_exp subst e))
    | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2) 
    | OR (e1, e2) | AND (e1, e2) | LESS (e1, e2) | LESSEQ (e1, e2) | LARGER (e1, e2) | LARGEREQ (e1, e2) 
    | EQUAL (e1, e2) | NOTEQ (e1, e2) | DOUBLECOLON (e1, e2) | AT (e1, e2) | STRCON (e1, e2) | EApp (e1, e2) 
    | EAssign (e1, e2) -> (l, update_binary exp (annotate_exp subst e1, annotate_exp subst e2))
    | EList es -> (l, EList (List.map (fun e -> annotate_exp subst e) es))
    | ETuple es -> (l, ETuple (List.map (fun e -> annotate_exp subst e) es))
    | ECtor (x, es) -> (l, ECtor (x, List.map (fun e -> annotate_exp subst e) es))
    | IF (e1, e2, e3) -> (l, IF (annotate_exp subst e1, annotate_exp subst e2, annotate_exp subst e3))
    | EMatch (e, bs) -> (l, EMatch (annotate_exp subst e, List.map (fun (p, e) -> (p, annotate_exp subst e)) bs))
    | ELet (f, is_rec, args, typ, e1, e2) -> (l, ELet (f, is_rec, annotate_args subst args, Type2.Subst.apply typ subst, annotate_exp subst e1, annotate_exp subst e2))
    | EBlock (is_rec, bindings, e2) -> (l, EBlock (is_rec, List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, annotate_args subst args, Type2.Subst.apply typ subst, annotate_exp subst e)) bindings, annotate_exp subst e2))
    | _ -> raise (Failure ("Type annotation : invalid exp (" ^ Print.exp_to_string (l, exp)))

  let rec annotate_decl : Type2.Subst.t -> decl -> decl
  = fun subst decl ->
    match decl with
    | DLet (f, is_rec, args, typ, e) -> DLet (f, is_rec, annotate_args subst args, Type2.Subst.apply typ subst, annotate_exp subst e)
    | DBlock (is_rec, bindings) -> DBlock (is_rec, List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, annotate_args subst args, Type2.Subst.apply typ subst, annotate_exp subst e)) bindings)
    | _ -> decl

  let run : prog -> prog
  = fun pgm ->
    let (_, _, _, subst) = Type2.run pgm in
    List.map (fun decl -> annotate_decl subst decl) pgm
end 

module Decapsulation = struct
  (* Decapsulation: convert all function definitions "f = fun x -> e" into "f (x) = e" form *)
  let rec flatten_branch : branch list -> branch list
  = fun bs ->
    match bs with
    | [] -> []
    | (p, e)::tl ->
      begin match p with
      | Pats ps -> 
        let flat_bs = List.map (fun p -> (p, update_component e)) ps in
        (flatten_branch flat_bs)@(flatten_branch tl)
      | _ -> (p, e)::(flatten_branch tl) 
      end
  
  let rec get_output_typ : typ -> arg list -> typ
  = fun typ typs ->
    match typ with
    | TArr (t1, t2) -> 
      begin match typs with
      | [] -> typ
      | hd::tl -> get_output_typ t2 tl
      end 
    | _ -> typ

  let rec decapsulate_exp : lexp -> lexp 
  = fun (l, exp) ->
    match exp with
    | EUnit | Const _ | TRUE | FALSE | String _ | EVar _ -> (l, exp)
    | EFun (arg, e) -> (l, EFun (arg, decapsulate_exp e))
    | ERef e | EDref e | Raise e | MINUS e | NOT e -> (l, update_unary exp (decapsulate_exp e))
    | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2) 
    | OR (e1, e2) | AND (e1, e2) | LESS (e1, e2) | LESSEQ (e1, e2) | LARGER (e1, e2) | LARGEREQ (e1, e2) 
    | EQUAL (e1, e2) | NOTEQ (e1, e2) | DOUBLECOLON (e1, e2) | AT (e1, e2) | STRCON (e1, e2) | EApp (e1, e2) 
    | EAssign (e1, e2) -> (l, update_binary exp (decapsulate_exp e1, decapsulate_exp e2))
    | EList es -> (l, EList (List.map (fun e -> decapsulate_exp e) es))
    | ETuple es -> (l, ETuple (List.map (fun e -> decapsulate_exp e) es))
    | ECtor (x, es) -> (l, ECtor (x, List.map (fun e -> decapsulate_exp e) es))
    | IF (e1, e2, e3) -> (l, IF (decapsulate_exp e1, decapsulate_exp e2, decapsulate_exp e3))
    | EMatch (e, bs) -> (l, EMatch (decapsulate_exp e, List.map (fun (p, e) -> (p, decapsulate_exp e)) (flatten_branch bs)))
    | ELet (f, is_rec, args, typ, e1, e2) ->
      let (f, is_rec, args, typ, e1) = decapsulate_binding (f, is_rec, args, typ, e1) in
      (l, ELet (f, is_rec, args, typ, e1, decapsulate_exp e2))
    | EBlock (is_rec, ds, e) -> (l, EBlock (is_rec, List.map (fun binding -> decapsulate_binding binding) ds, decapsulate_exp e))
    | _ -> raise (Failure ("Decapsulation : invalid exp (" ^ Print.exp_to_string (l, exp)))

  and decapsulate_binding : binding -> binding
  = fun (f, is_rec, args, typ, e) ->
    let (args', e') = extract_func (decapsulate_exp e) in
    (f, is_rec, args@args', get_output_typ typ args', e')

  and extract_func : lexp -> arg list * lexp
  = fun (l, exp) ->
    match exp with
    | EFun (arg, e) -> 
      let (args, e) = extract_func e in
      (arg::args, e)
    | ELet (f, is_rec, args, typ, e1, e2) ->
      let (f, is_rec, args, typ, e1) = decapsulate_binding (f, is_rec, args, typ, e1) in
      let (args', e2) = extract_func e2 in
      (args', (l, ELet (f, is_rec, args, typ, e1, e2)))
    | EBlock (is_rec, ds, e) -> 
      let ds = List.map (fun binding -> decapsulate_binding binding) ds in
      let (args, e) = extract_func e in
      (args, (l, EBlock (is_rec, ds, e)))
    | _ -> ([], (l, exp))

  let rec decapsulate_decl : decl -> decl 
  = fun decl ->
    match decl with
    | DLet binding -> DLet (decapsulate_binding binding)
    | DBlock (is_rec, bindings) -> DBlock (is_rec, List.map (fun binding -> decapsulate_binding binding) bindings)
    | _ -> decl

  let run : prog -> prog
  = fun pgm -> List.map (fun decl -> decapsulate_decl decl) pgm
end 

module Renaming = struct
  (* Rename: renaming all variables to avoid redundant variable definition *)
  type env = (id, id) BatMap.t

  let r_env = ref (BatMap.empty) (* inverse transforamtion info (for restoring) *)
  let update x v env = 
    r_env := BatMap.add v x !r_env; (* store update info *)
    BatMap.add x v env
  let apply_env x env = try BatMap.find x env with _ -> x

  let count = ref 0
  let flag = ref false
  let fresh_var () = count := !count + 1; (if !flag then "__s" else "__v") ^ (string_of_int !count)

  let rec rename_arg : env -> arg -> env * arg
  = fun env arg ->
    match arg with
    | ArgUnder typ -> (env, arg)
    | ArgOne (x, typ) -> 
      let new_var = fresh_var () in
      (update x new_var env, ArgOne (new_var, typ))
    | ArgTuple args -> 
      let (env, args) = rename_arg_list env args in
      (env, ArgTuple args)
  and rename_arg_list : env -> arg list -> env * arg list
  = fun env args -> List.fold_left (fun (env, args) arg -> 
      let (env, arg) = rename_arg env arg in
      (env, args@[arg])
    ) (env, []) args

  let rec rename_pat : env -> pat -> env * pat
  = fun env pat ->
    match pat with
    | PUnit | PUnder | PInt _ | PBool _ -> (env, pat)
    | PVar x ->
      let new_var = fresh_var () in
      (update x new_var env, PVar new_var)
    | PList ps ->
      let (env, ps) = rename_pat_list env ps in
      (env, PList ps)
    | PTuple ps ->
      let (env, ps) = rename_pat_list env ps in
      (env, PTuple ps)
    | PCtor (c, ps) ->
      let (env, ps) = rename_pat_list env ps in
      (env, PCtor (c, ps))
    | PCons (phd, ptl) ->
      let (env, phd) = rename_pat env phd in
      let (env, ptl) = rename_pat env ptl in
      (env, PCons (phd, ptl))
    | Pats ps ->
      let (env, ps) = rename_pat_list env ps in
      (env, Pats ps)
  and rename_pat_list : env -> pat list -> env * pat list
  = fun env ps -> List.fold_left (fun (env, ps) p ->
      let (env, p) = rename_pat env p in
      (env, ps@[p])
    ) (env, []) ps
  
  let rec rename_binding : env -> let_bind -> env * let_bind
  = fun env binding ->
    match binding with
    | BindUnder -> (env, BindUnder)
    | BindOne x -> 
      let new_var = fresh_var () in
      (update x new_var env, BindOne new_var)
    | BindTuple bs -> 
      let (env, bs) = List.fold_left (fun (env, bs) b -> 
        let (env, b) = rename_binding env b in
        (env, bs@[b])
      ) (env, []) bs in
      (env, BindTuple bs)
 
  let rec rename_exp : env -> lexp -> lexp
  = fun env (l, exp) ->
    match exp with
    | EVar x -> (l, EVar (apply_env x env))
    | EUnit | Const _ | TRUE | FALSE | String _ -> (l, exp)
    | EFun (arg, e) -> 
      let (env, arg) = rename_arg env arg in
      (l, EFun (arg, rename_exp env e))
    | ERef e | EDref e | Raise e | MINUS e | NOT e -> (l, update_unary exp (rename_exp env e))
    | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2) 
    | OR (e1, e2) | AND (e1, e2) | LESS (e1, e2) | LESSEQ (e1, e2) | LARGER (e1, e2) | LARGEREQ (e1, e2) 
    | EQUAL (e1, e2) | NOTEQ (e1, e2) | DOUBLECOLON (e1, e2) | AT (e1, e2) | STRCON (e1, e2) | EApp (e1, e2) 
    | EAssign (e1, e2) -> (l, update_binary exp (rename_exp env e1, rename_exp env e2))
    | EList es -> (l, EList (List.map (fun e -> rename_exp env e) es))
    | ETuple es -> (l, ETuple (List.map (fun e -> rename_exp env e) es))
    | ECtor (x, es) -> (l, ECtor (x, List.map (fun e -> rename_exp env e) es))
    | IF (e1, e2, e3) -> (l, IF (rename_exp env e1, rename_exp env e2, rename_exp env e3))
    | EMatch (e, bs) -> 
      let bs = List.map (fun (p, e) -> 
        let (env, p) = rename_pat env p in
        (p, rename_exp env e)
      ) bs in
      (l, EMatch (rename_exp env e, bs))
    | ELet (f, is_rec, args, typ, e1, e2) -> 
      let (func_env, f) = rename_binding env f in
      let (env, args) = rename_arg_list (if is_rec then func_env else env) args in
      (l, ELet (f, is_rec, args, typ, rename_exp env e1, rename_exp func_env e2))
    | EBlock (is_rec, ds, e) -> 
      if is_rec then 
      (* Renaming function name first *)
      let (env, bs) = List.fold_left (fun (env, bs) (f, is_rec, args, typ, e) ->
        let (env, f) = rename_binding env f in
        (env, bs@[(f, is_rec, args, typ, e)])
      ) (env, []) ds in
      (* Renaming each decl with renamed function *)
      let bs = List.map (fun (f, is_rec, args, typ, e) -> 
        let (env, args) = rename_arg_list env args in
        (f, is_rec, args, typ, rename_exp env e)
      ) bs in
      (l, EBlock (is_rec, bs, rename_exp env e))
    else 
      let (env, bs) = List.fold_left (fun (env, bs) (f, is_rec, args, typ, e) ->
        let (func_env, f) = rename_binding env f in
        let (env, args) = rename_arg_list (if is_rec then func_env else env) args in
        (func_env, bs@[(f, is_rec, args, typ, rename_exp env e)])
      ) (env, []) ds in
      (l, EBlock (is_rec, bs, rename_exp env e))
    | _ -> raise (Failure ("Renaming : invalid exp (" ^ Print.exp_to_string (l, exp)))

  let rec rename_decls : env -> decl list -> env * decl list
  = fun env decls ->
    match decls with
    | [] -> (env, [])
    | decl::tl ->
      begin match decl with
      | DLet (f, is_rec, args, typ, e) -> 
        let (func_env, f) = rename_binding env f in
        let (env1, args) = rename_arg_list (if is_rec then func_env else env) args in
        let (env2, decls) = rename_decls func_env tl in
        (env2, (DLet (f, is_rec, args, typ, rename_exp env1 e))::decls)
      | DBlock (is_rec, bindings) -> 
        if is_rec then 
          (* Renaming function name first *)
          let (env, bs) = List.fold_left (fun (env, bs) (f, is_rec, args, typ, e) ->
            let (env, f) = rename_binding env f in
            (env, bs@[(f, is_rec, args, typ, e)])
          ) (env, []) bindings in
          (* Renaming each decl with renamed function *)
          let bs = List.map (fun (f, is_rec, args, typ, e) -> 
            let (env, args) = rename_arg_list env args in
            (f, is_rec, args, typ, rename_exp env e)
          ) bs in
          let (env, decls) = rename_decls env tl in
          (env, (DBlock (is_rec, bs))::decls)
        else 
          let (env, bs) = List.fold_left (fun (env, bs) (f, is_rec, args, typ, e) ->
            let (func_env, f) = rename_binding env f in
            let (env, args) = rename_arg_list (if is_rec then func_env else env) args in
            (func_env, bs@[(f, is_rec, args, typ, rename_exp env e)])
          ) (env, []) bindings in
          let (env, decls) = rename_decls env tl in
          (env, (DBlock (is_rec, bs))::decls)
      | _ -> 
        let (env, decls) = rename_decls env tl in
        (env, decl::decls)
      end

  let run : prog -> env * prog
  = fun pgm -> 
    r_env := BatMap.empty;
    let (env, new_pgm) = rename_decls BatMap.empty pgm in
    renamed_entry := BatMap.find !Options.opt_entry_func env;
    (!r_env, new_pgm)

  (* Renaming the program to orignial form *)
  let rec apply_arg : env -> arg -> arg
  = fun env arg ->
    match arg with
    | ArgOne (x, typ) -> ArgOne (apply_env x env, typ)
    | ArgTuple args -> ArgTuple (List.map (apply_arg env) args)
    | _ -> arg

  let rec apply_pat : env -> pat -> pat
  = fun env pat ->
    match pat with
    | PVar x -> PVar (apply_env x env)
    | PList ps -> PList (List.map (apply_pat env) ps)
    | PTuple ps -> PTuple (List.map (apply_pat env) ps)
    | PCtor (c, ps) -> PCtor (c, (List.map (apply_pat env) ps))
    | PCons (phd, ptl) -> PCons (apply_pat env phd, apply_pat env ptl)
    | Pats ps -> Pats (List.map (apply_pat env) ps)
    | _ -> pat

  let rec apply_binding : env -> let_bind -> let_bind
  = fun env binding ->
    match binding with
    | BindOne x -> BindOne (apply_env x env)
    | BindTuple bs -> BindTuple (List.map (apply_binding env) bs)
    | _ -> binding

  let rec apply_exp : env -> lexp -> lexp 
  = fun env (l, exp) ->
    match exp with
    | EVar x -> (l, EVar (apply_env x env))
    | EUnit | Const _ | TRUE | FALSE | String _ -> (l, exp)
    | EFun (arg, e) -> (l, EFun (apply_arg env arg, apply_exp env e))
    | ERef e | EDref e | Raise e | MINUS e | NOT e -> (l, update_unary exp (apply_exp env e))
    | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2) 
    | OR (e1, e2) | AND (e1, e2) | LESS (e1, e2) | LESSEQ (e1, e2) | LARGER (e1, e2) | LARGEREQ (e1, e2) 
    | EQUAL (e1, e2) | NOTEQ (e1, e2) | DOUBLECOLON (e1, e2) | AT (e1, e2) | STRCON (e1, e2) | EApp (e1, e2) 
    | EAssign (e1, e2) -> (l, update_binary exp (apply_exp env e1, apply_exp env e2))
    | EList es -> (l, EList (List.map (apply_exp env) es))
    | ETuple es -> (l, ETuple (List.map (apply_exp env) es))
    | ECtor (x, es) -> (l, ECtor (x, List.map (apply_exp env) es))
    | IF (e1, e2, e3) -> (l, IF (apply_exp env e1, apply_exp env e2, apply_exp env e3))
    | EMatch (e, bs) -> (l, EMatch (apply_exp env e, List.map (fun (p, e) -> (apply_pat env p, apply_exp env e)) bs))
    | ELet (f, is_rec, args, typ, e1, e2) -> (l, ELet (apply_binding env f, is_rec, List.map (apply_arg env) args, typ, apply_exp env e1, apply_exp env e2))
    | EBlock (is_rec, ds, e) -> 
      let ds = List.map (fun (f, is_rec, args, typ, e) -> (apply_binding env f, is_rec, List.map (apply_arg env) args, typ, apply_exp env e)) ds in
      (l, EBlock (is_rec, ds, apply_exp env e))
    | Hole n -> (l, exp)
    | _ -> raise (Failure ("Renaming : invalid exp (" ^ Print.exp_to_string (l, exp)))

  let apply_decl : env -> decl -> decl
  = fun env decl ->
    match decl with
    | DLet (f, is_rec, args, typ, e) -> DLet (apply_binding env f, is_rec, List.map (apply_arg env) args, typ, apply_exp env e)
    | DBlock (is_rec, ds) -> 
      let ds = List.map (fun (f, is_rec, args, typ, e) -> (apply_binding env f, is_rec, List.map (apply_arg env) args, typ, apply_exp env e)) ds in 
      DBlock (is_rec, ds)
    | _ -> decl

  let apply_pgm : env -> prog -> prog 
  = fun env pgm -> List.map (apply_decl env) pgm

  (* Call graph restoring *)
  let rec apply_path : env -> path -> path
  = fun env path ->
    match path with 
    | Aop (op, p1, p2) -> Aop (op, apply_path env p1, apply_path env p2)
    | Bop (comb, p1, p2) -> Bop (comb, apply_path env p1, apply_path env p2)
    | ABop (comp, p1, p2) -> ABop (comp, apply_path env p1, apply_path env p2)
    | EQop (eq, p1, p2) -> EQop (eq, apply_path env p1, apply_path env p2)
    | Strcon (p1, p2) -> Strcon (apply_path env p1, apply_path env p2)
    | Append (p1, p2) -> Append (apply_path env p1, apply_path env p2)
    | Concat (p1, p2) -> Concat (apply_path env p1, apply_path env p2)
    | Minus p -> Minus (apply_path env p)
    | Not p -> Not (apply_path env p)
    | List (ps, typ) -> List (List.map (apply_path env) ps, typ)
    | Tuple ps -> Tuple (List.map (apply_path env) ps)
    | Ctor (c, ps) -> Ctor (c, List.map (apply_path env) ps)
    | Var (x, typ) -> Var (apply_env x env, typ)
    | _ -> path
  
  (* Template restoring *)
  let apply_template : env -> Repair_template.repair_template ->  Repair_template.repair_template
  = fun env temp ->
    match temp with
    | ModifyExp (l, e) -> ModifyExp (l, apply_exp env e)
    | InsertBranch (l, (p, e)) -> InsertBranch (l, (apply_pat env p, apply_exp env e))
    | DeleteBranch (l, (p, e)) -> DeleteBranch (l, (apply_pat env p, apply_exp env e))
    | InsertFunction ((g, b, args, typ, e), f) -> InsertFunction ((g, b, args, typ, apply_exp env e), apply_env f env) 
end

let run : prog -> Renaming.env * prog
= fun pgm -> 
  Type_annotate.run pgm
  |> Decapsulation.run
  |> Renaming.run