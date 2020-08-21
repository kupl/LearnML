open Lang
open Util

(* Preprocessing : expanding all used variable definitions *)
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

(* Environment for storing information *)
let rec mem : var -> env -> bool
= fun x env ->
  match env with
  | [] -> false
  | (y, e)::tl -> if (y = x) then true else mem x tl

let rec find_env : env -> var -> exp
= fun env x ->
  match env with
  | [] -> raise (Failure ("Not Found : " ^ x))
  | (y, e)::tl -> if (y = x) then e else find_env tl x

let rec extend_env : (var * exp) -> env -> env
= fun (x, exp) env -> (x, exp)::env

let rec remove_env : env -> var -> env
= fun env x ->
  match env with
  | [] -> []
  | (y, e)::tl -> if (y=x) then remove_env tl x else (y,e)::(remove_env tl x)

(* Check a varaible is included in an expression *)
let rec is_used : var -> exp -> bool
= fun x exp ->
  match exp with
  | CONST n -> false
  | VAR x' -> x = x' 
  | ADD (e1, e2) -> (is_used x e1) || (is_used x e2) 
  | SUB (e1, e2) -> (is_used x e1) || (is_used x e2) 
  | MUL (e1, e2) -> (is_used x e1) || (is_used x e2) 
  | DIV (e1, e2) -> (is_used x e1) || (is_used x e2) 
  | ISZERO e -> is_used x e
  | READ -> false
  | IF (e1, e2, e3) -> (is_used x e1) || (is_used x e2) || (is_used x e3)
  | LET (x', e1, e2) -> if x = x' then (is_used x e1) else (is_used x e1) || (is_used x e2) 
  | LETREC (f, x', e1, e2) -> 
    if (x <> x' && x <> f) then (is_used x e1) || (is_used x e2) 
    else if (x <> f) then is_used x e2
    else false
  | PROC (x', e) -> if (x <> x') then is_used x e else false
  | CALL (e1, e2) -> (is_used x e1) || (is_used x e2) 

let rec trans_exp : exp -> env -> exp
= fun exp env ->
  match exp with
  | CONST n -> CONST n
  | VAR x -> if (mem x env) then find_env env x else VAR x
  | ADD (e1, e2) -> ADD (trans_exp e1 env, trans_exp e2 env)
  | SUB (e1, e2) -> SUB (trans_exp e1 env, trans_exp e2 env)
  | MUL (e1, e2) -> MUL (trans_exp e1 env, trans_exp e2 env)
  | DIV (e1, e2) -> DIV (trans_exp e1 env, trans_exp e2 env)
  | ISZERO e -> ISZERO (trans_exp e env)
  | READ -> READ
  | IF (e1, e2, e3) -> IF (trans_exp e1 env, trans_exp e2 env, trans_exp e3 env)
  | LET (x, e1, e2) ->  
    let e1' = trans_exp e1 env in
    let e2' = trans_exp e2 env in
    if (is_used x e2') then trans_exp e2' (extend_env (x, e1') env) else LET (x, e1', e2')
  | LETREC (f, x, e1, e2) -> 
    let e1' = trans_exp e1 (remove_env (remove_env env f) x) in
    LETREC (f, x, e1', trans_exp e2 (remove_env env f))
  | PROC (x, e) -> PROC (x, trans_exp e (remove_env env x))
  | CALL (e1, e2) -> CALL (trans_exp e1 env, trans_exp e2 env)

let expand : exp -> exp 
=fun e -> trans_exp e []


let rec subst_arg : Type.Subst.t -> arg -> arg
= fun subst arg -> 
  match arg with
  | ArgUnder typ -> ArgUnder (Type.Subst.apply typ subst)
  | ArgOne (x, typ) -> ArgOne (x, Type.Subst.apply typ subst)
  | ArgTuple args -> ArgTuple (subst_args subst args) 

and subst_args : Type.Subst.t -> arg list -> arg list
= fun subst args -> List.map (fun arg -> subst_arg subst arg) args

let rec subst_exp : Type.Subst.t -> lexp -> lexp
= fun subst (l, exp) ->
  let exp = 
    match exp with
    | Raise e -> Raise (relabel e)
    | EFun (arg, e) -> EFun (subst_arg subst arg, relabel e)
    | MINUS e -> MINUS (relabel e)
    | NOT e -> NOT (relabel e)
    | ADD (e1, e2) -> ADD (relabel e1, relabel e2)
    | SUB (e1, e2) -> SUB (relabel e1, relabel e2)
    | MUL (e1, e2) -> MUL (relabel e1, relabel e2)
    | DIV (e1, e2) -> DIV (relabel e1, relabel e2)
    | MOD (e1, e2) -> MOD (relabel e1, relabel e2)
    | OR (e1, e2) -> OR (relabel e1, relabel e2)
    | AND (e1, e2) -> AND (relabel e1, relabel e2)
    | LESS (e1, e2) -> LESS (relabel e1, relabel e2)
    | LESSEQ (e1, e2) -> LESSEQ (relabel e1, relabel e2)
    | LARGER (e1, e2) -> LARGER (relabel e1, relabel e2)
    | LARGEREQ (e1, e2) -> LARGEREQ (relabel e1, relabel e2)
    | EQUAL (e1, e2) -> EQUAL (relabel e1, relabel e2)
    | NOTEQ (e1, e2) -> NOTEQ (relabel e1, relabel e2)
    | DOUBLECOLON (e1, e2) -> DOUBLECOLON (relabel e1, relabel e2)
    | AT (e1, e2) -> AT (relabel e1, relabel e2)
    | STRCON (e1, e2) -> STRCON (relabel e1, relabel e2)
    | EApp (e1, e2) -> EApp (relabel e1, relabel e2)
    | EList es -> EList (List.map (fun e -> relabel e) es)
    | ETuple es -> ETuple (List.map (fun e -> relabel e) es)
    | ECtor (x, es) -> ECtor (x, List.map (fun e -> relabel e) es)
    | IF (e1, e2, e3) -> IF (relabel e1, relabel e2, relabel e3)
    | EMatch (e, bs) -> 
      let rec flatten_branch : branch list -> branch list
      = fun bs ->
        match bs with
        | [] -> []
        | (p, e)::bs -> 
          begin match p with
          | Pats ps -> 
            let flat_bs = (List.map (fun p -> (p, e)) ps) in
            (flatten_branch flat_bs)@(flatten_branch bs)
          | _ -> (p, e)::(flatten_branch bs)
          end
      in
      EMatch (relabel e, List.map (fun (p, e) -> (p, relabel e)) (flatten_branch bs))
    | ELet (f, is_rec, args, typ, e1, e2) -> ELet (f, is_rec, subst_args subst args, Type.Subst.apply typ subst, relabel e1, relabel e2)
    | EBlock (is_rec, bindings, e2) -> EBlock (is_rec, List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, subst_args subst args, Type.Subst.apply typ subst, relabel e)) bindings, relabel e2)
    | _ -> exp
  in
  (l, exp)

let rec subst_decl : Type.Subst.t -> decl -> decl
= fun subst decl ->
  match decl with
  | DLet (f, is_rec, args, typ, e) -> DLet (f, is_rec, subst_args subst args, Type.Subst.apply typ subst, relabel e)
  | DBlock (is_rec, bindings) -> DBlock (is_rec, List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, subst_args subst args, Type.Subst.apply typ subst, relabel e)) bindings)
  | _ -> decl

type env = (var, lexp) BatMap.t

let rec is_fun : typ -> bool
= fun typ ->
  match typ with
  | TyFun _ -> true
  | _ -> false

let rec relabel_exp : lexp -> lexp
= fun (l, exp) ->
  let exp = 
    match exp with
    | Raise e -> Raise (relabel e)
    | EFun (arg, e) -> EFun (arg, relabel e)
    | MINUS e -> MINUS (relabel e)
    | NOT e -> NOT (relabel e)
    | ADD (e1, e2) -> ADD (relabel e1, relabel e2)
    | SUB (e1, e2) -> SUB (relabel e1, relabel e2)
    | MUL (e1, e2) -> MUL (relabel e1, relabel e2)
    | DIV (e1, e2) -> DIV (relabel e1, relabel e2)
    | MOD (e1, e2) -> MOD (relabel e1, relabel e2)
    | OR (e1, e2) -> OR (relabel e1, relabel e2)
    | AND (e1, e2) -> AND (relabel e1, relabel e2)
    | LESS (e1, e2) -> LESS (relabel e1, relabel e2)
    | LESSEQ (e1, e2) -> LESSEQ (relabel e1, relabel e2)
    | LARGER (e1, e2) -> LARGER (relabel e1, relabel e2)
    | LARGEREQ (e1, e2) -> LARGEREQ (relabel e1, relabel e2)
    | EQUAL (e1, e2) -> EQUAL (relabel e1, relabel e2)
    | NOTEQ (e1, e2) -> NOTEQ (relabel e1, relabel e2)
    | DOUBLECOLON (e1, e2) -> DOUBLECOLON (relabel e1, relabel e2)
    | AT (e1, e2) -> AT (relabel e1, relabel e2)
    | STRCON (e1, e2) -> STRCON (relabel e1, relabel e2)
    | EApp (e1, e2) -> EApp (relabel e1, relabel e2)
    | EList es -> EList (List.map (fun e -> relabel e) es)
    | ETuple es -> ETuple (List.map (fun e -> relabel e) es)
    | ECtor (x, es) -> ECtor (x, List.map (fun e -> relabel e) es)
    | IF (e1, e2, e3) -> IF (relabel e1, relabel e2, relabel e3)
    | EMatch (e, bs) -> EMatch (relabel e, List.map (fun (p, e) -> (p, relabel e)) bs)
    | ELet (f, is_rec, args, typ, e1, e2) -> ELet (f, is_rec, args, typ, relabel e1, relabel e2)
    | EBlock (is_rec, bindings, e2) -> EBlock (is_rec, List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, args, typ, relabel e)) bindings, relabel e2)
    | _ -> exp
  in
  (gen_label (), exp)

let expand_exp : env -> lexp -> lexp
= fun env (l, exp) ->
  match exp with
let expand_decls : env -> decls -> decls
= fun env decl ->
  | [] -> []
  | hd::tl ->
    begin match hd with
    | DLet (f, is_rec, args, typ, e) -> 
      if is_fun typ then 
    | DBlock (is_rec, bindings) -> DBlock (is_rec, List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, subst_args subst args, Type.Subst.apply typ subst, relabel e)) bindings)
    | _ -> hd::(expand_decls )

let run : prog -> prog
= fun pgm ->
  List.map (fun decl -> subst_decl subst decl) pgm