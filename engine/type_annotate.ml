open Lang
open Util

(* Preprocessing : type annotation for distingushing function type *)
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
    | Raise e -> Raise (subst_exp subst e)
    | EFun (arg, e) -> EFun (subst_arg subst arg, subst_exp subst e)
    | MINUS e -> MINUS (subst_exp subst e)
    | NOT e -> NOT (subst_exp subst e)
    | ADD (e1, e2) -> ADD (subst_exp subst e1, subst_exp subst e2)
    | SUB (e1, e2) -> SUB (subst_exp subst e1, subst_exp subst e2)
    | MUL (e1, e2) -> MUL (subst_exp subst e1, subst_exp subst e2)
    | DIV (e1, e2) -> DIV (subst_exp subst e1, subst_exp subst e2)
    | MOD (e1, e2) -> MOD (subst_exp subst e1, subst_exp subst e2)
    | OR (e1, e2) -> OR (subst_exp subst e1, subst_exp subst e2)
    | AND (e1, e2) -> AND (subst_exp subst e1, subst_exp subst e2)
    | LESS (e1, e2) -> LESS (subst_exp subst e1, subst_exp subst e2)
    | LESSEQ (e1, e2) -> LESSEQ (subst_exp subst e1, subst_exp subst e2)
    | LARGER (e1, e2) -> LARGER (subst_exp subst e1, subst_exp subst e2)
    | LARGEREQ (e1, e2) -> LARGEREQ (subst_exp subst e1, subst_exp subst e2)
    | EQUAL (e1, e2) -> EQUAL (subst_exp subst e1, subst_exp subst e2)
    | NOTEQ (e1, e2) -> NOTEQ (subst_exp subst e1, subst_exp subst e2)
    | DOUBLECOLON (e1, e2) -> DOUBLECOLON (subst_exp subst e1, subst_exp subst e2)
    | AT (e1, e2) -> AT (subst_exp subst e1, subst_exp subst e2)
    | STRCON (e1, e2) -> STRCON (subst_exp subst e1, subst_exp subst e2)
    | EApp (e1, e2) -> EApp (subst_exp subst e1, subst_exp subst e2)
    | EList es -> EList (List.map (fun e -> subst_exp subst e) es)
    | ETuple es -> ETuple (List.map (fun e -> subst_exp subst e) es)
    | ECtor (x, es) -> ECtor (x, List.map (fun e -> subst_exp subst e) es)
    | IF (e1, e2, e3) -> IF (subst_exp subst e1, subst_exp subst e2, subst_exp subst e3)
    | EMatch (e, bs) -> EMatch (subst_exp subst e, List.map (fun (p, e) -> (p, subst_exp subst e)) bs)
    | ELet (f, is_rec, args, typ, e1, e2) -> ELet (f, is_rec, subst_args subst args, Type.Subst.apply typ subst, subst_exp subst e1, subst_exp subst e2)
    | EBlock (is_rec, bindings, e2) -> EBlock (is_rec, List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, subst_args subst args, Type.Subst.apply typ subst, subst_exp subst e)) bindings, subst_exp subst e2)
    | _ -> exp
  in
  (l, exp)

let rec subst_decl : Type.Subst.t -> decl -> decl
= fun subst decl ->
  match decl with
  | DLet (f, is_rec, args, typ, e) -> DLet (f, is_rec, subst_args subst args, Type.Subst.apply typ subst, subst_exp subst e)
  | DBlock (is_rec, bindings) -> DBlock (is_rec, List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, subst_args subst args, Type.Subst.apply typ subst, subst_exp subst e)) bindings)
  | _ -> decl

let run : prog -> prog
= fun pgm ->
  let (_, _, _, subst) = Type.run pgm in
  List.map (fun decl -> subst_decl subst decl) pgm