open Lang
open Type

(******************************************************************)
(* Extract all functions' bodies and construct a mapping relation *)
(******************************************************************)

module T = struct
  (* Preanalysis : type annotation for distingushing function type *)

  let rec subst_exp : Type.Subst.t -> lexp -> lexp
  = fun subst (l, exp) ->
    let exp = 
      match exp with
      | Raise e -> Raise (subst_exp subst e)
      | EFun (arg, e) -> EFun (arg, subst_exp subst e)
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
      | ELet (f, is_rec, args, typ, e1, e2) -> ELet (f, is_rec, args, Type.Subst.apply typ subst, subst_exp subst e1, subst_exp subst e2)
      | EBlock (is_rec, bindings, e2) -> EBlock (is_rec, List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, args, Subst.apply typ subst, subst_exp subst e)) bindings, subst_exp subst e2)
      | _ -> exp
    in
    (l, exp)

  let rec subst_decl : Type.Subst.t -> decl -> decl
  = fun subst decl ->
    match decl with
    | DLet (f, is_rec, args, typ, e) -> DLet (f, is_rec, args, Type.Subst.apply typ subst, subst_exp subst e)
    | DBlock (is_rec, bindings) -> DBlock (is_rec, List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, args, Type.Subst.apply typ subst, subst_exp subst e)) bindings)
    | _ -> decl

  let run : prog -> prog
  = fun pgm ->
    let (_, _, _, subst) = Type.run pgm in
    List.map (fun decl -> subst_decl subst decl) pgm
end 

type t = (id, lexp) BatMap.t (* Normalized program P_N : function name -> function body *)

let rec is_fun : typ -> bool
= fun typ ->
  match typ with
  | TList t -> is_fun t
  | TTuple ts -> List.exists is_fun ts
  | TCtor (t, ts) -> List.exists is_fun (t::ts)
  | TArr _ -> true
  | _ -> false

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

let rec normalize : t -> lexp -> t * lexp
= fun t (l, exp) -> 
  match exp with  
  | MINUS e ->
    let (t, e) = normalize t e in
    (t, (l, MINUS e))
  | NOT  e ->
    let (t, e) = normalize t e in
    (t, (l, NOT e))  
  | Raise e -> 
    let (t, e) = normalize t e in
    (t, (l, Raise e))
  | ADD (e1, e2) ->
    let (t, (e1, e2)) = normalize_tuple t (e1, e2) in
    (t, (l, ADD (e1, e2)))
  | SUB (e1, e2) ->
    let (t, (e1, e2)) = normalize_tuple t (e1, e2) in
    (t, (l, SUB (e1, e2)))
  | MUL (e1, e2) ->
    let (t, (e1, e2)) = normalize_tuple t (e1, e2) in
    (t, (l, MUL (e1, e2)))
  | DIV (e1, e2) ->
    let (t, (e1, e2)) = normalize_tuple t (e1, e2) in
    (t, (l, DIV (e1, e2)))
  | MOD (e1, e2) ->
    let (t, (e1, e2)) = normalize_tuple t (e1, e2) in
    (t, (l, MOD (e1, e2)))
  | OR (e1, e2) ->
    let (t, (e1, e2)) = normalize_tuple t (e1, e2) in
    (t, (l, OR (e1, e2)))
  | AND (e1, e2) ->
    let (t, (e1, e2)) = normalize_tuple t (e1, e2) in
    (t, (l, AND (e1, e2)))
  | LESS (e1, e2) ->
    let (t, (e1, e2)) = normalize_tuple t (e1, e2) in
    (t, (l, LESS (e1, e2)))
  | LARGER (e1, e2) ->
    let (t, (e1, e2)) = normalize_tuple t (e1, e2) in
    (t, (l, LARGER (e1, e2)))
  | EQUAL (e1, e2) ->
    let (t, (e1, e2)) = normalize_tuple t (e1, e2) in
    (t, (l, EQUAL (e1, e2)))
  | NOTEQ (e1, e2) ->
    let (t, (e1, e2)) = normalize_tuple t (e1, e2) in
    (t, (l, NOTEQ (e1, e2)))
  | LESSEQ (e1, e2) ->
    let (t, (e1, e2)) = normalize_tuple t (e1, e2) in
    (t, (l, LESSEQ (e1, e2)))
  | LARGEREQ (e1, e2) ->
    let (t, (e1, e2)) = normalize_tuple t (e1, e2) in
    (t, (l, LARGEREQ (e1, e2)))
  | AT (e1, e2) ->
    let (t, (e1, e2)) = normalize_tuple t (e1, e2) in
    (t, (l, AT (e1, e2)))
  | DOUBLECOLON (e1, e2) ->
    let (t, (e1, e2)) = normalize_tuple t (e1, e2) in
    (t, (l, DOUBLECOLON (e1, e2)))
  | STRCON (e1, e2) ->
    let (t, (e1, e2)) = normalize_tuple t (e1, e2) in
    (t, (l, STRCON (e1, e2)))
  | EApp (e1, e2) ->
    let (t, (e1, e2)) = normalize_tuple t (e1, e2) in
    (t, (l, EApp (e1, e2)))
  | EList es -> 
    let (t, es) = normalize_list t es in
    (t, (l, EList es))
  | ETuple es ->
    let (t, es) = normalize_list t es in
    (t, (l, ETuple es))
  | ECtor (x, es) ->
    let (t, es) = normalize_list t es in
    (t, (l, ECtor (x, es)))
  | EFun (arg, e) -> normalize t e 
  | ELet (f, is_rec, args, typ, e1, e2) ->
    let (t, e1) = normalize t e1 in
    if args <> [] || is_fun typ then 
      begin match f with
      | BindOne f -> 
        let (t, e2) = normalize (BatMap.add f e1 t) e2 in
        (t, e2)
      | _ -> raise (Failure "Extract error : invalid function format")
      end
    else 
      let (t, e2) = normalize t e2 in
      (t, (l, ELet (f, is_rec, args, typ, e1, e2)))
  | EBlock (is_rec, ds, e) ->
    let (t, ds) = List.fold_left (fun (t, ds) (f, is_rec, args, typ, e) ->
      if args <> [] || is_fun typ then 
        begin match f with
        | BindOne f -> 
          let (t, e) = normalize t e in
          (BatMap.add f e t, ds)
        | _ -> raise (Failure "Extract error : invalid function format")
        end
      else 
        let (t, e) = normalize t e in
        (t, (f, is_rec, args, typ, e)::ds)
    ) (t, []) ds in
    let (t, e) = normalize t e in
    if ds = [] then (t, e) else (t, (l, EBlock (is_rec, List.rev ds, e)))
  | EMatch (e, bs) ->
    let bs = flatten_branch bs in 
    let (t, e) = normalize t e in
    let (t, bs) = List.fold_left (fun (t, bs) (p, e) ->
      let (t, e) = normalize t e in
      (t, (p, e)::bs)
    ) (t, []) bs in
    (t, (l, EMatch (e, List.rev bs)))
  | IF (e1, e2, e3) ->
    let (t, e1) = normalize t e1 in
    let (t, e2) = normalize t e2 in
    let (t, e3) = normalize t e3 in
    (t, (l, IF (e1, e2, e3)))
  | _ -> (t, (l, exp))

and normalize_tuple : t -> (lexp * lexp) -> t * (lexp * lexp)
= fun t (e1, e2) ->
  let (t, e1) = normalize t e1 in
  let (t, e2) = normalize t e2 in
  (t, (e1, e2))

and normalize_list : t -> lexp list -> t * lexp list
= fun t es ->
  match es with
  | [] -> (t, [])
  | hd::tl ->
    let (t, e) = normalize t hd in
    let (t, es') = normalize_list t tl in 
    (t, e::es')

let rec normalize_decl : t -> decl -> t
= fun t decl -> 
  match decl with
  | DLet (f, is_rec, args, typ, e) -> 
    let (t, e) = normalize t e in
    if args <> [] || is_fun typ then 
      begin match f with
      | BindOne f -> BatMap.add f e t
      | _ -> raise (Failure "Extract error : invalid function format")
      end
    else t
  | DBlock (is_rec, bindings) -> List.fold_left (fun t binding -> normalize_decl t (DLet binding)) t bindings
  | _ -> t

let normalize_all : prog -> t
= fun pgm ->
  let pgm' = T.run pgm in
  let t = List.fold_left (fun t decl -> normalize_decl t decl) BatMap.empty pgm' in
  if BatMap.is_empty t then raise (Failure "Empty_norm") else t

let print : t -> unit
= fun t ->
  BatMap.iter (fun f e -> 
    print_endline ("---------------------");
    print_endline ("Functoin name : " ^ f);
    print_endline (Print.exp_to_string e)
  ) t