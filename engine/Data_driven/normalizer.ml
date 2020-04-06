open Lang
open Type

(*********************************)
(* Extract all functions' bodies *)
(*********************************)
module T = Type_annotate

type t = (id, (arg list * typ * lexp)) BatMap.t (* Normalized program P_N : function name -> (input, output, body) *)

let print : t -> unit
= fun t ->
  BatMap.iter (fun f (args, typ, e) -> 
    print_endline (Print.decl_to_string (DLet (BindOne f, true, args, typ, e)) "")
  ) t

(* Preprocessing : convert all function definitions "f = fun x -> e" into "f (x) = e" form *)
let rec get_output_typ : typ -> typ
= fun typ ->
  match typ with
  | TArr (t1, t2) -> get_output_typ t2
  | _ -> typ

let rec preprocess_exp : lexp -> lexp 
= fun (l, exp) ->
  let exp = 
    match exp with
    | EList es -> EList (List.map (fun e -> preprocess_exp e) es)
    | ECtor (x, es) -> ECtor (x, (List.map (fun e -> preprocess_exp e) es))
    | ETuple es -> ETuple (List.map (fun e -> preprocess_exp e) es)
    | EFun (arg, e) -> EFun (arg, preprocess_exp e)
    | MINUS e -> MINUS (preprocess_exp e)
    | NOT e -> NOT (preprocess_exp e)
    | ADD (e1, e2) -> ADD (preprocess_exp e1, preprocess_exp e2)
    | SUB (e1, e2) -> SUB (preprocess_exp e1, preprocess_exp e2)
    | MUL (e1, e2) -> MUL (preprocess_exp e1, preprocess_exp e2)
    | DIV (e1, e2) -> DIV (preprocess_exp e1, preprocess_exp e2)
    | MOD (e1, e2) -> MOD (preprocess_exp e1, preprocess_exp e2)
    | OR (e1, e2) -> OR (preprocess_exp e1, preprocess_exp e2)
    | AND (e1, e2) -> AND (preprocess_exp e1, preprocess_exp e2) 
    | LESS (e1, e2) -> LESS (preprocess_exp e1, preprocess_exp e2) 
    | LARGER (e1, e2) -> LARGER (preprocess_exp e1, preprocess_exp e2) 
    | LESSEQ (e1, e2) -> LESSEQ (preprocess_exp e1, preprocess_exp e2) 
    | LARGEREQ (e1, e2) -> LARGEREQ (preprocess_exp e1, preprocess_exp e2)
    | EQUAL (e1, e2) -> EQUAL (preprocess_exp e1, preprocess_exp e2)
    | NOTEQ (e1, e2) -> NOTEQ (preprocess_exp e1, preprocess_exp e2)
    | AT (e1, e2) -> AT (preprocess_exp e1, preprocess_exp e2) 
    | DOUBLECOLON (e1, e2) -> DOUBLECOLON (preprocess_exp e1, preprocess_exp e2) 
    | STRCON (e1, e2) -> STRCON (preprocess_exp e1, preprocess_exp e2) 
    | EApp (e1, e2) -> EApp (preprocess_exp e1, preprocess_exp e2)
    | ELet (f, is_rec, args, typ, e1, e2) ->
      let (f, is_rec, args, typ, e1) = preprocess_binding (f, is_rec, args, typ, e1) in
      ELet (f, is_rec, args, typ, e1, preprocess_exp e2)
    | EBlock (is_rec, ds, e) -> EBlock (is_rec, List.map (fun binding -> preprocess_binding binding) ds, preprocess_exp e)
    | EMatch (e, bs) -> EMatch (preprocess_exp e, List.map (fun (p, e) -> (p, preprocess_exp e)) bs)
    | IF (e1, e2, e3) -> IF (preprocess_exp e1, preprocess_exp e2, preprocess_exp e3)
    | _ -> exp 
  in
  (l, exp)

and preprocess_binding : binding -> binding
= fun (f, is_rec, args, typ, e) ->
  let (args', e') = extract_func (preprocess_exp e) in
  (f, is_rec, args@args', get_output_typ typ, e')

and extract_func : lexp -> arg list * lexp
= fun (l, exp) ->
  match exp with
  | EFun (arg, e) -> 
    let (args, e) = extract_func e in
    (arg::args, e)
  | ELet (f, is_rec, args, typ, e1, e2) ->
    let (f, is_rec, args, typ, e1) = preprocess_binding (f, is_rec, args, typ, e1) in
    let (args', e2) = extract_func e2 in
    (args', (l, ELet (f, is_rec, args, typ, e1, e2)))
  | EBlock (is_rec, ds, e) -> 
    let ds = List.map (fun binding -> preprocess_binding binding) ds in
    let (args, e) = extract_func e in
    (args, (l, EBlock (is_rec, ds, e)))
  | _ -> ([], (l, exp))

let rec preprocess_decl : decl -> decl 
= fun decl ->
  match decl with
  | DLet binding -> DLet (preprocess_binding binding)
  | DBlock (is_rec, bindings) -> DBlock (is_rec, List.map (fun binding -> preprocess_binding binding) bindings)
  | _ -> decl

let preprocess : prog -> prog
= fun pgm -> List.map (fun decl -> preprocess_decl decl) pgm

let rec is_fun : typ -> bool
= fun typ ->
  match typ with
  (*
  | TList t -> is_fun t
  | TTuple ts -> List.exists is_fun ts
  | TCtor (t, ts) -> List.exists is_fun (t::ts)
  *)
  | TArr _ -> true
  | _ -> false

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
  | EFun (arg, e) -> 
    let (t, e) = normalize t e in
    (t, (l, EFun (arg, e)))
  | ELet (f, is_rec, args, typ, e1, e2) ->
    let (t, e1) = normalize t e1 in
    if args <> [] || is_fun typ then 
      begin match f with
      | BindOne f -> 
        let (t, e2) = normalize (BatMap.add f (args, typ, e1) t) e2 in
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
          (BatMap.add f (args, typ, e) t, ds)
        | _ -> raise (Failure "Extract error : invalid function format")
        end
      else 
        let (t, e) = normalize t e in
        (t, (f, is_rec, args, typ, e)::ds)
    ) (t, []) ds in
    let (t, e) = normalize t e in
    if ds = [] then (t, e) else (t, (l, EBlock (is_rec, List.rev ds, e)))
  | EMatch (e, bs) ->
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
      | BindOne f -> BatMap.add f (args, typ, e) t
      | _ -> raise (Failure "Extract error : invalid function format")
      end
    else t
  | DBlock (is_rec, bindings) -> List.fold_left (fun t binding -> normalize_decl t (DLet binding)) t bindings
  | _ -> t

let normalize_all : prog -> t
= fun pgm ->
  let pgm' = 
    T.run pgm 
    |> preprocess
  in
  let t = List.fold_left (fun t decl -> normalize_decl t decl) BatMap.empty pgm' in
  let _ = 
    Print.print_header "Norm";
    print t 
  in
  if BatMap.is_empty t then raise (Failure "Empty_norm") else t
