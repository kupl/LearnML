open Lang
open Util
open Matching

(*********************************************************)
(* Extract repair template from the matching information *)
(*********************************************************)

type repair_template = (label * lexp) (* error label, expected patch *)
type sset = (label * string) BatSet.t (* overlapping checking *)

let rec get_template : UsageAnalysis.t -> UsageAnalysis.t -> lexp -> lexp -> repair_template BatSet.t
= fun t1 t2 (l1, exp1) exp2 ->
  match (exp1, snd exp2) with
  (* Exceptional expressions *)
  | SInt _, SInt _ | SStr _, SStr _ | Hole _, Hole _ | _, Raise _ -> BatSet.empty
  (* Constant *)
  | EUnit, EUnit | TRUE, TRUE | FALSE, FALSE -> BatSet.empty
  | Const n1, Const n2 when n1 = n2 -> BatSet.empty
  | String s1, String s2 when s1 = s2 -> BatSet.empty
  (* List *)
  | EList es1, EList es2 | ETuple es1, ETuple es2 ->
    begin 
      try List.fold_left2 (fun acc e1 e2 -> BatSet.union acc (get_template t1 t2 e1 e2)) BatSet.empty es1 es2 
      with _ -> BatSet.singleton (l1, exp2)
    end
  | ECtor (x1, es1), ECtor (x2, es2) when x1 = x2 ->
    begin 
      try List.fold_left2 (fun acc e1 e2 -> BatSet.union acc (get_template t1 t2 e1 e2)) BatSet.empty es1 es2 
      with _ -> BatSet.singleton (l1, exp2)
    end
  (* Unary *)
  | MINUS e1, MINUS e2 | NOT e1, NOT e2 | EFun (_, e1), EFun (_, e2) -> get_template t1 t2 e1 e2
  (* Commutative binary : compute minimal distance *)
  | ADD (e1, e2), ADD (e1', e2') | MUL (e1, e2), MUL (e1', e2') | OR (e1, e2), OR (e1', e2') | AND (e1, e2), AND (e1', e2') 
  | EQUAL (e1, e2), EQUAL (e1', e2') | NOTEQ(e1, e2), NOTEQ (e1', e2') -> 
    (* TODO : minimalize?? *)
    BatSet.union (get_template t1 t2 e1 e1') (get_template t1 t2 e2 e2')
  (* Noncommutative binary *)
  | SUB (e1, e2), SUB (e1', e2') | DIV (e1, e2), DIV (e1', e2') | MOD (e1, e2), MOD (e1', e2') 
  | LESS (e1, e2), LESS (e1', e2') | LARGER (e1, e2), LARGER (e1', e2') 
  | LESSEQ (e1, e2), LESSEQ (e1', e2') | LARGEREQ (e1, e2), LARGEREQ (e1', e2') 
  | AT (e1, e2), AT (e1', e2') | DOUBLECOLON (e1, e2), DOUBLECOLON (e1', e2')
  | STRCON (e1, e2), STRCON (e1', e2') -> BatSet.union (get_template t1 t2 e1 e1') (get_template t1 t2 e2 e2')
  (* Condition *)
  | IF (e1, e2, e3), IF (e1', e2', e3') ->
    BatSet.union (get_template t1 t2 e1 e1') (get_template t1 t2 e2 e2') 
    |> BatSet.union (get_template t1 t2 e3 e3')
  | EMatch (e1, bs1), EMatch (e2, bs2) ->
    (* Get template from matched branches *)
    let (matches, unmatches) = List.fold_left (fun (matches, unmatches) (p, e) ->
      try 
        let (p', e') = List.find (fun (p', e') -> match_pat p p') unmatches in
        ((e, e')::matches, List.remove_assoc p' unmatches)
      with _ -> (matches, unmatches)
    ) ([], bs2) bs1 in
    (* if all branches are matched with solution *)
    if matches = [] then 
      BatSet.add (l1, exp2) (get_template t1 t2 e1 e2)
    else List.fold_left (fun acc (e1, e2) -> BatSet.union acc (get_template t1 t2 e1 e2)) (get_template t1 t2 e1 e2) matches
  (* Special Cases : var, function, bindings *)
  | EVar x, EVar y -> BatSet.singleton (l1, exp2)
    (*
    (try 
      let typ1 = fst (BatMap.find x (BatMap.find l1 t1)) in
      let typ2 = fst (BatMap.find y (BatMap.find (fst exp2) t2)) in
      if Type.check_typs typ1 typ2 then BatSet.empty else BatSet.singleton (l1, exp2)
    with _ -> BatSet.singleton (l1, exp2))
    *)
  | _, EApp _ | _, ELet _ | _, EBlock _ -> BatSet.singleton (l1, exp2)
  (* Syntatically different *)
  | _ -> BatSet.singleton (l1, exp2)

(* Replace special templates (var, func) with hole *)
let rec make_hole : UsageAnalysis.usage_map -> UsageAnalysis.usage_map -> lexp -> lexp
= fun m1 m2 (l, exp) ->
  let exp = 
    match exp with
    | EVar x -> gen_hole ()
      (*
      (try 
        let typ2 = fst (BatMap.find x m2) in
        let similar_usages = BatMap.filter () m1 in
        let y = fst (List.find (fun (y, (typ1, _)) -> Type.check_typs typ1 typ2) m1) in
        EVar y
      with _ -> gen_hole ())
      *)
    | EApp (e1, e2) -> EApp (gen_labeled_hole (), gen_labeled_hole ())
    | Raise e -> Raise (make_hole m1 m2 e)
    | EFun (arg, e) -> EFun (arg, make_hole m1 m2 e)
    | MINUS e -> MINUS (make_hole m1 m2 e)
    | NOT e -> NOT (make_hole m1 m2 e)
    | ADD (e1, e2) -> ADD (make_hole m1 m2 e1, make_hole m1 m2 e2)
    | SUB (e1, e2) -> SUB (make_hole m1 m2 e1, make_hole m1 m2 e2)
    | MUL (e1, e2) -> MUL (make_hole m1 m2 e1, make_hole m1 m2 e2)
    | DIV (e1, e2) -> DIV (make_hole m1 m2 e1, make_hole m1 m2 e2)
    | MOD (e1, e2) -> MOD (make_hole m1 m2 e1, make_hole m1 m2 e2)
    | OR (e1, e2) -> OR (make_hole m1 m2 e1, make_hole m1 m2 e2)
    | AND (e1, e2) -> AND (make_hole m1 m2 e1, make_hole m1 m2 e2)
    | LESS (e1, e2) -> LESS (make_hole m1 m2 e1, make_hole m1 m2 e2)
    | LESSEQ (e1, e2) -> LESSEQ (make_hole m1 m2 e1, make_hole m1 m2 e2)
    | LARGER (e1, e2) -> LARGER (make_hole m1 m2 e1, make_hole m1 m2 e2)
    | LARGEREQ (e1, e2) -> LARGEREQ (make_hole m1 m2 e1, make_hole m1 m2 e2)
    | EQUAL (e1, e2) -> EQUAL (make_hole m1 m2 e1, make_hole m1 m2 e2)
    | NOTEQ (e1, e2) -> NOTEQ (make_hole m1 m2 e1, make_hole m1 m2 e2)
    | DOUBLECOLON (e1, e2) -> DOUBLECOLON (make_hole m1 m2 e1, make_hole m1 m2 e2)
    | AT (e1, e2) -> AT (make_hole m1 m2 e1, make_hole m1 m2 e2)
    | STRCON (e1, e2) -> STRCON (make_hole m1 m2 e1, make_hole m1 m2 e2)
    | EList es -> EList (List.map (make_hole m1 m2) es)
    | ETuple es -> ETuple (List.map (make_hole m1 m2) es)
    | ECtor (x, es) -> ECtor (x, List.map (make_hole m1 m2) es)
    | IF (e1, e2, e3) -> IF (make_hole m1 m2 e1, make_hole m1 m2 e2, make_hole m1 m2 e3)
    | EMatch (e, bs) -> EMatch (make_hole m1 m2 e, List.map (fun (p, e) -> (p, make_hole m1 m2 e)) bs)
    | ELet (f, is_rec, args, typ, e1, e2) -> ELet (f, is_rec, args, typ, make_hole m1 m2 e1, make_hole m1 m2 e2)
    | EBlock (is_rec, bindings, e2) -> EBlock (is_rec, List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, args, typ, make_hole m1 m2 e)) bindings, make_hole m1 m2 e2)
    | _ -> exp
  in
  (l, exp)

let print : repair_template BatSet.t -> unit
= fun temps ->
  BatSet.iter(fun (l, e) ->
    print_endline ("Fix : " ^ string_of_int l ^ " using Exp : " ^ Print.exp_to_string e)
  ) temps

let add_template : repair_template -> (repair_template BatSet.t * sset) -> (repair_template BatSet.t * sset)
= fun (l, temp) (temps, sset) ->
  let temp_string = (l, Print.exp_to_string temp) in
  if BatSet.mem (temp_string) sset then (temps, sset) else (BatSet.add (l, temp) temps, BatSet.add temp_string sset)

let union_templates : repair_template BatSet.t -> (repair_template BatSet.t * sset) -> (repair_template BatSet.t * sset)
= fun temps acc -> BatSet.fold (fun temp acc -> add_template temp acc) temps acc 

let run : UsageAnalysis.t -> UsageAnalysis.t -> ((id * lexp) * (id * lexp)) BatSet.t -> (repair_template BatSet.t * sset)
= fun t1 t2 matching ->
  BatSet.fold (fun ((id1, e1), (id2, e2)) acc ->
    let new_templates = BatSet.map (fun (l, e) ->
      let (m1, m2) = (BatMap.find l t1, BatMap.find (fst e) t2) in
      (l, make_hole m1 m2 e)
    ) (get_template t1 t2 e1 e2) in
    union_templates new_templates acc
  ) matching (BatSet.empty, BatSet.empty)

let run2 : UsageAnalysis.t -> UsageAnalysis.t -> ((id * lexp) * (id * lexp)) BatSet.t -> repair_template BatSet.t
= fun t1 t2 matching ->
  BatSet.fold (fun ((id1, e1), (id2, e2)) acc ->
    let new_templates = BatSet.map (fun (l, e) ->
      let (m1, m2) = (BatMap.find l t1, BatMap.find (fst e) t2) in
      (l, make_hole m1 m2 e)
    ) (get_template t1 t2 e1 e2) in
    BatSet.union new_templates acc
  ) matching BatSet.empty