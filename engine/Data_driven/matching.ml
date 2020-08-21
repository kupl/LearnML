open Lang
open Util

(******************************************************************)
(* Compute a minimal function mathcing of two normalized programs *)
(******************************************************************)

type matching_info = ((id * (lexp * typ)) * (id * (lexp * typ))) BatSet.t * (id * (lexp * typ)) BatSet.t * int

(* ?constraint -> 'a list1 -> 'a list2 -> comp1 -> comp2 -> minum matching * unmatched elem * score *)
let rec find_minimum_combination ?(const = (fun e1 e2 -> true)) : 'a list -> 'a list -> ('a -> int) -> ('a -> 'a -> int) -> ('a * 'a) list * 'a list * int
= fun lst1 lst2 comp1 comp2 ->
  (* Need optimization & generalization *)
  let (l1, l2) = (List.length lst1, List.length lst2) in
  if l1 <= l2 then
    List.fold_left (fun (matches, unmatches, cost) cand -> 
      if (List.for_all2 const lst1 cand) then
        let remain = list_sub lst2 cand in
        let cost' = (List.fold_left2 (fun acc e1 e2 -> acc + comp2 e1 e2) 0 lst1 cand) + (List.fold_left (fun acc e -> comp1 e) 0 remain) in
        if cost' < cost || matches = [] then (List.map2 (fun e1 e2 -> (e1, e2)) lst1 cand, remain, cost') else (matches, unmatches, cost)
      else 
        (matches, unmatches, cost)
    ) ([], [], 0) (list_permutationk lst2 l1)
  else
    (* Redundant *)
    List.fold_left (fun (matches, unmatches, cost) cand ->
      if (List.for_all2 const lst2 cand) then
        let remain = list_sub lst1 cand in
        let cost' = (List.fold_left2 (fun acc e1 e2 -> acc + comp2 e1 e2) 0 lst2 cand) + (List.fold_left (fun acc e -> comp1 e) 0 remain) in
        if cost' < cost || matches = [] then (List.map2 (fun e1 e2 -> (e1, e2)) cand lst2, remain, cost') else (matches, unmatches, cost)
      else 
        (matches, unmatches, cost)
    ) ([], [], 0) (list_permutationk lst1 l2)

let rec find_all_combination : 'a list -> 'a list -> ('a -> int) -> ('a -> 'a -> int) -> (('a * 'a) list * 'a list * int) list
= fun lst1 lst2 comp1 comp2 ->
  let (l1, l2) = (List.length lst1, List.length lst2) in
  if l1 <= l2 then
    List.fold_left (fun acc cand -> 
      let remain = list_sub lst2 cand in
      let cost' = (List.fold_left2 (fun acc e1 e2 -> acc + comp2 e1 e2) 0 lst1 cand) + (List.fold_left (fun acc e -> comp1 e) 0 remain) in
      (List.map2 (fun e1 e2 -> (e1, e2)) lst1 cand, remain, cost')::acc
    ) [] (list_permutationk lst2 l1)
  else
    List.fold_left (fun acc cand -> 
      let remain = list_sub lst1 cand in
      let cost' = (List.fold_left2 (fun acc e1 e2 -> acc + comp2 e1 e2) 0 lst2 cand) + (List.fold_left (fun acc e -> comp1 e) 0 remain) in
      (List.map2 (fun e1 e2 -> (e1, e2)) cand lst2, remain, cost')::acc
    ) [] (list_permutationk lst1 l2)

(* Compute syntatic difference : work with the same strategy of template extraction *)
let min : int -> int -> int
= fun n1 n2 -> if n1 < n2 then n1 else n2

(* Minimal edit distance *)
let rec match_pat : pat -> pat -> bool
= fun p1 p2 ->
  match (p1, p2) with
  | PUnit, PUnit -> true
  | PInt n1, PInt n2 -> n1 = n2
  | PBool b1, PBool b2 -> b1 = b2
  | PList ps1, PList ps2 | PTuple ps1, PTuple ps2 | PCons ps1, PCons ps2 -> (try List.for_all2 match_pat ps1 ps2 with _ -> false)
  | PCtor (x, ps1), PCtor (y, ps2) -> (x = y) && (try List.for_all2 match_pat ps1 ps2 with _ -> false)
  | Pats ps, _ | _, Pats ps -> raise (Failure "Nomalized programs do not have this pattern")
  | PUnder, _ | _, PUnder | PVar _, _ | _, PVar _ -> true
  | _ -> false

let rec edit_distance : lexp -> lexp -> int
= fun exp1 exp2 ->
  match (snd exp1, snd exp2) with
  (* Exceptional expressions *)
  | SInt _, SInt _ | SStr _, SStr _ | Hole _, Hole _ | Raise _, Raise _ | EVar _, EVar _ -> 0
  (* Constant *)
  | EUnit, EUnit | TRUE, TRUE | FALSE, FALSE -> 0
  | Const n1, Const n2 when n1 = n2 -> 0
  | String s1, String s2 when s1 = s2 -> 0
  (* List *)
  | EList es1, EList es2 | ETuple es1, ETuple es2 ->
    begin 
      try List.fold_left2 (fun acc e1 e2 -> acc + edit_distance e1 e2) 0 es1 es2 
      with _ -> exp_size exp1 + exp_size exp2 
    end
  | ECtor (x1, es1), ECtor (x2, es2) when x1 = x2 ->
    begin 
      try List.fold_left2 (fun acc e1 e2 -> acc + edit_distance e1 e2) 0 es1 es2 
      with _ -> exp_size exp1 + exp_size exp2 
    end
  (* Unary *)
  | MINUS e1, MINUS e2 | NOT e1, NOT e2 | EFun (_, e1), EFun (_, e2) -> edit_distance e1 e2
  (* Commutative binary : compute minimal distance *)
  | ADD (e1, e2), ADD (e1', e2') | MUL (e1, e2), MUL (e1', e2') | OR (e1, e2), OR (e1', e2') | AND (e1, e2), AND (e1', e2') 
  | EQUAL (e1, e2), EQUAL (e1', e2') | NOTEQ(e1, e2), NOTEQ (e1', e2') -> 
    let (_, _, d) = find_minimum_combination [e1; e2] [e1'; e2'] exp_size edit_distance in 
    d
  (* Noncommutative binary *)
  | SUB (e1, e2), SUB (e1', e2') | DIV (e1, e2), DIV (e1', e2') | MOD (e1, e2), MOD (e1', e2') 
  | LESS (e1, e2), LESS (e1', e2') | LARGER (e1, e2), LARGER (e1', e2') 
  | LESSEQ (e1, e2), LESSEQ (e1', e2') | LARGEREQ (e1, e2), LARGEREQ (e1', e2') 
  | AT (e1, e2), AT (e1', e2') | DOUBLECOLON (e1, e2), DOUBLECOLON (e1', e2')| STRCON (e1, e2), STRCON (e1', e2') 
  | EApp (e1, e2), EApp (e1', e2') -> edit_distance e1 e1' + edit_distance e2 e2'
  (* Condition *)
  | IF (e1, e2, e3), IF (e1', e2', e3') -> edit_distance e1 e1' + edit_distance e2 e2' + edit_distance e3 e3'
  | EMatch (e1, bs1), EMatch (e2, bs2) ->
    (* Distance between matched branches *)
    let (d1, unmatches) = List.fold_left (fun (d1, unmatches) (p, e) ->
      try 
        let (p', e') = List.find (fun (p', e') -> match_pat p p') unmatches in
        (d1 + edit_distance e e', List.remove_assoc p' unmatches)
      with _ -> (d1 + exp_size e, unmatches)
    ) (0, bs2) bs1 in
    (* Distance of unmatches branches *)
    let d2 = List.fold_left (fun acc (p, e) -> acc + exp_size e) 0 unmatches in
    edit_distance e1 e2 + d1 + d2
  (* Binding *)
  | ELet (_, _, _, _, e1, e2), ELet (_, _, _, _, e1', e2') -> edit_distance e1 e1' + edit_distance e2 e2'
  | EBlock (_, bs, e), EBlock (_, bs', e') ->
    let (es1, es2) = (List.map (fun (_, _, _, _, e) -> e) bs, List.map (fun (_, _, _, _, e) -> e) bs') in
    let (_, _, score) = find_minimum_combination es1 es2 exp_size edit_distance in
    edit_distance e e' + score
  (* Syntatically different : edit dist = size of two exp *)
  | _ -> exp_size exp1 + exp_size exp2

let measure_norm : (id * (lexp * typ)) -> int
= fun (f, (e, typ)) -> exp_size e

let compare_norms : (id * (lexp * typ)) -> (id * (lexp * typ)) -> int
= fun (f1, (e1, typ1)) (f2, (e2, typ2)) -> edit_distance e1 e2

let constraint_norms : (id * (lexp * typ)) -> (id * (lexp * typ)) -> bool
= fun (f1, (e1, typ1)) (f2, (e2, typ2)) -> Type.check_typs typ1 typ2

let print : matching_info -> unit
= fun (matches, unmatches, score) ->
  print_endline ("------Match Informations------");
  BatSet.iter (fun ((f1, (e1, typ1)), (f2, (e2, typ2))) -> 
    let score = compare_norms (f1, (e1, typ1)) (f2, (e2, typ2)) in
    print_endline "===========================";
    print_endline (f1 ^ "(" ^ Print.type_to_string typ1 ^ ")" ^ " <~> " ^ f2 ^ "(" ^ Print.type_to_string typ2 ^ ")" ^ " : " ^ string_of_int score);
    print_endline (Print.exp_to_string e1);
    print_endline "--------------------------";
    print_endline (Print.exp_to_string e2);
    print_endline "==========================="
  ) matches;
  print_endline ("------Unmatch Informations------");
  BatSet.iter (fun (f, (e, typ)) -> 
    print_endline "===========================";
    print_endline (f ^ "(" ^ Print.type_to_string typ ^ ")" ^ " : " ^ string_of_int (exp_size e));
    print_endline (Print.exp_to_string e);
    print_endline "==========================="
  ) unmatches;
  print_endline ("------Difference : " ^ string_of_int score ^ "------")

let run : Normalizer.t -> Normalizer.t -> matching_info
= fun t1 t2 ->
  let (t1, t2) = (BatMap.bindings t1, BatMap.bindings t2) in
  let (matches, unmatches, score) = find_minimum_combination ~const:constraint_norms t1 t2 measure_norm compare_norms in
  (list2set matches, list2set unmatches, score)

let run2 : Normalizer.t -> Normalizer.t -> unit
= fun t1 t2 ->
  let (t1, t2) = (BatMap.bindings t1, BatMap.bindings t2) in
  let r = List.map (fun (matches, unmatches, cost) -> (list2set matches, list2set unmatches, cost)) (find_all_combination t1 t2 measure_norm compare_norms) in
  List.iter (fun comb ->
    print_endline "***********************";
    print comb;
    print_endline "***********************";
  ) r
