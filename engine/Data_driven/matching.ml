open Lang
open Util

(******************************************************************)
(* Compute a minimal function mathcing of two normalized programs *)
(******************************************************************)

(* 'a list1 -> 'a list2 -> comp -> minum matching * unmatched elem *)
let rec find_minimum_combination : 'a list -> 'a list -> ('a -> 'a -> int) -> ('a * 'a * int) list * 'a list 
= fun lst1 lst2 comp ->
  (* Need optimization *)
  let (l1, l2) = (List.length lst1, List.length lst2) in
  if l1 <= l2 then
    let l2_sub = list_permutationk lst2 l1 in
    let (cost, perm) = List.fold_left (fun (cost, perm) perm' -> 
      let cost' = List.fold_left2 (fun acc e1 e2 -> acc + comp e1 e2) 0 lst1 perm' in
      if cost' < cost || cost < 0 then (cost', perm') else (cost, perm)
    ) (-1, []) l2_sub in
    let l2_remain = list_sub lst2 perm in
    (List.map2 (fun e1 e2 -> (e1, e2, comp e1 e2)) lst1 perm, l2_remain)
  else
    let l1_sub = list_permutationk lst1 l2 in
    let (cost, perm) = List.fold_left (fun (cost, perm) perm' -> 
      let cost' = List.fold_left2 (fun acc e1 e2 -> acc + comp e1 e2) 0 lst2 perm' in
      if cost' < cost || cost < 0 then (cost', perm') else (cost, perm)
    ) (-1, []) l1_sub in
    let l1_remain = list_sub lst1 perm in
    (List.map2 (fun e1 e2 -> (e1, e2, comp e1 e2)) perm lst2, l1_remain)

let rec find_all_combination : 'a list -> 'a list -> ('a -> 'a -> int) -> (('a * 'a * int) list * 'a list) list
= fun lst1 lst2 comp ->
  let (l1, l2) = (List.length lst1, List.length lst2) in
  if l1 <= l2 then
    let l2_sub = list_permutationk lst2 l1 in
    List.fold_left (fun acc perm -> 
      (List.map2 (fun e1 e2 -> (e1, e2, comp e1 e2)) lst1 perm, list_sub lst2 perm)::acc
    ) [] l2_sub 
  else
    let l1_sub = list_permutationk lst1 l2 in
    List.fold_left (fun acc perm -> 
      (List.map2 (fun e1 e2 -> (e1, e2, comp e1 e2)) perm lst2, list_sub lst1 perm)::acc
    ) [] l1_sub 

(* Compute syntatic difference : work with the same strategy of template extraction *)
let min : int -> int -> int
= fun n1 n2 -> if n1 < n2 then n1 else n2

let rec exp_size : lexp -> int 
= fun (l, exp) ->
  match exp with
  | EList es | ECtor (_, es) | ETuple es -> 1 + List.fold_left (fun acc e -> acc + exp_size e) 1 es
  | MINUS e | NOT e | EFun (_, e) -> 1 + exp_size e
  | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2)
  | OR (e1, e2) | AND (e1, e2) | LESS (e1, e2) | LARGER (e1, e2) | EQUAL (e1, e2) | NOTEQ (e1, e2)
  | LESSEQ (e1, e2) | LARGEREQ (e1, e2) | AT (e1, e2) | DOUBLECOLON (e1, e2) | STRCON (e1, e2)
  | EApp (e1, e2) | ELet (_, _, _, _, e1, e2) -> 1 + exp_size e1 + exp_size e2
  | EBlock (_, ds, e2) -> 1 + exp_size e2 + List.fold_left (fun acc (f, is_rec, args, typ, e) -> acc + exp_size e) 0 ds
  | EMatch (e, bs) -> 1 + exp_size e + List.fold_left (fun acc (p, e) -> acc + exp_size e) 0 bs
  | IF (e1, e2, e3) -> 1 + exp_size e1 + exp_size e2 + exp_size e3
  | _ -> 1

(* Minimal edit distance *)
let rec match_pat : pat -> pat -> bool
= fun p1 p2 ->
  match (p1, p2) with
  | PUnit, PUnit | PUnder, PUnder | PVar _, PVar _ -> true
  | PInt n1, PInt n2 -> n1 = n2
  | PBool b1, PBool b2 -> b1 = b2
  | PList ps1, PList ps2 | PTuple ps1, PTuple ps2 | PCons ps1, PCons ps2 -> (try List.for_all2 match_pat ps1 ps2 with _ -> false)
  | PCtor (x, ps1), PCtor (y, ps2) -> (x = y) && (try List.for_all2 match_pat ps1 ps2 with _ -> false)
  | Pats ps, _ | _, Pats ps -> raise (Failure "Nomalized programs do not have this pattern")
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
      try List.fold_left2 (fun acc e1 e2 -> acc + edit_distance e1 e2) 1 es1 es2 
      with _ -> exp_size exp1 + exp_size exp2 
    end
  (* Unary *)
  | MINUS e1, MINUS e2 | NOT e1, NOT e2 | EFun (_, e1), EFun (_, e2) -> edit_distance e1 e2
  (* Commutative binary : compute minimal distance *)
  | ADD (e1, e2), ADD (e1', e2') | MUL (e1, e2), MUL (e1', e2') | OR (e1, e2), OR (e1', e2') | AND (e1, e2), AND (e1', e2') 
  | EQUAL (e1, e2), EQUAL (e1', e2') | NOTEQ(e1, e2), NOTEQ (e1', e2') -> 
    (* TODO : generalized *)
    let d1 = edit_distance e1 e1' + edit_distance e2 e2' in
    let d2 = edit_distance e1 e2' + edit_distance e2 e1' in
    min d1 d2
  (* Noncommutative binary *)
  | SUB (e1, e2), SUB (e1', e2') | DIV (e1, e2), DIV (e1', e2') | MOD (e1, e2), MOD (e1', e2') 
  | LESS (e1, e2), LESS (e1', e2') | LARGER (e1, e2), LARGER (e1', e2') 
  | LESSEQ (e1, e2), LESSEQ (e1', e2') | LARGEREQ (e1, e2), LARGEREQ (e1', e2') 
  | AT (e1, e2), AT (e1', e2') | DOUBLECOLON (e1, e2), DOUBLECOLON (e1', e2')| STRCON (e1, e2), STRCON (e1', e2') 
  | EApp (e1, e2), EApp (e1', e2') -> edit_distance e1 e1' + edit_distance e2 e2'
  (* Condition *)
  | IF (e1, e2, e3), IF (e1', e2', e3') -> 
    (* TODO : generalized *)
    let d1 = edit_distance e2 e2' + edit_distance e3 e3' in
    let d2 = edit_distance e2 e3' + edit_distance e3 e2' in
    edit_distance e1 e1' + min d1 d2
  | EMatch (e1, bs1), EMatch (e2, bs2) ->
    (* Distance between matched branches *)
    let (d1, unmatches) = List.fold_left (fun (d1, unmatches) (p, e) ->
      try 
        let (p', e') = List.find (fun (p', e') -> match_pat p p') unmatches in
        (d1 + edit_distance e e', List.remove_assoc p' unmatches)
      with _ -> (d1 + exp_size e, unmatches)
    ) (0, bs2) bs1 in
    (* Distance of unmatches branches *)
    let d2 = List.fold_left (fun acc (p, e) -> acc + (2 * exp_size e)) 0 unmatches in
    edit_distance e1 e2 + d1 + d2
  (* Binding *)
  | ELet (_, _, _, _, e1, e2), ELet (_, _, _, _, e1', e2') -> edit_distance e1 e1' + edit_distance e2 e2'
  | EBlock (_, bs, e), EBlock (_, bs', e') ->
    let (es1, es2) = (List.map (fun (_, _, _, _, e) -> e) bs, List.map (fun (_, _, _, _, e) -> e) bs') in
    let (matches, unmatches) = find_minimum_combination es1 es2 edit_distance in
    edit_distance e e' + List.fold_left (fun acc (e1, e2, d) -> acc + d) 0 matches + List.fold_left (fun acc e -> acc + exp_size e) 0 unmatches
  (* Syntatically different : edit dist = size of two exp *)
  | _ -> exp_size exp1 + exp_size exp2

let compare_norms : (id * lexp) -> (id * lexp) -> int
= fun (f1, e1) (f2, e2) -> edit_distance e1 e2

let print : ((id * lexp) * (id * lexp) * int) BatSet.t * (id * lexp) BatSet.t -> unit
= fun (matches, unmatches) ->
  print_endline ("------Match Informations------");
  BatSet.iter (fun ((f1, e1), (f2, e2), score) -> 
    print_endline "===========================";
    print_endline (f1 ^ " <~> " ^ f2 ^ " : " ^ string_of_int score);
    print_endline (Print.exp_to_string e1);
    print_endline "--------------------------";
    print_endline (Print.exp_to_string e2);
    print_endline "==========================="
  ) matches;
  print_endline ("------Unmatch Informations------");
  BatSet.iter (fun (f, e) -> 
    print_endline "===========================";
    print_endline (f ^ " : ");
    print_endline (Print.exp_to_string e);
    print_endline "==========================="
  ) unmatches

let run : Extractor.t -> Extractor.t -> ((id * lexp) * (id * lexp) * int) BatSet.t * (id * lexp) BatSet.t
= fun t1 t2 ->
  let (t1, t2) = (BatMap.bindings t1, BatMap.bindings t2) in
  let (matches, unmatches) = find_minimum_combination t1 t2 compare_norms in
  (list2set matches, list2set unmatches)

let run2 : Extractor.t -> Extractor.t -> unit
= fun t1 t2 ->
  let (t1, t2) = (BatMap.bindings t1, BatMap.bindings t2) in
  let r = List.map (fun (matches, unmatches) -> (list2set matches, list2set unmatches)) (find_all_combination t1 t2 compare_norms) in
  List.iter (fun (matches, unmathces) ->
    print_endline "***********************";
    print (matches, unmathces);
    print_endline "***********************";
  ) r

