open Lang
open Util
open Print
open Type
open CallGraph

(******************************************************************)
(* Compute a minimal function mathcing of two normalized programs *)
(******************************************************************)

(* sub -> sol matching, unmatchings in sol *)
(* TODO unique matchign *)
type matching_info = (id, id) BatMap.t * id BatSet.t

let print : matching_info -> unit
= fun (matching, remaining_sol) ->
  print_endline ("------Match Informations (submissoin)------");
  print_endline (string_of_map id id matching);
  print_endline ("------Remainings in solution ------");
  print_endline (string_of_set id remaining_sol)

let rec check_args : arg list -> arg list -> bool
= fun args1 args2 ->
  try
    List.for_all2 (fun arg1 arg2 -> check_arg arg1 arg2) args1 args2 
  with _ -> false 

and check_arg : arg -> arg -> bool
= fun arg1 arg2 ->
  match arg1, arg2 with
  | ArgUnder t1, ArgUnder t2 | ArgOne (_, t1), ArgOne (_, t2) -> check_typs t1 t2 
  | ArgTuple args1, ArgTuple args2 -> check_args args1 args2
  | _ -> false

let rec get_matching_score : matching_info -> int 
= fun (matching, remainings) ->
  let matching_score = BatMap.cardinal matching in
  let remaining_score = BatSet.cardinal remainings in
  matching_score - remaining_score

(* Get solution functions which are already matched *)
let rec get_range_of_matching : (id, id) BatMap.t -> id BatSet.t
= fun matching -> BatMap.fold (fun id acc -> BatSet.add id acc) matching BatSet.empty 

(* TODO : deal mutually recursive case *)
let rec check_matching_caller : (id, id) BatMap.t -> graph -> graph -> id * id -> bool
= fun matching g_sub g_sol (f_sub, f_sol) -> 
  (* Based on the assumption, checking matching of caller functions *)
  let callers_sub = get_caller_by_name f_sub g_sub in 
  let callers_sol = get_caller_by_name f_sol g_sol in
  let _ = 
    print_endline ("callers_sub " ^ f_sub);
    print_endline (Print.string_of_set id callers_sub);
    print_endline ("callers_sol " ^ f_sol);
    print_endline (Print.string_of_set id callers_sol);
  in
  if BatSet.cardinal callers_sub <> BatSet.cardinal callers_sol then 
    (* if two functions have different number of callers then matching fail *)
    false
  else 
    (* Check callers are matched based on the assumption *)
    let (remainings_sub, remainings_sol) = BatSet.fold (fun caller_sub (remainings_sub, remainings_sol) -> 
      (* There exists an unmatching caller in sub => false *)
      if not (BatSet.is_empty remainings_sub) then 
        (remainings_sub, remainings_sol)
      else 
        let remainings_sol' = BatSet.fold (fun caller_sol remainings_sol -> 
          if BatMap.mem caller_sub matching then
            (* Rule1 : Check if a caller of sub match with caller of solution *)
            if caller_sub = (BatMap.find caller_sub matching) then remainings_sol else BatSet.add caller_sol remainings_sol  
          else
            (* Rule2 (TODO) : if the caller call current function (mutual recursion), check mutually recursive case??? *)
            remainings_sol
        ) remainings_sol BatSet.empty in 
        if BatSet.equal remainings_sol remainings_sol' then 
          (BatSet.add caller_sub remainings_sub, remainings_sol)
        else (remainings_sub, remainings_sol')
    ) callers_sub (BatSet.empty, callers_sol) in
    let remainings = BatSet.union remainings_sub remainings_sol in
    not (BatSet.is_empty remainings)

let rec compute_matching : (id, id) BatMap.t -> graph -> graph -> matching_info
= fun matching g_sub g_sol ->
  (* Compute matching set based on the previous matching *)
  let matching' = BatMap.foldi (fun f_sub (id_sub, args_sub, typ_sub, body_sub) matching -> 
    let _ =
      print_endline ("-------\nCurrent Matching : " ^ f_sub ^ "\n--------");
      print (matching, BatMap.fold (fun f_sol remainings -> BatSet.remove f_sol remainings) matching (keys (get_node g_sol)))
    in
    if BatMap.mem f_sub matching then
      (* If the target funtion already has matching do not update *)
      matching
    else 
      let _ = print_endline "Here" in 
      let matched = get_range_of_matching matching in
      (* Step1 : filtering functions whose types are equivalent with the submission's *)
      let result = BatMap.foldi (fun f_sol (id_sol, args_sol, typ_sol, body_sol) result ->
        if BatSet.mem f_sol matched then
          result 
        else if check_typs typ_sub typ_sol && check_args (BatSet.to_list args_sub) (BatSet.to_list args_sol) then
          BatSet.add f_sol result
        else result
      ) (get_node g_sol) BatSet.empty in
      let _ = 
        print_endline "Candidate1";
        print_endline (Print.string_of_set id result)
      in
      (* Step2 : filtering functions with the same call-relation *)
      let result = BatSet.filter (fun f_sol -> check_matching_caller matching g_sub g_sol (f_sub, f_sol)) result in
      let _ = 
        print_endline "Candidate2";
        print_endline (Print.string_of_set id result)
      in
      (* Step3 (TODO) : pick most similar one *)
      if not (BatSet.is_empty result) then
        let matching_sol = BatSet.choose result in
        BatMap.add f_sub matching_sol matching
      else 
        matching 
  ) (get_node g_sub) matching in
  if BatMap.equal (=) matching matching' then 
    (* If no matchings are found terminate the algorithm => since it scan all possible node there must be new matchings *)
    (matching', BatMap.fold (fun f_sol remainings -> BatSet.remove f_sol remainings) matching' (keys (get_node g_sol)))
  else  
    (* If not, repeat again with the new matching informations *)
    compute_matching matching' g_sub g_sol

let run : prog -> prog -> matching_info
= fun pgm cpgm ->
  let (g_sub, g_sol) = (CallGraph.run pgm, CallGraph.run cpgm) in
  let (entry_sub, entry_sol) = (get_entry_name g_sub, get_entry_name g_sol) in
  compute_matching (BatMap.singleton entry_sub entry_sol) g_sub g_sol
