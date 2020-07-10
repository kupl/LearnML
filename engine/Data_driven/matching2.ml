open Lang
open Util
open Print
open Type
open CallGraph

(******************************************************************)
(* Compute a minimal function mathcing of two normalized programs *)
(******************************************************************)

(* sub -> sol matching, unmatchings in sub, unmatchings in sol *)
type matching = (id, id) BatMap.t
type matching_info = matching * id BatSet.t * id BatSet.t

let print : matching_info -> unit
= fun (matching, remaining_sub, remaining_sol) ->
  print_endline ("------Match Informations (submissoin)------");
  print_endline (string_of_map id id matching);
  print_endline ("------Remainings in submission ------");
  print_endline (string_of_set id remaining_sub);
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
= fun (matching, remaining_sub, remaining_sol) ->
  let matching_score = (BatMap.cardinal matching) in
  let remaining_score = (BatSet.cardinal remaining_sub) + (BatSet.cardinal remaining_sol) in
  matching_score - remaining_score

(* Get solution functions which are already matched *)
let rec get_range : (id, id) BatMap.t -> id BatSet.t
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

let rec compute_matching : matching -> graph -> graph -> matching_info
= fun matching g_sub g_sol ->
  (* worklist = functions in submission with no matching *)
  let worklist = BatSet.filter (fun f_sub -> not (BatMap.mem f_sub matching)) (keys (get_node g_sub)) in
  let matching' = BatSet.fold (fun f_sub matching -> 
    (* target = functions in solution are not matched yet *)
    let already_match = BatMap.fold (fun f_sol acc -> BatSet.add f_sol acc) matching BatSet.empty in
    let targets = BatSet.filter (fun f_sol -> not (BatSet.mem f_sol already_match)) (keys (get_node g_sol)) in
    (* Step1 : filtering functions whose types are equivalent with the submission's *)
    let targets = BatSet.filter (fun f_sol -> 
      let (args_sub, typ_sub) = (fun (_, args, typ, _) -> (args, typ)) (BatMap.find f_sub (get_node g_sub)) in
      let (args_sol, typ_sol) = (fun (_, args, typ, _) -> (args, typ)) (BatMap.find f_sol (get_node g_sol)) in
      check_typs typ_sub typ_sol && check_args args_sub args_sol
    ) targets in
    (* Step2 : filtering functions with the same call-relation *)
    (*
    let result = BatSet.filter (fun f_sol -> check_matching_caller matching g_sub g_sol (f_sub, f_sol)) result in
    let _ = 
      print_endline "Candidate2";
      print_endline (Print.string_of_set id result)
    in
    *)  
    (* Step3 : pick most similar(TODO) one *)
    if not (BatSet.is_empty targets) then
      let f_sol = BatSet.choose targets in
      BatMap.add f_sub f_sol matching
    else 
      matching 
  ) worklist matching in
  if BatMap.equal (=) matching matching' then 
    (* If no matchings are found terminate the algorithm => since it scan all possible node there must be new matchings *)
    let remaining_sub = BatMap.foldi (fun f_sub _ remainings -> BatSet.remove f_sub remainings) matching' (keys (get_node g_sub)) in
    let remaining_sol = BatMap.fold (fun f_sol remainings -> BatSet.remove f_sol remainings) matching' (keys (get_node g_sol)) in
    (matching', remaining_sub, remaining_sol)
  else  
    (* If not, repeat again with the new matching informations *)
    compute_matching matching' g_sub g_sol

let run : prog -> prog -> matching_info
= fun pgm cpgm ->
  let (g_sub, g_sol) = (CallGraph.run pgm, CallGraph.run cpgm) in
  (*
  let _ = 
    Print.print_header "Sub graph"; print_graph g_sub;
    Print.print_header "Sol graph"; print_graph g_sol
  in
  *)
  let (entry_sub, entry_sol) = (get_entry_name g_sub, get_entry_name g_sol) in
  compute_matching (BatMap.singleton entry_sub entry_sol) g_sub g_sol
