open Lang
open Util
open Print
open Type
open CallGraph
open Path_score

(* patch unit = name * args * output typ * calling context (path) * function body *)
type patch_unit = id * arg list * typ * calling_ctx BatSet.t * lexp 
(* (depth1) calling ctx: caller function info * calling path *)
and calling_ctx = (id * arg list) * path 
type patch_units = patch_unit BatSet.t

(* 
  Matching info
  error function => solution function * callee funcions
*)
type unit_matching = (patch_unit, patch_unit * patch_unit BatSet.t) BatMap.t
type unit_matching2 = (patch_unit, string * patch_unit * patch_unit BatSet.t) BatMap.t

let empty_matching = BatMap.empty

let ctor_table = ref BatMap.empty 

(* Utility functions *)
let get_id (id, args, typ, ctxs, body) = id 
let get_args (id, args, typ, ctxs, body) = args
let get_typ (id, args, typ, ctxs, body) = typ
let get_ctxs (id, args, typ, ctxs, body) = ctxs
let get_body (id, args, typ, ctxs, body) = body
let get_func_typ (id, args, typ, ctxs, body) = 
  let rec arg_to_typ arg =
    match arg with
    | ArgUnder typ | ArgOne (_, typ) -> typ 
    | ArgTuple args -> TTuple (List.map arg_to_typ args)
  in
  List.fold_left (fun acc arg -> TArr (arg_to_typ arg, acc)) typ (List.rev args)

(* pp *)
let string_of_paths paths = 
  BatSet.fold (fun path acc -> 
    if acc = "" then string_of_path path else acc ^ "\\/\n" ^ string_of_path path
  ) paths ""

let string_of_ctx ((f, args), path) = 
  "(" ^ f ^ "[" ^ (args_to_string args "") ^ "]" ^ "):" ^ string_of_path path 

let string_of_ctxs ctxs = 
  BatSet.fold (fun ctx acc -> 
    if acc = "" then string_of_ctx ctx else acc ^ "\n" ^ string_of_ctx ctx
  ) ctxs ""

let string_of_unit patch_unit =
  "Typ : " ^ type_to_string (get_func_typ patch_unit) ^ "\n" ^
  "Calling_Ctx : " ^ string_of_ctxs (get_ctxs patch_unit) ^ "\n" ^ 
  "Body : " ^ exp_to_string (get_body patch_unit)

let string_of_callees callees =
  BatSet.fold (fun callee_func acc -> 
    let s = string_of_unit callee_func in
    if acc = "" then s else acc ^ "\n---------------------------\n" ^ s
  ) callees ""

let string_of_matching matching = 
  BatMap.foldi (fun unit_sub unit_sol acc ->
    let s = string_of_unit unit_sub ^ "\n => \n" ^ string_of_unit unit_sol in
    if acc = "" then s else acc ^ "\n---------------------------\n" ^ s
  ) matching ""

let string_of_matching2 matching = 
  BatMap.foldi (fun unit_sub (f_name, unit_sol, callees) acc ->
    let s = 
      string_of_unit unit_sub ^ "\n => \n" ^ string_of_unit unit_sol ^
      "\n" ^ "-- Callee functions --\n" ^ string_of_callees callees ^
      "\n" ^ "Solution : " ^ f_name
    in
    if acc = "" then s else acc ^ "\n---------------------------\n" ^ s
  ) matching ""

(* Compute syntactic difference *)
let rec match_pat : pat -> pat -> bool
= fun p1 p2 ->
  match (p1, p2) with
  | PInt n1, PInt n2 -> n1 = n2
  | PBool b1, PBool b2 -> b1 = b2
  | PList ps1, PList ps2 | PTuple ps1, PTuple ps2 | PCons ps1, PCons ps2 -> (try List.for_all2 match_pat ps1 ps2 with _ -> false)
  | PCtor (x, ps1), PCtor (y, ps2) -> (x = y) && (try List.for_all2 match_pat ps1 ps2 with _ -> false)
  | Pats ps, _ | _, Pats ps -> raise (Failure "Nomalized programs do not have this pattern")
  | PUnit, PUnit | PUnder, PUnder | PVar _, PVar _ -> true
  | _ -> false

let rec edit_distance : lexp -> lexp -> int
= fun exp1 exp2 ->
  match (snd exp1, snd exp2) with
  | SInt _, SInt _ | SStr _, SStr _ | Hole _, Hole _ -> 0
  (* Constant *)
  | EUnit, EUnit | TRUE, TRUE | FALSE, FALSE | EVar _, EVar _ -> 0
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
  | MINUS e1, MINUS e2 | NOT e1, NOT e2 | ERef e1, ERef e2 | EDref e1, EDref e2 | Raise e1, Raise e2 | EFun (_, e1), EFun (_, e2) -> edit_distance e1 e2
  (* Binary *)
  | ADD (e1, e2), ADD (e1', e2') | SUB (e1, e2), SUB (e1', e2') | MUL (e1, e2), MUL (e1', e2') | DIV (e1, e2), DIV (e1', e2') | MOD (e1, e2), MOD (e1', e2') 
  | OR (e1, e2), OR (e1', e2') | AND (e1, e2), AND (e1', e2') | LESS (e1, e2), LESS (e1', e2') | LESSEQ (e1, e2), LESSEQ (e1', e2')
  | LARGER (e1, e2), LARGER (e1', e2') | LARGEREQ (e1, e2), LARGEREQ (e1', e2') | EQUAL (e1, e2), EQUAL (e1', e2') | NOTEQ(e1, e2), NOTEQ (e1', e2') 
  | DOUBLECOLON (e1, e2), DOUBLECOLON (e1', e2') | AT (e1, e2), AT (e1', e2') | STRCON (e1, e2), STRCON (e1', e2') | EAssign (e1, e2), EAssign (e1', e2') 
  | EApp (e1, e2), EApp (e1', e2') | ELet (_, _, _, _, e1, e2), ELet (_, _, _, _, e1', e2') -> edit_distance e1 e1' + edit_distance e2 e2'
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
  (* Binding block *)
  | EBlock (_, bs1, e1), EBlock (_, bs2, e2) ->
    let (es1, es2) = (List.map (fun (_, _, _, _, e) -> e) bs1, List.map (fun (_, _, _, _, e) -> e) bs2) in
    begin 
      try List.fold_left2 (fun acc e1 e2 -> acc + edit_distance e1 e2) (edit_distance e1 e2) es1 es2 
      with _ -> (edit_distance e1 e2) + (List.fold_left (fun acc e -> exp_size e) 0 es1) + (List.fold_left (fun acc e -> exp_size e) 0 es2)
    end
  (* Syntatically different *)
  | _ -> exp_size exp1 + exp_size exp2

let edit_distance_unit : patch_unit -> patch_unit -> int
= fun unit1 unit2 -> edit_distance (get_body unit1) (get_body unit2)

let get_patch_unit : graph -> patch_units
= fun cg ->
  let nodes = get_node cg in
  BatMap.foldi (fun name (id, args, typ, body) acc ->
    let edges = get_linked_edge id cg in
    let ctxs = BatMap.foldi (fun (s, t) ctx acc -> 
      let caller_f = get_function_name s cg in
      let caller_args = (fun (id, args, typ, body) -> args) (BatMap.find caller_f nodes) in 
      let calling_ctxs = BatSet.map (fun (l, path) -> ((caller_f, caller_args), path)) ctx in
      BatSet.union calling_ctxs acc
    ) edges BatSet.empty
    in
    BatSet.add (name, args, typ, ctxs, body) acc
  ) nodes BatSet.empty

let gen_vc : path -> path BatSet.t -> path
= fun path_sub paths_sol ->
  if BatSet.is_empty paths_sol then path_sub
  else if BatSet.cardinal paths_sol = 1 then
    Bop (And, path_sub, BatSet.choose paths_sol)
  else
    let (path_sol, paths_sol) = BatSet.pop paths_sol in
    let path_sol = BatSet.fold (fun path_sol acc ->
      Bop (Or, acc, path_sol)
    ) paths_sol path_sol in
    Bop (And, path_sub, path_sol)

let rec arg_to_path : arg -> path
= fun arg ->
  match arg with
  | ArgUnder typ -> Symbol (fresh_symbol (), typ)
  | ArgOne (x, typ) -> Var (x, typ)
  | ArgTuple args -> Tuple (List.map arg_to_path args)

let rec match_arg : arg -> arg -> bool
= fun arg1 arg2 ->
  match arg1, arg2 with
  | ArgUnder typ1, ArgUnder typ2 | ArgUnder typ1, ArgOne (_, typ2) 
  | ArgOne (_, typ1), ArgUnder typ2 | ArgOne (_, typ1), ArgOne (_, typ2) -> 
    Type.check_typs typ1 typ2 
  | ArgTuple args1, ArgTuple args2 -> (try List.for_all2 match_arg args1 args2 with _ -> false)
  | _ -> false

let gen_vc : (arg list * path) -> (arg list * path) -> path
= fun (args_sub, path_sub) (args_sol, path_sol) ->
  try
    List.fold_left2 (fun vc arg_sub arg_sol ->
      if match_arg arg_sub arg_sol then
        let new_path = EQop (Eq, arg_to_path arg_sub, arg_to_path arg_sol) in
        Bop (And, new_path, vc)
      else Bool false 
    ) (Bop (And, path_sub, path_sol)) args_sub args_sol 
  with _ -> Bool false 

let rec find_local_matching2 : patch_units -> (id * prog) -> unit_matching2
= fun units_sub (f_name, cpgm) ->
  let cg_sol = extract_graph cpgm in
  let units_sol = get_patch_unit cg_sol in
  print_endline f_name;
  let local_matching = BatSet.fold (fun unit_sub matching -> 
    let typ_sub = get_func_typ unit_sub in
    (* Find a solution functions whose type is the same with the type of the target function *)
    let candidates = BatSet.filter (fun unit_sol -> check_typs typ_sub (get_func_typ unit_sol)) units_sol in
    (* Pick solution functions whose path are the most simiar with the target function *)
    let (_, candidates) = BatSet.fold (fun unit_sol (score, candidates) ->
      (* Compute path matching score*)
      let score' = BatSet.fold (fun ((caller_sub, args_sub), path_sub) acc ->
        let is_match = BatSet.exists (fun ((caller_sol, args_sol), path_sol) -> 
          let vc = gen_vc (args_sub, path_sub) (args_sol, path_sol) in
          let _ = print_endline (string_of_path vc) in
          not (check_path !ctor_table vc)
        ) (get_ctxs unit_sol) in
        if is_match then acc+1 else acc
      ) (get_ctxs unit_sub) 0 in
      (* Collecting the functions with the highest score *)
      if score' = score then (score', BatSet.add unit_sol candidates)
      else if score' > score then (score', BatSet.singleton unit_sol)
      else (score, candidates)
    ) candidates (-1, BatSet.empty) in
    if BatSet.is_empty candidates then
      (* If there are no matched solutions, do not change the matching relation *)
      matching
    else
      (* Pick the most similar ones in candidates *)
      let (candidate, remains) = BatSet.pop candidates in
      let unit_sol = BatSet.fold (fun cur acc -> 
        if edit_distance_unit unit_sub cur < edit_distance_unit unit_sub acc then cur else acc 
      ) remains candidate in
      BatMap.add unit_sub (f_name, unit_sol, BatSet.empty) matching
  ) units_sub empty_matching in
  (* Update calling information of unmatched functions *)
  let unmatched_funcs = BatSet.filter (fun func -> 
    not (BatMap.exists (fun unit_sub (f_name, unit_sol, callees) -> 
          (get_id func) = (get_id unit_sol)
        ) local_matching)
  ) units_sol in 
  BatSet.fold (fun func matching ->
    let callers = BatSet.map (fun ((caller_id, _), _) -> caller_id) (get_ctxs func) in
    BatMap.map (fun (f_name, unit_sol, callees) -> 
      if BatSet.mem (get_id unit_sol) callers then 
        (f_name, unit_sol, BatSet.add func callees)
      else (f_name, unit_sol, callees)
    ) matching
  ) unmatched_funcs local_matching

let rec update_matching2 : unit_matching2 -> unit_matching2 -> unit_matching2
= fun global_matching local_matching ->
  BatMap.foldi (fun unit_sub (f_name, unit_sol, callees) global ->
    if BatMap.mem unit_sub global then
      let (f_name', unit_sol', callee') = BatMap.find unit_sub global in
      (* Compare path matching score*)
      let score = 0 in
      let score' = 0 in
      (* Compare syntactic distance  *)
      if score = score' then 
        let d1 = edit_distance_unit unit_sub unit_sol in
        let d2 = edit_distance_unit unit_sub unit_sol' in
        if d1 < d2 then BatMap.add unit_sub (f_name, unit_sol, callees) global else global
      else 
        if score > score' then BatMap.add unit_sub (f_name, unit_sol, callees) global else global
    else 
      BatMap.add unit_sub (f_name, unit_sol, callees) global
  ) local_matching global_matching

let select_solutions2 : prog -> (string * prog) list -> unit_matching2
= fun pgm cpgms -> 
  (* compute patch unit of submission *)
  let cg_sub = extract_graph pgm in
  let units_sub = get_patch_unit cg_sub in
  print_endline "Submission";
  (* print_endline (string_of_set string_of_unit units_sub) *)
  let ctor_table_sub = CtorTable.gen_ctor_table BatMap.empty pgm in
  List.fold_left (fun acc (f_name, cpgm) ->
    (* compute patch unit of solution *) 
    let _ = ctor_table := CtorTable.gen_ctor_table ctor_table_sub cpgm in
    (* Compute matching based with the current solution *)
    let local_matching = find_local_matching2 units_sub (f_name, cpgm) in
    (* Update global matching result *)
    let r = update_matching2 acc local_matching in
    r
  ) empty_matching cpgms 