open Lang
open Util
open Print
open Type
open CallGraph

(* Function summary *)
type summary = {
  node : node; (* Call-graph node *)
  incomming : calling_ctx BatSet.t; (* Incomming edge from other nodes *)
  outgoing : calling_ctx BatSet.t  (* Outgoing edge to other nodes *)
}
(* Calling context : (args of caller, calling path of caller ) *)
and calling_ctx = arg list * path

type reference = {
  source : string; (* Source file of reference function *)
  summary : summary; (* Funciton summary for patch *)
  usecase : lexp BatSet.t (* Use case of reference function *)
}

(* Matching *)
type matching = (summary, reference) BatMap.t

let empty_matching = BatMap.empty
let ctor_table = ref BatMap.empty
(* pp *)
let string_of_ctx (args, path) = "[" ^ (args_to_string args "") ^ "]:" ^ string_of_path path 

let string_of_summary summary = 
  string_of_node summary.node ^ "\n" ^
  "Incomming : " ^ string_of_set ~sep:",\n" string_of_ctx summary.incomming ^ "\n" ^
  "Outgoing : " ^ string_of_set ~sep:",\n" string_of_ctx summary.outgoing 

let string_of_reference reference = 
  string_of_summary reference.summary ^ "\n" ^
  "Usecase : " ^ string_of_set exp_to_string reference.usecase ^ "\n" ^
  "Source file : " ^ reference.source

let string_of_matching matching = 
  BatMap.foldi (fun target reference acc ->
    let s = string_of_summary target ^ "\n => \n" ^ string_of_summary reference.summary in
    if acc = "" then s else acc ^ "\n---------------------------\n" ^ s
  ) matching ""

(* Get summary from the extracted call graph *)
let rec get_incomming_edges : node_id -> graph -> calling_ctx BatSet.t
= fun id cg ->
  let calling_edges = BatSet.filter (fun edge -> id = edge.sink) cg.edges in
  BatSet.fold (fun edge acc ->
    if edge.ctx = fresh_path then
      (* Apply tunneling *)
      let callers_ctx = get_incomming_edges edge.src cg in
      BatSet.union callers_ctx acc
    else 
      let node = get_node_by_id edge.src cg in
      BatSet.add (node.args, edge.ctx) acc
  ) calling_edges BatSet.empty

let rec get_outgoing_edges : node_id -> graph -> calling_ctx BatSet.t
= fun id cg ->
  let invoked_edges = BatSet.filter (fun edge -> id = edge.src) cg.edges in
  BatSet.fold (fun edge acc ->
    let node = get_node_by_id edge.src cg in
    BatSet.add (node.args, edge.ctx) acc
  ) invoked_edges BatSet.empty

let get_summaries : graph -> summary BatSet.t
= fun cg ->
  BatSet.fold (fun node acc ->
    let summary = {
      node = node;
      incomming = get_incomming_edges node.id cg;
      outgoing = get_outgoing_edges node.id cg
    } in
    BatSet.add summary acc
  ) cg.nodes BatSet.empty

(* Get reference from the extracted call graph *)
let rec replace_usecase : id -> lexp -> lexp
= fun target (l, exp) ->
  match exp with
  | EVar x -> if x = target then (l, EVar x) else (l, gen_hole ())
  | EUnit | Const _ | TRUE | FALSE | String _ -> (l, exp)
  | EFun (arg, e) -> (l, EFun (arg, replace_usecase target e))
  | ERef e | EDref e | Raise e | MINUS e | NOT e -> (l, update_unary exp (replace_usecase target e))
  | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2) 
  | OR (e1, e2) | AND (e1, e2) | LESS (e1, e2) | LESSEQ (e1, e2) | LARGER (e1, e2) | LARGEREQ (e1, e2) 
  | EQUAL (e1, e2) | NOTEQ (e1, e2) | DOUBLECOLON (e1, e2) | AT (e1, e2) | STRCON (e1, e2) | EApp (e1, e2) 
  | EAssign (e1, e2) -> (l, update_binary exp (replace_usecase target e1, replace_usecase target e2))
  | EList es -> (l, EList (List.map (fun e -> replace_usecase target e) es))
  | ETuple es -> (l, ETuple (List.map (fun e -> replace_usecase target e) es))
  | ECtor (x, es) -> (l, ECtor (x, List.map (fun e -> replace_usecase target e) es))
  | IF (e1, e2, e3) -> (l, IF (replace_usecase target e1, replace_usecase target e2, replace_usecase target e3))
  | EMatch (e, bs) -> (l, EMatch (replace_usecase target e, List.map (fun (p, e) -> (p, replace_usecase target e)) bs))
  | ELet (f, is_rec, args, typ, e1, e2) -> (l, ELet (f, is_rec, args, typ, replace_usecase target e1, replace_usecase target e2))
  | EBlock (is_rec, ds, e) -> (l, EBlock (is_rec, List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, args, typ, replace_usecase target e)) ds, replace_usecase target e))
  | _ -> raise (Failure ("Usecase : invalid exp (" ^ Print.exp_to_string (l, exp)))

let rec get_usecase_exp : id -> lexp list -> lexp -> lexp BatSet.t 
= fun target prev (l, exp) ->
  match exp with
  | EVar x -> 
    if x = target then 
      let usecase = List.fold_left (fun acc e -> (0, EApp (acc, replace_usecase target e))) (l, exp) prev in
      BatSet.singleton usecase
    else BatSet.empty
  | EApp (e1, e2) -> 
    let es1 = get_usecase_exp target (e2::prev) e1 in
    let es2 = get_usecase_exp target prev e2 in
    BatSet.union es1 es2
  | EUnit | Const _ | TRUE | FALSE | String _ -> BatSet.empty
  | EFun (_, e) | ERef e | EDref e | Raise e | MINUS e | NOT e -> get_usecase_exp target prev e
  | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2) 
  | OR (e1, e2) | AND (e1, e2) | LESS (e1, e2) | LESSEQ (e1, e2) | LARGER (e1, e2) | LARGEREQ (e1, e2) 
  | EQUAL (e1, e2) | NOTEQ (e1, e2) | DOUBLECOLON (e1, e2) | AT (e1, e2) | STRCON (e1, e2)  
  | EAssign (e1, e2) | ELet (_, _, _, _, e1, e2) ->
    let es1 = get_usecase_exp target prev e1 in
    let es2 = get_usecase_exp target prev e2 in
    BatSet.union es1 es2
  | EList es | ETuple es | ECtor (_, es) -> List.fold_left (fun acc e -> 
      let es = get_usecase_exp target prev e in
      BatSet.union es acc 
    ) BatSet.empty es
  | IF (e1, e2, e3) -> 
    let es1 = get_usecase_exp target prev e1 in
    let es2 = get_usecase_exp target prev e2 in
    let es3 = get_usecase_exp target prev e3 in
    BatSet.union es1 (BatSet.union es2 es3)
  | EMatch (e, bs) -> List.fold_left (fun acc (p, e) ->
      BatSet.union acc (get_usecase_exp target prev e)
    ) (get_usecase_exp target prev e) bs 
  | EBlock (is_rec, ds, e) -> List.fold_left (fun acc (_, _, _, _ , e) ->
      BatSet.union acc (get_usecase_exp target prev e)
    ) (get_usecase_exp target prev e) ds
  | _ -> raise (Failure ("Call template : invalid exp (" ^ Print.exp_to_string (l, exp)))

let get_usecase : summary -> graph -> lexp BatSet.t
= fun summary cg ->
  let target_node = summary.node in
  let nodes = BatSet.filter (fun node -> 
    BatSet.exists (fun edge -> edge.src = node.id && edge.sink = target_node.id) cg.edges
  ) cg.nodes in
  BatSet.fold (fun node acc ->
    BatSet.union acc (get_usecase_exp target_node.name [] node.body)
  ) nodes BatSet.empty 

let get_references : string -> graph -> reference BatSet.t
= fun source cg ->
  BatSet.fold (fun summary acc ->
    let referece = { source = source; summary = summary; usecase = get_usecase summary cg } in
    BatSet.add referece acc
  ) (get_summaries cg) BatSet.empty

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

let rec syntactic_distance : lexp -> lexp -> int
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
      try List.fold_left2 (fun acc e1 e2 -> acc + syntactic_distance e1 e2) 0 es1 es2 
      with _ -> exp_size exp1 + exp_size exp2 
    end
  | ECtor (x1, es1), ECtor (x2, es2) when x1 = x2 ->
    begin 
      try List.fold_left2 (fun acc e1 e2 -> acc + syntactic_distance e1 e2) 0 es1 es2 
      with _ -> exp_size exp1 + exp_size exp2 
    end
  (* Unary *)
  | MINUS e1, MINUS e2 | NOT e1, NOT e2 | ERef e1, ERef e2 | EDref e1, EDref e2 | Raise e1, Raise e2 | EFun (_, e1), EFun (_, e2) -> syntactic_distance e1 e2
  (* Binary *)
  | ADD (e1, e2), ADD (e1', e2') | SUB (e1, e2), SUB (e1', e2') | MUL (e1, e2), MUL (e1', e2') | DIV (e1, e2), DIV (e1', e2') | MOD (e1, e2), MOD (e1', e2') 
  | OR (e1, e2), OR (e1', e2') | AND (e1, e2), AND (e1', e2') | LESS (e1, e2), LESS (e1', e2') | LESSEQ (e1, e2), LESSEQ (e1', e2')
  | LARGER (e1, e2), LARGER (e1', e2') | LARGEREQ (e1, e2), LARGEREQ (e1', e2') | EQUAL (e1, e2), EQUAL (e1', e2') | NOTEQ(e1, e2), NOTEQ (e1', e2') 
  | DOUBLECOLON (e1, e2), DOUBLECOLON (e1', e2') | AT (e1, e2), AT (e1', e2') | STRCON (e1, e2), STRCON (e1', e2') | EAssign (e1, e2), EAssign (e1', e2') 
  | EApp (e1, e2), EApp (e1', e2') | ELet (_, _, _, _, e1, e2), ELet (_, _, _, _, e1', e2') -> syntactic_distance e1 e1' + syntactic_distance e2 e2'
  (* Condition *)
  | IF (e1, e2, e3), IF (e1', e2', e3') -> syntactic_distance e1 e1' + syntactic_distance e2 e2' + syntactic_distance e3 e3'
  | EMatch (e1, bs1), EMatch (e2, bs2) ->
    (* Distance between matched branches *)
    let (d1, unmatches) = List.fold_left (fun (d1, unmatches) (p, e) ->
      try 
        let (p', e') = List.find (fun (p', e') -> match_pat p p') unmatches in
        (d1 + syntactic_distance e e', List.remove_assoc p' unmatches)
      with _ -> (d1 + exp_size e, unmatches)
    ) (0, bs2) bs1 in
    (* Distance of unmatches branches *)
    let d2 = List.fold_left (fun acc (p, e) -> acc + exp_size e) 0 unmatches in
    syntactic_distance e1 e2 + d1 + d2
  (* Binding block *)
  | EBlock (_, bs1, e1), EBlock (_, bs2, e2) ->
    let (es1, es2) = (List.map (fun (_, _, _, _, e) -> e) bs1, List.map (fun (_, _, _, _, e) -> e) bs2) in
    begin 
      try List.fold_left2 (fun acc e1 e2 -> acc + syntactic_distance e1 e2) (syntactic_distance e1 e2) es1 es2 
      with _ -> (syntactic_distance e1 e2) + (List.fold_left (fun acc e -> exp_size e) 0 es1) + (List.fold_left (fun acc e -> exp_size e) 0 es2)
    end
  (* Syntatically different *)
  | _ -> exp_size exp1 + exp_size exp2

(* Path similarity *)
let rec match_arg : arg -> arg -> bool
= fun arg1 arg2 ->
  match arg1, arg2 with
  | ArgUnder typ1, ArgUnder typ2 | ArgUnder typ1, ArgOne (_, typ2) 
  | ArgOne (_, typ1), ArgUnder typ2 | ArgOne (_, typ1), ArgOne (_, typ2) -> check_typs typ1 typ2 
  | ArgTuple args1, ArgTuple args2 -> (try List.for_all2 match_arg args1 args2 with _ -> false)
  | _ -> false

let verify_ctx : calling_ctx -> calling_ctx -> bool
= fun (args_sub, path_sub) (args_sol, path_sol) ->
  try
    let vc = List.fold_left2 (fun vc arg_sub arg_sol ->
      (* TODO : Fix solver engine and remove checking *)
      if match_arg arg_sub arg_sol then
        let new_path = EQop (Eq, arg_to_path arg_sub, arg_to_path arg_sol) in
        Bop (And, new_path, vc)
      else Bool false 
    ) (Bop (And, path_sub, path_sol)) args_sub args_sol in
    Path_score.check_path !ctor_table vc
  with _ -> false 

let rec compute_path_score : calling_ctx BatSet.t -> calling_ctx BatSet.t -> float
= fun ctxs_sub ctxs_sol ->
  let total_num = (float_of_int (BatSet.cardinal ctxs_sub)) *. (float_of_int (BatSet.cardinal ctxs_sol)) in
  let matching_num = BatSet.fold (fun ctx_sub acc ->
    if BatSet.exists (fun ctx_sol -> verify_ctx ctx_sub ctx_sol) ctxs_sol then acc +. 0. else acc +. 1.
  ) ctxs_sub 0. in
  let score = if total_num = 0. then 0. else matching_num /. total_num in
  score

(* Compute (local)matching result *)
let rec find_matching : summary BatSet.t -> reference BatSet.t -> matching
= fun summaries references ->
  BatSet.fold (fun summary matching -> 
    (* Find a solution functions whose type is the same with the type of the submission *)
    let candidates = BatSet.filter (fun reference -> 
      check_typs summary.node.typ reference.summary.node.typ
    ) references in
    (* Select solution functions with the minimal semantic distance *)
    let (_, candidates) = BatSet.fold (fun reference (score, candidates) ->
      let score' = compute_path_score summary.incomming reference.summary.incomming in
      if score' < score then (score', BatSet.singleton reference)
      else if score' = score then (score, BatSet.add reference candidates)
      else (score, candidates)
    ) candidates (0., BatSet.empty) in
    if BatSet.is_empty candidates then
      (* If there are no matched solutions, do not change the matching relation *)
      matching
    else
      (* Pick the most similar ones in candidates *)
      let (candidate, remains) = BatSet.pop candidates in
      let reference = BatSet.fold (fun cur acc -> 
        let d1 = syntactic_distance summary.node.body cur.summary.node.body in
        let d2 = syntactic_distance summary.node.body acc.summary.node.body in
        if d1 < d2 then cur else acc 
      ) remains candidate in
      BatMap.add summary reference matching
  ) summaries empty_matching
  
(* TODO : need more fine graining *)
let rec update_matching : matching -> matching -> matching
= fun global local ->
  BatMap.foldi (fun summary reference global ->
    if BatMap.mem summary global then
      let reference' = BatMap.find summary global in
      (* Compare semantic distance *)
      let p1 = compute_path_score summary.incomming reference.summary.incomming in
      let p2 = compute_path_score summary.incomming reference'.summary.incomming in
      if p1 < p2 then BatMap.add summary reference global 
      else 
        (* Compare syntactic distance  *)
        let d1 = syntactic_distance summary.node.body reference.summary.node.body in
        let d2 = syntactic_distance summary.node.body reference'.summary.node.body in
        if d1 < d2 then BatMap.add summary reference global else global
    else BatMap.add summary reference global
  ) local global

let select_solutions : prog -> (string * graph) list -> matching
= fun pgm cg_sols -> 
  let summaries = get_summaries (extract_graph pgm) in
  let _ = ctor_table := Path_score.CtorTable.gen_ctor_table BatMap.empty pgm in
  List.fold_left (fun acc (f_name, cg_sol) ->
    let matching = find_matching summaries (get_references f_name cg_sol) in
    update_matching acc matching
  ) empty_matching cg_sols 