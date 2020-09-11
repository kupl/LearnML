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
and calling_ctx = typ * arg list * path

type reference = {
  source : string; (* Source file of reference function *)
  summary : summary; (* Funciton summary for patch *)
  usecase : lexp BatSet.t (* Use case of reference function *)
}

(* Matching *)
type matching = (summary, (reference * int * float)) BatMap.t

let empty_matching = BatMap.empty
let ctor_table = ref BatMap.empty

(* pp *)
let string_of_ctx (typ, args, path) = type_to_string typ ^ " - [" ^ (args_to_string args "") ^ "]:" ^ string_of_path path 

let string_of_summary summary = 
  string_of_node summary.node ^ "\n" ^
  "Incomming : " ^ string_of_set ~sep:",\n" string_of_ctx summary.incomming ^ "\n" ^
  "Outgoing : " ^ string_of_set ~sep:",\n" string_of_ctx summary.outgoing 

let string_of_reference reference = 
  string_of_summary reference.summary ^ "\n" ^
  "Usecase : " ^ string_of_set exp_to_string reference.usecase ^ "\n" ^
  "Source file : " ^ reference.source

let string_of_matching matching = 
  BatMap.foldi (fun target (reference, _, _) acc ->
    let s = string_of_summary target ^ "\n => \n" ^ string_of_reference reference in
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
      if BatSet.is_empty callers_ctx then
        let node = get_node_by_id edge.src cg in
        BatSet.add (get_output_typ node.typ, node.args, edge.ctx) acc
      else 
        BatSet.union callers_ctx acc
    else 
      let node = get_node_by_id edge.src cg in
      BatSet.add (get_output_typ node.typ, node.args, edge.ctx) acc
  ) calling_edges BatSet.empty

let rec get_outgoing_edges : node_id -> graph -> calling_ctx BatSet.t
= fun id cg ->
  let invoked_edges = BatSet.filter (fun edge -> id = edge.src) cg.edges in
  BatSet.fold (fun edge acc ->
    if edge.ctx = fresh_path then
      (* Apply tunneling *)
      let callees_ctx = get_outgoing_edges edge.sink cg in
      if BatSet.is_empty callees_ctx then
        let cur_node = get_node_by_id edge.src cg in
        let callee = get_node_by_id edge.sink cg in
        BatSet.add (get_output_typ callee.typ, cur_node.args, edge.ctx) acc
      else 
        BatSet.union callees_ctx acc
    else 
      let cur_node = get_node_by_id edge.src cg in
      let callee = get_node_by_id edge.sink cg in
      BatSet.add (get_output_typ callee.typ, cur_node.args, edge.ctx) acc
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
  | PList ps1, PList ps2 | PTuple ps1, PTuple ps2 -> (try List.for_all2 match_pat ps1 ps2 with _ -> false)
  | PCtor (x, ps1), PCtor (y, ps2) -> (x = y) && (try List.for_all2 match_pat ps1 ps2 with _ -> false)
  | PCons (phd1, ptl1), PCons (phd2, ptl2) -> match_pat phd1 phd2 && match_pat ptl1 ptl2
  | Pats ps, _ | _, Pats ps -> raise (Failure "Nomalized programs do not have this pattern")
  | PUnit, PUnit | PUnder, PUnder | PVar _, PVar _ -> true
  | _ -> false

let rec syntactic_distance : lexp -> lexp -> float
= fun exp1 exp2 ->
  match (snd exp1, snd exp2) with
  | SInt _, SInt _ | SStr _, SStr _ | Hole _, Hole _ -> 0.
  (* Constant *)
  | EUnit, EUnit | TRUE, TRUE | FALSE, FALSE | EVar _, EVar _ -> 0.
  | Const n1, Const n2 when n1 = n2 -> 0.
  | String s1, String s2 when s1 = s2 -> 0.
  (* List *)
  | EList es1, EList es2 | ETuple es1, ETuple es2 ->
    begin 
      try List.fold_left2 (fun acc e1 e2 -> acc +. syntactic_distance e1 e2) 0. es1 es2 
      with _ -> float_of_int (exp_size exp1 + exp_size exp2)
    end
  | ECtor (x1, es1), ECtor (x2, es2) when x1 = x2 ->
    begin 
      try List.fold_left2 (fun acc e1 e2 -> acc +. syntactic_distance e1 e2) 0. es1 es2 
      with _ -> float_of_int (exp_size exp1 + exp_size exp2)
    end
  (* Unary *)
  | MINUS e1, MINUS e2 | NOT e1, NOT e2 | ERef e1, ERef e2 | EDref e1, EDref e2 | Raise e1, Raise e2 | EFun (_, e1), EFun (_, e2) -> syntactic_distance e1 e2
  (* Binary *)
  | ADD (e1, e2), ADD (e1', e2') | SUB (e1, e2), SUB (e1', e2') | MUL (e1, e2), MUL (e1', e2') | DIV (e1, e2), DIV (e1', e2') | MOD (e1, e2), MOD (e1', e2') 
  | OR (e1, e2), OR (e1', e2') | AND (e1, e2), AND (e1', e2') | LESS (e1, e2), LESS (e1', e2') | LESSEQ (e1, e2), LESSEQ (e1', e2')
  | LARGER (e1, e2), LARGER (e1', e2') | LARGEREQ (e1, e2), LARGEREQ (e1', e2') | EQUAL (e1, e2), EQUAL (e1', e2') | NOTEQ(e1, e2), NOTEQ (e1', e2') 
  | DOUBLECOLON (e1, e2), DOUBLECOLON (e1', e2') | AT (e1, e2), AT (e1', e2') | STRCON (e1, e2), STRCON (e1', e2') | EAssign (e1, e2), EAssign (e1', e2') 
  | EApp (e1, e2), EApp (e1', e2') | ELet (_, _, _, _, e1, e2), ELet (_, _, _, _, e1', e2') -> syntactic_distance e1 e1' +. syntactic_distance e2 e2'
  (* Condition *)
  | IF (e1, e2, e3), IF (e1', e2', e3') -> syntactic_distance e1 e1' +. syntactic_distance e2 e2' +. syntactic_distance e3 e3'
  | EMatch (e1, bs1), EMatch (e2, bs2) ->
    (* Distance between matched branches *)
    let (d1, unmatches) = List.fold_left (fun (d1, unmatches) (p, e) ->
      try 
        let (p', e') = List.find (fun (p', e') -> match_pat p p') unmatches in
        (d1 +. syntactic_distance e e', List.remove_assoc p' unmatches)
      with _ -> (d1 +. float_of_int (exp_size e), unmatches)
    ) (0., bs2) bs1 in
    (* Distance of unmatches branches *)
    let d2 = List.fold_left (fun acc (p, e) -> acc +. float_of_int (exp_size e)) 0. unmatches in
    syntactic_distance e1 e2 +. d1 +. d2
  (* Binding block *)
  | EBlock (_, bs1, e1), EBlock (_, bs2, e2) ->
    let (es1, es2) = (List.map (fun (_, _, _, _, e) -> e) bs1, List.map (fun (_, _, _, _, e) -> e) bs2) in
    begin 
      try List.fold_left2 (fun acc e1 e2 -> acc +. syntactic_distance e1 e2) (syntactic_distance e1 e2) es1 es2 
      with _ -> (syntactic_distance e1 e2) +. (List.fold_left (fun acc e -> float_of_int (exp_size e)) 0. es1) +. (List.fold_left (fun acc e -> float_of_int (exp_size e)) 0. es2)
    end
  (* Syntatically different *)
  | _ -> float_of_int (exp_size exp1 + exp_size exp2)

(*
let rec syntactic_distance : lexp -> lexp -> float
= fun e1 e2 -> Syntactic_dist.syntactic_distance e1 e2
*)

(* Path similarity *)
let rec match_arg : arg -> arg -> bool
= fun arg1 arg2 ->
  match arg1, arg2 with
  | ArgUnder typ1, ArgUnder typ2 | ArgUnder typ1, ArgOne (_, typ2) 
  | ArgOne (_, typ1), ArgUnder typ2 | ArgOne (_, typ1), ArgOne (_, typ2) -> check_typs typ1 typ2 
  | ArgTuple args1, ArgTuple args2 -> (try List.for_all2 match_arg args1 args2 with _ -> false)
  | _ -> false

let rec gen_eqn_path : CallGraph.path -> typ -> Type.typ_eqn
= fun path ty ->
  match path with
  | Unit -> [ty, TUnit]
  | Int _ -> [ty, TInt]
  | Bool _ -> [ty, TBool]
  | Str _ -> [ty, TString]
  | Aop (_, p1, p2) -> [ty, TInt]@(gen_eqn_path p1 TInt)@(gen_eqn_path p2 TInt)
  | Bop (_, p1, p2) -> [ty, TBool]@(gen_eqn_path p1 TBool)@(gen_eqn_path p2 TBool)
  | ABop (_, p1, p2) -> [ty, TBool]@(gen_eqn_path p1 TInt)@(gen_eqn_path p2 TInt)
  | EQop (_, p1, p2) -> 
    let new_tv = fresh_tvar () in
    [ty, TBool]@(gen_eqn_path p1 new_tv)@(gen_eqn_path p2 new_tv)
  | Strcon (p1, p2) -> [ty, TString]@(gen_eqn_path p1 TString)@(gen_eqn_path p2 TString)
  | Append (p1, p2) -> 
    let new_tv = fresh_tvar () in
    [ty, TList new_tv]@(gen_eqn_path p1 (TList new_tv))@(gen_eqn_path p2 (TList new_tv))
  | Concat (p1, p2) -> 
    let new_tv = fresh_tvar () in
    [ty, TList new_tv]@(gen_eqn_path p1 new_tv)@(gen_eqn_path p2 (TList new_tv))
  | Minus p -> [ty, TInt]@(gen_eqn_path p TInt)
  | Not p -> [ty, TBool]@(gen_eqn_path p TBool)
  | Tuple ps ->
    let (ts, eqns) = List.fold_left (fun (ts, eqns) p -> 
      let new_tv = fresh_tvar () in
      (new_tv::ts, (gen_eqn_path p new_tv)@eqns)
    ) ([], []) ps in
    (ty, TTuple (List.rev ts))::eqns
  | List (ps, typ) -> 
    let new_tv = fresh_tvar () in
    List.fold_left (fun eqns p -> 
      (gen_eqn_path p new_tv)@eqns
    ) [ty, TList new_tv] ps
  | Ctor (c, ps) -> [ty, fresh_tvar ()]
  | Var (_, typ) | Symbol (_, typ) -> [ty, typ]

let solve_path_eqn : Type.typ_eqn -> Type.Subst.t
= fun eqns -> List.fold_left (fun subst (t1, t2) -> 
    Type.unify subst ((Type.Subst.apply t1 subst), Type.Subst.apply t2 subst)
  ) Type.Subst.empty eqns

let rec apply_subst : Type.Subst.t -> CallGraph.path -> CallGraph.path
= fun subst path ->
  match path with 
  | Aop (op, p1, p2) -> Aop (op, apply_subst subst p1, apply_subst subst p2)
  | Bop (comb, p1, p2) -> Bop (comb, apply_subst subst p1, apply_subst subst p2)
  | ABop (comp, p1, p2) -> ABop (comp, apply_subst subst p1, apply_subst subst p2)
  | EQop (eq, p1, p2) -> EQop (eq, apply_subst subst p1, apply_subst subst p2)
  | Strcon (p1, p2) -> Strcon (apply_subst subst p1, apply_subst subst p2)
  | Append (p1, p2) -> Append (apply_subst subst p1, apply_subst subst p2)
  | Concat (p1, p2) -> Concat (apply_subst subst p1, apply_subst subst p2)
  | Minus p -> Minus (apply_subst subst p)
  | Not p -> Not (apply_subst subst p)
  | Tuple ps -> Tuple (List.map (apply_subst subst) ps)
  | Ctor (c, ps) -> Ctor (c, List.map (apply_subst subst) ps)
  | List (ps, typ) -> List (List.map (apply_subst subst) ps, Type.Subst.apply typ subst)
  | Var (x, typ) -> Var (x, Type.Subst.apply typ subst)
  | Symbol (n, typ) -> Symbol (n, Type.Subst.apply typ subst)
  | _ -> path

let subst_path : CallGraph.path -> CallGraph.path
= fun path -> 
  let eqns = gen_eqn_path path TBool in
  let subst = solve_path_eqn eqns in
  apply_subst subst path 

let verify_ctx : calling_ctx -> calling_ctx -> bool
= fun (typ_sub, args_sub, path_sub) (typ_sol, args_sol, path_sol) ->
  try
    if check_typs typ_sub typ_sol then 
      (*
      let _ = 
        print_endline ("Sub : " ^ (args_to_string args_sub "") ^ " " ^ string_of_path path_sub);
        print_endline ("Sol : " ^ (args_to_string args_sol "") ^ " " ^ string_of_path path_sol);
      in
      *)
      let vc = List.fold_left2 (fun vc arg_sub arg_sol ->
        (* TODO : Fix solver engine and remove checking *)
        if match_arg arg_sub arg_sol then
          let new_path = EQop (Eq, arg_to_path arg_sub, arg_to_path arg_sol) in
          Bop (And, new_path, vc)
        else Bool false
      ) (Bop (And, path_sub, path_sol)) args_sub args_sol in
      let vc = subst_path vc in
      (*
      print_endline "--------------------";
      print_endline (string_of_path vc);
      Path_score.check_sat !ctor_table vc 
      *)
      Path_score.check_sat !ctor_table vc
      (* not (Path_score.check_sat !ctor_table (Not vc)) *)
    else false
  with Type.TypeError | Invalid_argument ("List.fold_left2") -> false

let rec semantic_distance_ctx : calling_ctx BatSet.t -> calling_ctx BatSet.t -> int
= fun ctxs_sub ctxs_sol ->
  let rec iter ctxs_sub unmatched sem_dist =
    if BatSet.is_empty ctxs_sub then sem_dist + (BatSet.cardinal unmatched) 
    else 
      let (ctx_sub, remains) = BatSet.pop ctxs_sub in 
      let matched = BatSet.filter (fun ctx_sol -> verify_ctx ctx_sub ctx_sol) ctxs_sol in
      if BatSet.is_empty matched then 
        iter remains unmatched (sem_dist + 1) 
      else 
        iter remains (BatSet.diff unmatched matched) sem_dist
  in
  iter ctxs_sub ctxs_sol 0
  (*
  if BatSet.cardinal ctxs_sub = 0 && BatSet.cardinal ctxs_sol = 0 then 0.
  else if BatSet.cardinal ctxs_sub = 0 && BatSet.cardinal ctxs_sol <> 0 then 1.
  else
    (*
    let total_num = (float_of_int (BatSet.cardinal ctxs_sub)) *. (float_of_int (BatSet.cardinal ctxs_sol)) in
    let matching_num = BatSet.fold (fun ctx_sub acc ->
      BatSet.fold (fun ctx_sol acc ->
        (*
        print_endline "---------------------------";
        print_endline ("Sub ctx : " ^ string_of_ctx ctx_sub);
        print_endline ("Sol ctx : " ^ string_of_ctx ctx_sol);
        print_endline (string_of_bool (verify_ctx ctx_sub ctx_sol));
        .*)
        if verify_ctx ctx_sub ctx_sol then acc +. 0. else acc +. 1.
      ) ctxs_sol acc
    ) ctxs_sub 0. in
    matching_num /. total_num
    *)
    BatSet.fold (fun ctx_sub acc -> 
      if BatSet.exists (fun ctx_sol -> verify_ctx ctx_sub ctx_sol) ctxs_sol then acc else acc +. 1.
    ) ctxs_sub 0.
  *)

let rec semantic_distance : summary -> summary -> int
= fun summary_sub summary_sol -> 
  (* Semantic distance of incomming edges *)
  let (incomming_sub, incomming_sol) = (summary_sub.incomming, summary_sol.incomming) in
  let incomming_distance = semantic_distance_ctx incomming_sub incomming_sol in
  (* Semantic distance of outgoing edges *)
  let (outgoing_sub, outgoing_sol) = (summary_sub.outgoing, summary_sol.outgoing) in
  let outgoing_distance = semantic_distance_ctx outgoing_sub outgoing_sol in
  2 * incomming_distance + outgoing_distance

(* Compute (local)matching result *)
let rec find_matching : summary BatSet.t -> reference BatSet.t -> matching
= fun summaries references ->
  BatSet.fold (fun summary matching -> 
    (* Find a solution functions whose type is the same with the type of the submission *)
    let candidates = BatSet.filter (fun reference -> 
      check_typs summary.node.typ reference.summary.node.typ
    ) references in
    (* Select solution functions with the minimal semantic distance *)
    let (sem_dist, candidates) = BatSet.fold (fun reference (score, candidates) ->
      let score' = semantic_distance summary reference.summary in
      if score' < score then (score', BatSet.singleton reference)
      else if score' = score then (score, BatSet.add reference candidates)
      else (score, candidates)
    ) candidates (max_int, BatSet.empty) in
    if BatSet.is_empty candidates then
      (* If there are no matched solutions, do not change the matching relation *)
      matching
    else
      (* Pick the most similar ones in candidates *)
      let (candidate, remains) = BatSet.pop candidates in
      let (syn_dist, reference) = BatSet.fold (fun reference (score, acc) -> 
        let score' = syntactic_distance summary.node.body reference.summary.node.body in
        if score' < score then (score', reference)
        else (score, acc)
      ) remains (syntactic_distance summary.node.body candidate.summary.node.body, candidate) in
      BatMap.add summary (reference, sem_dist, syn_dist) matching
  ) summaries empty_matching

(* Update (global)matching result *)
let rec update_matching : matching -> matching -> matching
= fun local global ->
  BatMap.foldi (fun summary (reference, sem_dist, syn_dist) global ->
    if BatMap.mem summary global then
      let (reference', sem_dist', syn_dist') = BatMap.find summary global in
      (* Compare semantic distance *)
      let p1 = sem_dist in
      let p2 = sem_dist' in 
      (*
      let _ =
        if summary.node.id = 2 then
          (
          print_header "submission"; print_endline (string_of_summary summary);
          print_header ("local :" ^ string_of_int p1); print_endline (string_of_reference reference);
          print_header ("global :" ^ string_of_int p2); print_endline (string_of_reference reference');
          )
        else 
          ()
      in
      *)
      if p1 < p2 then BatMap.add summary (reference, sem_dist, syn_dist) global 
      else if p1 > p2 then global
      else
        (* Compare syntactic distance  *)
        let d1 = syn_dist in
        let d2 = syn_dist' in
        if d1 < d2 then BatMap.add summary (reference, sem_dist, syn_dist) global else global
    else BatMap.add summary (reference, sem_dist, syn_dist) global
  ) local global

let select_solutions : prog -> (string * graph) list -> matching
= fun pgm cg_sols -> 
  let summaries = get_summaries (extract_graph pgm) in
  let _ = ctor_table := Path_score.CtorTable.gen_ctor_table BatMap.empty pgm in
  List.fold_left (fun acc (f_name, cg_sol) ->
    (* print_endline f_name; *)
    let matching = find_matching summaries (get_references f_name cg_sol) in
    update_matching matching acc
  ) empty_matching cg_sols