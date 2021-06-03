open Lang
open Util
open Print
open Type
open CallGraph

(* Calling context : (caller info, calling path of caller * callee info) *)
type calling_ctx = {
  caller_typ : typ;
  caller_args : arg list;
  path : path; 
  callee_typ : typ;
  callee_args : arg list
}

let compare_ctx : calling_ctx -> calling_ctx -> bool 
= fun ctx1 ctx2 -> 
	(check_typs ctx1.caller_typ ctx2.caller_typ) &&
	(ctx1.caller_args = ctx2.caller_args) &&
	(compare_path ctx1.path ctx2.path) &&
	(check_typs ctx1.callee_typ ctx2.callee_typ) &&
	(ctx1.callee_args = ctx2.callee_args) 

let compare_ctxs : calling_ctx BatSet.t -> calling_ctx BatSet.t -> bool
= fun ctxs1 ctxs2 -> compare_set ctxs1 ctxs2 compare_ctx

(* Function summary *)
type summary = {
  node : node; (* Call-graph node *)
  incomming : calling_ctx BatSet.t; (* Incomming edge from other nodes *)
  outgoing : calling_ctx BatSet.t  (* Outgoing edge to other nodes *)
}

type summaries = summary BatSet.t

let compare_summary : summary -> summary -> bool
= fun s1 s2 -> 
	(compare_node s1.node s2.node) && (compare_ctxs s1.incomming s2.incomming) && (compare_ctxs s1.outgoing s2.outgoing)

(* Reference *)
type reference = {
  source : string; (* Source file of reference function *)
  summary : summary; (* Funciton summary for patch *)
  usecase : lexp BatSet.t; (* Use case of reference function *)
  helpers : reference BatSet.t (* Helper functions *)
}

type references = reference BatSet.t

let compare_ref : reference -> reference -> bool
= fun ref1 ref2 ->	
	try
		(compare_summary ref1.summary ref2.summary) && (BatSet.equal ref1.usecase ref2.usecase)
	with _ -> (* print_endline ("Compare " ^ ref1.source ^ ", " ^ ref2.source); *) false

let rec compare_refs : references -> references -> bool
= fun refs1 refs2 -> compare_set refs1 refs2 compare_ref 

(* Matching *)
type matching = (summary, (reference * int * float)) BatMap.t

let empty_matching = BatMap.empty
let ctor_table = ref BatMap.empty

(* pp *)
let string_of_ctx ctx = 
  "[ " ^ type_to_string ctx.caller_typ ^ " - " ^ (args_to_string ctx.caller_args "") ^ "] -> " ^ 
  "[ " ^ type_to_string ctx.callee_typ ^ " - " ^ (args_to_string ctx.callee_args "") ^ "]\n" ^ 
  "  " ^ string_of_path ctx.path 

let string_of_summary summary = 
  string_of_node summary.node ^ "\n" ^
  "Incomming \n" ^ string_of_set ~first:"" ~last:"" ~sep:",\n" string_of_ctx summary.incomming ^ "\n" ^
  "Outgoing \n" ^ string_of_set ~first:"" ~last:"" ~sep:",\n" string_of_ctx summary.outgoing 

let rec string_of_reference ?(depth=0) reference = 
  let s = 
    string_of_summary reference.summary ^ "\n" ^
    "Usecase : " ^ string_of_set exp_to_string reference.usecase ^ "\n" ^
    "Helpers : \n" ^ string_of_set ~first:"" ~last:"" ~sep:",\n" (fun helper -> string_of_reference ~depth:(depth+1) helper) reference.helpers ^ "\n" ^
    "Source file : " ^ reference.source
  in 
  insert_tab depth s

let string_of_matching matching = 
  BatMap.foldi (fun target (reference, sem_dist, syn_dist) acc ->
    let s = 
      string_of_summary target ^ "\n => \n" ^ string_of_reference reference ^ "\n" ^ 
      "Semantic : " ^ string_of_int sem_dist ^ "\n" ^
      "Syntactic : " ^ string_of_float syn_dist
    in
    if acc = "" then s else acc ^ "\n---------------------------\n" ^ s
  ) matching ""

(* Get summary from the extracted call graph *)
let rec is_passing_edge : CallGraph.path -> bool
= fun path ->
  match path with
  | Bop (And, p1, p2) -> is_passing_edge p1 && is_passing_edge p2
  | EQop (Eq, p1, p2) -> is_passing_edge p1 && is_passing_edge p2
  | Var _ -> true
  | Bool true -> true
  | _ -> false

let rec get_incomming_edges : node_id -> graph -> calling_ctx BatSet.t
= fun id cg ->
  let calling_edges = BatSet.filter (fun edge -> id = edge.sink) cg.edges in
  let cur_node = get_node_by_id id cg in
  BatSet.fold (fun edge acc ->
    if is_passing_edge edge.ctx then
      (* Apply tunneling *)
      let callers_ctx = get_incomming_edges edge.src cg in
      if BatSet.is_empty callers_ctx then
        let caller_node = get_node_by_id edge.src cg in
        let ctx = {
          caller_typ = get_output_typ caller_node.typ;
          caller_args = caller_node.args;
          path = edge.ctx;
          callee_typ = get_output_typ cur_node.typ;
          callee_args = cur_node.args
        } in
        BatSet.add ctx acc
      else 
        BatSet.union callers_ctx acc
    else 
      let caller_node = get_node_by_id edge.src cg in
      let ctx = {
        caller_typ = get_output_typ caller_node.typ;
        caller_args = caller_node.args;
        path = edge.ctx;
        callee_typ = get_output_typ cur_node.typ;
        callee_args = cur_node.args
      } in
      BatSet.add ctx acc
  ) calling_edges BatSet.empty

let rec get_outgoing_edges : node_id -> graph -> calling_ctx BatSet.t
= fun id cg ->
  let invoked_edges = BatSet.filter (fun edge -> id = edge.src) cg.edges in
  let cur_node = get_node_by_id id cg in
  BatSet.fold (fun edge acc ->
    if is_passing_edge edge.ctx then
      (* Apply tunneling *)
      let callees_ctx = get_outgoing_edges edge.sink cg in
      if BatSet.is_empty callees_ctx then
        let callee_node = get_node_by_id edge.sink cg in
        let ctx = {
          caller_typ = get_output_typ cur_node.typ;
          caller_args = cur_node.args;
          path = edge.ctx;
          callee_typ = get_output_typ callee_node.typ;
          callee_args = callee_node.args
        } in
        BatSet.add ctx acc
      else 
        BatSet.union callees_ctx acc
    else 
      let callee_node = get_node_by_id edge.sink cg in
      let ctx = {
        caller_typ = get_output_typ cur_node.typ;
        caller_args = cur_node.args;
        path = edge.ctx;
        callee_typ = get_output_typ callee_node.typ;
        callee_args = callee_node.args
      } in
      BatSet.add ctx acc
  ) invoked_edges BatSet.empty

let get_summaries : graph -> summaries
= fun cg ->
  BatSet.fold (fun node acc ->
    if is_external_var node.name then acc
    else 
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
  let l = 0 in
  match exp with
  | EVar x -> 
    (* If it is a target function or predefeind variable use as it is  *)
    if x = target || is_external_var x then (l, EVar x) else dummy_hole ()
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
      let usecase = List.fold_left (fun acc e -> (0, EApp (acc, replace_usecase target e))) (0, exp) prev in
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

let rec get_helpers : reference BatSet.t -> graph -> reference BatSet.t 
= fun references cg ->
  let references' = BatSet.fold (fun reference acc ->
    let cur_node = reference.summary.node.id in
    let next_nodes = BatSet.map (fun edge -> edge.sink) (BatSet.filter (fun edge -> cur_node = edge.src) cg.edges) in
    let helpers = BatSet.filter (fun reference -> BatSet.mem reference.summary.node.id next_nodes) references in 
    let reference = {
      source = reference.source;
      summary = reference.summary;
      usecase = reference.usecase;
      helpers = helpers
    } in
    BatSet.add reference acc 
  ) references references in
  if BatSet.equal references references' then references' else get_helpers references' cg

let get_references : string -> graph -> references
= fun source cg ->
  let references = BatSet.fold (fun summary acc ->
    let referece = { source = source; summary = summary; usecase = get_usecase summary cg; helpers = BatSet.empty } in
    BatSet.add referece acc
  ) (get_summaries cg) BatSet.empty in
  (* Make 1-call relation *)
  let references = BatSet.map (fun reference ->
    let cur_node = reference.summary.node.id in
    let next_nodes = BatSet.map (fun edge -> edge.sink) (BatSet.filter (fun edge -> cur_node = edge.src) cg.edges) in
    let helpers = BatSet.filter (fun reference -> BatSet.mem reference.summary.node.id next_nodes) references in 
    {
      source = reference.source;
      summary = reference.summary;
      usecase = reference.usecase;
      helpers = helpers
    }
  ) references in
  (* let references = get_helpers references cg in *)
  references
  
(* Context-aware-matching *)
let unsat_score = 100 

let rec gen_path_eqn : CallGraph.path -> typ -> Type.typ_eqn
= fun path ty ->
  match path with
  | Unit -> [ty, TUnit] | Int _ -> [ty, TInt] | Bool _ -> [ty, TBool] | Str _ -> [ty, TString]
  | Aop (_, p1, p2) -> [ty, TInt]@(gen_path_eqn p1 TInt)@(gen_path_eqn p2 TInt)
  | Bop (_, p1, p2) -> [ty, TBool]@(gen_path_eqn p1 TBool)@(gen_path_eqn p2 TBool)
  | ABop (_, p1, p2) -> [ty, TBool]@(gen_path_eqn p1 TInt)@(gen_path_eqn p2 TInt)
  | EQop (_, p1, p2) -> 
    let new_tv = fresh_tvar () in
    [ty, TBool]@(gen_path_eqn p1 new_tv)@(gen_path_eqn p2 new_tv)
  | Strcon (p1, p2) -> [ty, TString]@(gen_path_eqn p1 TString)@(gen_path_eqn p2 TString)
  | Append (p1, p2) -> 
    let new_tv = fresh_tvar () in
    [ty, TList new_tv]@(gen_path_eqn p1 (TList new_tv))@(gen_path_eqn p2 (TList new_tv))
  | Concat (p1, p2) -> 
    let new_tv = fresh_tvar () in
    [ty, TList new_tv]@(gen_path_eqn p1 new_tv)@(gen_path_eqn p2 (TList new_tv))
  | Minus p -> [ty, TInt]@(gen_path_eqn p TInt)
  | Not p -> [ty, TBool]@(gen_path_eqn p TBool)
  | Tuple ps ->
    let (ts, eqns) = List.fold_left (fun (ts, eqns) p -> 
      let new_tv = fresh_tvar () in
      (new_tv::ts, (gen_path_eqn p new_tv)@eqns)
    ) ([], []) ps in
    (ty, TTuple (List.rev ts))::eqns
  | List (ps, typ) -> 
    let new_tv = fresh_tvar () in
    List.fold_left (fun eqns p -> 
      (gen_path_eqn p new_tv)@eqns
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

let rec abstract_path : CallGraph.path -> CallGraph.path
= fun path ->
  match path with
  | Int _ -> Int 0 
  | Str _ -> Str ""
  | Aop (op, p1, p2) -> Aop (op, abstract_path p1, abstract_path p2)
  | Bop (comb, p1, p2) -> Bop (comb, abstract_path p1, abstract_path p2)
  | ABop (comp, p1, p2) -> ABop (comp, abstract_path p1, abstract_path p2)
  | EQop (eq, p1, p2) -> EQop (eq, abstract_path p1, abstract_path p2)
  | Strcon (p1, p2) -> Strcon (abstract_path p1, abstract_path p2)
  | Append (p1, p2) -> Append (abstract_path p1, abstract_path p2)
  | Concat (p1, p2) -> Concat (abstract_path p1, abstract_path p2)
  | Minus p -> Minus (abstract_path p)
  | Not p -> Not (abstract_path p)
  | Tuple ps -> Tuple (List.map abstract_path ps)
  | List (ps, typ) -> List (List.map abstract_path ps, typ)
  | Ctor (c, ps) -> Ctor (c, List.map abstract_path ps)
  | Var (_, typ) | Symbol (_, typ) -> Var ("x", typ)
  | _ -> path

let rec path_similarity : CallGraph.path -> CallGraph.path -> int
= fun path_sub path_sol -> 
  let clauses_sub = extract_clauses (abstract_path path_sub) in
  let clauses_sol = extract_clauses (abstract_path path_sol) in
  let (unmatched, dist) = List.fold_left (fun (unmatched, dist) path_sub -> 
    (try
      let same_clause = List.find (fun path_sol -> path_sub = path_sol) unmatched in
      (list_remove1 same_clause unmatched, dist)
    with Not_found -> (unmatched, size_of_path path_sub))
  ) (clauses_sol, 0) clauses_sub in
  List.fold_left (fun acc path_sol -> acc + size_of_path path_sol) dist unmatched 

let rec gen_vc : CallGraph.path -> arg list -> arg list -> CallGraph.path
= fun vc args_sub args_sol ->
  List.fold_left2 (fun vc arg_sub arg_sol ->
    let new_path = EQop (Eq, arg_to_path arg_sub, arg_to_path arg_sol) in
    Bop (And, new_path, vc)
  ) vc args_sub args_sol

let compute_path_score : calling_ctx -> calling_ctx -> int
= fun ctx_sub ctx_sol ->
  try
    if check_typs ctx_sub.caller_typ ctx_sol.caller_typ && check_typs ctx_sub.callee_typ ctx_sol.callee_typ then 
      (* Generate verification condition *)
      let vc = gen_vc (Bop (And, ctx_sub.path, ctx_sol.path)) ctx_sub.caller_args ctx_sol.caller_args in
      let vc = gen_vc vc ctx_sub.callee_args ctx_sol.callee_args in
      (* Normalize verification condition *)
      let vc_subst = solve_path_eqn (gen_path_eqn vc TBool) in
      let vc = apply_subst vc_subst vc in
      if Path_score.check_sat !ctor_table vc then 
        (* If two contexts are comparable compute similarity of two ctx *)
        path_similarity (apply_subst vc_subst ctx_sub.path) (apply_subst vc_subst ctx_sol.path)
      else -1
    else -1
  with _ -> -1

let rec semantic_distance_ctx : calling_ctx BatSet.t -> calling_ctx BatSet.t -> int
= fun ctxs_sub ctxs_sol ->
  let rec iter remains unmatched sem_dist =
    if BatSet.is_empty remains then 
      (* If there are some remaining solution paths, increase distance *)
      BatSet.fold (fun ctx acc -> acc + size_of_path ctx.path) unmatched sem_dist
    else if BatSet.is_empty unmatched then
      (* If solution paths cannot cover submission, give high weight *)
      sem_dist + (unsat_score * BatSet.cardinal remains)
    else 
      let (ctx_sub, remains) = BatSet.pop remains in 
      let (ctx_sol, ctxs_sol) = BatSet.pop unmatched in
      let (dist', ctx_sol') = BatSet.fold (fun ctx_sol (dist', ctx_sol') -> 
        let dist = compute_path_score ctx_sub ctx_sol in
        if dist <> -1 then
          if dist < dist' || dist' = -1 then (dist, ctx_sol) else (dist', ctx_sol')
        else 
          (dist', ctx_sol')
        (* if dist < dist' && dist <> -1 || dist' = -1 then (dist, ctx_sol) else (dist', ctx_sol') *)
      ) ctxs_sol (compute_path_score ctx_sub ctx_sol, ctx_sol) in
      if dist' = -1 then 
        (* There are no paths in solution which are satisfiable with submission's path *)
        iter remains unmatched sem_dist + unsat_score
      else 
        iter remains (BatSet.remove ctx_sol' unmatched) (sem_dist + dist')
  in
  iter ctxs_sub ctxs_sol 0

let rec semantic_distance : summary -> summary -> int
= fun summary_sub summary_sol -> 
  (* Semantic distance of incomming contexts *)
  let (incomming_sub, incomming_sol) = (summary_sub.incomming, summary_sol.incomming) in
  let incomming_distance = semantic_distance_ctx incomming_sub incomming_sol in
  (* Semantic distance of outgoing contexts *)
  let (outgoing_sub, outgoing_sol) = (summary_sub.outgoing, summary_sol.outgoing) in
  let outgoing_distance = semantic_distance_ctx outgoing_sub outgoing_sol in
  2 * incomming_distance + outgoing_distance

let syntactic_distance = Syntactic_dist.syntactic_distance

(* Main procedure *)
let rec find_refs_by_prototype : summary -> references -> references
= fun summary references -> BatSet.filter (fun reference -> 
    (List.length summary.node.args) = (List.length reference.summary.node.args) &&
    check_typs summary.node.typ reference.summary.node.typ
  ) references

let rec find_matching : matching -> summaries -> references -> matching
= fun matching summaries references ->
  if BatSet.is_empty summaries then matching
  else 
    let (summary, summaries) = BatSet.pop summaries in 
    if BatMap.mem summary matching then 
      let (reference, sem_dist, syn_dist) = BatMap.find summary matching in
      if sem_dist = 0 then find_matching matching summaries references
      else
        (* Find a solution functions whose type is the same with the type of the submission *)
        let candidates = find_refs_by_prototype summary references in
        (* Select reference which is more similar than current one *)
        let (reference', sem_dist', syn_dist') = BatSet.fold (fun reference' (reference, sem_dist, syn_dist) ->
          let sem_dist' = semantic_distance summary reference'.summary in
          if sem_dist' < sem_dist then (reference', sem_dist', syntactic_distance summary.node.body reference'.summary.node.body)
          else if sem_dist' = sem_dist then 
            let syn_dist' = syntactic_distance summary.node.body reference'.summary.node.body in
            if syn_dist' < syn_dist then (reference', sem_dist', syn_dist')
            else (reference, sem_dist, syn_dist) 
          else (reference, sem_dist, syn_dist)
        ) candidates (reference, sem_dist, syn_dist) in 
        let new_matching = BatMap.add summary (reference', sem_dist', syn_dist') matching in
        find_matching new_matching summaries (* (BatSet.remove reference' references) *) references
    else
      let candidates = find_refs_by_prototype summary references in
      (* Select solution functions with the minimal semantic distance *)
      let (sem_dist', candidates) = BatSet.fold (fun reference (score, candidates) ->
        let score' = semantic_distance summary reference.summary in
        if score' < score then (score', BatSet.singleton reference)
        else if score' = score then (score, BatSet.add reference candidates)
        else (score, candidates)
      ) candidates (max_int, BatSet.empty) in
      if BatSet.is_empty candidates then find_matching matching summaries references
      else
        (* Pick the most similar ones in candidates *)
        let (candidate, remains) = BatSet.pop candidates in
        let (syn_dist', reference') = BatSet.fold (fun reference (score, acc) -> 
          let score' = syntactic_distance summary.node.body reference.summary.node.body in
          if score' < score then (score', reference)
          else (score, acc)
        ) remains (syntactic_distance summary.node.body candidate.summary.node.body, candidate) in
        let new_matching = BatMap.add summary (reference', sem_dist', syn_dist') matching in
        find_matching new_matching summaries (* (BatSet.remove reference' references) *) references

let select_solutions : prog -> references list -> matching
= fun pgm references -> 
  let _ = 
    library_pgm := Preprocessor.Type_annotate.run !library_pgm
    |> Preprocessor.Decapsulation.run
  in
  let cg_sub = extract_graph (!library_pgm@pgm) in 
  let summaries = get_summaries (cg_sub) in
  let _ = ctor_table := Path_score.CtorTable.gen_ctor_table BatMap.empty pgm in
  List.fold_left (fun acc reference -> find_matching acc summaries reference) empty_matching references

(* Syntactic matchign (SARFGEN) *)
let rec find_matching_syn : matching -> summaries -> references -> matching
= fun matching summaries references ->
  if BatSet.is_empty summaries then matching
  else 
    let (summary, summaries) = BatSet.pop summaries in 
    if BatMap.mem summary matching then 
      let (reference, sem_dist, syn_dist) = BatMap.find summary matching in
      let candidates = find_refs_by_prototype summary references in
      (* Select reference which is more similar than current one *)
      let (reference', sem_dist', syn_dist') = BatSet.fold (fun reference' (reference, sem_dist, syn_dist) ->
        let syn_dist' = syntactic_distance summary.node.body reference'.summary.node.body in
        if syn_dist' < syn_dist then (reference', 0, syn_dist')
        else (reference, 0, syn_dist)
      ) candidates (reference, sem_dist, syn_dist) in 
      let new_matching = BatMap.add summary (reference', sem_dist', syn_dist') matching in
      find_matching_syn new_matching summaries (* (BatSet.remove reference' references) *) references
    else
      let candidates = find_refs_by_prototype summary references in
      if BatSet.is_empty candidates then find_matching matching summaries references
      else
        let (candidate, remains) = BatSet.pop candidates in
        let (syn_dist', reference') = BatSet.fold (fun reference (score, acc) -> 
          let score' = syntactic_distance summary.node.body reference.summary.node.body in
          if score' < score then (score', reference)
          else (score, acc)
        ) remains (syntactic_distance summary.node.body candidate.summary.node.body, candidate) in
        let new_matching = BatMap.add summary (reference', 0, syn_dist') matching in
        find_matching_syn new_matching summaries (* (BatSet.remove reference' references) *) references

let select_solutions_syn : prog -> references list -> matching
= fun pgm references -> 
  let _ = 
    library_pgm := Preprocessor.Type_annotate.run !library_pgm
    |> Preprocessor.Decapsulation.run
  in
  let summaries = get_summaries (extract_graph (!library_pgm@pgm)) in
  let _ = ctor_table := Path_score.CtorTable.gen_ctor_table BatMap.empty pgm in
  List.fold_left (fun acc reference -> find_matching_syn acc summaries reference) empty_matching references

let select_solution_syn : prog -> references list -> matching
= fun pgm references -> 
  let _ = 
    library_pgm := Preprocessor.Type_annotate.run !library_pgm
    |> Preprocessor.Decapsulation.run
  in
  let summaries = get_summaries (extract_graph (!library_pgm@pgm)) in
  let _ = ctor_table := Path_score.CtorTable.gen_ctor_table BatMap.empty pgm in
  List.fold_left (fun acc reference -> 
    let new_matching = find_matching_syn BatMap.empty summaries reference in
    if BatMap.cardinal acc = 0 then new_matching
    else if BatMap.cardinal new_matching = 0 then acc
    else
    let score1 = (BatMap.fold (fun (r, sem, syn) acc -> acc +. syn) acc 0.) /. float_of_int (BatMap.cardinal acc) in
    let score2 = (BatMap.fold (fun (r, sem, syn) acc -> acc +. syn) new_matching 0.) /. float_of_int (BatMap.cardinal new_matching)in
    if score1 < score2 then acc else new_matching
  ) empty_matching references