open Lang
open Util
open Print
open CallGraph
open Selector
open Repair_template

(*********************************************************)
(* Extract repair template from the matching information *)
(*********************************************************)
(* Exact pattern matching *)
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

let rec extract_templates : lexp -> lexp -> exp_template BatSet.t
= fun (l1, exp1) exp2 ->
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
      try List.fold_left2 (fun acc e1 e2 -> BatSet.union acc (extract_templates e1 e2)) BatSet.empty es1 es2 
      with _ -> BatSet.singleton (ModifyExp (l1, exp2))
    end
  | ECtor (x1, es1), ECtor (x2, es2) when x1 = x2 ->
    begin 
      try List.fold_left2 (fun acc e1 e2 -> BatSet.union acc (extract_templates e1 e2)) BatSet.empty es1 es2 
      with _ -> BatSet.singleton (ModifyExp (l1, exp2))
    end
  (* Unary *)
  | MINUS e1, MINUS e2 | NOT e1, NOT e2 | ERef e1, ERef e2 | EDref e1, EDref e2 -> extract_templates e1 e2
  (* Commutative binary : compute minimal distance *)
  | ADD (e1, e2), ADD (e1', e2') | MUL (e1, e2), MUL (e1', e2') | OR (e1, e2), OR (e1', e2') | AND (e1, e2), AND (e1', e2') 
  | EQUAL (e1, e2), EQUAL (e1', e2') | NOTEQ(e1, e2), NOTEQ (e1', e2') -> BatSet.union (extract_templates e1 e1') (extract_templates e2 e2') (* TODO : minimalize?? *)
  (* Noncommutative binary *)
  | SUB (e1, e2), SUB (e1', e2') | DIV (e1, e2), DIV (e1', e2') | MOD (e1, e2), MOD (e1', e2') 
  | LESS (e1, e2), LESS (e1', e2') | LARGER (e1, e2), LARGER (e1', e2') 
  | LESSEQ (e1, e2), LESSEQ (e1', e2') | LARGEREQ (e1, e2), LARGEREQ (e1', e2') 
  | AT (e1, e2), AT (e1', e2') | DOUBLECOLON (e1, e2), DOUBLECOLON (e1', e2')
  | STRCON (e1, e2), STRCON (e1', e2') | EAssign (e1, e2), EAssign (e1', e2') -> BatSet.union (extract_templates e1 e1') (extract_templates e2 e2')
  (* Condition *)
  | IF (e1, e2, e3), IF (e1', e2', e3') ->
    BatSet.union (extract_templates e1 e1') (extract_templates e2 e2') 
    |> BatSet.union (extract_templates e3 e3')
  | EMatch (e1, bs1), EMatch (e2, bs2) ->
    (* extract_templates from matched branches *)
    let (matches, unmatches, temps) = List.fold_left (fun (matches, unmatches, temps) (p, e) ->
      try 
        let (p', e') = List.find (fun (p', e') -> match_pat p p') unmatches in
        ((e, e')::matches, List.remove_assoc p' unmatches, temps)
      with _ -> 
        (matches, unmatches, BatSet.add (DeleteBranch (l1, (p, e))) temps)
    ) ([], bs2, extract_templates e1 e2) bs1 in
    let match_template = List.fold_left (fun acc (e1, e2) -> BatSet.union acc (extract_templates e1 e2)) temps matches in
    List.fold_left (fun acc b -> BatSet.add (InsertBranch (l1, b)) acc) match_template unmatches
  (* Special Cases : var, function, bindings *)
  | EVar x, EVar y -> BatSet.singleton (ModifyExp (l1, exp2))
  | _, EFun _ | _, EApp _ -> BatSet.singleton (ModifyExp (l1, exp2))
  | _, ELet _ | _, EBlock _ -> BatSet.singleton (ModifyExp (l1, exp2))
  (* Syntatically different *)
  | _ -> BatSet.singleton (ModifyExp (l1, exp2))

(* Replace special variables with holes *)
let rec replace_var_exp : Alias.alias_info -> lexp -> lexp BatSet.t
= fun alias_info (l, exp) ->
  match exp with
  (* Variable -> replace solution variable with the one in submission whose data flow is the same *)
  | EVar x -> 
    if String.sub x 0 1 <> "#" then 
      (* If it is a external variable do not change *)
      BatSet.singleton (l, EVar x)
    else 
      let alias_set = BatMap.find l alias_info in
      let xs = BatSet.map snd (BatSet.filter (fun (a, b) -> x = a) alias_set) in
      if BatSet.is_empty xs then 
        (* if there are no variables whose data-flow is exactly match with the solution variable *)
        BatSet.singleton (l, gen_hole ())
      else 
        BatSet.map (fun x -> (l, EVar x)) xs
  | EApp (e1, e2) -> 
    let es = join_tuple (replace_var_exp alias_info e1) (replace_var_exp alias_info e2) in
    BatSet.map (fun (e1, e2) -> (l, update_binary exp (e1, e2))) es 
  (* Exception *)
  | Raise e -> 
    let new_exception = (gen_label (), ECtor ("Failure", [gen_label (), String "Exception(Template)"])) in
    BatSet.singleton (l, Raise new_exception)
  | EUnit | Const _ | TRUE | FALSE | String _ -> BatSet.singleton (l, exp)
  | EFun (arg, e) -> 
    let es = replace_var_exp alias_info e in
    BatSet.map (fun e -> (l, EFun (arg, e))) es
  | ERef e | EDref e | MINUS e | NOT e -> 
    let es = replace_var_exp alias_info e in
    BatSet.map (fun e -> (l, update_unary exp e)) es
  | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2) 
  | OR (e1, e2) | AND (e1, e2) | LESS (e1, e2) | LESSEQ (e1, e2) | LARGER (e1, e2) | LARGEREQ (e1, e2) 
  | EQUAL (e1, e2) | NOTEQ (e1, e2) | DOUBLECOLON (e1, e2) | AT (e1, e2) | STRCON (e1, e2)
  | EAssign (e1, e2) ->
    let es = join_tuple (replace_var_exp alias_info e1) (replace_var_exp alias_info e2) in
    BatSet.map (fun (e1, e2) -> (l, update_binary exp (e1, e2))) es 
  | EList es -> 
    let es = join_list (List.map (fun e -> replace_var_exp alias_info e) es) in
    BatSet.map (fun es -> (l, EList es)) es
  | ETuple es -> 
    let es = join_list (List.map (fun e -> replace_var_exp alias_info e) es) in
    BatSet.map (fun es -> (l, ETuple es)) es
  | ECtor (x, es) -> 
    let es = join_list (List.map (fun e -> replace_var_exp alias_info e) es) in
    BatSet.map (fun es -> (l, ECtor (x, es))) es
  | IF (e1, e2, e3) -> 
    let es = join_triple (replace_var_exp alias_info e1) (replace_var_exp alias_info e2) (replace_var_exp alias_info e3) in
    BatSet.map (fun (e1, e2, e3) -> (l, IF (e1, e2, e3))) es
  | EMatch (e, bs) -> 
    let es = replace_var_exp alias_info e in
    let bs = join_list (List.map (fun (p, e) -> BatSet.map (fun e -> (p, e)) (replace_var_exp alias_info e)) bs) in
    BatSet.fold (fun e acc ->
      BatSet.fold (fun bs acc ->
        BatSet.add (l, EMatch (e, bs)) acc
      ) bs acc 
    ) es BatSet.empty
  | ELet (f, is_rec, args, typ, e1, e2) -> 
    let es = join_tuple (replace_var_exp alias_info e1) (replace_var_exp alias_info e2) in
    BatSet.map (fun (e1, e2) -> (l, ELet (f, is_rec, args, typ, e1, e2))) es 
  | EBlock (is_rec, bindings, e2) -> 
    let ds = join_list (
      List.map (fun (f, is_rec, args, typ, e) -> 
        BatSet.map (fun e -> (f, is_rec, args, typ, e)) (replace_var_exp alias_info e)
      ) bindings
    ) in
    let es = replace_var_exp alias_info e2 in 
    BatSet.fold (fun ds acc ->
      BatSet.fold (fun e acc ->
        BatSet.add (l, EBlock (is_rec, ds, e)) acc
      ) es acc
    ) ds BatSet.empty
  | _ -> raise (Failure ("Extractor: invalid template (" ^ Print.exp_to_string (l, exp) ^ ")"))

let replace_var_template : Alias.t -> Alias.t -> exp_template -> exp_template BatSet.t
= fun t1 t2 e_temp ->
  match e_temp with
  | ModifyExp (l, e) -> 
    let alias_info = Alias.compute_alias_info (BatMap.find l t1) t2 in
    let es = replace_var_exp alias_info e in
    BatSet.map (fun e -> ModifyExp (l, e)) es
  | InsertBranch (l, (p, e)) -> 
    let alias_info = Alias.compute_alias_info (BatMap.find l t1) t2 in
    let es = replace_var_exp alias_info e in
    BatSet.map (fun e -> InsertBranch (l, (p, e))) es
  | _ -> BatSet.singleton e_temp

let extract_templates : matching -> repair_template BatSet.t * call_templates
= fun matching ->
  BatMap.foldi (fun target (reference, _, _) (repair_temps, call_temps) ->
    let (target_node, reference_node) = (target.node, reference.summary.node) in
    let (t1, t2) = (Alias.analysis_node target_node, Alias.analysis_node reference_node) in
    let temps = extract_templates target_node.body reference_node.body in 
    (* Replace variables in templates with comparable ones *)
    let temps = BatSet.fold (fun e_temp acc -> BatSet.union acc (replace_var_template t1 t2 e_temp)) temps BatSet.empty in
    (* TODO : how to deal helper functions *)
    let temps = BatSet.map (fun e_temp -> (e_temp, BatMap.empty)) temps in
    (* Save how current function is used in reference *)
    let r_env = BatMap.singleton reference_node.name target_node.name in
    let call_temp = BatSet.map (fun e -> Preprocessor.Renaming.apply_exp r_env e) reference.usecase in
    (BatSet.union repair_temps temps, BatMap.add target_node.name call_temp call_temps)
  ) matching (BatSet.empty, BatMap.empty)