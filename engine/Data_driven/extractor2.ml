open Lang
open Util
open Print
open CallGraph
open Matching2
open Repair_template

(*********************************************************)
(* Extract repair template from the matching information *)
(*********************************************************)
module Label = struct
  let rec get_labels_exp : lexp -> label BatSet.t
  = fun (l, exp) ->
    match exp with
    | EList es | ECtor (_, es) | ETuple es -> 
      List.fold_left (fun labels e -> BatSet.union labels (get_labels_exp e)) BatSet.empty es
    | EFun (_, e) | MINUS e | NOT e -> BatSet.union (get_labels_exp e) (BatSet.singleton l)
    | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2) 
    | OR (e1, e2) | AND (e1, e2) | LESS (e1, e2) | LARGER (e1, e2) | LESSEQ (e1, e2) | LARGEREQ (e1, e2) 
    | EQUAL (e1, e2) | NOTEQ (e1, e2) | AT (e1, e2) | DOUBLECOLON (e1, e2) | STRCON (e1, e2) | EApp (e1, e2) 
    | ELet (_, _, _, _, e1, e2) ->
      BatSet.singleton l
      |> BatSet.union (get_labels_exp e1)
      |> BatSet.union (get_labels_exp e2)
    | EBlock (_, ds, e) ->
      let es = e::(List.map (fun (_, _, _, _, e) -> e) ds) in
      List.fold_left (fun labels e -> BatSet.union labels (get_labels_exp e)) BatSet.empty es
    | EMatch (e, bs) ->
      let es = e::(List.map (fun (p, e) -> e) bs) in
      List.fold_left (fun labels e -> BatSet.union labels (get_labels_exp e)) BatSet.empty es
    | IF (e1, e2, e3) ->
      BatSet.singleton l
      |> BatSet.union (get_labels_exp e1)
      |> BatSet.union (get_labels_exp e2)
      |> BatSet.union (get_labels_exp e3)
    | _ -> BatSet.singleton l

  let get_labels_decl : decl -> label BatSet.t 
  = fun decl ->
    match decl with
    | DLet (_, _, _, _, e) -> get_labels_exp e
    | DBlock (_, ds) -> List.fold_left (fun labels (_, _, _, _, e) -> BatSet.union labels (get_labels_exp e)) BatSet.empty ds 
    | _ -> BatSet.empty

  let get_labels : prog -> label BatSet.t
  = fun pgm -> List.fold_left (fun labels decl -> BatSet.union labels (get_labels_decl decl)) BatSet.empty pgm

  let rec get_label_template : exp_template -> label BatSet.t
  = fun e_temp ->
    match e_temp with
    | ModifyExp (_, e) | InsertBranch (_, (_, e)) -> get_labels_exp e  

  let rec get_label_templates : exp_templates -> label BatSet.t
  = fun e_temps -> BatSet.fold (fun e_temp acc -> BatSet.union acc (get_label_template e_temp)) e_temps BatSet.empty
end 

(* Exact pattern matching *)
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

let rec get_template : lexp -> lexp -> exp_template BatSet.t
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
      try List.fold_left2 (fun acc e1 e2 -> BatSet.union acc (get_template e1 e2)) BatSet.empty es1 es2 
      with _ -> BatSet.singleton (ModifyExp (l1, exp2))
    end
  | ECtor (x1, es1), ECtor (x2, es2) when x1 = x2 ->
    begin 
      try List.fold_left2 (fun acc e1 e2 -> BatSet.union acc (get_template e1 e2)) BatSet.empty es1 es2 
      with _ -> BatSet.singleton (ModifyExp (l1, exp2))
    end
  (* Unary *)
  | MINUS e1, MINUS e2 | NOT e1, NOT e2 | EFun (_, e1), EFun (_, e2) -> get_template e1 e2
  (* Commutative binary : compute minimal distance *)
  | ADD (e1, e2), ADD (e1', e2') | MUL (e1, e2), MUL (e1', e2') | OR (e1, e2), OR (e1', e2') | AND (e1, e2), AND (e1', e2') 
  | EQUAL (e1, e2), EQUAL (e1', e2') | NOTEQ(e1, e2), NOTEQ (e1', e2') -> BatSet.union (get_template e1 e1') (get_template e2 e2') (* TODO : minimalize?? *)
  (* Noncommutative binary *)
  | SUB (e1, e2), SUB (e1', e2') | DIV (e1, e2), DIV (e1', e2') | MOD (e1, e2), MOD (e1', e2') 
  | LESS (e1, e2), LESS (e1', e2') | LARGER (e1, e2), LARGER (e1', e2') 
  | LESSEQ (e1, e2), LESSEQ (e1', e2') | LARGEREQ (e1, e2), LARGEREQ (e1', e2') 
  | AT (e1, e2), AT (e1', e2') | DOUBLECOLON (e1, e2), DOUBLECOLON (e1', e2')
  | STRCON (e1, e2), STRCON (e1', e2') -> BatSet.union (get_template e1 e1') (get_template e2 e2')
  (* Condition *)
  | IF (e1, e2, e3), IF (e1', e2', e3') ->
    BatSet.union (get_template e1 e1') (get_template e2 e2') 
    |> BatSet.union (get_template e3 e3')
  | EMatch (e1, bs1), EMatch (e2, bs2) ->
    (* get_template from matched branches *)
    let (matches, unmatches) = List.fold_left (fun (matches, unmatches) (p, e) ->
      try 
        let (p', e') = List.find (fun (p', e') -> match_pat p p') unmatches in
        ((e, e')::matches, List.remove_assoc p' unmatches)
      with _ -> (matches, unmatches)
    ) ([], bs2) bs1 in
    let match_template = List.fold_left (fun acc (e1, e2) -> BatSet.union acc (get_template e1 e2)) (get_template e1 e2) matches in
    List.fold_left (fun acc b -> BatSet.add (InsertBranch (l1, b)) acc) match_template unmatches
  (* Special Cases : var, function, bindings *)
  | EVar x, EVar y -> BatSet.singleton (ModifyExp (l1, exp2))
  | _, EApp _ -> BatSet.singleton (ModifyExp (l1, exp2))
  | _, ELet _ | _, EBlock _ -> BatSet.singleton (ModifyExp (l1, exp2))
  (* Syntatically different *)
  | _ -> BatSet.singleton (ModifyExp (l1, exp2))

(* Replace special templates (var, func) with hole *)
let rec make_hole_exp : lexp -> lexp
= fun (l, exp) ->
  let exp = 
    match exp with
    | EVar x -> gen_hole ()
    | EApp (e1, e2) -> EApp (make_hole_exp e1, make_hole_exp e2)
    | Raise e -> Raise (make_hole_exp e)
    | EFun (arg, e) -> EFun (arg, make_hole_exp e)
    | MINUS e -> MINUS (make_hole_exp e)
    | NOT e -> NOT (make_hole_exp e)
    | ADD (e1, e2) -> ADD (make_hole_exp e1, make_hole_exp e2)
    | SUB (e1, e2) -> SUB (make_hole_exp e1, make_hole_exp e2)
    | MUL (e1, e2) -> MUL (make_hole_exp e1, make_hole_exp e2)
    | DIV (e1, e2) -> DIV (make_hole_exp e1, make_hole_exp e2)
    | MOD (e1, e2) -> MOD (make_hole_exp e1, make_hole_exp e2)
    | OR (e1, e2) -> OR (make_hole_exp e1, make_hole_exp e2)
    | AND (e1, e2) -> AND (make_hole_exp e1, make_hole_exp e2)
    | LESS (e1, e2) -> LESS (make_hole_exp e1, make_hole_exp e2)
    | LESSEQ (e1, e2) -> LESSEQ (make_hole_exp e1, make_hole_exp e2)
    | LARGER (e1, e2) -> LARGER (make_hole_exp e1, make_hole_exp e2)
    | LARGEREQ (e1, e2) -> LARGEREQ (make_hole_exp e1, make_hole_exp e2)
    | EQUAL (e1, e2) -> EQUAL (make_hole_exp e1, make_hole_exp e2)
    | NOTEQ (e1, e2) -> NOTEQ (make_hole_exp e1, make_hole_exp e2)
    | DOUBLECOLON (e1, e2) -> DOUBLECOLON (make_hole_exp e1, make_hole_exp e2)
    | AT (e1, e2) -> AT (make_hole_exp e1, make_hole_exp e2)
    | STRCON (e1, e2) -> STRCON (make_hole_exp e1, make_hole_exp e2)
    | EList es -> EList (List.map (make_hole_exp) es)
    | ETuple es -> ETuple (List.map (make_hole_exp) es)
    | ECtor (x, es) -> ECtor (x, List.map (make_hole_exp) es)
    | IF (e1, e2, e3) -> IF (make_hole_exp e1, make_hole_exp e2, make_hole_exp e3)
    | EMatch (e, bs) -> EMatch (make_hole_exp e, List.map (fun (p, e) -> (p, make_hole_exp e)) bs)
    | ELet (f, is_rec, args, typ, e1, e2) -> ELet (f, is_rec, args, typ, make_hole_exp e1, make_hole_exp e2)
    | EBlock (is_rec, bindings, e2) -> EBlock (is_rec, List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, args, typ, make_hole_exp e)) bindings, make_hole_exp e2)
    | _ -> exp
  in
  (l, exp)

let rec make_hole_template : exp_template -> exp_template
= fun e_temp ->
  match e_temp with
  | ModifyExp (l, e) -> ModifyExp (l, make_hole_exp e)
  | InsertBranch (l, (p, e)) -> InsertBranch (l, (p, make_hole_exp e))

(* Obtaining related function with templates *)
let rec get_decl_template : (id, id) BatMap.t -> graph -> Normalizer.t -> exp_template -> required_function
= fun matching g_sol np e_temp -> 
  let labels = Label.get_label_template e_temp in
  (* Get all invoked functions' id by the templates *)
  let callees = BatMap.foldi (fun (caller, callee) ctxs acc -> 
    let labels' = BatSet.map (fun (l, ctx) -> l) ctxs in
    if BatSet.is_empty (BatSet.intersect labels labels') then 
      acc (* If repair target does not include additional funciton call *)
    else
      if caller = callee then acc else BatSet.union acc (get_reachable callee g_sol)
  ) (get_edge g_sol) BatSet.empty in
  (* TODO : if a invoked function has matching pair, except it *)
  BatMap.foldi (fun f (id, _, _, _) acc -> 
    if BatSet.mem id callees && not (BatMap.mem f matching) then
      let caller = get_caller id g_sol in
      let (args, typ, body) = BatMap.find f np in
      let is_rec = BatSet.mem id caller in
      let caller = BatSet.filter (fun id' -> id <> id') caller in (* TODO : Convert caller in submission program *)
      BatMap.add f (is_rec, args, typ, body, BatSet.map (fun id -> get_function_name id g_sol) caller) acc
    else 
      acc
  ) (get_node g_sol) BatMap.empty

let run : prog -> prog -> repair_template BatSet.t
= fun sub sol ->
  let (np_sub, np_sol) = (Normalizer.normalize_all sub, Normalizer.normalize_all sol) in
  let (cg_sub, cg_sol) = (CallGraph.run sub, CallGraph.run sol) in
  let (matching, _) = Matching2.run sub sol in
  let (matching_sol, _) = Matching2.run sol sub in
  (* let _ = Print.print_header "Matching Info (Sol)"; Matching2.print (Matching2.run sol sub) in *)
  BatMap.foldi (fun sub sol acc -> 
    let (_, _, _, body_sub) = BatMap.find sub (get_node cg_sub) in 
    let (_, _, _, body_sol) = BatMap.find sol (get_node cg_sol) in
    let e_temps = BatSet.map (fun e_temp -> make_hole_template e_temp) (get_template body_sub body_sol) in
    let temps = BatSet.map (fun e_temp -> (e_temp, get_decl_template matching_sol cg_sol np_sol e_temp)) e_temps in 
    BatSet.union acc temps
  ) matching BatSet.empty

(* When functions in submission are unmatched *)
(*
if BatSet.is_empty sol then
  let labels = Label.get_labels_exp body_sub in
  let e_temps = BatSet.map (fun l -> ModifyExp (l, gen_labeled_hole ())) labels in
  let temps = BatSet.map (fun e_temp -> (e_temp, BatMap.empty)) e_temps in
  BatSet.union acc temps
  acc
*)