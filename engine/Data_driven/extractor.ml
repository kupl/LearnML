open Lang
open Util
open Print
open CallGraph
open Selector
open Repair_template
open Explore

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
    | DeleteBranch (_, _) -> BatSet.empty  

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
  | MINUS e1, MINUS e2 | NOT e1, NOT e2 | ERef e1, ERef e2 | EDref e1, EDref e2 -> get_template e1 e2
  (* Commutative binary : compute minimal distance *)
  | ADD (e1, e2), ADD (e1', e2') | MUL (e1, e2), MUL (e1', e2') | OR (e1, e2), OR (e1', e2') | AND (e1, e2), AND (e1', e2') 
  | EQUAL (e1, e2), EQUAL (e1', e2') | NOTEQ(e1, e2), NOTEQ (e1', e2') -> BatSet.union (get_template e1 e1') (get_template e2 e2') (* TODO : minimalize?? *)
  (* Noncommutative binary *)
  | SUB (e1, e2), SUB (e1', e2') | DIV (e1, e2), DIV (e1', e2') | MOD (e1, e2), MOD (e1', e2') 
  | LESS (e1, e2), LESS (e1', e2') | LARGER (e1, e2), LARGER (e1', e2') 
  | LESSEQ (e1, e2), LESSEQ (e1', e2') | LARGEREQ (e1, e2), LARGEREQ (e1', e2') 
  | AT (e1, e2), AT (e1', e2') | DOUBLECOLON (e1, e2), DOUBLECOLON (e1', e2')
  | STRCON (e1, e2), STRCON (e1', e2') | EAssign (e1, e2), EAssign (e1', e2') -> BatSet.union (get_template e1 e1') (get_template e2 e2')
  (* Condition *)
  | IF (e1, e2, e3), IF (e1', e2', e3') ->
    BatSet.union (get_template e1 e1') (get_template e2 e2') 
    |> BatSet.union (get_template e3 e3')
  | EMatch (e1, bs1), EMatch (e2, bs2) ->
    (* get_template from matched branches *)
    let (matches, unmatches, temps) = List.fold_left (fun (matches, unmatches, temps) (p, e) ->
      try 
        let (p', e') = List.find (fun (p', e') -> match_pat p p') unmatches in
        ((e, e')::matches, List.remove_assoc p' unmatches, temps)
      with _ -> 
        (matches, unmatches, BatSet.add (DeleteBranch (l1, (p, e))) temps)
    ) ([], bs2, get_template e1 e2) bs1 in
    let match_template = List.fold_left (fun acc (e1, e2) -> BatSet.union acc (get_template e1 e2)) temps matches in
    List.fold_left (fun acc b -> BatSet.add (InsertBranch (l1, b)) acc) match_template unmatches
  (* Special Cases : var, function, bindings *)
  | EVar x, EVar y -> BatSet.singleton (ModifyExp (l1, exp2))
  | _, EFun _ | _, EApp _ -> BatSet.singleton (ModifyExp (l1, exp2))
  | _, ELet _ | _, EBlock _ -> BatSet.singleton (ModifyExp (l1, exp2))
  (* Syntatically different *)
  | _ -> BatSet.singleton (ModifyExp (l1, exp2))

(* Replace special variables with holes *)
let rec make_hole_exp : Alias.D.analysis -> Alias.D.t -> lexp -> lexp BatSet.t
= fun map t2 (l, exp) ->
  match exp with
  (* Variable -> replace solution variable with the one in submission whose data flow is the same *)
  | EVar x -> 
    let d2 = BatMap.find x (BatMap.find l t2) in
    let xs = BatMap.foldi (fun x1 d1 xs -> 
      if Alias.D.compare d1 d2 then BatSet.add x1 xs else xs 
    ) map BatSet.empty in
    if BatSet.is_empty xs then 
      (* if there are no variables whose data-flow is exactly match with the solution variable *)
      BatSet.singleton (l, gen_hole ())
    else 
      BatSet.map (fun x -> (l, EVar x)) xs
  (* Function call -> replace function with special hole which is filled with the call templates *)
  | EApp (e1, e2) -> BatSet.singleton (l, gen_fhole ())
  (* Exception *)
  | Raise e -> 
    let new_exception = (gen_label (), ECtor ("Failure", [gen_label (), String "Exception(Template)"])) in
    BatSet.singleton (l, Raise new_exception)
  | EUnit | Const _ | TRUE | FALSE | String _ -> BatSet.singleton (l, exp)
  | EFun (arg, e) -> 
    let es = make_hole_exp map t2 e in
    BatSet.map (fun e -> (l, EFun (arg, e))) es
  | ERef e | EDref e | MINUS e | NOT e -> 
    let es = make_hole_exp map t2 e in
    BatSet.map (fun e -> (l, update_unary exp e)) es
  | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2) 
  | OR (e1, e2) | AND (e1, e2) | LESS (e1, e2) | LESSEQ (e1, e2) | LARGER (e1, e2) | LARGEREQ (e1, e2) 
  | EQUAL (e1, e2) | NOTEQ (e1, e2) | DOUBLECOLON (e1, e2) | AT (e1, e2) | STRCON (e1, e2)
  | EAssign (e1, e2) ->
    let es = join_tuple (make_hole_exp map t2 e1) (make_hole_exp map t2 e2) in
    BatSet.map (fun (e1, e2) -> (l, update_binary exp (e1, e2))) es 
  | EList es -> 
    let es = join_list (List.map (fun e -> make_hole_exp map t2 e) es) in
    BatSet.map (fun es -> (l, EList es)) es
  | ETuple es -> 
    let es = join_list (List.map (fun e -> make_hole_exp map t2 e) es) in
    BatSet.map (fun es -> (l, ETuple es)) es
  | ECtor (x, es) -> 
    let es = join_list (List.map (fun e -> make_hole_exp map t2 e) es) in
    BatSet.map (fun es -> (l, ECtor (x, es))) es
  | IF (e1, e2, e3) -> 
    let es = join_triple (make_hole_exp map t2 e1) (make_hole_exp map t2 e2) (make_hole_exp map t2 e3) in
    BatSet.map (fun (e1, e2, e3) -> (l, IF (e1, e2, e3))) es
  | EMatch (e, bs) -> 
    let es = make_hole_exp map t2 e in
    let bs = join_list (
      List.map (fun (p, e) -> 
        BatSet.map (fun e -> (p, e)) (make_hole_exp map t2 e)
      ) bs
    ) in
    BatSet.fold (fun e acc ->
      BatSet.fold (fun bs acc ->
        BatSet.add (l, EMatch (e, bs)) acc
      ) bs acc 
    ) es BatSet.empty
  | ELet (f, is_rec, args, typ, e1, e2) -> 
    let es = join_tuple (make_hole_exp map t2 e1) (make_hole_exp map t2 e2) in
    BatSet.map (fun (e1, e2) -> (l, ELet (f, is_rec, args, typ, e1, e2))) es 
  | EBlock (is_rec, bindings, e2) -> 
    let ds = join_list (
      List.map (fun (f, is_rec, args, typ, e) -> 
        BatSet.map (fun e -> (f, is_rec, args, typ, e)) (make_hole_exp map t2 e)
      ) bindings
    ) in
    let es = make_hole_exp map t2 e2 in 
    BatSet.fold (fun ds acc ->
      BatSet.fold (fun e acc ->
        BatSet.add (l, EBlock (is_rec, ds, e)) acc
      ) es acc
    ) ds BatSet.empty
  | _ -> raise (Failure ("Extractor: invalid template (" ^ Print.exp_to_string (l, exp) ^ ")"))

let rec make_hole_template : Alias.D.t -> Alias.D.t -> exp_template -> exp_template BatSet.t
= fun t1 t2 e_temp ->
  match e_temp with
  | ModifyExp (l, e) -> 
    let es = make_hole_exp (BatMap.find l t1) t2 e in
    BatSet.map (fun e -> ModifyExp (l, e)) es
  | InsertBranch (l, (p, e)) -> 
    let es = make_hole_exp (BatMap.find l t1) t2 e in
    BatSet.map (fun e -> InsertBranch (l, (p, e))) es
  | _ -> BatSet.singleton e_temp

(* Obtaining related function with templates *)
(*
let rec get_decl_template : matching -> graph -> exp_template -> required_function
= fun matching g_sol e_temp -> 
  let labels = Label.get_label_template e_temp in
  (* Get all invoked functions' id by the templates *)
  let callees = BatMap.foldi (fun (caller, callee) ctxs acc -> 
    let labels' = BatSet.map (fun (l, ctx) -> l) ctxs in
    if BatSet.is_empty (BatSet.intersect labels labels') then 
      acc (* If repair target does not include additional funciton call *)
    else
      if caller = callee then acc else BatSet.union acc (get_reachable callee g_sol)
  ) (get_edge g_sol) BatSet.empty in
  (* TODO *)
  BatMap.foldi (fun f_sol (id, args, typ, body) acc -> 
    if BatSet.mem id callees && not (BatMap.mem f_sol matching) then
      let caller = get_caller id g_sol in
      let is_rec = BatSet.mem id caller in
      let caller = BatSet.filter (fun id' -> id <> id') caller in
      BatMap.add f_sol (is_rec, args, typ, body, BatSet.map (fun id -> get_function_name id g_sol) caller) acc
    else 
      acc
  ) (get_node g_sol) BatMap.empty
*)

let extract_templates : t -> repair_template BatSet.t
= fun matching ->
  BatMap.foldi (fun unit_sub (unit_sol, callees) acc ->
    let t1 = Alias.D.analysis_unit unit_sub in
    let t2 = Alias.D.analysis_unit unit_sol in
    let _ =
      print_header "Submission function"; print_endline (string_of_unit unit_sub);
      print_header "Solution function"; print_endline (string_of_unit unit_sol);
      print_header "Submission Data-flow"; Alias.D.print t1;
      print_header "Solution Data-flow"; Alias.D.print t2
    in
    let (body_sub, body_sol) = (get_body unit_sub, get_body unit_sol) in
    let temps = get_template body_sub body_sol in 
    let temps = BatSet.fold (fun e_temp acc -> 
      let temps = make_hole_template t1 t2 e_temp in
      BatSet.union acc temps
    ) temps BatSet.empty in
    (* TODO : function addtion *)
    (*
    let decls = BatSet.fold (fun (f_id, args, typ, _, body) acc -> 
      BatMap.add f_id (true, args, typ, body, BatSet.singleton (get_id unit_sub)) acc
    ) callees BatMap.empty in
    *)
    let decls = BatMap.empty in
    let temps = BatSet.map (fun e_temp -> (e_temp, decls)) temps in
    BatSet.union acc temps
  ) matching BatSet.empty
  (*
  let (cg_sub, cg_sol) = (CallGraph.run sub, CallGraph.run sol) in
  let (matching, remaining_sub, _) = Matching2.run sub sol in
  let matching_sol = BatMap.foldi (fun f_sub f_sol acc -> BatMap.add f_sol f_sub acc) matching BatMap.empty in
  (* let _ = Print.print_header "Matching Info (Sol)"; Matching2.print (Matching2.run sol sub) in *)
  (* Find syntactic discrepancies based on the matching information *)
  let temps = BatMap.foldi (fun sub sol acc -> 
    let (_, _, _, body_sub) = BatMap.find sub (get_node cg_sub) in 
    let (_, _, _, body_sol) = BatMap.find sol (get_node cg_sol) in
    let e_temps = BatSet.map (fun e_temp -> make_hole_template e_temp) (get_template body_sub body_sol) in
    let temps = BatSet.map (fun e_temp -> 
      let related_functions = 
        get_decl_template matching_sol cg_sol e_temp
        |> BatMap.map (fun (is_rec, args, typ, e, callers) -> (is_rec, args, typ, e, BatSet.map (fun f_sol -> BatMap.find f_sol matching_sol) callers))
      in
      (e_temp, related_functions)
    ) e_temps in 
    BatSet.union acc temps  
  ) matching BatSet.empty in
  (* Try to explore which are not matched *)
  let temps = BatSet.fold (fun f_sub acc ->
    let (_, _, _, body_sub) = BatMap.find f_sub (get_node cg_sub) in 
    let labels = Label.get_labels_exp body_sub in
    (* BatSet.union acc (BatSet.map (fun l -> (Explore l, BatMap.empty)) labels) *)
    BatSet.union acc (explore labels sub) 
  ) remaining_sub temps in  
  (* Remove template which fixs the library code or never be executed *)
  let library_labels = Label.get_labels (!library_pgm) in
  BatSet.filter (fun (e_temp, _) -> 
    match e_temp with
    | ModifyExp (l, _) | InsertBranch (l, _)
    | DeleteBranch (l, _) | Explore l -> not (BatSet.mem l library_labels)
  ) temps   
  *) 