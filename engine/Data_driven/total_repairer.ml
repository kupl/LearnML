open Lang
open Repairer
open Vrepairer

type analysis = (label, (labels * labels)) BatMap.t (* label -> parents, child *)
and labels = label BatSet.t

let print_analysis : analysis -> unit
= fun m ->
  BatMap.iter (fun l (parents, subs) -> 
    print_endline (string_of_int l ^ " |-> ");
    print_endline ("Parents : ");
    print_endline ("{" ^ BatSet.fold (fun l str -> str ^ ", " ^ string_of_int l) parents "" ^ "}");
    print_endline ("Subs : ");
    print_endline ("{" ^ BatSet.fold (fun l str -> str ^ ", " ^ string_of_int l) subs "" ^ "}")
  ) m

let get_label (l, e) = l

let rec analysis_exp : analysis -> labels -> lexp -> (labels * analysis)
= fun m parents (l, exp) ->
  match exp with
  | MINUS e | NOT e | EFun (_, e) | Raise e -> 
    let (subs, m) = analysis_exp m (BatSet.add l parents) e in
    let subs = BatSet.add (get_label e) subs in
    (subs, BatMap.add l (parents, subs) m)
  | EList es | ECtor (_, es) | ETuple es -> 
    let (subs, m) = analysis_exp_list m (BatSet.add l parents) es in
    let subs = List.fold_left (fun subs e -> BatSet.add (get_label e) subs) subs es in
    (subs, BatMap.add l (parents, subs) m)
  | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2)
  | OR (e1, e2) | AND (e1, e2) | EQUAL (e1, e2) | NOTEQ (e1, e2)
  | LESS (e1, e2) | LARGER (e1, e2)| LESSEQ (e1, e2) | LARGEREQ (e1, e2)
  | AT (e1, e2) | DOUBLECOLON (e1, e2) | STRCON (e1, e2) | EApp (e1, e2) 
  | ELet (_, _, _, _, e1, e2) -> 
    let (subs, m) = analysis_exp_list m (BatSet.add l parents) [e1; e2] in
    let subs = List.fold_left (fun subs e -> BatSet.add (get_label e) subs) subs [e1; e2] in
    (subs, BatMap.add l (parents, subs) m)
  | IF (e1, e2, e3) -> 
    let (subs, m) = analysis_exp_list m (BatSet.add l parents) [e1; e2; e3] in
    let subs = List.fold_left (fun subs e -> BatSet.add (get_label e) subs) subs [e1; e2; e3] in
    (subs, BatMap.add l (parents, subs) m)
  | EMatch (e, bs) -> 
    let es = e::(List.map (fun (p, e) -> e) bs) in
    let (subs, m) = analysis_exp_list m (BatSet.add l parents) es in
    let subs = List.fold_left (fun subs e -> BatSet.add (get_label e) subs) subs es in
    (subs, BatMap.add l (parents, subs) m)
  | EBlock (_, bs, e) -> 
    let es = e::(List.map (fun (_, _, _, _, e) -> e) bs) in
    let (subs, m) = analysis_exp_list m (BatSet.add l parents) es in
    let subs = List.fold_left (fun subs e -> BatSet.add (get_label e) subs) subs es in
    (subs, BatMap.add l (parents, subs) m)
  | _ -> (BatSet.empty, BatMap.add l (parents, BatSet.empty) m)

and analysis_exp_list : analysis -> labels -> lexp list -> (labels * analysis)
= fun m parents es ->
  match es with
  | [] -> (BatSet.empty, m)
  | e::tl ->
    let (subs1, m) = analysis_exp m parents e in
    let (subs2, m) = analysis_exp_list m parents tl in
    (BatSet.union subs1 subs2, m)

let rec analysis_decl : analysis -> decl -> analysis
= fun m decl ->
  match decl with
  | DLet (_, _, _, _, e) -> snd (analysis_exp m BatSet.empty e)
  | DBlock (_, ds) -> List.fold_left (fun m (_, _, _, _, e) -> snd (analysis_exp m BatSet.empty e)) m ds
  | _ -> m

let label_analysis : prog -> analysis
= fun decls -> List.fold_left (fun m decl -> analysis_decl m decl) BatMap.empty decls

let join_cand : analysis -> repair_cand BatSet.t -> repair_cand BatSet.t -> repair_cand BatSet.t
= fun m c1 c2 ->
  BatSet.fold (fun (l1, e1) c ->
    let c' = BatSet.filter (fun (l2, e2) -> (l1 = l2) && (Workset.exp_cost e1 < Workset.exp_cost e2)) c2 in
    if BatSet.is_empty c' then 
      let (parents, subs) = BatMap.find l1 m in
      let redundant = BatSet.filter (fun (l2, e2) -> (BatSet.mem l2 parents)) c2 in (* Except less significant templates than current one *)
      if BatSet.exists (fun (l2, e2) -> BatSet.mem l2 subs) c then c (* More significant one already exists => do not use current one *)
      else if BatSet.is_empty redundant then c (* No redundant ones => we do not have to use current one *)
      else BatSet.add (l1, e1) (BatSet.diff c redundant) 
    else (* current candidate (l1, e1) is the smallest one *)
      BatSet.add (l1, e1) (BatSet.diff c c') 
  ) c1 c2

let run : prog -> prog list -> examples -> prog option
= fun pgm cpgms testcases ->
  start_time := Unix.gettimeofday();
  let t = Normalizer.normalize_all pgm in
  let m = BatMap.fold (fun e m -> snd (analysis_exp m BatSet.empty e)) t BatMap.empty in
  let repair_template = List.fold_left (fun template cpgm ->
    let map = Freq_vector.find_mapping pgm cpgm in
    let repair_cand = get_repair_candidate pgm cpgm map in
    let repair_cand = gen_func_temp repair_cand in
    Print.print_header "Solution"; Print.print_pgm2 cpgm;
    print_mapping map;
    print_endline ("Initial Repair Cand : ");
    print_endline ("------------------------------");
    BatSet.iter (fun (l, e) -> print_endline (string_of_int l ^ " : " ^ Print.exp_to_string e)) repair_cand;
    print_endline ("------------------------------");
    let template = if BatSet.is_empty template then repair_cand else join_cand m repair_cand template in
    print_endline ("Resulting Repair Cand : ");
    print_endline ("------------------------------");
    BatSet.iter (fun (l, e) -> print_endline (string_of_int l ^ " : " ^ Print.exp_to_string e)) template;
    print_endline ("------------------------------");
    template
  ) BatSet.empty cpgms
  in
  print_endline ("Resulting Repair Cand : ");
  print_endline ("------------------------------");
  BatSet.iter (fun (l, e) -> print_endline (string_of_int l ^ " : " ^ Print.exp_to_string e)) repair_template;
  print_endline ("------------------------------");
  let cpgm = List.hd cpgms in
  let map = Freq_vector.find_mapping pgm cpgm in
  let pre_result = PreAnalysis.run cpgm in
  let repair_cand = get_repair_candidate pgm cpgm map in
  let repair_cand = gen_func_temp repair_cand in
  (*
  print_endline ("Initial Repair Cand : ");
  print_endline ("------------------------------");
  BatSet.iter (fun (l, e) -> print_endline (string_of_int l ^ " : " ^ Print.exp_to_string e)) repair_cand;
  print_endline ("------------------------------");
  *)
  let repair_cand = update_var_comp pre_result pgm repair_cand in
  let repair_cand = Function_unifier.update_func_comp pre_result pgm repair_cand in
  (*
  print_endline ("Refined Repair Cand : ");
  print_endline ("------------------------------");
  BatSet.iter (fun (l, e) -> print_endline (string_of_int l ^ " : " ^ Print.exp_to_string e)) repair_cand;
  print_endline ("------------------------------");
  print_endline ("Size of repair Cand : " ^ string_of_int (BatSet.cardinal repair_cand));
  *)
  let repair = work pgm (Workset.init repair_cand) testcases in
  match repair with
  | Some pgm' ->
    Print.print_header "Repair result"; Print.print_pgm pgm';
    print_endline ("Time : " ^ string_of_float (Unix.gettimeofday() -. !start_time));
    Some pgm'
  | None -> print_endline ("Fail to Repair"); None