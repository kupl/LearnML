open Lang
open Repairer

let get_repair_candidate : prog -> prog -> (string*string) list -> repair_cand BatSet.t
= fun pgm cpgm map ->
  let pgm_fmap = Extractor.extract_func_all pgm in
  let cpgm_fmap = Extractor.extract_func_all cpgm in 
  List.fold_left (fun acc (x, y) -> 
                  try 
                    let f = List.assoc x pgm_fmap in 
                    let f'= List.assoc y cpgm_fmap in
                    let repair_cand = Repairer.get_repair_candidate_exp f f' in
                    BatSet.union repair_cand acc
                  with Not_found -> acc
                 ) BatSet.empty map 

let print_mapping : (string * string) list -> unit
= fun map -> 
  print_endline ("map size " ^ string_of_int (List.length map));
  List.iter (fun (x,y) -> print_endline ("map : #" ^ x ^ " <~> #" ^ y)) map

let test : prog -> unit
= fun pgm ->
  let res = Extractor.extract_func_all pgm in
  let str = List.fold_left (fun acc (s,exp) -> acc ^ "\nFunc : " ^ s ^ "\n"^(Print.exp_to_string exp) ^"\n---------------------\n") "" res in print_endline str

let compare_exp : lexp -> lexp -> int
= fun e1 e2 ->
  let (c1, c2) = (exp_cost e1, exp_cost e2) in
  if c1 = c2 then 0 else
  if c1 > c2 then 1
  else -1

let run : prog -> prog -> (string*string) list -> examples -> prog option
= fun pgm cpgm map testcases ->
<<<<<<< HEAD
  let start_time = Unix.gettimeofday () in
  let pre_result = PreAnalysis.run cpgm in
  let repair_cand = 
    get_repair_candidate pgm cpgm map 
    |> gen_func_temp
    |> update_var_comp pre_result pgm
    |> Function_unifier.update_func_comp pre_result pgm
  in
  print_mapping map;
  print_endline ("Size of repair Cand : " ^ string_of_int (BatSet.cardinal repair_cand));
  let sorted_repair_cand = List.sort (fun (_, e1) (_, e2) -> compare_exp e1 e2) (BatSet.to_list repair_cand) in
  let repair = List.find_opt (fun (l, e) ->
	  print_endline ("label : " ^ string_of_int l);
    print_endline (Print.exp_to_string e);
    let pgm' = List.map(fun decl -> subst_decl decl (l,e)) pgm in
    (* Print.print_pgm pgm'; *)
    Eval.is_solution pgm' testcases
  ) sorted_repair_cand
  in
  match repair with
  | Some (l,e) ->
      let pgm' = List.map (fun decl -> subst_decl decl (l,e)) pgm in
      Print.print_header "Repair result"; Print.print_pgm pgm';
      print_endline ("Time : " ^ string_of_float (Unix.gettimeofday() -. start_time));
      Some pgm'
  | None -> print_endline ("Fail to Repair"); None
=======
    let start_time = Unix.gettimeofday () in
    let repair_cand = get_repair_candidate pgm cpgm map in
    let repair_cand = update_var_comp pgm repair_cand in
    print_mapping map;
    print_endline ("Size of repair Cand : " ^ string_of_int (BatSet.cardinal repair_cand));
    let repair = List.find_opt (fun (l, e) ->
		  print_endline ("label : " ^ string_of_int l);
      print_endline (Print.exp_to_string (l, e));
      let pgm' = List.map(fun decl -> subst_decl decl (l,e)) pgm in
      (* Print.print_pgm pgm'; *)
      Eval.is_solution pgm' testcases
    )  (BatSet.to_list repair_cand)
    in
    match repair with
    | Some (l,e) ->
        let pgm' = List.map (fun decl -> subst_decl decl (l,e)) pgm in
        Print.print_header "Repair result"; Print.print_pgm pgm';
        print_endline ("Time : " ^ string_of_float (Unix.gettimeofday() -. start_time));
        Some pgm'
    | None -> print_endline ("Fail to Repair"); None
>>>>>>> 71e92c8fdd38858c4a5a6c7bcdacf03224a2c523
