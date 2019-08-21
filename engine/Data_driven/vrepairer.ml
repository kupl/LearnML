open Lang
open Repairer

let get_repair_candidate : prog -> prog -> (string*string) list -> (label * exp) list
= fun pgm cpgm map ->
  let pgm_fmap = Extractor.extract_func_all pgm in
  let cpgm_fmap = Extractor.extract_func_all cpgm in 
  List.fold_left (fun acc (x,y) -> let f = List.assoc x pgm_fmap in 
                    let f'= List.assoc y cpgm_fmap in
                    let repair_cand = Repairer.get_repair_candidate_exp f f' in
                    repair_cand @ acc
                 ) [] map 

let run : prog -> prog -> (string*string) list -> examples -> prog option
= fun pgm cpgm map testcases ->
    let start_time = Unix.gettimeofday () in
    let repair_cand = get_repair_candidate pgm cpgm map in
    let repair_cand = update_var_comp pgm repair_cand in

    print_endline ("Size of repair Cand : " ^ string_of_int (BatSet.cardinal repair_cand));
    let repair = List.find_opt (fun (l,e) ->
        let pgm' = List.map(fun decl -> subst_decl decl (l,e)) pgm in
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

