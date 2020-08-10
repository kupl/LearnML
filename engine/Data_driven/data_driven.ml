open Lang
open Util
open Print
open Selector
open Repair_template
open Extractor

(*
	Input : An incorrect program pgm, a set of correct programs cpgms, and a set of testcases testcases
	Output : A repaired program pgm' satisfying all testcases
*)
let run2 : prog -> (string * prog) list -> examples -> prog
= fun pgm cpgms testcases -> 
	(* Preprocessing *)
	library_pgm := Preprocessor.run (!library_pgm);
	print_header "Preprocessing";
	(* print_pgm2 pgm; *)
	let preprocessing_time = Unix.gettimeofday () in
	let pgm = Preprocessor.run pgm in 
	let (r_env, pgm) = Preprocessor.Renaming.run pgm in
	let r_env = r_env in
	let cpgms = List.map (fun (f_name, cpgm) -> 
		(* print_header ("Preprocessing : " ^ f_name); *)
		(* print_pgm2 pgm; *)
		let cpgm = Preprocessor.run cpgm in
		let cpgm = snd (Preprocessor.Renaming.run cpgm) in
		(f_name, CallGraph.extract_graph cpgm)
	) cpgms in
	let preprocessing_time = Unix.gettimeofday () -. preprocessing_time in
	(* Selection *)
	print_header "Selecting";
	let select_time = Unix.gettimeofday () in
	let (select_result, call_temps) = select_solutions2 pgm cpgms in
	let call_temps = BatMap.foldi (fun x call_temp acc -> 
		let x = Preprocessor.Renaming.apply_env x r_env in
		let call_temp = BatSet.map (fun e -> Preprocessor.Renaming.apply_exp r_env e) call_temp in
		BatMap.add x call_temp acc
	) call_temps BatMap.empty in
	let select_time = Unix.gettimeofday () -. select_time in
	let _ =
		Print.print_header "Selection Result";
		print_endline (string_of_matching2 select_result)
	in
	let _ =
		Print.print_header "Call-templates";
		print_endline (string_of_map (id) (string_of_set exp_to_string) call_temps)
	in
	let select_result = BatMap.map (fun (f_name, summary, callees) -> (summary, callees)) select_result in
	(* Template extraction *)
	let repair_templates = Extractor.extract_templates select_result in
	let repair_templates = BatSet.map (fun (e_temp, d_temp) ->
		let e_temp = 
			match e_temp with
			| ModifyExp (l, e) -> ModifyExp (l, Preprocessor.Renaming.apply_exp r_env e)
			| InsertBranch (l, (p, e)) -> InsertBranch (l, (p, Preprocessor.Renaming.apply_exp r_env e))
			| _ -> e_temp
		in
		(e_temp, d_temp)
	) repair_templates in
	let _ =
		Print.print_header "Extracted Templates";
		print_endline (string_of_set string_of_template repair_templates)
	in
	(* Patch generation *)
	let pgm = Preprocessor.Renaming.apply r_env pgm in
	let repair_templates = BatSet.filter (fun temp -> not (check_redundant_template pgm temp)) repair_templates in
	let _ =
		Print.print_header "Filtered Templates";
		print_endline (string_of_set string_of_template repair_templates)
	in
	print_pgm pgm;
	match Repairer.run pgm call_temps repair_templates testcases with
	| Some pgm' -> 
		let repair_time = Unix.gettimeofday() -. !(Repairer.start_time) in
		print_header "Result"; 
		print_pgm pgm'; 
		print_endline ("Preprocessing Time : " ^ string_of_float preprocessing_time); 
		print_endline ("Selection Time : " ^ string_of_float select_time); 
		print_endline ("Repair Time : " ^ string_of_float repair_time);
		pgm'
	| None -> print_endline "Fail to repair"; pgm