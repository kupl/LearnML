open Lang
open Util
open Preproc
open Print
open Normalize
open CallGraph
open Selector
open Repair_template
open Extractor
(*
	Input : An incorrect program pgm, a set of correct programs cpgms, and a set of testcases testcases
	Output : A repaired program pgm' satisfying all testcases
*)
let start_time = ref 0.0

(* Preprocess given submission *)
let check_and_create_dir : string -> bool 
= fun path ->
	if Sys.file_exists path && Sys.is_directory path then true
	else (
		print_endline (path ^ " does not exist, create a directory");
		Unix.mkdir path 0o755;
		false )

let rec save_data ?(logging_flag=false) : (string * prog) -> unit
= fun (f_name, pgm) -> 
	let model_path = "../models" in
	if check_and_create_dir model_path then
		let problem_name = List.hd (List.tl (List.rev (Str.split (Str.regexp "/+") f_name))) in 
		let problem_path = model_path ^ "/" ^ problem_name in
		if check_and_create_dir problem_path then
			(* Extract call graph from given program *)
			let _ = 
				Preprocessor.Renaming.flag := true;
				library_pgm := Preprocessor.Type_annotate.run !library_pgm
					|> Preprocessor.Decapsulation.run
			in
			let (r_env, renamed_pgm) = Preprocessor.run (append_grading pgm) in
			let references = get_references f_name (extract_graph (append_library renamed_pgm)) in
			(* Save modeled reference *)
			let file_name = 
				List.hd (List.rev (Str.split (Str.regexp "/+") f_name)) 
				|> Str.replace_first (Str.regexp ".ml$") ""
			in
			let target_path = problem_path ^ "/" ^ file_name ^ ".marshalled" in
			let data_oc = open_out target_path in
			Marshal.to_channel data_oc references [];
			close_out data_oc;
		else save_data (f_name, pgm)
	else save_data (f_name, pgm)

let rec load_data : (string * prog) -> references
= fun (f_name, pgm) ->
	let problem_name = List.hd (List.tl (List.rev (Str.split (Str.regexp "/+") f_name))) in 
	let file_name = 
		List.hd (List.rev (Str.split (Str.regexp "/+") f_name)) 
		|> Str.replace_first (Str.regexp ".ml$") ""
	in
	let data_path = "../models/" ^ problem_name ^ "/" ^ file_name ^ ".marshalled" in
	if Sys.file_exists data_path then
		let data_in = open_in data_path in
		let reference = Marshal.from_channel data_in in
		close_in data_in;
		reference
	else 
		(* If the solution file is not preprocessed then conduct preprocessing & reload *)
		(save_data (f_name, pgm);
		load_data (f_name, pgm))

(* Removing redundant references *)
let rec remove_redundant_refs : references list -> references list
= fun refs ->
	match refs with
	| [] -> []
	| hd::tl -> 
		let uniq_list = List.filter (fun reference -> not (compare_refs hd reference)) tl in
		hd::(remove_redundant_refs uniq_list)

(* Main algorithm *)
let run : (string * prog) -> (string * prog) list -> examples -> unit
= fun (f_sub, pgm) cpgms testcases -> 
	(* Load preprocessed references *)
	let (r_env, pgm) = Preprocessor.run (append_grading pgm) in
	let references = List.map load_data cpgms in
	let uniq_refs = remove_redundant_refs references in
	(* Find mapping according to given matching algorithm option *)
	let matching = 
		if Options.(!opt_fix) then select_solutions pgm uniq_refs (* Function-level context-aware matching *)
		else if Options.(!opt_func) then select_solutions_syn pgm uniq_refs (* Function-level SARFGEN *)
		else if Options.(!opt_prog) then select_solution_syn pgm uniq_refs (* Program-level SARFGEN *) 
		else raise (Failure "Fail to run data-driven feedback generator")
	in
	(* Extracting repair templates *)
	let (repair_templates, call_temps) = Extractor.extract_templates matching in
	let repair_templates = 
		BatSet.filter (fun (temp, source) -> not (check_redundant_template pgm temp)) repair_templates 
		|> BatSet.map (fun (temp, source) -> (Preprocessor.Renaming.apply_template r_env temp, source))
	in
	let call_temps = BatMap.foldi (fun x es acc -> 
		BatMap.add (Preprocessor.Renaming.apply_env x r_env) (BatSet.map (Preprocessor.Renaming.apply_exp r_env) es) acc
	) call_temps BatMap.empty in
	(* Patch generation *)
	let pgm = Preprocessor.Renaming.apply_pgm r_env (remove_grading pgm) in
	let _ = 
		start_time := Unix.gettimeofday();
		Print.print_header ("Submission (" ^ f_sub ^ ")"); Print.print_pgm pgm;
	in
	match Repairer.run pgm call_temps repair_templates testcases with
	| Some (pgm', cand) -> 
		let removed_labels = BatSet.fold (fun temp acc -> 
			match temp with 
			| ModifyExp (l, _)| DeleteBranch (l, _) -> BatSet.add l acc
			| InsertBranch _ | InsertFunction _ -> acc) (BatSet.map fst cand) BatSet.empty
		in
		let removed_exps_size = BatSet.fold (fun l acc ->
			match get_sub pgm l with
			| Some e -> acc + (exp_size e)
			| None -> acc 
		) removed_labels 0
		in
		let tool_name = 
			if Options.(!opt_fix) then "CAFE"
			else if Options.(!opt_func) then "Function-level SARFGEN"
			else if Options.(!opt_prog) then "Program-level SARFGEN"
			else raise (Failure "Invalid matching algorithm")
		in
		print_header ("Generated patch by " ^ tool_name); print_pgm pgm'; 
		print_header "Results";
		print_endline ("Size of removed expressions : " ^ string_of_int removed_exps_size);
		print_endline ("Size of applied templates : " ^ string_of_int (size_of_templates cand));
		print_endline ("Time elappsed  : " ^ string_of_float (Unix.gettimeofday() -. !start_time))
	| None -> print_endline "CAFE fails to generate a patch"