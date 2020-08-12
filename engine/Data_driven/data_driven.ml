open Lang
open Util
open Print
open CallGraph
open Selector
open Repair_template
open Extractor

(*
	Input : An incorrect program pgm, a set of correct programs cpgms, and a set of testcases testcases
	Output : A repaired program pgm' satisfying all testcases
*)

(* Preprocess given submission *)
let save_data ?(logging_flag=false) : prog -> string -> unit
= fun pgm path ->
	(* Extract call graph *)
  let (r_env, renamed_pgm) = Preprocessor.Renaming.run (Preprocessor.run pgm) in
  let cg = CallGraph.extract_graph renamed_pgm in
	(* Save preprocessed data file *)
	let data_path = path ^ ".marshalled" in
	let data_oc = open_out data_path in
	Marshal.to_channel data_oc cg [];
	close_out data_oc;
	(* Save resulting log*)
	if logging_flag then 
		let log_path = path ^ ".txt" in
  	let log_oc = open_out log_path in
  	let nodes = BatMap.foldi (fun f (id, args, typ, body) acc->
	    let f = Preprocessor.Renaming.apply_env f r_env in
	    let args = List.map (Preprocessor.Renaming.apply_arg r_env) args in
	    let body = Preprocessor.Renaming.apply_exp r_env body in
	    BatMap.add f (id, args, typ, body) acc 
	  ) (get_nodes cg) BatMap.empty in
	  let edges = BatMap.map (fun ctxs ->
	   	BatSet.map (fun (l, path) -> (l, Preprocessor.Renaming.apply_path r_env path)) ctxs
	  ) (get_edges cg) in
	  let log_str = 
		  "Node : \n" ^ string_of_nodes nodes ^
		  "\nEdge : \n" ^ string_of_edges edges 
		in
		Printf.fprintf log_oc "%s" log_str;
  	close_out log_oc

let load_data : string -> graph
= fun file_path ->
	let data_path = "../preprocessed_data" ^ (Preproc.get_file_path file_path) ^ ".marshalled" in
  let data_in = open_in data_path in
  let cg = Marshal.from_channel data_in in
  close_in data_in;
  cg

let run2 : prog -> (string * prog) list -> examples -> prog
= fun pgm cpgms testcases -> 
	(* Preprocessing *)
	library_pgm := Preprocessor.run (!library_pgm);
	print_header "Preprocessing";
	(* print_pgm2 pgm; *)
	let preprocessing_time = Unix.gettimeofday () in
	let pgm = Preprocessor.run pgm in 
	let (r_env, pgm) = Preprocessor.Renaming.run pgm in
	let cpgms = List.map (fun (f_name, cpgm) -> 
		(* print_header ("Preprocessing : " ^ f_name); *)
		(* print_pgm2 pgm; *)
		(*
		let cpgm = Preprocessor.run cpgm in
		let cpgm = snd (Preprocessor.Renaming.run cpgm) in
		(f_name, CallGraph.extract_graph cpgm)
		*)
		(f_name, load_data f_name)
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
	let pgm = Preprocessor.Renaming.apply_pgm r_env pgm in
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