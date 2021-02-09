open Lang
open Util
open Print
open Type
open Normalize
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
	let _ = 
		Preprocessor.Renaming.flag := true;
		library_pgm := Preprocessor.Type_annotate.run !library_pgm
			|> Preprocessor.Decapsulation.run
	in
  let (r_env, renamed_pgm) = Preprocessor.run (append_grading pgm) in
  let references = get_references path (extract_graph (append_library renamed_pgm)) in
	(* Save preprocessed data file *)
	let data_path = path ^ ".marshalled" in
	let data_oc = open_out data_path in
	Marshal.to_channel data_oc references [];
	close_out data_oc;
	(* Save resulting log*)
	if logging_flag then 
		let log_path = path ^ ".txt" in
		let log_oc = open_out log_path in
	  let log_str = string_of_set ~first:"" ~last:"" ~sep:"\n" string_of_reference references in
		Printf.fprintf log_oc "%s" log_str;
  	close_out log_oc

let load_data : string -> references
= fun file_path ->
	let data_path = "../preprocessed_data" ^ (Preproc.get_file_path file_path) ^ ".marshalled" in
  let data_in = open_in data_path in
  let cg = Marshal.from_channel data_in in
  close_in data_in;
	cg

let rec compare_set : 'a BatSet.t -> 'a BatSet.t -> ('a -> 'a -> bool) -> bool
= fun set1 set2 comp -> 
	if BatSet.is_empty (BatSet.union set1 set2) then true
	else 
		let (elem1, set1) = BatSet.pop set1 in
		let matched = BatSet.filter (fun elem2 -> comp elem1 elem2) set2 in
		if BatSet.cardinal matched = 1 then compare_set set1 (BatSet.diff set2 matched) comp
		else false

let compare_node : node -> node -> bool
= fun node1 node2 -> (check_typs node1.typ node2.typ) && (normalize_exp node1.body = normalize_exp node2.body)

let compare_path : path -> path -> bool
= fun p1 p2 ->
	(BatSet.equal (BatSet.of_list (extract_clauses p1)) (BatSet.of_list (extract_clauses p2)))

let compare_ctx : calling_ctx -> calling_ctx -> bool 
= fun ctx1 ctx2 -> 
	(check_typs ctx1.caller_typ ctx2.caller_typ) &&
	(ctx1.caller_args = ctx2.caller_args) &&
	(compare_path ctx1.path ctx2.path) &&
	(check_typs ctx1.callee_typ ctx2.callee_typ) &&
	(ctx1.callee_args = ctx2.callee_args) 

let compare_ctxs : calling_ctx BatSet.t -> calling_ctx BatSet.t -> bool
= fun ctxs1 ctxs2 -> compare_set ctxs1 ctxs2 compare_ctx

let compare_summary : summary -> summary -> bool
= fun s1 s2 -> 
	(compare_node s1.node s2.node) && (compare_ctxs s1.incomming s2.incomming) && (compare_ctxs s1.outgoing s2.outgoing)

let compare_ref : reference -> reference -> bool
= fun ref1 ref2 ->	
	try
		(compare_summary ref1.summary ref2.summary) && (BatSet.equal ref1.usecase ref2.usecase)
	with _ -> (* print_endline ("Compare " ^ ref1.source ^ ", " ^ ref2.source); *) false

let rec compare_refs : references -> references -> bool
= fun refs1 refs2 -> compare_set refs1 refs2 compare_ref 

let rec remove_redundant_refs : references list -> references list
= fun refs ->
	match refs with
	| [] -> []
	| hd::tl -> 
		let uniq_list = List.filter (fun reference -> not (compare_refs hd reference)) tl in
		hd::(remove_redundant_refs uniq_list)

let rec choose_n_element : 'a list -> int -> 'a list 
= fun lst n ->
	if n = 0 then [] else 
	match lst with 
	| [] -> raise (Failure "Choose error")
	| _ -> 
		let _ = Random.self_init () in
		let selected = List.nth lst (Random.int (List.length lst)) in
		selected::(choose_n_element (list_remove1 selected lst) (n-1))

let rec choose_n_percent : 'a list -> float -> 'a list
= fun lst percent ->
	let number = int_of_float ((float_of_int (List.length lst)) *. percent) in 
	choose_n_element lst number

(* Main algorithm *)
let run : prog -> string list -> examples -> prog option 
= fun pgm cpgms testcases -> 
	print_endline ("Before Sampling : " ^ string_of_int (List.length cpgms));
	(* let cpgms = choose_n_percent cpgms 0.5 in *)
	print_endline ("After Sampling : " ^ string_of_int (List.length cpgms));
	(* Preprocessing *)
	let preprocessing_time = Unix.gettimeofday () in 
	let (r_env, pgm) = Preprocessor.run (append_grading pgm) in
	let references = List.map (fun f_name -> load_data f_name) cpgms in
	let uniq_refs = remove_redundant_refs references in
	let preprocessing_time = Unix.gettimeofday () -. preprocessing_time in
	(* Selection *)
	let select_time = Unix.gettimeofday () in
	let matching = select_solutions pgm uniq_refs in
	let select_time = Unix.gettimeofday () -. select_time in
	let _ =
		Print.print_header "Selection Result";
		print_endline (string_of_matching matching)
	in
	let (repair_templates, call_temps) = Extractor.extract_templates matching in
	(*
	let _ =
		Print.print_header "Extracted Templates";
		print_endline (string_of_templates repair_templates)
	in
	*)
	let repair_templates = BatSet.filter (fun temp -> not (check_redundant_template pgm temp)) repair_templates in
	(* Patch generation *)
	let repair_templates = BatSet.map (fun temp ->
		match temp with
		| ModifyExp (l, e) -> ModifyExp (l, Preprocessor.Renaming.apply_exp r_env e)
		| InsertBranch (l, (p, e)) -> InsertBranch (l, (Preprocessor.Renaming.apply_pat r_env p, Preprocessor.Renaming.apply_exp r_env e))
		| DeleteBranch (l, (p, e)) -> DeleteBranch (l, (Preprocessor.Renaming.apply_pat r_env p, Preprocessor.Renaming.apply_exp r_env e))
		| InsertFunction ((g, is_rec, args, typ, e), f) -> 
		  InsertFunction ((g, is_rec, args, typ, Preprocessor.Renaming.apply_exp r_env e), Preprocessor.Renaming.apply_env f r_env) 
	) repair_templates in
	let _ =
		Print.print_header "Extracted Templates";
		print_endline (string_of_templates repair_templates)
	in
	let call_temps = BatMap.foldi (fun x es acc -> 
		BatMap.add (Preprocessor.Renaming.apply_env x r_env) (BatSet.map (Preprocessor.Renaming.apply_exp r_env) es) acc
	) call_temps BatMap.empty in
	let _ =
		Print.print_header "Call-templates";
		print_endline (string_of_map (id) (string_of_set exp_to_string) call_temps)
	in
	let pgm = Preprocessor.Renaming.apply_pgm r_env (remove_grading pgm) in
	print_pgm pgm;
	match Repairer.run pgm call_temps repair_templates testcases with
	| Some pgm' -> 
		let repair_time = Unix.gettimeofday() -. !(Repairer.start_time) in
		print_header "Result"; 
		print_pgm pgm'; 
		print_endline ("Preprocessing Time : " ^ string_of_float preprocessing_time); 
		print_endline ("Selection Time : " ^ string_of_float select_time); 
		print_endline ("Repair Time : " ^ string_of_float repair_time);
		print_endline ("Total Time : " ^ string_of_float (preprocessing_time +. select_time +. repair_time));
		Some pgm'
	| None -> print_endline "Fail to repair"; None

(* Enhanced algorithm *)
let rec run2 : prog -> prog -> string list -> examples -> prog option 
= fun pgm sol cpgms testcases -> 
	(* Preprocessing *)
	let preprocessing_time = Unix.gettimeofday () in 
	let (r_env, pgm) = Preprocessor.run (append_grading pgm) in
	let references = List.map (fun f_name -> load_data f_name) cpgms in
	let uniq_refs = remove_redundant_refs references in
	let preprocessing_time = Unix.gettimeofday () -. preprocessing_time in
	(* Selection *)
	let select_time = Unix.gettimeofday () in
	let matching = select_solutions pgm uniq_refs in
	let select_time = Unix.gettimeofday () -. select_time in
	let (repair_templates, call_temps) = Extractor.extract_templates matching in
	let repair_templates = BatSet.filter (fun temp -> not (check_redundant_template pgm temp)) repair_templates in
	let repair_templates = BatSet.map (fun temp ->
		match temp with
		| ModifyExp (l, e) -> ModifyExp (l, Preprocessor.Renaming.apply_exp r_env e)
		| InsertBranch (l, (p, e)) -> InsertBranch (l, (Preprocessor.Renaming.apply_pat r_env p, Preprocessor.Renaming.apply_exp r_env e))
		| DeleteBranch (l, (p, e)) -> DeleteBranch (l, (Preprocessor.Renaming.apply_pat r_env p, Preprocessor.Renaming.apply_exp r_env e))
		| InsertFunction ((g, is_rec, args, typ, e), f) -> 
		  InsertFunction ((g, is_rec, args, typ, Preprocessor.Renaming.apply_exp r_env e), Preprocessor.Renaming.apply_env f r_env) 
	) repair_templates in
	let call_temps = BatMap.foldi (fun x es acc -> 
		BatMap.add (Preprocessor.Renaming.apply_env x r_env) (BatSet.map (Preprocessor.Renaming.apply_exp r_env) es) acc
	) call_temps BatMap.empty in
	let pgm = Preprocessor.Renaming.apply_pgm r_env (remove_grading pgm) in
	print_pgm pgm;
	(* Iterative loop *)
	let rec iter : prog -> call_templates -> repair_templates -> examples -> prog option
	= fun pgm' call_temps repair_templates testcases ->
		match TestGenerator.gen_counter_example pgm' sol with
		| Some t -> 
			let temp_time = Unix.gettimeofday () in
			begin match Repairer.run pgm call_temps repair_templates testcases with
			| Some pgm' -> 
				let _ = 
					print_header "Candidate"; 
					print_pgm pgm';
					print_endline ("Patch Time : " ^ string_of_float (Unix.gettimeofday() -. temp_time))
				in
				iter pgm' call_temps repair_templates (t::testcases)
			| None -> print_endline "Fail to repair"; None
			end
		| None -> 
			let repair_time = Unix.gettimeofday() -. !(Repairer.start_time) in
			print_header "Result"; 
			print_pgm pgm'; 
			print_endline ("Total Time : " ^ string_of_float (preprocessing_time +. select_time +. repair_time));
			print_endline ("# of iter : " ^ string_of_int (List.length testcases));
			Some pgm'
	in
	iter pgm call_temps repair_templates testcases