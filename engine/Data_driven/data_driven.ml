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
(*
let run : prog -> prog list -> examples -> prog
= fun pgm cpgms testcases -> 
	(* Preprocessing *)
	let pgm = Preprocessor.run pgm in 
	let cpgms = List.map (Preprocessor.run) cpgms in
	let _ =
		print_header "Submission"; print_pgm pgm;
		List.iter (fun cpgm ->
			print_header "Solution"; print_pgm cpgm
		) cpgms
	in
	(* Selection *)
	let select_result = select_solutions pgm cpgms in
	let _ =
		Print.print_header "Selection Result";
		print_endline (string_of_matching select_result)
	in
	pgm
*)

let run2 : prog -> (string * prog) list -> examples -> prog
= fun pgm cpgms testcases -> 
	(* Preprocessing *)
	library_pgm := Preprocessor.run (!library_pgm);
	let pgm = Preprocessor.run pgm in 
	let (r_env, pgm) = Preprocessor.Renaming.run pgm in
	print_header "Submission"; print_pgm2 pgm;
	let cpgms = List.map (fun (f_name, cpgm) -> 
		let cpgm = Preprocessor.run cpgm in
		let cpgm = snd (Preprocessor.Renaming.run cpgm) in
		(f_name, cpgm)
	) cpgms in
	let _ =
		List.iter (fun (f_name, cpgm) ->
			print_header ("Solution (" ^ f_name ^ ")"); print_pgm cpgm
		) cpgms
	in
	(* Selection *)
	let select_result = select_solutions2 pgm cpgms in
	let _ =
		Print.print_header "Selection Result";
		print_endline (string_of_matching2 select_result)
	in
	let select_result = BatMap.map (fun (f_name, summary) -> summary) select_result in
	let repair_templates = Extractor.extract_templates select_result in
	let _ =
		Print.print_header "Extracted Templates";
		print_endline (string_of_set string_of_template repair_templates)
	in 
	pgm