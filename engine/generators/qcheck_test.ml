open Lang
open Options 
open TestGenerator

(*
 ******************************************************
 	Code for quickcheck testing
 ******************************************************
*)

let start_time = ref 0.0
let test_count = ref 0

(*
	QuickCheck Input Generator
*)
module type Generator = sig	
	(* type of input *)
	type t  
	(* generate functinos  *)
	val to_string : t -> string
	val shrink : t ->  t QCheck.Iter.t 
	val gen : t QCheck.Gen.t 
end

(*
	Parametric Testing Using Given Generator
*)
module Make (G: Generator) = struct
	(*
		Input Manipulation
	*)
	let parse_testcase : string -> input
	= fun str_test ->
		(*let _ = print_endline ("PARSE : " ^ str_test) in*)
		try 
			let parsed_string = 
				str_test
				|> Lexing.from_string
	    	|> Parser.prog Lexer.token
	    in
			fst (List.hd (fst (parsed_string)))
		with _ -> raise (Failure ("error during parsing generated_testcases : " ^ str_test)) 

	let transform_input : 'a -> input
	= fun generated_input ->
		let input_string = "{" ^ G.to_string generated_input ^ " => [];}" in (* Parsing Template (input => dummy output;) *)
		parse_testcase input_string

	(*
		Main Procedure
	*)
	(*let log = ref (open_out "qcheck_input.txt")*)

	let rec run : prog -> prog -> example option
	= fun pgm cpgm ->
		let open QCheck.TestResult in
		let cell = QCheck.Test.make_cell ~count:2000000000 (QCheck.make G.gen (*~shrink:G.shrink*) )
		(fun generated_input -> 
			test_count := !test_count + 1;
			if Unix.gettimeofday() -. (!start_time) > 1800.0 then false (* Timeout *)
			else 
				let input = transform_input generated_input in
				(* let _ = Printf.fprintf (!log) "%s\n" ("Input : " ^ Print.input_to_string input) in *)
				try
					let v1 = get_output cpgm input in
					try
						let v2 = get_output pgm input in
						let t = Eval.value_equality v1 v2 in
						t
					with e -> print_endline (Printexc.to_string e);false 
				with _ -> true
		)
		in
		match (QCheck.Test.check_cell cell).state with
		| Failed instances -> 
			if (Unix.gettimeofday()) -. (!start_time) > 60.0 then None
			else
				let generated_input = (List.hd instances).instance in
				let input = transform_input generated_input in
				(*let _ = print_endline ("Input Candidate : " ^ Print.input_to_string input) in*)
				return_counter_example pgm cpgm input
		| _ -> run pgm cpgm
end

module Test_Max = Make (Max_generator)
module Test_Mirror = Make (Mirror_generator)
module Test_Mem = Make (Mem_generator)
module Test_Crazy2add = Make (Crazy2add_generator)
module Test_Formula1 = Make (Formula1_generator)
module Test_Formula2 = Make (Formula2_generator)
module Test_CheckMetro = Make (CheckMetro_generator)
module Test_Lambda = Make (Lambda_generator)
module Test_Diff1 = Make (Diff1_generator)
module Test_Diff2 = Make (Diff2_generator)

let run : prog -> prog -> unit
= fun pgm cpgm ->
	start_time := Unix.gettimeofday();
	let result = 
		match !opt_generator with
		| "max" -> Test_Max.run pgm cpgm
		| "mirror" -> Test_Mirror.run pgm cpgm 
		| "mem" -> Test_Mem.run pgm cpgm
		| "crazy2add" -> Test_Crazy2add.run pgm cpgm
		| "formula1" -> Test_Formula1.run pgm cpgm
		| "formula2" -> Test_Formula2.run pgm cpgm
		| "checkMetro" -> Test_CheckMetro.run pgm cpgm
		| "lambda" -> Test_Lambda.run pgm cpgm
		| "diff1" -> Test_Diff1.run pgm cpgm
		| "diff2" -> Test_Diff2.run pgm cpgm
		| _ -> raise (Failure "Invalid Generator")
	in
	match result with
	| Some ex ->
		Print.print_header "Submission"; Print.print_pgm pgm;
    Print.print_header "Generated examples"; Print.print_examples [ex];
		print_endline ("Counter-example Time : " ^ string_of_float (Unix.gettimeofday() -. (!start_time)));
	| None -> print_endline ("Fail to Generate Counter Example")