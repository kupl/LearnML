open Lang
open Print
open Options

exception Arg_exception

let usage_msg = "main.native -run (or -fix) -submission <filename> -solution <filename>"

let parse_file (f:string) : (examples * prog) =
  Preproc.preprocess_file f
    |> Lexing.from_string
    |> Parser.prog Lexer.token

let run_testcases : prog -> examples -> unit
=fun prog examples ->
  List.iter (fun (exps, value) ->
    let res_var = "__res__" in
    let prog' = prog @ [(DLet (res_var,false,[],TPoly,(Lang.appify (EVar !opt_entry_func) exps)))] in
		let env = Eval.run prog' false in
		let result_value = Lang.lookup_env res_var env in
      print_endline ("Result: " ^ Print.string_of_value result_value ^ " " ^  
                     "Expected: " ^ Print.string_of_value value);
  ) examples 

let run_prog : prog -> examples -> unit
=fun prog examples ->
  print_header "Program"; Print.print_pgm prog;
  print_header "Test-cases"; print_examples examples;
  print_header "Run test-cases"; run_testcases prog examples 

let fix_with_solution : prog -> prog -> examples -> unit
=fun submission solution examples ->  (* TODO *)
  print_header "Solution"; Print.print_pgm solution;
  print_header "Submission"; Print.print_pgm submission;
  print_header "Test-cases"; print_examples examples;
  let ranked_prog_set = Localize.localization submission examples in
  let initial_set = BatSet.map
   (
      fun (n,prog)->
        let (entry_input,_) = List.hd examples in
        let entry_exp = Lang.appify (EVar !opt_entry_func) entry_input in
        let pgm = prog@[DLet ("@",false,[],TPoly,entry_exp)] in
        let _ = Type.run pgm in
        let _ = Extract_from_error.run prog in
        let pgm_hole_table = !Type.hole_tbl in
        let pgm_at_hole_table = !Type.at_hole_ttbl in
        let pgm_hole_var_table = !(Extract_from_error.var_table) in
        let _ = print_endline("------------------------") in
        let _ = Print.print_pgm prog in
        let _ = print_endline (BatMap.fold (fun v_set s-> BatSet.fold (fun v s-> v^" "^s) v_set s) pgm_hole_var_table "") in
        let _ = print_endline (BatMap.fold (fun t s-> (Type.string_of_type t)^s) pgm_hole_table "") in
        let _ = print_endline (BatMap.fold (fun env s -> BatMap.foldi (fun x t r-> r^ x ^ "|->" ^ (Type.string_of_type t) ^ " ") env "") pgm_at_hole_table "") in
        let _ = print_endline("------------------------") in
        (n,prog,pgm_hole_var_table,pgm_hole_table,pgm_at_hole_table)
    ) ranked_prog_set in
  let components_from_correct = Extract.extract_component solution in
  let correct_program = Synthesize.hole_synthesize submission initial_set components_from_correct examples in
  ()
 
let fix_without_solution : prog -> examples -> unit
=fun submission examples -> () (* TODO *)


let main () = 
  let _ = Arg.parse options (fun s->()) usage_msg in
  let testcases = 
    if !opt_testcases_filename = "" then [] 
    else try fst (parse_file !opt_testcases_filename) 
         with _ -> raise (Failure ("error during parsing testcases: " ^ !opt_testcases_filename)) in 
  let submission = try Some (snd (parse_file !opt_submission_filename)) with _ -> None in
  let solution= try Some (snd (parse_file !opt_solution_filename)) with _ -> None in
    match !opt_run, !opt_fix with
    | true, false -> (* execution mode *)
      begin
        match submission with
        | Some sub -> run_prog sub testcases
        | _ -> raise (Failure (!opt_submission_filename ^ " does not exist"))
      end
    | false, true -> (* fix mode *)
      begin
        match submission, solution with
        | Some sub, Some sol -> fix_with_solution sub sol testcases 
        | Some sub, None -> fix_without_solution sub testcases 
        | _ -> raise (Failure (!opt_submission_filename ^ " does not exist"))
      end
    | _ -> Arg.usage options usage_msg

let _ = main ()
