open Lang
open Print
open Options
open Preproc

(* Framework for learning functional programming *)
let usage_msg = "main.native -fix (or -gen_test) -submission <filename> -solution <filename> (or -solutions <directory>) -testcases <testcase> -entry <entry>"

let is_same_type : prog -> prog -> unit
= fun pgm cpgm ->
  try 
    let (tenv1, _, _, _) = Type.run pgm in
    let (tenv2, _, _, _) = Type.run cpgm in
    let (t1, t2) = (Type.TEnv.find tenv1 !opt_entry_func, Type.TEnv.find tenv2 !opt_entry_func) in
    let _ = Type.unify Type.Subst.empty (t1, t2) in
    ()
  with _ -> raise (Failure "The types of submission and solution are mismatched")

(* Run testcases *)
let except_handling : exn -> value -> unit
= fun except output ->
  begin match except with
  |EExcept v ->
    print_endline("Result : " ^ string_of_value v ^ " " ^
                  "Expected: " ^ string_of_value output);
  |TimeoutError ->
    print_endline("Result : Timeout" ^ " " ^
                  "Expected: " ^ string_of_value output);
  |Failure s ->
    print_endline("Result : Error "^ s ^ " " ^
                  "Expected: " ^ string_of_value output);
  |EqualError -> 
    print_endline("Result : Equal Error" ^
                  "Expected: " ^ string_of_value output);
  |_ ->
     print_endline("Result : Evaluation Error "^
                  "Expected: " ^ string_of_value output);
  end

let run_testcases : prog -> examples -> unit
= fun prog examples ->
  let score = List.fold_left (fun score (inputs, output) ->
    try
      let result_value = Eval.get_output prog inputs in
        print_endline ("Result: " ^ string_of_value result_value ^ " " ^  
                      "Expected: " ^ string_of_value output);
      if try (Eval.value_equality result_value output) with _ -> false then score+1 else score
    with 
      | EExcept v -> 
        print_endline ("Result: " ^ string_of_value v ^ " " ^  
                      "Expected: " ^ string_of_value output);
        if try (Eval.value_equality v output) with _ -> false then score+1 else score
      | except -> except_handling except output; score
  ) 0 examples in
  print_endline("score : "^(string_of_int score))

let run_prog : prog -> examples -> unit
=fun prog examples ->
  let _ = Type.run prog in
  print_header "Program"; print_pgm prog;
  print_header "Test-cases"; print_examples examples;
  print_header "Run test-cases"; run_testcases prog examples 

(* FixML *)
let fix_with_solution : (string * prog) -> (string * prog) -> examples -> unit
= fun (f_sub, pgm) (f_sol, cpgm) examples ->
  let _ = is_same_type pgm cpgm in
  Print.print_header ("Submission (" ^ f_sub ^ ")"); Print.print_pgm pgm;
  (* Localize *)
  let initial_set =
    Localize.localization pgm examples |> 
    BatSet.map (fun (n, prog) ->
      let (_,hole_type,variable_type, _) = Type.run prog in
      (n,prog,hole_type,variable_type)
    ) 
  in
  (* Component Extraction *)
  let components = Comp.extract_component cpgm in
  (* Patch Generation *)
  match Synthesize.hole_synthesize pgm initial_set components examples with
  | None -> 
		print_header ("Generated patch by FixML"); print_endline ("None"); 
		print_header "Results"; 
		print_endline ("FixML fails to generate a patch");
		print_endline ("Time elappsed :" ^ string_of_float (Sys.time() -. !Synthesize.start_time))
  | Some pgm' ->
    Print.print_header "Generated patch by FixML"; Print.print_pgm pgm';
    Print.print_header "Result"; print_endline ("Time elappsed :" ^ string_of_float (Sys.time() -. !Synthesize.start_time))

(* TestML *)
let generate_testcases : (string * prog) -> (string * prog) -> examples
= fun (f_sub, pgm) (f_sol, cpgm) -> 
  (* type checking *)
  let _ = is_same_type pgm cpgm in
  let test_gen_result = TestGenerator.gen_counter_example pgm cpgm in
  let test_time = Unix.gettimeofday() -. !(TestGenerator.start_time) in 
  match test_gen_result with
  | None -> 
    (* Test-case generation fail *)
    print_endline ("TestML fails to find a counter-example of \'" ^ f_sub ^ "\'"); [] 
  | Some ex ->
    print_header ("Submission (" ^ f_sub ^ ")"); print_pgm pgm;
    print_header "Generated counter-example"; print_examples [ex];
    print_endline ("Time elappsed  : " ^ string_of_float test_time);
    [ex]

let main () = 
  (* Arg Parse *)
  let _ = Arg.parse options (fun s->()) usage_msg in
  let testcases = read_testcases !opt_testcases_filename in
  let solution = read_pgm !opt_solution_filename in
  let solutions = read_pgms !opt_solution_dirname in
  let submission = read_pgm !opt_submission_filename in
  let _ = 
    library_pgm := read_external !opt_external_filename;
    grading_pgm := read_external !opt_grading_filename
  in
  (* Main Procedure *)
  if !opt_run then (* Run testcase *)
    begin
      match submission with
      | Some (f_sub, sub) -> print_header ("Submission (" ^ f_sub ^ ")"); print_pgm sub; run_prog sub testcases
      | _ -> raise (Failure (!opt_submission_filename ^ " does not exist"))
    end
  else if !opt_fix then (* FixML or CAFE *)
    begin
      match submission, solution, solutions with
      | None, _, _ -> raise (Failure (!opt_submission_filename ^ " does not exist"))
      | Some sub, Some sol, [] -> fix_with_solution sub sol testcases
      | Some sub, _, (hd::tl as sols) -> Data_driven.run sub sols testcases
      | _ -> raise (Failure "Submission or solution files are not provided")
    end
  else if !opt_prog || !opt_func then (* SARFGEN (modeled) *)
    begin
      match submission, solutions with
      | Some sub, (hd::tl as sols) -> Data_driven.run sub sols testcases
      | _ -> raise (Failure "Submission or solution files are not provided")
    end
  else if !opt_gentest then (* TestML *)
    begin
      match submission, solution with
      | Some sub, Some sol -> ignore (generate_testcases sub sol)
      | _ -> raise (Failure "Submission or solution is not provided")
    end
  else if !opt_preproc then (* Data preprocessing for running CAFE *)
    begin match submission with
    | Some (f_sub, sub) -> Data_driven.save_data ~logging_flag:false (f_sub, sub) 
    | _ -> raise (Failure "Submission file is not provided")
    end
  else
    Arg.usage options usage_msg 

let _ = main ()