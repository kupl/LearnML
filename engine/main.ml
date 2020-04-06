open Lang
open Print
open Options
open Preproc

(* Framework for learning functional programming *)
let usage_msg = "main.native -run (or -fix) -submission <filename> -solution <filename>"

let is_same_type : prog -> prog -> unit
= fun pgm cpgm ->
  let (tenv1, _, _, _) = Type.run pgm in
  let (tenv2, _, _, _) = Type.run cpgm in
  let (t1, t2) = (Type.TEnv.find tenv1 !opt_entry_func, Type.TEnv.find tenv2 !opt_entry_func) in
  let _ = Type.unify Type.Subst.empty (t1, t2) in
  ()

(* Run testcases *)
let except_handling : exn -> value -> unit
= fun except output ->
  begin match except with
  |EExcept v ->
    print_endline("Result : " ^ value_to_string v ^ " " ^
                  "Expected: " ^ value_to_string output);
  |TimeoutError ->
    print_endline("Result : Timeout" ^ " " ^
                  "Expected: " ^ value_to_string output);
  |Failure s ->
    print_endline("Result : Error "^ s ^ " " ^
                  "Expected: " ^ value_to_string output);
  |EqualError -> 
    print_endline("Result : Equal Error" ^
                  "Expected: " ^ value_to_string output);
  |_ ->
     print_endline("Result : Evaluation Error "^
                  "Expected: " ^ value_to_string output);
  end

let run_testcases : prog -> examples -> unit
= fun prog examples ->
  let score = List.fold_left (fun score (inputs, output) ->
    try
      let result_value = Eval.get_output prog inputs in
        print_endline ("Result: " ^ value_to_string result_value ^ " " ^  
                     "Expected: " ^ value_to_string output);
      if try (Eval.value_equality result_value output) with _ -> false then score+1 else score
    with except -> except_handling except output; score
  ) 0 examples in
  print_endline("score : "^(string_of_int score))

let run_prog : prog -> examples -> unit
=fun prog examples ->
  let _ = Type.run prog in
  print_header "Program"; print_pgm prog;
  print_header "Test-cases"; print_examples examples;
  print_header "Run test-cases"; run_testcases prog examples 

let fix_with_solution : prog -> prog -> examples -> unit
= fun pgm cpgm examples ->
  let _ = is_same_type pgm cpgm in
  let _ = if Eval.is_solution pgm examples then raise (Failure "The submission is correct code") in
  let _ = Print.print_header "original" ; Print.print_pgm pgm in
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
  | None -> print_endline ("FixML fails to generate a patch")
  | Some pgm' ->
    Print.print_header "result"; Print.print_pgm pgm';
    print_endline ("Total time :" ^ string_of_float (Sys.time() -. !Synthesize.start_time))

let generate_testcases : prog -> prog -> examples
= fun submission solution -> 
  (* type checking *)
  let _ = is_same_type submission solution in
  let test_gen_result = TestGenerator.gen_counter_example submission solution in
  let test_time = Unix.gettimeofday() -. !(TestGenerator.start_time) in 
  match test_gen_result with
  | None -> 
    (* Test-case generation fail *)
    let _ = 
      print_endline ("Correct Code");
      print_endline ("Num of Inputs : " ^ string_of_int (!TestGenerator.count));
      print_endline ("Num of Crashes : " ^ string_of_int (!TestGenerator.num_of_crash))
    in
    [] 
  | Some ex ->
    let examples = ex::[] in
    let _ =
      print_header "original"; print_pgm submission;
      print_header "Generated examples"; print_examples examples;
      print_endline ("Counter-example Time : " ^ string_of_float test_time);
      print_endline ("Num of Inputs : " ^ string_of_int (!TestGenerator.count));
      print_endline ("Num of Crashes : " ^ string_of_int (!TestGenerator.num_of_crash))
    in
    [ex]

let fix_without_testcases : prog -> prog -> unit
= fun submission solution ->
  (* type checking *)
  let _ = is_same_type submission solution in
  (* main procedure *)
  let task_time = ref 0.0 in
  let components = Comp.extract_component solution in 
  (* iteration *)
  let rec iter : prog -> prog -> prog -> examples -> (prog option * examples)
  = fun pgm cpgm candidate examples ->
    let _ = 
      print_header "Generated examples"; print_examples examples;
      print_header "Repair candidate"; print_pgm candidate;
    in
    let test_gen_result = TestGenerator.gen_counter_example candidate cpgm in
    let test_time = Unix.gettimeofday() -. !(TestGenerator.start_time) in 
    match test_gen_result with
    | None -> (Some candidate, examples) (* Test-case generation fail *)
    | Some ex ->
      let _ = print_endline ("Counter-example Time : " ^ string_of_float test_time) in
      let examples = ex::examples in
      let ranked_pgm_set = Localize.localization pgm examples in
      let initial_set = BatSet.map(
        fun (rank, pgm)->   
          let (_, h_t, v_t, _) = Type.run pgm in
          (rank, pgm, h_t, v_t)
      ) ranked_pgm_set in
      let correction_result = Synthesize.hole_synthesize pgm initial_set components examples in
      let correction_time = Unix.gettimeofday() -. !(Synthesize.start_time) in 
      task_time := correction_time +. test_time +. !task_time;
      match correction_result with
      | None -> (None, examples) (* Repair fail *)
      | Some pgm' -> 
        (*
        let _ = 
          print_header "Generated examples"; print_examples examples;
          print_header "Repair candidate"; print_pgm candidate;
        in
        *)
        iter pgm cpgm pgm' examples
  in
  match iter submission solution submission [] with
  | (None, examples) -> 
      print_header "original"; print_pgm submission;
      print_header "Generated examples"; print_examples examples;
      print_header "Fail to repair";
      print_endline ("Total Time : " ^ string_of_float !task_time)
  | (Some pgm, examples) -> 
    if (examples = []) then print_endline ("Correct Code")
    else (
      print_header "original"; print_pgm submission;
      print_header "Generated examples"; print_examples examples;
      print_header "Correction"; print_pgm pgm;
      print_endline ("Total Time : " ^ string_of_float !task_time)
    )

let execute : prog -> unit
= fun prog ->
  let (tenv,_,_,_) = Type.run prog in
  let env = Eval.run prog in
  (print_REPL prog tenv env)

let main () = 
  (* Arg Parse *)
  let _ = Arg.parse options (fun s->()) usage_msg in
  let testcases = read_testcases !opt_testcases_filename in
  let solution = read_prog !opt_solution_filename in
  let solutions = read_pgms !opt_solution_dirname in
  let solutions_debug = read_pgms_debug !opt_solution_dirname in
  let submission = read_prog !opt_submission_filename in
  let _ = 
    init_pgm := read_external !opt_external_filename;
    grading_pgm := read_external !opt_grading_filename
  in
  (* Main Procedure *)
  if !opt_run then (* Run testcase *)
    begin
      match submission with
      | Some sub -> run_prog sub testcases
      | _ -> raise (Failure (!opt_submission_filename ^ " does not exist"))
    end
  else if !opt_fix then (* FixML *)
    begin
      match submission, solution with
      | Some sub, Some sol ->
        begin match testcases with
        | [] -> fix_without_testcases sub sol
        | _ -> fix_with_solution sub sol testcases
        end      
      | _ -> raise (Failure (!opt_submission_filename ^ " does not exist"))
    end
  else if !opt_execute then (* Execute Program *)
    begin 
      match submission with
      | Some sub -> execute sub
      | _ -> raise (Failure "Submission file is not provided")
    end
  else if !opt_gentest then (* Counter Example Generation *)
    begin
      match submission, solution with
      | Some sub, Some sol -> ignore (generate_testcases sub sol)
      | _ -> raise (Failure "Submission or solution is not provided")
    end
  else if !opt_dd then (* Data-driven FixML *)
    begin 
      match submission, solutions_debug with
      | Some sub, sols -> 
        (* Searching a closest solution (matching rate high) *)
        let (score, (fname, sol)) = List.fold_left (fun (prev_score, prev_sol) (fname, sol) -> 
          let matching = Matching2.run sub sol in
          (* Matching2.print matching; *)
          let score = Matching2.get_matching_score matching in
          (* print_header ("Sol " ^ fname ^ " Score : " ^ string_of_int score); print_pgm2 sol; *)
          if prev_score < score then (score, (fname, sol)) else (prev_score, prev_sol)
        ) (-100, List.hd sols) sols in
        print_header ("Submission"); print_pgm2 sub;
        print_header ("Most Similar Sol " ^ fname ^ " Score : " ^ string_of_int score); print_pgm2 sol;
        (* Callgraph Matching *)
        let (g_sub, g_sol) = (CallGraph.run sub, CallGraph.run sol) in
        Print.print_header "Call graph (sub)"; CallGraph.print_graph g_sub;
        Print.print_header "Call graph (sol)"; CallGraph.print_graph g_sol;
        let matching = Matching2.run sub sol in
        Print.print_header "Matching Info"; Matching2.print matching;
        (* Template *)
        let temps = Extractor2.run sub sol in
        (* print_header ("Templates"); print_endline (string_of_set Repair_template.string_of_template temps); *)
        match Repairer2.run sub temps testcases with
        | Some patch -> 
          print_header "Patch"; print_pgm patch;
          print_endline ("Time : " ^ string_of_float (Unix.gettimeofday() -. !(Repairer2.start_time)))
        | None -> print_endline ("Fail to Repair")
      | _ -> raise (Failure "Submission or solutions are not provided")
    end
  else if !opt_cfg then
    begin 
      match submission, solutions_debug with
      | Some sub, cpgms -> 
        let cfg = Cfg.extract_cfg sub in
        Print.print_header "original"; Print.print_pgm sub;
        Print.print_header "CFG"; Cfg.print cfg;
        let all_matchings = List.fold_left (fun acc (file, cpgm) -> 
          let cfg' = Cfg.extract_cfg cpgm in
          Print.print_header ("Code of " ^ file); Print.print_pgm cpgm;
          Print.print_header ("CFG of " ^ file); Cfg.print cfg';
          match Cfg.match_t cfg cfg' with
          | None -> acc
          | Some matching -> (file, matching)::acc
        ) [] cpgms
        in
        print_endline ("# of matched solutions : " ^ string_of_int (List.length all_matchings));
        List.iter (fun (file, matching) -> 
          print_endline file
          (*
          print_endline ("\n Matchign relations : \n");
          Cfg.print_matching matching
          *)
        ) all_matchings;
        ()
      | _ -> raise (Failure "Submission or solutions are not provided")
    end
  else
    (* Arg.usage options usage_msg *)
    begin
      match submission, solution with
      | Some sub, Some sol ->
        let (g_sub, g_sol) = (CallGraph.run sub, CallGraph.run sol) in
        Print.print_header "Call graph (sub)"; CallGraph.print_graph g_sub;
        Print.print_header "Call graph (sol)"; CallGraph.print_graph g_sol;
        let matching = Matching2.run sub sol in
        Print.print_header "Matching Info"; Matching2.print matching;
        let (np_sub, np_sol) = (Normalizer.normalize_all sub, Normalizer.normalize_all sol) in
        Print.print_header "Norm (sub)"; Normalizer.print np_sub;
        Print.print_header "Norm (sol)"; Normalizer.print np_sol;
        ()
      | _ -> raise (Failure (!opt_submission_filename ^ " does not exist"))
    end

let _ = main ()