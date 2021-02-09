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
    print_endline ("Total time :" ^ string_of_float (Sys.time() -. !Synthesize.start_time));
    begin match TestGenerator.gen_counter_example pgm' cpgm with
    | Some _ -> print_endline "Overfitted patch";
    | None -> print_endline "Success to repair";
    end 

let generate_testcases : prog -> prog -> examples
= fun submission solution -> 
  (* type checking *)
  let _ = is_same_type submission solution in
  let test_gen_result = TestGenerator.gen_counter_example submission solution in
  let test_time = Unix.gettimeofday() -. !(TestGenerator.start_time) in 
  match test_gen_result with
  | None -> 
    (* Test-case generation fail *)
    let _ = print_endline ("Correct Code") in
    [] 
  | Some ex ->
    let examples = ex::[] in
    let _ =
      print_header "original"; print_pgm submission;
      print_header "Generated examples"; print_examples examples;
      print_endline ("Counter-example Time : " ^ string_of_float test_time);
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
    let test_gen_result = TestGenerator.gen_counter_example candidate cpgm in
    let test_time = Unix.gettimeofday() -. !(TestGenerator.start_time) in 
    match test_gen_result with
    | None -> (Some candidate, examples) (* Test-case generation fail *)
    | Some ex ->
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
        let _ = 
          print_header "Generated examples"; print_examples examples;
          print_header "Repair candidate"; print_pgm candidate;
        in
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
  let (tenv,_,_,_) = Type2.run prog in
  let (env, mem) = Eval.run prog in
  (print_REPL prog tenv env mem)

let main () = 
  (* Arg Parse *)
  let _ = Arg.parse options (fun s->()) usage_msg in
  let testcases = read_testcases !opt_testcases_filename in
  let solution = read_prog !opt_solution_filename in
  let solutions = read_pgms !opt_solution_dirname in
  let solutions_debug = read_pgms_debug !opt_solution_dirname in
  let submission = read_prog !opt_submission_filename in
  let _ = 
    library_pgm := read_external !opt_external_filename;
    grading_pgm := read_external !opt_grading_filename
  in
  (* Main Procedure *)
  if !opt_run then (* Run testcase *)
    begin
      match submission with
      | Some sub -> 
        print_header ("Submission"); print_pgm sub; run_prog sub testcases
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
      | Some sub -> print_header ("Submission"); print_pgm sub; execute sub
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
      match submission, solutions_debug, solution with
      | Some sub, sols, Some sol -> ignore (Data_driven.run2 sub sol (List.map fst sols) [])
      (*
      | Some sub, sols, Some sol -> 
        (*
        begin match Data_driven.run sub (List.map fst sols) testcases with
        | Some pgm' ->
          begin match TestGenerator.gen_counter_example pgm' sol with
          | Some _ -> print_endline "Overfitted patch";
          | None -> print_endline "Success to repair";
          end 
        | None -> print_endline "Fail to repair";
        end
        *)
      *)
      | Some sub, sols, None -> ignore (Data_driven.run sub (List.map fst sols) testcases)
      | _ -> raise (Failure "Submission or solutions are not provided")
    end
  (*
  else if !opt_cfg then
    begin 
      match submission, solutions_debug with
      | Some sub, cpgms -> 
        let cfg = Cfg.extract_cfg sub in
        let all_matchings = List.fold_left (fun acc (file, cpgm) -> 
          let cfg' = Cfg.extract_cfg cpgm in
          match Cfg.match_t cfg cfg' with
          | None -> acc
          | Some matching -> (file, matching)::acc
        ) [] cpgms
        in
        if List.length all_matchings = 0 then (print_endline "X") else (print_endline "O")
      | _ -> raise (Failure "Submission or solutions are not provided")
    end
  *)
  else if !opt_preproc then
    begin match submission with
    | Some sub -> 
      let file_name = get_file_path !opt_submission_filename in
      let save_path = "../preprocessed_data" ^ file_name in
      Data_driven.save_data ~logging_flag:true sub save_path;
      let cg = Data_driven.load_data !opt_submission_filename in
      ()
    | _ -> raise (Failure "Submission file is not provided")
    end
  else
    Arg.usage options usage_msg 

let _ = main ()