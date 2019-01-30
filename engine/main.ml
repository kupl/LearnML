open Lang
open Print
open Options

exception Arg_exception

let usage_msg = "main.native -run (or -fix) -submission <filename> -solution <filename>"

let parse_file (f:string) : (examples * prog) =
  Preproc.preprocess_file f
    |> Lexing.from_string
    |> Parser.prog Lexer.token

let program_with_grading prog = prog@(External.grading_prog)

let program_with_input prog inputs =
  let res_var = "__res__" in
  prog @ [(DLet (BindOne res_var,false,[],fresh_tvar(),(Lang.appify (gen_label(),EVar !opt_entry_func) inputs)))] 

let except_handling : exn -> value -> unit
= fun except output ->
  begin match except with
  |EExcept v ->
    print_endline("Result : " ^ Print.value_to_string v ^ " " ^
                  "Expected: " ^ Print.value_to_string output);
  |TimeoutError ->
    print_endline("Result : Timeout" ^ " " ^
                  "Expected: " ^ Print.value_to_string output);
  |Failure s ->
    print_endline("Result : Error "^ s ^ " " ^
                  "Expected: " ^ Print.value_to_string output);
	|EqualError -> 
    print_endline("Result : Equal Error" ^
                  "Expected: " ^ Print.value_to_string output);
  |_ ->
     print_endline("Result : Evaluation Error "^
                  "Expected: " ^ Print.value_to_string output);
  end
 
let is_same_type : prog -> prog -> unit
= fun pgm cpgm ->
  let (tenv1, _, _) = Type.run pgm in
  let (tenv2, _, _) = Type.run cpgm in
  let (t1, t2) = (Type.TEnv.find tenv1 !opt_entry_func, Type.TEnv.find tenv2 !opt_entry_func) in
  let _ = Type.unify Type.Subst.empty (t1, t2) in
  ()

let run_testcases : prog -> examples -> unit
=fun prog examples ->
  let score = List.fold_left (fun score (inputs, output) ->
    let prog = program_with_grading prog in
    let prog' = program_with_input prog inputs in
	  try
      let env = Eval.run prog' in
		  let result_value = Lang.lookup_env "__res__" env in
        print_endline ("Result: " ^ Print.value_to_string result_value ^ " " ^  
                     "Expected: " ^ Print.value_to_string output);
        if try (Eval.value_equality result_value output) with _ -> false then score+1 else score
    with except when except <> EqualError -> except_handling except output; score
  ) 0 examples in
  print_endline("score : "^(string_of_int score))

let run_prog : prog -> examples -> unit
=fun prog examples ->
  let _ = Type.run prog in
  print_header "Program"; Print.print_pgm prog;
  print_header "Test-cases"; print_examples examples;
  print_header "Run test-cases"; run_testcases prog examples 

let fix_with_solution : prog -> prog -> examples -> unit
=fun submission solution examples ->  (* TODO *)
  let _ = Type.run submission in
  let score = Util.list_fold (fun (inputs, output) score->
    let submission = program_with_grading submission in
    let prog = program_with_input submission inputs in
    let _ = 
      try
        (Type.run prog)
      with |_ -> raise (Failure "The submission and type are mismatched")
    in
    try
      let env = Eval.run prog in
      let result_value = Lang.lookup_env "__res__" env in
      if(result_value=output) then score+1 else score
    with |_ -> score
  ) examples 0 in
  let _ = if(score=List.length examples) then raise (Failure "The submission is correct code") in
  let ranked_prog_set = Localize.localization submission examples in
  let initial_set = BatSet.map
   (
      fun (n,prog)->
        let (_,hole_type,variable_type) = Type.run prog in
        (n,prog,hole_type,variable_type)
    ) ranked_prog_set in
  let components = Comp.extract_component solution in
  let _ = Synthesize.hole_synthesize submission initial_set components examples in
  ()

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
      Print.print_header "original"; Print.print_pgm submission;
      Print.print_header "Generated examples"; Print.print_examples examples;
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
      Print.print_header "Generated examples"; Print.print_examples examples;
      Print.print_header "Repair candidate"; Print.print_pgm candidate;
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
          let (_, h_t, v_t) = Type.run pgm in
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
          Print.print_header "Generated examples"; Print.print_examples examples;
          Print.print_header "Repair candidate"; Print.print_pgm candidate;
        in
        *)
        iter pgm cpgm pgm' examples
  in
  match iter submission solution submission [] with
  | (None, examples) -> 
      Print.print_header "original"; Print.print_pgm submission;
      Print.print_header "Generated examples"; Print.print_examples examples;
      Print.print_header "Fail to repair";
      print_endline ("Total Time : " ^ string_of_float !task_time)
  | (Some pgm, examples) -> 
    if (examples = []) then print_endline ("Correct Code")
    else (
      Print.print_header "original"; Print.print_pgm submission;
      Print.print_header "Generated examples"; Print.print_examples examples;
      Print.print_header "Correction"; Print.print_pgm pgm;
      print_endline ("Total Time : " ^ string_of_float !task_time)
    )

let execute : prog -> unit
=fun prog ->
  let (tenv,_,_) = Type.run prog in
  let env = Eval.run prog in
  (Print.print_REPL prog tenv env)

let fix_without_solution : prog -> examples -> unit
=fun submission examples -> () (* TODO *)

let synthesize : prog -> examples -> prog
=fun sketch examples -> [] (* TODO *)

let clonecheck : prog list -> prog list list
=fun submissions -> [] (* TOOD *)

let read_prog : string -> prog option
=fun filename ->
  try 
    if Sys.file_exists filename then Some (snd (parse_file filename)) 
    else None 
  with _ -> raise (Failure ("parsing error: " ^ filename)) 

let main () = 
  (* Arg Parse *)
	let _ = print_endline("file: "^Sys.argv.(0)) in
  let _ = Arg.parse options (fun s->()) usage_msg in
  let testcases = 
    if !opt_testcases_filename = "" then [] 
    else 
      try fst (parse_file !opt_testcases_filename) 
      with _ -> raise (Failure ("error during parsing testcases: " ^ !opt_testcases_filename)) 
  in 
  let solution = read_prog !opt_solution_filename in
  let submission = read_prog !opt_submission_filename in
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
      | Some sub, None -> fix_without_solution sub testcases 
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
  else if !opt_test then (* Symbolic testing *)
    begin
      match submission, solution with
      | Some sub, Some sol -> List.fold_left (fun acc (input, output) -> 
        let temp_time = Unix.gettimeofday () in
        if (Sym_exec.run sub sol input) = None then 
          print_endline ("SAT") 
        else 
          print_endline ("UNSAT")
        ;
        print_endline ("Time : " ^ string_of_float (Unix.gettimeofday () -. temp_time))
      ) () testcases
      | _ -> raise (Failure "Submission or solution is not provided")
    end
  else Arg.usage options usage_msg

let _ = main ()
