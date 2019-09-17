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

let topk = 1

(*
 * function_level 
 * position_aware_vector
 *)
let fix_with_vectors3 : prog -> (string*prog) list -> examples -> unit
= fun sub solutions testcases->
  let k = topk in
  let topk_lst = Pos_vector.search k sub solutions in

  print_endline "@#$@#$@#$";
  Print.print_header "Submission"; Print.print_pgm sub;

  List.iter (fun (filename, sol,(map,dist)) ->
  print_endline "@#$@#$@#$";
  Print.print_header ("filename: "^filename); 
  (*Print.print_header "solution"; Print.print_pgm sol;*)
  Print.print_header ("distance with submission: "^(string_of_float dist));
  ignore (Vrepairer.run sub sol map testcases);

  print_endline "@#$@#$@#$";
  ) topk_lst


(*
 * function_level 
 * frequency_vector
 *)
let fix_with_vectors2 : prog -> (string*prog) list -> examples -> unit
= fun sub solutions testcases->
  let k = topk in
  let topk_lst = Freq_vector.search2 k sub solutions in

  print_endline "@#$@#$@#$";
  Print.print_header "Submission"; Print.print_pgm sub;

  List.iter (fun (filename, sol,(map,dist)) ->
  print_endline "@#$@#$@#$";
  Print.print_header ("filename: "^filename); 
  Print.print_header "solution"; Print.print_pgm sol;
  Print.print_header ("distance with submission: "^(string_of_float dist));
  ignore (Vrepairer.run sub sol map testcases);

  print_endline "@#$@#$@#$";
  ) topk_lst

(*
 * program_level
 * frequency_vector
 *)
let fix_with_vectors : prog -> (string*prog) list -> examples -> unit
= fun sub solutions testcases->
  let k = topk in
  let topk_lst = Freq_vector.search k sub solutions in

  print_endline "@#$@#$@#$";
  Print.print_header "Submission"; Print.print_pgm sub;

  List.iter (fun (filename, sol, dist) ->
  print_endline "@#$@#$@#$";
  Print.print_header ("filename: "^filename); 
  (*Print.print_header "solution"; Print.print_pgm sol;*)
  Print.print_header ("distance with submission: "^(string_of_float dist));
  ignore (Repairer.run sub sol testcases);

  print_endline "@#$@#$@#$";
  ) topk_lst

let execute : prog -> unit
= fun prog ->
  let (tenv,_,_,_) = Type.run prog in
  let env = Eval.run prog in
  (print_REPL prog tenv env)

let main () = 
  (* Arg Parse *)
  let _ = print_endline("file: "^Sys.argv.(0)) in
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
(*
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
    *)
  else if !opt_execute then (* Execute Program *)
    begin 
      match submission with
      | Some sub -> execute sub
      | _ -> raise (Failure "Submission file is not provided")
    end
    (*
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
    *)
  else if !opt_tree then (* For debugging *)
    begin 
      match submission with
      | Some sub -> Print.print_header (Print.program_to_string sub); Print.print_header (Pp_tree.program_to_tree 0 sub);
                    print_header "Summary"; print_endline (Selector.A.string_of_t (Selector.get_summary sub));                   
                    Print.print_header "test function extractor"; Vrepairer.test sub;

      | _ -> raise (Failure(!opt_submission_filename ^ " does not exist"))
    end
  else if !opt_vector then 
    begin 
      match submission, solutions_debug with 
      | Some sub, [] -> raise (Failure (!opt_solution_dirname ^" may not exist"))
      | Some sub, hd::tl -> fix_with_vectors sub solutions_debug testcases
      | _ -> raise (Failure(!opt_submission_filename ^ " does not exist"))
    end 
  else if !opt_vector2 then 
    begin 
      match submission, solutions_debug with 
      | Some sub, [] -> raise (Failure (!opt_solution_dirname ^" may not exist"))
      | Some sub, hd::tl -> fix_with_vectors2 sub solutions_debug testcases
      | _ -> raise (Failure(!opt_submission_filename ^ " does not exist"))
    end 
  else if !opt_vector3 then 
    begin 
      match submission, solutions_debug with 
      | Some sub, [] -> raise (Failure (!opt_solution_dirname ^" may not exist"))
      | Some sub, hd::tl -> fix_with_vectors3 sub solutions_debug testcases
      | _ -> raise (Failure(!opt_submission_filename ^ " does not exist"))
    end 
    (*
  else if !opt_offline then 
    begin
      match solutions_debug with
      | [] -> raise (Failure (!opt_solution_dirname ^ " not include file")) 
      | hd::tl -> 
        let file = "vector.txt" in 
        let log = ref (open_out file) in
        let print_test : (string*prog) -> unit =
        begin
        fun (fname, prog) -> 
          let vector = Freq_vector.to_string (Freq_vector.prog_vectorize prog) in
          Printf.fprintf (!log) "%s : %s\n" fname vector
        end 
        in List.iter print_test solutions_debug 
    end
    *)
   else if !opt_experiment then
     begin 
       match submission, solutions_debug with
       | Some sub, [] -> raise (Failure (!opt_solution_dirname ^ " not include file"))
       | Some sub, hd::tl -> 
         begin match Cfg.run sub solutions_debug with
         | [] -> Print.print_parsing_header "Matched Solution : 0"
         | l -> 
           Print.print_parsing_header ("Matched Solution : "^ string_of_int (List.length l));
           List.iter (fun (f,Some sol) -> Print.print_parsing_header f;
                      ignore (Repairer.run sub sol testcases)) l
         end
     end
   else 
    begin 
      match submission, solutions_debug with
      | Some sub, [] -> print_endline (string_of_int (List.length solutions)); print_endline (Cfg.S.string_of_t (Cfg.S.run sub)); 
      | Some sub, hd::tl -> 
        begin match Cfg.run sub solutions_debug with
        | [] -> 
          Print.print_header "Submission"; Print.print_pgm sub;
          Print.print_header "Matched solution"; print_endline ("Not found.")
        | l -> 
          Print.print_header ("Size of Matched solution: "^(string_of_int (List.length l)));
          List.iter (fun (f,Some sol) ->
        (*
          Print.print_header "Submission"; Print.print_pgm sub; *)
          Print.print_header ("Matched solution: "^f); (*Print.print_pgm sol;*)
          ignore (Repairer.run sub sol testcases)) l
          
      (*  | (f,None) ->
          Print.print_header "Submission"; Print.print_pgm sub;
          Print.print_header "Matched solution"; print_endline ("Not found.")*)
        end
      | _ -> raise (Failure(!opt_submission_filename ^ " does not exist"))
    end
    (*Arg.usage options usage_msg*)


let _ = main ()