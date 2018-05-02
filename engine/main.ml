open Lang
open Print
open Options

exception Arg_exception

let all_component () =
  let e_t = BatSet.empty in
  let e_t = BatSet.add (0,Const 0) e_t in
  let e_t = BatSet.add (0,Const 1) e_t in
  let e_t = BatSet.add (0,TRUE) e_t in
  let e_t = BatSet.add (0,FALSE) e_t in
  let e_t = BatSet.add (0,EList []) e_t in
  let e_t = BatSet.add (0,(ADD (dummy_hole (),dummy_hole ()))) e_t in
  let e_t = BatSet.add (0,(SUB (dummy_hole (),dummy_hole ()))) e_t in
  let e_t = BatSet.add (0,(MUL (dummy_hole (),dummy_hole ()))) e_t in
  let e_t = BatSet.add (0,(DIV (dummy_hole (),dummy_hole ()))) e_t in
  let e_t = BatSet.add (0,(MOD (dummy_hole (),dummy_hole ()))) e_t in
  let e_t = BatSet.add (0,(MINUS (dummy_hole ()))) e_t in
  let e_t = BatSet.add (0,(NOT (dummy_hole ()))) e_t in
  let e_t = BatSet.add (0,(OR (dummy_hole (),dummy_hole ()))) e_t in
  let e_t = BatSet.add (0,(AND (dummy_hole (),dummy_hole ()))) e_t in
  let e_t = BatSet.add (0,(LESS (dummy_hole (),dummy_hole ()))) e_t in
  let e_t = BatSet.add (0,(LARGER (dummy_hole (),dummy_hole ()))) e_t in
  let e_t = BatSet.add (0,(EQUAL (dummy_hole (),dummy_hole ()))) e_t in
  let e_t = BatSet.add (0,(NOTEQ (dummy_hole (),dummy_hole ()))) e_t in
  let e_t = BatSet.add (0,(LESSEQ (dummy_hole (),dummy_hole ()))) e_t in
  let e_t = BatSet.add (0,(LARGEREQ (dummy_hole (),dummy_hole ()))) e_t in
  let e_t = BatSet.add (0,(EApp (dummy_hole (),dummy_hole ()))) e_t in
  let e_t = BatSet.add (0,(AT (dummy_hole (),dummy_hole ()))) e_t in
  let e_t = BatSet.add (0,(DOUBLECOLON (dummy_hole (),dummy_hole ()))) e_t in
  let e_t = BatSet.add (0,(IF (dummy_hole (),dummy_hole (),dummy_hole ()))) e_t in
  (* let e_t = BatSet.add EUnit e_t in *)
  (* let e_t = BatSet.add (STRCON (Hole 0, Hole 0)) e_t in *)
  (* let e_t = BatSet.add (Raise (Hole 0)) e_t in *)
  e_t

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
	|EqualError -> raise except
  |_ ->
     print_endline("Result : Evaluation Error "^
                  "Expected: " ^ Print.value_to_string output);
  end
 
let run_testcases : prog -> examples -> unit
=fun prog examples ->
  let score = List.fold_left (fun score (inputs, output) ->
    let prog = program_with_grading prog in
    let prog' = program_with_input prog inputs in
    let _ = Type.run prog' in
	  try
      let env = Eval.run prog' in
		  let result_value = Lang.lookup_env "__res__" env in
        print_endline ("Result: " ^ Print.value_to_string result_value ^ " " ^  
                     "Expected: " ^ Print.value_to_string output);
        if(Eval.value_equality result_value output) then score+1 else score
    with except -> except_handling except output; 
		(*let _ = Eval.value_equality (Lang.lookup_env "__res__" (Eval.run prog')) output in*)
		score
  ) 0 examples in
  print_endline("score : "^(string_of_int score))

let run_prog : prog -> examples -> unit
=fun prog examples ->
  let _ = Type.run prog in
  print_header "Program"; Print.print_pgm prog;
  print_header "Test-cases"; print_examples examples;
  print_header "Run test-cases"; run_testcases prog examples 

let localization : prog -> examples -> unit
= fun pgm examples ->
  let basic_candidate = Localize.localization pgm examples in
  let smt_candidate =
  BatSet.filter (
    fun (_, pgm) -> Smt_pruning.smt_pruning pgm examples
  ) basic_candidate in
  print_header ("Candidate Set (base) : " ^ string_of_int (BatSet.cardinal basic_candidate)); BatSet.iter (fun (_, pgm) -> Print.print_pgm pgm) basic_candidate;
  print_header ("Candidate Set (smt) : " ^ string_of_int (BatSet.cardinal smt_candidate)); BatSet.iter (fun (_, pgm) -> Print.print_pgm pgm) smt_candidate;
  ()

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
  (*print_header "Score"; print_endline(string_of_int score);
  print_header "Solution"; Print.print_pgm solution;
  print_header "Submission"; Print.print_pgm submission;
  print_header "Test-cases"; print_examples examples;*)
  let ranked_prog_set = Localize.localization submission examples in
  let initial_set = BatSet.map
   (
      fun (n,prog)->
        let (_,hole_type,variable_type) = Type.run prog in
				(*Print.print_pgm prog;
				print_endline(string_of_int n);*)
				(*Type.HoleType.print hole_type;
				Type.VariableType.print variable_type;*)
        (n,prog,hole_type,variable_type)
    ) ranked_prog_set in
  (*print_header "initial-set"; print_endline(string_of_int (BatSet.cardinal initial_set));*)
  let components = Comp.extract_component solution in
  
	(*let components = BatSet.filter (fun x ->
    match x with
    | Const _ -> false
    | EList _ -> false
    | _ -> true
  ) components
  in
  let components = BatSet.union components (all_component ()) in*)
  let _ = Synthesize.hole_synthesize submission initial_set components examples in
  ()
 
let execute : prog -> unit
=fun prog ->
  let (tenv,_,_) = Type.run prog in
  let env = Eval.run prog in
  (Print.print_REPL prog tenv env)

let fix_without_solution : prog -> examples -> unit
=fun submission examples -> () (* TODO *)

let synthesize : prog -> examples -> prog
=fun sketch examples -> [] (* TODO *)

let generate_testcases : prog -> prog -> examples
=fun submission solution -> [] (* TODO *)

let clonecheck : prog list -> prog list list
=fun submissions -> [] (* TOOD *)

let read_prog : string -> prog option
=fun filename ->
  try 
    if Sys.file_exists filename then Some (snd (parse_file filename)) 
    else None 
  with _ -> raise (Failure ("parsing error: " ^ filename)) 

let main () = 
  let _ = Arg.parse options (fun s->()) usage_msg in
  let testcases = 
    if !opt_testcases_filename = "" then [] 
    else try fst (parse_file !opt_testcases_filename) 
         with _ -> raise (Failure ("error during parsing testcases: " ^ !opt_testcases_filename)) in 
  let submission = read_prog !opt_submission_filename in
  (**) 
  (*
  let Some pgm = submission in
  Print.print_pgm pgm;
  print_endline (string_of_bool (Smt_pruning.smt_pruning pgm testcases));
  List.iter (
    fun example ->
      let sv = Symbol_eval.gen_constraint pgm example in
      let f = 
        try 
          Smt_pruning.Converter.symbol_to_formula sv 
        with _ -> False
      in
      let t = if Smt_pruning.solve sv then "SAT" else "UNSAT" in
      print_examples [example];
      print_endline ("Constraint : " ^ Print.symbol_to_string sv ^ " => " ^ t);
      print_endline ("Formula : " ^ Print.formula_to_string f  ^ "\n")
  ) testcases;
  *)
  (**)
  let solution = read_prog !opt_solution_filename in
    if !opt_localize then 
      begin
        match submission with
        | Some sub -> localization sub testcases
        | _ -> raise (Failure (!opt_submission_filename ^ " does not exist"))
      end
    else
    match !opt_run, !opt_fix, !opt_gentest, !opt_execute with
    | true, false, false, false -> (* execution mode *)
      begin
        match submission with
        | Some sub -> run_prog sub testcases
        | _ -> raise (Failure (!opt_submission_filename ^ " does not exist"))
      end
    | false, true, false, false -> (* fix mode *)
      begin
        match submission, solution with
        | Some sub, Some sol -> fix_with_solution sub sol testcases 
        | Some sub, None -> fix_without_solution sub testcases 
        | _ -> raise (Failure (!opt_submission_filename ^ " does not exist"))
      end
    | false, false, true, false -> (* testcase-generation mode *)
      begin
        match submission, solution with
        | Some sub, Some sol -> ignore (generate_testcases sub sol)
        | _ -> raise (Failure "Submission or solution not provided")
      end
    | false, false, false, true -> (* execution mode *)
      begin 
        match submission with
        | Some sub -> execute sub
        | _ -> raise (Failure "Submission file is not provided")
      end
    | _ -> Arg.usage options usage_msg

let _ = main ()
