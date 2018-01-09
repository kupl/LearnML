let opt_pp = ref false
let opt_verbose = ref false
let opt_solution_filename = ref ""
let opt_submission_filename = ref ""
let opt_testcases_filename = ref ""
let opt_entry_func = ref "f"
let opt_run = ref false
let opt_fix = ref false
let opt_gentest = ref false

let options =
  [
    ("-run", Arg.Set opt_run, " Run submission");
    ("-fix", Arg.Set opt_fix, " Fix submission");
    ("-gentest", Arg.Set opt_gentest, " Generate testcases");
    ("-solution", Arg.String (fun fname -> opt_solution_filename := fname), " Solution filename");
    ("-submission", Arg.String (fun fname -> opt_submission_filename := fname), " Submission filename");
    ("-testcases", Arg.String (fun fname -> opt_testcases_filename := fname), " Test-cases filename");
    ("-entry", Arg.String (fun f -> opt_entry_func := f), " Name of the entry function (default: f)");
    ("-verbose" , Arg.Set opt_verbose, " Verbose mode");
    ("-pp", Arg.Set opt_pp, " Print AST");
  ]
  |> Arg.align

