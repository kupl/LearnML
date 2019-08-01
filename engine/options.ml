let rec path_to_str : string list -> string
= fun str_lst ->
	match str_lst with
	|[] -> ""
	|hd::tl -> (path_to_str tl)^hd^"/"

let path =
	let executable_path = Sys.argv.(0) in
	let str_list = String.split_on_char '/' executable_path in
	let str_list = List.rev str_list in
	match str_list with
	|[] -> raise(Failure "external path is incorrect")
	|[hd] -> "./moduleSpec.ml"
	|hd::tl -> (path_to_str tl)^"moduleSpec.ml"


let opt_solution_filename = ref ""
let opt_solution_dirname = ref ""
let opt_submission_filename = ref ""
let opt_submission_dirname = ref ""
let opt_testcases_filename = ref ""
let opt_grading_filename = ref ""
let opt_entry_func = ref "f"
let opt_external_filename = ref path
let opt_generator = ref ""

let opt_run = ref false
let opt_fix = ref false
let opt_gentest = ref false
let opt_execute = ref false
let opt_localize = ref false
let opt_test = ref false
let opt_qcheck = ref false
let opt_exp_cover = ref false

(*For debugging*)
let opt_tree = ref false
let opt_vector = ref false
let opt_offline = ref false
let opt_search = ref false
let k = ref 5
let opt_experiment = ref false

let options =
  [
    ("-solution", Arg.String (fun fname -> opt_solution_filename := fname), " Solution filename");
    ("-solutions", Arg.String (fun fname -> opt_solution_dirname := fname), " Solution dirname");
    ("-submission", Arg.String (fun fname -> opt_submission_filename := fname), " Submission filename");
    ("-submissions", Arg.String (fun fname -> opt_submission_dirname := fname), " Submission dirname");
    ("-testcases", Arg.String (fun fname -> opt_testcases_filename := fname), " Test-cases filename");
    ("-grading", Arg.String (fun fname -> opt_grading_filename := fname), " Grading filename");
    ("-entry", Arg.String (fun f -> opt_entry_func := f), " Name of the entry function (default: f)");
    ("-external", Arg.String (fun fname -> opt_external_filename := fname), " moduleSpec path");
    ("-generator", Arg.String (fun fname -> opt_generator := fname), " qcheck_generator path");
    ("-run", Arg.Set opt_run, " Run submission");
    ("-fix", Arg.Set opt_fix, " Fix submission");
    ("-execute",Arg.Set opt_execute, " Execute submission");
    ("-gentest", Arg.Set opt_gentest, " Generate testcases");
    ("-localize", Arg.Set opt_localize, " Localize");
    ("-test", Arg.Set opt_test, " Symbolic testing");
    ("-qcheck", Arg.Set opt_qcheck, " Qcheck testing");
    ("-exp_cover", Arg.Set opt_exp_cover, " Check expression coverage");
    ("-tree", Arg.Set opt_tree, " Print code ast");
    ("-vector", Arg.Set opt_vector, " Print vectorzation result");
    ("-offline", Arg.Set opt_offline, " Offline vector embedding");
    ("-search", Arg.Set opt_search, " Find most k closest vector");
    ("-experiment", Arg.Set opt_experiment, " for parsing experiment result");
  ]
  |> Arg.align

