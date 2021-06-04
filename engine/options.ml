(* Extracting binary's path *)
let rec path_to_str : string list -> string
= fun str_lst ->
	match str_lst with
	|[] -> ""
	|hd::tl -> (path_to_str tl)^hd^"/"

let lib_path =
	let executable_path = Sys.argv.(0) in
	let str_list = String.split_on_char '/' executable_path in
	let str_list = List.rev str_list in
	match str_list with
	|[] -> raise(Failure "external path is incorrect")
	|[hd] -> "./moduleSpec.ml"
	|hd::tl -> (path_to_str tl)^"moduleSpec.ml"

let model_path =
	let executable_path = Sys.argv.(0) in 
	let str_list = String.split_on_char '/' executable_path in
	let str_list = List.rev str_list in
	match str_list with
	|[] -> raise(Failure "external path is incorrect")
	|[hd] -> "/models/"
  |hd::tl -> (path_to_str tl)^ "models/"
  
let opt_solution_filename = ref ""
let opt_solution_dirname = ref ""
let opt_submission_filename = ref ""
let opt_testcases_filename = ref ""
let opt_grading_filename = ref ""
let opt_entry_func = ref ""
let opt_external_filename = ref lib_path
let opt_model_path = ref model_path

let opt_run = ref false (* Run test cases *)
let opt_fix = ref false (* FixML or CAFE *)
let opt_gentest = ref false (* TestML *)
let opt_prog = ref false (* Program-level SARFGEN *)
let opt_func = ref false (* Function-level SARFGEN *)
let opt_preproc = ref false (* Preprocessing solution *)

let options =
  [
    ("-solution", Arg.String (fun fname -> opt_solution_filename := fname), " Solution filename");
    ("-solutions", Arg.String (fun fname -> opt_solution_dirname := fname), " Solution dirname");
    ("-submission", Arg.String (fun fname -> opt_submission_filename := fname), " Submission filename");
    ("-testcases", Arg.String (fun fname -> opt_testcases_filename := fname), " Test-cases filename");
    ("-grading", Arg.String (fun fname -> opt_grading_filename := fname), " Grading filename");
    ("-entry", Arg.String (fun f -> opt_entry_func := f), " Name of the entry function (default: f)");
    ("-external", Arg.String (fun fname -> opt_external_filename := fname), " moduleSpec path");
    ("-fix", Arg.Set opt_fix, " Fix submission");
    ("-gentest", Arg.Set opt_gentest, " Generate testcases");
    ("-prog", Arg.Set opt_prog, " Fix submission with SARFGEN");
    ("-func", Arg.Set opt_func, " Fix submission with SARFGEN");
    ("-preproc", Arg.Set opt_preproc, " Conduct preprocessing");
    ("-run", Arg.Set opt_run, " Run submission");
  ]
  |> Arg.align

