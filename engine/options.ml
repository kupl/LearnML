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
let opt_testcases_filename = ref ""
let opt_grading_filename = ref ""
let opt_entry_func = ref "f"
let opt_external_filename = ref path
let opt_generator = ref ""

let opt_fix = ref false (* FixML *)
let opt_gentest = ref false (* TestML *)
let opt_dd = ref false (* Data-driven FixML *)
let opt_preproc = ref false (* Preprocessing solution *)

(* Other experiments *)
let opt_run = ref false
let opt_execute = ref false 
let opt_qcheck = ref false
let opt_cfg = ref false 

let options =
  [
    ("-solution", Arg.String (fun fname -> opt_solution_filename := fname), " Solution filename");
    ("-solutions", Arg.String (fun fname -> opt_solution_dirname := fname), " Solution dirname");
    ("-submission", Arg.String (fun fname -> opt_submission_filename := fname), " Submission filename");
    ("-testcases", Arg.String (fun fname -> opt_testcases_filename := fname), " Test-cases filename");
    ("-grading", Arg.String (fun fname -> opt_grading_filename := fname), " Grading filename");
    ("-entry", Arg.String (fun f -> opt_entry_func := f), " Name of the entry function (default: f)");
    ("-external", Arg.String (fun fname -> opt_external_filename := fname), " moduleSpec path");
    ("-generator", Arg.String (fun fname -> opt_generator := fname), " qcheck_generator path");
    ("-fix", Arg.Set opt_fix, " Fix submission");
    ("-gentest", Arg.Set opt_gentest, " Generate testcases");
    ("-dd", Arg.Set opt_dd, " Fix submission using solutions");
    ("-preproc", Arg.Set opt_preproc, " Conduct preprocessing");
    ("-run", Arg.Set opt_run, " Run submission");
    ("-execute",Arg.Set opt_execute, " Execute submission");
    ("-qcheck", Arg.Set opt_qcheck, " Qcheck testing");
    ("-cfg", Arg.Set opt_cfg, " CFG testing");
  ]
  |> Arg.align

