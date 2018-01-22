open Lang

let usage_msg = "main.native -run (or -fix) -submission <filename> -solution <filename>"

let _ = Arg.parse Options.options (fun s -> ()) usage_msg 

let parse_file f =  
  Preproc.preprocess_file f
  |> Lexing.from_string
  |> Parser.prog Lexer.token

let parse_init_prog () =
  let (_,prog) = parse_file (!Options.opt_external_filename) in
  prog

let parse_grading_prog () =
  if(!Options.opt_grading_filename = "") then []
  else 
    let (_,prog) = parse_file (!Options.opt_grading_filename) in
    prog

let init_prog = parse_init_prog ()
let grading_prog = parse_grading_prog ()
