open Lang

let usage_msg = "main.native -run (or -fix) -submission <filename> -solution <filename>"

let init_prog () =
  let _ = Arg.parse Options.options (fun s->()) usage_msg in
  let (_,prog) = 
    Preproc.preprocess_file (!Options.opt_external_filename)
    |> Lexing.from_string
    |> Parser.prog Lexer.token
  in prog

let init_prog = init_prog ()
