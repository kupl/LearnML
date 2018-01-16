open Lang

let init () =
  let (_,prog) = 
    Preproc.preprocess_file (!Options.opt_external_filename)
    |> Lexing.from_string
    |> Parser.prog Lexer.token
  in prog

let init_prog = init ()
