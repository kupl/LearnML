open Lang 

(* Read program from file *)

let preprocess_file : string -> string
= fun filename -> 
  let ic = open_in filename in
  let lines = ref [] in
  begin try
    while true do
      lines := (input_line ic)::!lines
    done
  with End_of_file -> 
    close_in ic
  end;
  List.rev !lines |> String.concat "\n"

let parse_file (f:string) : (examples * prog) =
  preprocess_file f
    |> Lexing.from_string
    |> Parser.prog Lexer.token

let read_prog : string -> prog option
= fun filename ->
  try 
    if Sys.file_exists filename then Some (snd (parse_file filename)) 
    else None 
  with _ -> raise (Failure ("parsing error: " ^ filename)) 


