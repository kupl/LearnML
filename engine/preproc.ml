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

let parse_file : string -> examples * prog
= fun f -> 
  try 
    preprocess_file f
    |> Lexing.from_string
    |> Parser.prog Lexer.token
  with _ -> raise (Failure ("Parsing error : " ^ f))

let read_pgm : string -> (string * prog) option
= fun filename -> if Sys.file_exists filename then Some (filename, snd (parse_file filename)) else None 

(* Read set of programs from directory *)
let dir_contents : string -> string list
= fun dir ->
  Sys.readdir dir
  |> Array.to_list 
  |> List.map (Filename.concat dir)
  |> List.sort compare 

let read_pgms : string -> (string * prog) list
= fun problem ->
  if Sys.file_exists problem then 
    let (pgms) = List.fold_left (fun pgms f ->
        match read_pgm f with
        | Some pgm -> 
          (* Sort benchmarks by increasing order *)
          let file_name = 
            List.hd (List.rev (Str.split (Str.regexp "/+") f)) 
            |> Str.replace_first (Str.regexp ".ml$") ""
          in
          if Str.string_match (Str.regexp "sol+[1-9]*[0-9]$") file_name 0 then
            let idx = Str.search_forward (Str.regexp "[1-9]*[0-9]$") file_name 0 in
            let benchmark_number = Str.string_after file_name idx in
            (int_of_string benchmark_number, pgm)::pgms
          else
            (-1, pgm)::pgms
        | None -> pgms
      ) [] (dir_contents problem)
    in
    List.rev (List.sort compare pgms |> List.map snd)
  else []

let read_external : string -> prog 
= fun filename -> 
  match read_pgm filename with
  | Some (fname, pgm) -> pgm 
  | None -> []

(* Read testcases *)
let read_testcases : string -> examples
= fun file_name ->
  if file_name = "" then [] 
  else 
    try fst (parse_file file_name) 
    with e -> raise e