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

let read_prog : string -> prog option
= fun filename -> if Sys.file_exists filename then Some (snd (parse_file filename)) else None 

let read_prog_debug : string -> (string * prog) option
= fun filename -> if Sys.file_exists filename then Some (filename, snd (parse_file filename)) else None 

(* Read set of programs from directory *)
let dir_contents : string -> string list
= fun dir ->
  Sys.readdir dir
  |> Array.to_list 
  |> List.map (Filename.concat dir)
  |> List.sort compare 

let read_pgms : string -> prog list
= fun problem ->
  if Sys.file_exists problem then 
    let dirs = dir_contents problem in
    let pgms = List.fold_left (fun pgms dir ->
      if Sys.is_directory dir then
        let files = dir_contents dir in
        List.fold_left (fun pgms f -> 
          match read_prog f with
          | Some pgm -> pgm::pgms
          | None -> pgms
        ) pgms files
      else pgms
    ) [] dirs 
    in
    List.rev pgms
  else []

let read_pgms_debug : string -> (string * prog) list
 = fun problem ->
   if Sys.file_exists problem then
     let dirs = dir_contents problem in
     let pgms = List.fold_left
       (fun pgms dir -> if Sys.is_directory dir then
         let files = dir_contents dir in
         List.fold_left (fun pgms f ->
           match read_prog f with
           | Some pgm -> (f,pgm)::pgms
           | None -> pgms) pgms files
         else pgms) [] dirs 
    in pgms 
    else []

(* Read testcases *)
let read_testcases : string -> examples
= fun file_name ->
  if file_name = "" then [] 
  else 
    try fst (parse_file file_name) 
    with e -> raise e

(* Read external library *)
let read_external : string -> prog 
= fun filename -> if Sys.file_exists filename then snd (parse_file filename) else []

(* Preprocessing *)
let get_file_path : string -> string
= fun filename ->
  let start_idx = Str.search_backward (Str.regexp "benchmarks_correct") filename (String.length filename) in
  let end_idx = Str.search_forward (Str.regexp "\.ml$") filename 0 in
  String.sub filename (String.length "filename") end_idx
