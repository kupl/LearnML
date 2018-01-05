
print_endline "hw2 e3 2010-11969";;

let x =3;;

type name = string
type metro =
  STATION of name
  | AREA of name * metro
  | CONNECT of metro * metro

let _ =
  let rec string_of_metro (m: metro): string =
    match m with
    | STATION n -> Printf.sprintf "(STATION %s)" n
    | AREA (n, m') -> Printf.sprintf "(AREA %s %s)" n (string_of_metro m')
    | CONNECT (m', m'') -> Printf.sprintf "(CONNECT %s %s)" (string_of_metro m') (string_of_metro m'')
    | _ -> ""
  in
  print_endline "END";
  string_of_metro (STATION "blah") |> print_endline;
  string_of_metro (AREA ("a", (STATION "blah"))) |> print_endline;
