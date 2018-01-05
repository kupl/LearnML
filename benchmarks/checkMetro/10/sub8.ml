type metro = STATION of name
  | AREA of name * metro
  | CONNECT of metro * metro
and name = string

let rec check (metro, list) =
  match metro with AREA (name, m) -> check (m, name::list)
  | STATION x -> List.mem x list
  | CONNECT (a, b) -> check (a, list) && check (b, list)

let checkMetro metro =
  check (metro, [])

let print_bool b =
  match b with true -> print_string "true"
  | false -> print_string "false"

(* let _ = print_bool (checkMetro ( AREA("a", AREA("b", CONNECT(STATION "a", STATION "c"))) ));; *)