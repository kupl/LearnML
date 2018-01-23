type metro = STATION of name
  | AREA of name * metro
  | CONNECT of metro * metro
and name = string

let rec check (metro, lst) =
  match metro with AREA (name, m) -> check (m, name::lst)
  | STATION x -> List.mem x lst
  | CONNECT (a, b) -> check (a, lst) && check (b, lst)

let checkMetro metro =
  check (metro, [])

