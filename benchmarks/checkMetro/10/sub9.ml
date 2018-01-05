type metro =
  | STATION of name
  | AREA of name * metro
  | CONNECT of metro * metro
and name = string;;

let checkMetro metro =
  let rec check met ids =
    match met with
    | STATION id -> List.mem id ids
    | AREA (id, m) -> check m (id :: ids)
    | CONNECT (m, m') -> check m ids && check m' ids
  in
  check metro [];;
