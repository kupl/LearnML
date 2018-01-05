type metro =
    STATION of name
  | AREA of name * metro
  | CONNECT of metro * metro

and name = string


let checkMetro m =
  let rec check m set =
    match m with
    | STATION n -> List.mem n set
    | AREA (n, m) -> check m (n::set)
    | CONNECT (m1, m2) -> check m1 set && check m2 set
  in
  check m []
