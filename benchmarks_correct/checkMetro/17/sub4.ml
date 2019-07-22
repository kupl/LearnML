type metro = STATION of name
            | AREA of name * metro
            | CONNECT of metro * metro
and name = string

let checkMetro : metro -> bool = fun m ->
  let rec check_help : (metro -> name list -> bool) = fun m areas ->
    match m with
    | STATION v -> List.mem v areas
    | AREA (a, b) -> check_help b (a::areas)
    | CONNECT (a, b) -> (check_help a areas) && (check_help b areas)
  in check_help m []
