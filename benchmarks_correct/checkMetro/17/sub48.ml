type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
         and name = string

let checkMetro: metro -> bool = fun m ->
  let rec check: metro -> string list -> bool = fun m areas ->
    match m with
    | STATION a -> (List.mem a areas)
    | AREA (a, b) -> (check b (areas @ [a]))
    | CONNECT (a, b) -> (check a areas) && (check b areas) in
  check m []

