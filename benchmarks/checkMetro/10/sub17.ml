exception Error of string
type metro = STATION of name
  | AREA of name * metro
  | CONNECT of metro * metro
                         and name = string

let checkMetro : metro -> bool =
  fun metro ->
    let rec checkMetro : metro -> name list -> bool =
      fun metro -> fun env ->
        match metro
        with STATION(n) -> (List.mem n env)
          | AREA(n,m) -> (checkMetro m (n::env))
          | CONNECT(m1,m2) -> ((checkMetro m1 env) && (checkMetro m2 env))
    in
      try (checkMetro metro []) with _ -> raise (Error "cake is a lie")
