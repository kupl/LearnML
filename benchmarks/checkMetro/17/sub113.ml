type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

let rec checkMetro: metro -> bool = fun (input) ->
  let rec checkMetro2: metro * name list -> bool = fun (input, names) ->
    match input with
    | STATION n -> List.mem n names
    | AREA (n, m) -> checkMetro2(m, n :: names)
    | CONNECT (m1, m2) -> checkMetro2(m1, names) && checkMetro2(m2, names)
  in
  checkMetro2(input, [])