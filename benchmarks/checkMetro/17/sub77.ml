type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

let checkMetro(inputmetro : metro) : bool =
  let rec check((m : metro),(validarea : name list)) : bool =
    match m with
    | STATION(name) -> List.mem name validarea
    | AREA(name, metro) -> check(metro, name :: validarea)
    | CONNECT(metro1, metro2) -> check(metro1, validarea) && check(metro2, validarea)
  in
  check(inputmetro, [])
