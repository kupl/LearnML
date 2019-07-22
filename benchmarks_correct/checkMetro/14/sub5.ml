type metro = 
  STATION of name
  | AREA of name * metro
  | CONNECT of metro * metro
and name = string

let rec checkMetro : metro -> bool =
  fun metro ->
    let rec checkMetroSub : metro * string list -> bool =
      fun (metro, lst) ->
        match metro with
        | STATION name -> List.mem name lst
        | AREA (name, metro) -> checkMetroSub (metro, lst @ [name])
        | CONNECT (m1, m2) -> checkMetroSub (m1, lst) && checkMetroSub (m2, lst)
    in
    checkMetroSub (metro, [])

