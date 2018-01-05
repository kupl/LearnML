type metro =
  STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name =
  string

let checkMetro(metro: metro): bool =
  let rec checkMetroInternal(metro, validNames: metro * name list) =
    match metro with
      STATION(id) ->
        List.mem id validNames
    | AREA(id, metro) ->
        checkMetroInternal(metro, id :: validNames)
    | CONNECT(metro1, metro2) ->
        checkMetroInternal(metro1, validNames) &&
        checkMetroInternal(metro2, validNames)
  in checkMetroInternal(metro, [])
