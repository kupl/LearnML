type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
  and name = string

let rec checkMetroList ((m : metro), (l : name list)) : bool =
    match m with
    | STATION (n1) -> if (List.mem n1 l) then true
                      else false
    | AREA (n1, m1) -> checkMetroList (m1, n1::l)
    | CONNECT (m1, m2) -> checkMetroList (m1, l) && checkMetroList (m2, l)

let checkMetro (m : metro) : bool =
    checkMetroList (m, [])