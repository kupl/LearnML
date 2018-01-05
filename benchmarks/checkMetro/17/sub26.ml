type metro = STATION of name
            |AREA of name * metro
            |CONNECT of metro * metro
and name = string

let rec checkList (met, l) =
  match met with
  STATION n -> if (List.mem n l) then true else false
  |AREA (n, m) -> checkList(m, [n]@l)
  |CONNECT (m1, m2) -> checkList(m1, l) && checkList(m2, l)

let rec checkMetro met =
  checkList(met, [])


