type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

let checkMetro metro =
  let rec checkAux met area =
    match met with
    | STATION name -> List.mem name area
    | AREA (name, metro) -> checkAux metro (name::area)
    | CONNECT (m1, m2) -> (checkAux m1 area) && (checkAux m2 area) in
  checkAux metro []
