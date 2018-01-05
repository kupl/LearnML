type metro = 
  | STATION of name
  | AREA of name * metro
  | CONNECT of metro * metro
and name = string

let rec checkMetro2 someMetro chkList = 
    match someMetro with 
    | STATION (u) -> List.mem u chkList
    | AREA (u,v) -> checkMetro2 v (u::chkList)
    | CONNECT (u,v) -> (checkMetro2 u chkList) && (checkMetro2 v chkList)

let checkMetro someMetro = checkMetro2 someMetro []