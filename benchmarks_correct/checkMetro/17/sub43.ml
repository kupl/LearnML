type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro

and name = string

let arealist : string list = []

let rec checkarea ((a : string list) , (b : metro)) : bool =
  match b with
  | STATION nm -> List.mem nm a
  | AREA (nm, mt) -> checkarea(nm::a, mt)
  | CONNECT (m1, m2) -> checkarea(a, m1) && checkarea(a, m2)

let checkMetro (b: metro) : bool =
  checkarea (arealist,b)