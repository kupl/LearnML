type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string

let rec findName : (name * (name list)) -> bool = fun (x,y) ->
 match y with
 | [] -> false
 | y_h::y_t ->
  if (y_h = x) then true
  else findName (x, y_t)

let rec checkMetroList : (metro * (name list)) -> bool = fun (x,y) ->
 match x with
 | STATION n -> findName (n, y)
 | AREA (n, m) -> checkMetroList (m, (n::y))
 | CONNECT (m1, m2) -> ((checkMetroList (m1, y)) && (checkMetroList (m2, y)))
 
let checkMetro : metro -> bool = fun (x) -> checkMetroList(x, [])
