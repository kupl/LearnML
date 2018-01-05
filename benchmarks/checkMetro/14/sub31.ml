type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string

let checkMetro met : bool =
(*let arealst = getarea met in*)
let rec checkMetrorec met lst : bool =
match met with
  | STATION a -> if List.mem a lst = true then true else false
  | AREA (a,b) -> checkMetrorec b (a::lst)
  | CONNECT (a,b) -> (checkMetrorec a lst) && (checkMetrorec b lst) in
checkMetrorec met []

