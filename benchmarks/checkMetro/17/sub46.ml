type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string

let rec checkMetro : metro -> bool = fun metro ->
match metro with
| STATION _ -> false
| AREA (a, b) -> true
| CONNECT (a, b) -> true
