type metro =
  STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string

let STATION (s) = "STATION (" ^ s ^ ")";; (* For Test *)

let rec checkmetro_sub : STATION * metro -> bool = (
  function (s, c) ->
  match c with
  STATION x -> if s == STATION x then true else false
  | AREA (x, y) -> checkmetro_sub (s, y)
  | CONNECT (x, y) -> checkmetro_sub (s, x) or checkmetro_sub (s, y)
);;

let checkmetro (s) =
match s with
AREA(x, y) -> checkmetro_sub (x, y)
;;
