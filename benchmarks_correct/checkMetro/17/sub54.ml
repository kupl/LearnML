type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

let rec checkMetro2 : metro * string list -> bool = fun (m, l) ->
  match m with
  | STATION a -> List.mem a l
  | AREA(a,m2) -> checkMetro2(m2,a::l)
  | CONNECT(m1,m2) -> checkMetro2(m1,l) && checkMetro2(m2,l)

let checkMetro : metro -> bool = fun m ->
  checkMetro2(m, [])
