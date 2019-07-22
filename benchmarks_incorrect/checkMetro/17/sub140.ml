type metro = STATION of name
  | AREA of name * metro
  | CONNECT of metro * metro
and name = string

let rec checkMetro: metro -> bool = fun m ->
  match m with
  | STATION _ -> false
  | CONNECT _ -> false
  | AREA(n', m') -> List.mem n' (listMetro(m'))

and listMetro: metro -> 'a list = fun m ->
  match m with
  | STATION m' -> [m']
  | CONNECT(m1, m2) -> (match (listMetro m1), (listMetro m2) with
    | (a::b, c::d) -> List.append (a::b) (c::d)
    | (_, _) -> [])
  | AREA(n', m') -> (match checkMetro m with
    | true -> listMetro m'
    | false -> [])


(* TESTING FIELD BELOW *)


