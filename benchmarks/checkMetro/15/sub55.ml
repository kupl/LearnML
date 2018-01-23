
type metro = STATION of name
  | AREA of name * metro
  | CONNECT of metro * metro
and name = string

let rec badStationList: metro -> name list = fun m ->
  match m with
  | STATION id -> [id]
  | CONNECT (m1, m2) -> (List.append (badStationList m1) (badStationList m2))
  | AREA (id, m) -> List.filter (fun x -> x != id) (badStationList m)

let rec checkMetro: metro -> bool = fun m ->
  (=) (badStationList m) []
