type name = string

type metro = STATION of name
           | AREA of name * metro
	   | CONNECT of metro * metro

let rec check m l =
  match m with
  | STATION id -> (List.mem id l)
  | CONNECT (m1, m2) -> ((check m1 l) && (check m2 l))
  | AREA (id, met) -> (check met (id::l))

let rec checkMetro m =
  match m with
  | STATION id -> false
  | CONNECT (m1, m2) -> ((checkMetro m1) && (checkMetro m2))
  | AREA (id, met) -> (check met [id])
