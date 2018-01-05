type metro = STATION of name
           | AREA of name * metro
		   | CONNECT of metro * metro
and name = string

let rec checkMetro m =
  let rec cm m l =
    match m with
	  STATION n -> if (List.mem n l) then true else false
	| AREA (n, m) -> (cm m (if (List.mem n l) then l else n::l))
	| CONNECT (m1, m2) -> ((cm m1 l) && (cm m2 l))
  in
    (cm m [])
