type metro = STATION of name | AREA of name * metro | CONNECT of metro * metro
and name = string

let rec checklst lst m =
	match m with
	STATION id -> (List.mem id lst)
	| AREA (id, m1) -> (checklst (id::lst) m1)
	| CONNECT (m1, m2) -> (checklst lst m1) && (checklst lst m2)

let checkMetro m =
	(checklst [] m)
