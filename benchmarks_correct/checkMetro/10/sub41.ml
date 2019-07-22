type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string

let checkMetro m =
let rec checkMetro m pl = match m with
	  STATION n -> (List.mem n pl)
	| AREA (n,m1) -> (checkMetro m1 (n::pl))
	| CONNECT (m1,m2) -> (checkMetro m1 pl) && (checkMetro m2 pl)
in
	(checkMetro m [])

(**
AREA("a", STATION "a")
AREA("a", AREA("a", STATION "a"))
AREA("a", AREA("b", CONNECT(STATION "a", STATION "b")))
AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))

AREA("a", STATION "b")
AREA("a", CONNECT(STATION "a", AREA("b", STATION "c")))
AREA("a", AREA("b", CONNECT(STATION "a", STATION "c")))
*)

