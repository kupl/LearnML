
type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string

module SS = Set.Make(String)


let rec checkMetroS (m, s) = (match m with
	| STATION x -> (SS.mem x s)
	| AREA (name, metro) -> checkMetroS (metro, SS.add name s)
	| CONNECT (m1, m2) -> ( (checkMetroS(m1, s))&&(checkMetroS(m2, s)) )
	)

let checkMetro m = checkMetroS(m, SS.empty)


