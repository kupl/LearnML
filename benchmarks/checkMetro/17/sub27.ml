type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string

module SS = Set.Make(String)

let rec _c set arg = match arg with
	| STATION(name) -> let (_, b, _) = SS.split name set in b
	| AREA(name, metro) -> _c (SS.add name set) metro
	| CONNECT(metro1, metro2) -> (_c set metro1) && (_c set metro2)

let checkMetro = _c SS.empty