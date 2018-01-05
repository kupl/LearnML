type name = string

type metro = STATION of name
        | AREA of name * metro
        | CONNECT of metro * metro

let rec apl (b, m) =
        match (b, m) with
        | (b, STATION a) -> if (b = a) then AREA (b, STATION a) else STATION a
        | (b, AREA(a, STATION c)) -> if (a != c && b = c) then AREA(b, STATION c) else AREA(a, STATION c)
        | (b, AREA(a, AREA(c, d))) -> AREA (a, AREA (c, apl (b, d)))
        | (b, AREA(a, CONNECT (c, d))) -> AREA (a, apl (b, CONNECT (c, d)))
        | (b, CONNECT(a, c)) -> CONNECT(apl (b, a), apl (b, c))

let rec checkMetro m =
        match m with
        | AREA(a, STATION b) -> if (a = b) then true else false
        | AREA(a, CONNECT (b, c)) -> checkMetro (AREA (a, b)) && checkMetro (AREA (a, c))
        | AREA(a, AREA(b, STATION c)) -> checkMetro (AREA (a, STATION c)) || checkMetro (AREA (b, STATION c))
        | AREA(a, AREA(b, c)) -> checkMetro (apl(b, AREA (a, c))) || checkMetro (apl(a, AREA (b, c)))
        | CONNECT(a, b) -> checkMetro a && checkMetro b
	| _ -> false
