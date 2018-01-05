type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string

let rec checkMetro: metro -> bool = fun x ->
	let rec listing m l = 
		(match m with
		| STATION st -> List.mem st l
		| AREA (aname, mm) -> listing mm (aname::l)
		| CONNECT (m1, m2) -> (listing m1 l) && (listing m2 l)) in
	listing x []

let x1 = AREA("a", STATION "a")
let x2 = AREA("a", AREA("a", STATION "a"))
let x3 = AREA("a", AREA("b", CONNECT(STATION "a", STATION "b")))
let x4 = AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))
let x5 = AREA("a", STATION "b")
let x6 = AREA("a", CONNECT(STATION "a", AREA("b", STATION "c")))
let x7 = AREA("a", AREA("b", CONNECT(STATION "a", STATION "c")))