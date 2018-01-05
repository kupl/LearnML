type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string



let rec checkNameList(l, n) =
	match l with
	| [] -> false
	| hd::tl -> if hd = n then true
				else checkNameList(tl, n)

let rec checkMetroRec (l, m) =
	match m with
	| STATION n -> checkNameList(l, n)
	| AREA (n, m0) -> checkMetroRec(n::l, m0)
	| CONNECT (m0, m1) -> checkMetroRec(l, m0) && checkMetroRec(l, m1)

let rec checkMetro m =
	match m with
	| STATION n -> false
	| AREA (n, m0) -> checkMetroRec(n::[], m0)
	| CONNECT (m0, m1) -> checkMetro m0 && checkMetro m1
