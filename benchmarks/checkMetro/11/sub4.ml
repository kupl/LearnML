
type metro = STATION of name
		   | AREA of name * metro
		   | CONNECT of metro * metro
and name = string

let rec checklist(me, li) =
	match me with
	| STATION n -> List.mem n li
	| AREA(n1, me1) -> checklist (me1, n1::li)
	| CONNECT(me1,me2) -> checklist(me1, li) && checklist(me2, li)


let rec checkMetro me =
	let l = [] in
		checklist(me, l)


