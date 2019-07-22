(* Department: Electrical and Computer Engineering *)
(* Student ID: 2010-11834 *)
(* Name: Kwonjoon Lee *)
(* Exercise #3 *)
type metro = STATION of name
			| AREA of name * metro
			| CONNECT of metro * metro
and name = string

let rec checkMetroAux (m, l) : bool = 
	match m with
	| AREA(x, y) -> checkMetroAux(y, x::l)
	| CONNECT(x, y) -> checkMetroAux(x, l) && checkMetroAux(y, l)
	| STATION x -> List.mem x l

let checkMetro (m : metro) : bool = checkMetroAux(m, [])
