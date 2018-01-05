type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string

let ncor a b = a <> b

let rec slist l = match l with
	| STATION s -> [s]
	| AREA (n, m) -> List.filter (ncor n) (slist m)
	| CONNECT (m1, m2) -> List.append (slist m1) (slist m2)
let rec checkMetro metro = match (slist metro) with
	| [] -> true
	| _ -> false
