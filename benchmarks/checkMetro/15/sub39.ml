type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string

let rec contains ((n: name), (l: name list)): bool = (*List.contains function*)
	match l with 
	| [] -> false
	| h::t -> if ((String.compare h n) == 0) then true
			  else contains(n, t)

let rec checkMetrosub((m: metro), (l: name list)): bool = 
	match m with
	| STATION n -> contains(n, l)
	| AREA (n, m) -> checkMetrosub(m, [n] @ l)
	| CONNECT(m1, m2) -> checkMetrosub (m1, l) && checkMetrosub (m2, l)

let checkMetro (m: metro): bool = 
	match m with
	| STATION n -> false 
	| AREA (n, m) -> checkMetrosub (m, [n])
	| CONNECT (m1, m2) -> checkMetrosub (m1, []) && checkMetrosub (m2, [])