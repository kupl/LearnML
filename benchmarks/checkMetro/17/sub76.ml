type metro = STATION of name
			| AREA of name * metro
			| CONNECT of metro * metro
and name = string


let rec matchName (n: name) (l: name list): bool =
	match l with
	| [] -> false
	| h::t -> if (n = h) then true
			  else (matchName n t)

let rec checkMetroRec (m: metro) (areas: name list): bool =
	match m with
	| STATION n -> (matchName n areas)
	| CONNECT (m1, m2) -> 
			((checkMetroRec m1 areas) && (checkMetroRec m2 areas))
	| AREA (n, m) -> (checkMetroRec m (n::areas))

let checkMetro (m: metro): bool =
	(checkMetroRec m [])
