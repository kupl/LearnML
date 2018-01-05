type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string


let rec checkarea (a: string list) (s: string): bool = 
	match a with
	| [] -> false
	| h::t -> if (s = h) then true else (checkarea t s)

let rec areacount (m: metro) (a: string list): bool =
	match m with
	| STATION s -> (checkarea a s)
	| AREA (s, m) -> areacount m (s::a)
	| CONNECT (m1, m2) -> (areacount m1 a) && (areacount m2 a)

	
let rec checkMetro (m: metro): bool = 
	areacount m []
