type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string

let checkMetro met =
	let rec checklst (lst, x) =
		match lst with
		[] -> false
		| h::t -> if (String.compare x h)==0 then true else checklst (t, x)
	in
	let rec f (m, lst) =
		match m with
		STATION x -> checklst (lst, x)
		| AREA (x, y) -> f (y, x::lst)
		| CONNECT (x, y) -> (f (x, lst)) & (f (y, lst))
	in
	f (met, [])

