type metro = 
	  STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro

and name = string

let rec checklist (arlist, stlist) =
	let rec checkst (arlst, st) =
		match (arlst, st) with
		| ([], _) -> false
		| (a::b, c) -> if a = c then true else (checkst (b, c))
	in

	match (arlist, stlist) with
	| (_, []) -> true
	| (a, b::c) -> if (checkst (a, b)) then (checklist (a, c)) else false

let rec checkMetro (met:metro) = 
	let rec makestlist (me:metro) = 
		match me with
		| STATION a -> a::[]
		| AREA (a, b) -> (makestlist b)
		| CONNECT (a, b) -> (makestlist a) @ (makestlist b)
	in

	let rec makearlist (me:metro) = 
		match me with
		| AREA (a, b) -> a::(makearlist b)
		| CONNECT (a, b) -> 	(match (a, b) with
					| (STATION c, STATION d) -> []
					| (STATION c, AREA (d, e)) -> d::(makearlist e)
					| (STATION c, CONNECT _) -> (makearlist b)
					| (AREA (c, d), STATION e) -> c::(makearlist d)
					| (AREA (c, d), AREA (e, f)) -> c::e::(makearlist d) @ (makearlist f)
					| (AREA (c, d), CONNECT _) -> c::(makearlist d) @ (makearlist b)
					| (CONNECT _, STATION c) -> (makearlist a)
					| (CONNECT _, AREA (c, d)) -> (makearlist a) @ (c::(makearlist d))
					| (CONNECT _, CONNECT _) -> (makearlist a) @ (makearlist b)
					)
		| _ -> []
	in

	match met with
	| AREA (a, b) -> (checklist ((a::(makearlist b)), (makestlist b))) 
	| CONNECT (a, b) -> (checkMetro a) && (checkMetro b)
	| _ -> false
