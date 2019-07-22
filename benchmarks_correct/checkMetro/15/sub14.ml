type metro = 
  STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string

let rec checkArea (e, m) =
	match m with
	| STATION s ->
		(match e with
		| [] -> false
		| h::[] -> if(s = h) then true else false
		| h::t -> if(s = h) then true else checkArea(t, STATION s))
	| CONNECT (m1, m2) ->
		if (checkArea (e, m1) && checkArea(e, m2)) then true else false
	| AREA (a, m) -> checkArea (a::e, m)

let checkMetro m = checkArea ([], m)

