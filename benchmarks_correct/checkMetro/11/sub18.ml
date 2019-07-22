(* 컴퓨터공학부/2009-11679/김정명/7 *)

type metro = STATION of name | AREA of name * metro | CONNECT of metro * metro
and name = string

let checkMetro m =
	let rec iter (m, l) =
		match m with
		STATION s -> 
			List.mem s l
		| AREA (a, me) -> 
			iter(me, l @ [a])
		| CONNECT (m1, m2) ->
			iter (m1, l) && iter (m2, l)
	in
	iter (m, [])
