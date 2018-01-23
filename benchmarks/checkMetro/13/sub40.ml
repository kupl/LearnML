type metro = STATION of name
		| AREA of name * metro
		| CONNECT of metro * metro
and name = string

let rec calarea (lst, st) =
	match (lst, st) with
	| ([], _) -> false
	| (hd::tl, p) ->
		if hd = p then true
		else calarea (tl, p)

let rec mycheck (m, lst) =
	match m with
	| STATION p -> calarea (lst, p)
	| AREA (p, q) ->
		mycheck (q, lst @ [p])
	| CONNECT (p, q) ->
		mycheck (p, lst) && mycheck (q, lst)

let rec checkMetro m =
	let lst = []
	in mycheck (m, lst)

