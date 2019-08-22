type lambda = V of var
		| P of var * lambda
		| C of lambda * lambda
and var = string

let rec calarea (lst, st) =
	match (lst, st) with
	| ([], _) -> false
	| (hd::tl, p) ->
		if hd = p then true
		else calarea (tl, p)

let rec mycheck (m, lst) =
	match m with
	| V p -> calarea (lst, p)
	| P (p, q) ->
		mycheck (q, lst @ [p])
	| C (p, q) ->
		mycheck (p, lst) && mycheck (q, lst)

let rec check m =
	let lst = []
	in mycheck (m, lst)

