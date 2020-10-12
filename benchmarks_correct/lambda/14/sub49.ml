type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda 
and var = string

let check (met : lambda) : bool =
	let rec check_aux (l : var list) (m : lambda) : bool =
		match m with
		| V n -> List.mem n l
		| P (n1, m1) ->
			if (List.mem n1 l) then check_aux l m1
			else check_aux (n1::l) m1
		| C (m1, m2) -> (check_aux l m1) && (check_aux l m2)
	in
	check_aux [] met