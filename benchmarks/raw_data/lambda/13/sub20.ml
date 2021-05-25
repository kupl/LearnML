type lambda = V of var
			| P of var * lambda
			| C of lambda * lambda
and var = string

let rec check m =
	let rec is_connect m =
		match m with
		| V _ -> false
		| C (_, _) -> true
		| P (var, lambda) -> is_connect lambda in
	
	let rec get_left m =
		match m with
		| V _ -> m
		| C (l, _) -> l
		| P (var, lambda) -> P (var, get_left lambda) in
	
	let rec get_right m =
		match m with
		| V _ -> m
		| C (_, r) -> r
		| P (var, lambda) -> P (var, get_right lambda) in


	if is_connect m then (check (get_left m)) && (check (get_right m))
	else match m with
	| V _ -> false
	| C (l, r) -> (check l) && (check r) 
	| P (var, lambda) ->
		match lambda with
		| V s -> if var=s then true
					else false
		| C (left, right) -> (check (P(var, left))) && (check (P(var, right)))
		| P (var2, lambda2) -> (check (P(var, lambda2))) || (check (P(var2, lambda2)))










