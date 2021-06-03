type lambda = V of var
			|P of var * lambda
			|C of lambda * lambda
and var = string 

let rec inner_check: (var list*lambda) -> bool =
	fun (l, met) ->
		match met with 
			| V var ->  (List.mem var l)
			| C (m1, m2) -> (inner_check (l, m1)) && (inner_check (l, m2))
			| P (var, m) -> inner_check (var::l, m)

let check m = inner_check ([], m)	