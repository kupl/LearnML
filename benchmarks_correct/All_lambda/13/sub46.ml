type lambda = V of var
					 | P of var * lambda
					 | C of lambda * lambda
and var = string

let check lambda =
	let rec check2 m =
		match m with
		| V x -> [x]
		| P (x, y) -> List.filter (fun n -> not (x = n)) (check2 y)
		| C (x, y) -> (check2 x) @ (check2 y)
	in List.length (check2 lambda) = 0
