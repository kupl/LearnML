type lambda = V of var
					 | P of var * lambda
					 | C of lambda * lambda
and var = string

let check lambda =
	let rec check m =
		match m with
		| V x -> [x]
		| P (x, y) -> List.filter (fun n -> not (x = n)) (check y)
		| C (x, y) -> (check x) @ (check y)
	in List.length (check lambda) = 0
