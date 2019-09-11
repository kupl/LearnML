type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string

let rec check (m : lambda) : bool =
	let area_list = [] in
	let rec check (mtr : lambda) (al : var list) : bool =
		match mtr with
		| V n -> (List.mem n al)
		| P (n, mtr1) -> (check mtr1 (n::al))
		| C (mtr1, mtr2) -> (check mtr1 al) && (check mtr2 al) in
	(check m area_list)

