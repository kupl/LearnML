type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string

let rec check (m : lambda) : bool =
	let area_list = [] in
	let rec check2 (mtr : lambda) (al : var list) : bool =
		match mtr with
		| V n -> (List.mem n al)
		| P (n, mtr1) -> (check2 mtr1 (n::al))
		| C (mtr1, mtr2) -> (check2 mtr1 al) && (check2 mtr2 al) in
	(check2 m area_list)

