type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string

let check met =
	let rec chk (x, y) =
		match x with
		| V a -> List.mem a y
		| P (a, b) -> chk (b, a::y)
		| C (a, b) -> chk (a, y) && chk (b, y) in
	chk (met, [])

